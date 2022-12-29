{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Ghengin.Vulkan where

import Data.Word
import Control.Exception

import GHC.Ptr

import Data.IORef
import Control.Monad.Reader

import Data.ByteString (ByteString)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.List as L

import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan as Vk
import Vulkan.Zero (zero)

import Ghengin.Vulkan.Device.Instance
import Ghengin.Vulkan.Device
import Ghengin.Vulkan.SwapChain
import Ghengin.Vulkan.Command
import Ghengin.Vulkan.Frame
import Ghengin.Vulkan.GLFW.Window
import Ghengin.Vulkan.ImmediateSubmit
import Ghengin.Utils

data RendererEnv ext =
  REnv { _instance        :: !Vk.Instance
       , _vulkanDevice    :: !VulkanDevice
       , _vulkanWindow    :: !VulkanWindow
       , _vulkanSwapChain :: !VulkanSwapChain
       , _commandPool     :: !Vk.CommandPool
       , _frames          :: !(Vector VulkanFrameData)
       , _frameInFlight   :: !(IORef Int)
       , _immediateSubmit :: !ImmediateSubmitCtx
       , _extension       :: ext
       }
newtype Renderer ext a = Renderer { unRenderer :: ReaderT (RendererEnv ext) IO a } deriving (Functor, Applicative, Monad, MonadIO, MonadReader (RendererEnv ext), MonadFail)

pattern MAX_FRAMES_IN_FLIGHT :: Word32
pattern MAX_FRAMES_IN_FLIGHT = 2 -- We want to work on multiple frames but we don't want the CPU to get too far ahead of the GPU

runVulkanRenderer :: ext -> Renderer ext a -> IO a
runVulkanRenderer ext r =
  bracket
  (do

    initGLFW

    inst <- createInstance validationLayers

    win  <- createVulkanWindow inst (1280, 720) "Ghengin"

    device <- createVulkanDevice inst validationLayers deviceExtensions (rateFn win._surface)

    swapChain <- createSwapChain win device

    imsCtx <- createImmediateSubmitCtx device

    -- (For now) we allocate just one command pool and one command buffer
    commandPool <- createCommandPool device
    cmdBuffers <- createCommandBuffers (device._device) commandPool MAX_FRAMES_IN_FLIGHT

    frameDatas <- mapM (initVulkanFrameData device._device) cmdBuffers

    frameInFlight <- newIORef 0 

    pure (REnv inst device win swapChain commandPool frameDatas frameInFlight imsCtx ext)

    )

  (\(REnv inst device win swapchain commandPool frames _ioref imsCtx _ext) -> do

    logDebug "[Start] Clean up"

    mapM_ (destroyVulkanFrameData device._device) frames

    destroyCommandPool device._device commandPool

    destroyImmediateSubmitCtx device._device imsCtx
    destroySwapChain device._device swapchain
    destroyVulkanDevice device
    destroyVulkanWindow inst win
    destroyInstance inst

    terminateGLFW

    logDebug "[Done] Clean up"

    )

  (\renv -> do

    runReaderT (unRenderer r) renv

    )



-- | Run a 'Renderer' action that depends on a command buffer and the current
-- image index of the swapchain to typically by writing to the command buffer
-- the draw calls (to the renderpasse's framebuffer responsible for that
-- image).
--
-- Afterwards, submits that command buffer to the graphics queue and then
-- presents the current image.
-- 
-- It handles all the synchronization necessary so that the command buffer
-- being written to is not written again before it is submitted, and so that we
-- wait for the GPU after having written N frames, where N = 2 means we're
-- doing double buffering, and N = 3 triple buffering.
--
-- That is, this function will block the third time it's called if N = 2 but no
-- image has been presented yet
--
-- N is 'MAX_FRAMES_IN_FLIGHT'
--
-- TODO: Figure out mismatch between current image index and current image frame.
withCurrentFramePresent :: (MonadTrans t, MonadIO (t (Renderer ext)))
                        -- => ( ∀ α. Renderer ext α -> t (Renderer ext) α ) -- ^ A lift function
                        => ( Vk.CommandBuffer
                             -> Int -- ^ Current image index
                             -> Int -- ^ Current frame index
                             -> t (Renderer ext) a
                           )
                        -> t (Renderer ext) a
withCurrentFramePresent action = do

  device <- lift $ getDevice

  currentFrameIndex <- lift $ asks (._frameInFlight) >>= liftIO . readIORef
  currentFrame <- lift $ advanceCurrentFrame
  let
      cmdBuffer = currentFrame._commandBuffer
      inFlightFence = currentFrame._renderFence
      imageAvailableSem = currentFrame._renderSemaphore
      renderFinishedSem = currentFrame._presentSemaphore

  -- Wait for the previous frame to finish
  -- Acquire an image from the swap chain
  -- Record a command buffer which draws the scene onto that image
  -- Submit the recorded command buffer
  -- Present the swap chain image 
  _ <- Vk.waitForFences device [inFlightFence] True maxBound
  Vk.resetFences device [inFlightFence]

  i <- lift $ acquireNextImage imageAvailableSem

  Vk.resetCommandBuffer cmdBuffer zero

  a <- action cmdBuffer i currentFrameIndex

  -- Finally, submit and present
  lift $ submitGraphicsQueue cmdBuffer imageAvailableSem renderFinishedSem inFlightFence

  lift $ presentPresentQueue renderFinishedSem i

  pure a


-- | Get the current frame and increase the frame index (i.e. the next call to 'advanceCurrentFrame' will return the next frame)
advanceCurrentFrame :: Renderer ext VulkanFrameData
advanceCurrentFrame = do
  nref <- asks (._frameInFlight)
  n    <- liftIO $ readIORef nref
  liftIO $ modifyIORef' nref (\x -> (x + 1) `rem` fromIntegral MAX_FRAMES_IN_FLIGHT)
  asks ((V.! n) . (._frames))


acquireNextImage :: Vk.Semaphore -> Renderer ext Int
acquireNextImage sem = ask >>= \renv ->
  fromIntegral . snd <$> Vk.acquireNextImageKHR renv._vulkanDevice._device renv._vulkanSwapChain._swapchain maxBound sem Vk.NULL_HANDLE

submitGraphicsQueue :: Vk.CommandBuffer -> Vk.Semaphore -> Vk.Semaphore -> Vk.Fence -> Renderer ext ()
submitGraphicsQueue cb sem1 sem2 fence = do
  let
    submitInfo = Vk.SubmitInfo { next = ()
                                -- We want to wait with writing colors to the image until it's available
                               , waitSemaphores = [sem1]
                               , waitDstStageMask = [Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
                                -- Semaphores to signal when we are done
                               , signalSemaphores = [sem2]
                               , commandBuffers = [ cb.commandBufferHandle ]
                               }
  graphicsQueue <- asks (._vulkanDevice._graphicsQueue)
  Vk.queueSubmit graphicsQueue [Vk.SomeStruct submitInfo] fence


presentPresentQueue :: Vk.Semaphore -> Int -> Renderer ext ()
presentPresentQueue sem imageIndex = do
  swpc <- asks (._vulkanSwapChain._swapchain)
  let presentInfo = Vk.PresentInfoKHR { next = ()
                                      , waitSemaphores = [sem]
                                      , swapchains = [swpc]
                                      , imageIndices = [fromIntegral imageIndex]
                                      , results = nullPtr
                                      }
  presentQueue <- asks (._vulkanDevice._presentQueue)
  _ <- Vk.queuePresentKHR presentQueue presentInfo
  pure ()

validationLayers :: Vector ByteString
validationLayers = [ "VK_LAYER_KHRONOS_validation"
                   ]

deviceExtensions :: Vector ByteString
deviceExtensions = [ Vk.KHR_SWAPCHAIN_EXTENSION_NAME
                   ]

rateFn :: Vk.SurfaceKHR -> DeviceRateFunction
rateFn surface d = do
  props  <- Vk.getPhysicalDeviceProperties d
  _feats <- Vk.getPhysicalDeviceFeatures d
  (_, extensionProps) <- Vk.enumerateDeviceExtensionProperties d Nothing

  -- These can't be null
  (_, surfaceFormats)      <- Vk.getPhysicalDeviceSurfaceFormatsKHR d surface
  (_, surfacePresentModes) <- Vk.getPhysicalDeviceSurfacePresentModesKHR d surface
  
  queueFamilies <- findQueueFamilies d surface

  pure $ do
    let s1 = if props.deviceType == Vk.PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
               then 1000 else 0

        s2 = props.limits.maxImageDimension2D

        swapChainAdequate   = not (null surfaceFormats) && not (null surfacePresentModes)
        extensionsSupported = not . null $ L.intersect (V.toList deviceExtensions) (V.toList $ V.map (.extensionName) extensionProps)

    -- If the app couldn't function without geometry shaders
    -- guard feats.geometryShader

    (graphicsF, presentF) <- queueFamilies
    guard swapChainAdequate
    guard extensionsSupported
    pure (s1 + fromIntegral s2, graphicsF, presentF)

  where
    findQueueFamilies :: Vk.PhysicalDevice -> Vk.SurfaceKHR -> IO (Maybe (Word32, Word32))
    findQueueFamilies pd sr = do
      props <- Vk.getPhysicalDeviceQueueFamilyProperties pd
      graphicsF <- findM (isSuitableGraphics . snd) (V.indexed props)
      presentF  <- findM (isSuitablePresent  . fst) (V.indexed props)
      pure $ do
        ig <- graphicsF
        pg <- presentF
        pure (fromIntegral $ fst ig, fromIntegral $ fst pg)

      where
        isSuitableGraphics :: Vk.QueueFamilyProperties -> IO Bool
        isSuitableGraphics q = pure $ q.queueFlags .&&. Vk.QUEUE_GRAPHICS_BIT

        isSuitablePresent :: Int -> IO Bool
        isSuitablePresent  i = Vk.getPhysicalDeviceSurfaceSupportKHR pd (fromIntegral i) sr



-- :| Utils |:

getRenderExtent :: Renderer ext Vk.Extent2D
getRenderExtent = asks (._vulkanSwapChain._surfaceExtent)

getDevice :: Renderer ext Vk.Device
getDevice = asks (._vulkanDevice._device)

rendererBracket :: Renderer ext a -> (a -> Renderer ext b) -> (a -> Renderer ext c) -> Renderer ext c
rendererBracket x f g = Renderer $ ReaderT $ \renv ->
  bracket (runReaderT (unRenderer x) renv)
          ((`runReaderT` renv) . unRenderer . f)
          ((`runReaderT` renv) . unRenderer . g)

-- | Submit a command to the immediate submit command buffer that synchronously
-- submits it to the graphics queue
immediateSubmit :: Command (Renderer ext) -> Renderer ext ()
immediateSubmit cmd = do
  device <- asks (._vulkanDevice)
  ims <- asks (._immediateSubmit)
  immediateSubmit' device._device device._graphicsQueue ims cmd

