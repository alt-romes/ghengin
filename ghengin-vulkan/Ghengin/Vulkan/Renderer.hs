{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
module Ghengin.Vulkan.Renderer where

import GHC.TypeLits
import qualified Prelude
import Prelude.Linear hiding (zero, IO)
import qualified Unsafe.Linear as Unsafe

import Control.Functor.Linear as Linear
import qualified Data.Functor.Linear as Data.Linear
import Control.Monad.IO.Class.Linear
import System.IO.Linear

import Data.Bits
import Data.Word

import qualified Control.Exception
import qualified Control.Monad

import GHC.Ptr

import Data.ByteString (ByteString)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.V.Linear.Internal as VI
import qualified Data.List as L

import qualified Vulkan.Extensions
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan as Vk
import Vulkan.Zero (zero)

import Ghengin.Core.Log
import Ghengin.Vulkan.Renderer.Device.Instance
import Ghengin.Vulkan.Renderer.Device
import Ghengin.Vulkan.Renderer.SwapChain
import Ghengin.Vulkan.Renderer.Command
import Ghengin.Vulkan.Renderer.Frame
import Ghengin.Vulkan.Renderer.GLFW.Window
import Ghengin.Vulkan.Renderer.ImmediateSubmit
import Ghengin.Vulkan.Renderer.Kernel
import qualified System.IO.Linear as Linear

-- ROMES: Eventually thikn about bracketing again, but for linear types to work simply get rid of it
-- ROMES:TODO: Make runRenderer an hsig in Ghengin.Core.Renderer
runRenderer :: Renderer a -> Linear.IO a
runRenderer r = Linear.do

  -- Initialisation
  -----------------
  glfwtoken <- initGLFW

  inst <- createInstance validationLayers

  (win, inst) <- createVulkanWindow inst (1280, 720) "Ghengin"

  (Ur rateFunc, win) <- pure $ Unsafe.toLinear (\w -> (Ur (rateFn w._surface), w)) win

  (device, inst) <- createVulkanDevice inst validationLayers deviceExtensions rateFunc

  (swapchain, win, device) <- createSwapChain win device

  (imsCtx, device) <- createImmediateSubmitCtx device

  -- (For now) we allocate just one command pool and one command buffer
  (commandPool, device) <- createCommandPool device
  (cmdBuffers, device, commandPool) <- createCommandBuffers @MAX_FRAMES_IN_FLIGHT_T device commandPool

  (frames, device) <- runStateT (Data.Linear.mapM (StateT . initVulkanFrameData) cmdBuffers) device

  -- Can we get rid of these and the other IO refs in the game loop?
  Ur frameInFlight <- newIORef (0 :: Int)

  (Ur logger,cleanupLogger)  <- newLogger

  -- Run renderer
  ---------------
  (a, REnv inst device win swapchain commandPool' frames' imsCtx (Ur logger))
    <- runStateT (unRenderer r) (REnv inst device win swapchain commandPool frames imsCtx (Ur logger))

  -- Terminate
  ------------
  liftSystemIO $ logger "[Start] Clean up"

  (vunit, device) <- runStateT (Data.Linear.mapM (\f -> StateT (fmap ((),) . destroyVulkanFrameData f)) frames') device
  pure $ consumeUnits vunit

  device <- destroyCommandPool device commandPool'

  device <- destroyImmediateSubmitCtx device imsCtx
  device <- destroySwapChain device swapchain
  destroyVulkanDevice device
  inst <- destroyVulkanWindow inst win
  destroyInstance inst

  terminateGLFW glfwtoken

  liftSystemIO $ logger "[Done] Clean up"

  cleanupLogger

  pure a



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
withCurrentFramePresent :: (MonadTrans t, MonadIO (t (Renderer)))
                        -- => ( ∀ α. Renderer α -> t (Renderer ext) α ) -- ^ A lift function
                        => Int -- ^ Current frame index
                        -> ( Vk.CommandBuffer
                              ⊸ Int -- ^ Current image index
                             -> t (Renderer) (a, Vk.CommandBuffer)
                           )
                         ⊸ t (Renderer) a
withCurrentFramePresent currentFrameIndex action = Linear.do

  Ur unsafeCurrentFrame <- lift $ renderer $ Unsafe.toLinear $ \renv -> pure (Ur (case renv._frames of (VI.V vec) -> vec V.! currentFrameIndex),renv)
  -- These are all unsafe too
  let
      cmdBuffer         = unsafeCurrentFrame._commandBuffer
      inFlightFence     = unsafeCurrentFrame._renderFence
      imageAvailableSem = unsafeCurrentFrame._renderSemaphore
      renderFinishedSem = unsafeCurrentFrame._presentSemaphore

  -- Wait for the previous frame to finish
  -- Acquire an image from the swap chain
  -- Record a command buffer which draws the scene onto that image
  -- Submit the recorded command buffer
  -- Present the swap chain image 
  lift $ unsafeUseDevice (\device -> do
    Vk.waitForFences device [inFlightFence] True maxBound
    Vk.resetFences device [inFlightFence]
                  )

  (Ur i, imageAvailableSem') <- lift $ acquireNextImage imageAvailableSem

  liftSystemIO $ Vk.resetCommandBuffer cmdBuffer zero

  (a, cmdBuffer') <- action cmdBuffer i

  -- Finally, submit and present
  (cmdBuffer'',imageAvailableSem'', renderFinishedSem', inFlightFence')
    <- lift $ submitGraphicsQueue cmdBuffer' imageAvailableSem' renderFinishedSem inFlightFence

  renderFinishedSem'' <- lift $ presentPresentQueue renderFinishedSem' i

  -- Forget these as they're in the renderer environment still, remember we got them unsafely in the first place...
  Unsafe.toLinearN @4 (\_ _ _ _ -> pure ()) cmdBuffer'' imageAvailableSem'' renderFinishedSem'' inFlightFence'

  pure a


-- | Get the current frame and increase the frame index (i.e. the next call to 'advanceCurrentFrame' will return the next frame)
-- advanceCurrentFrame :: Renderer VulkanFrameData
-- advanceCurrentFrame = do
--   nref <- asks (._frameInFlight)
--   n    <- liftIO $ readIORef nref
--   liftIO $ modifyIORef' nref (\x -> (x + 1) `rem` fromIntegral MAX_FRAMES_IN_FLIGHT)
--   asks ((V.! n) . (._frames))

acquireNextImage :: Vk.Semaphore ⊸ Renderer (Ur Int, Vk.Semaphore)
acquireNextImage = Unsafe.toLinear $ \sem -> Linear.do
  Ur renv <- renderer (Unsafe.toLinear $ \renv -> pure (Ur renv, renv))
  i <- liftSystemIOU $ Prelude.fromIntegral Prelude.. Prelude.snd Prelude.<$> Vk.acquireNextImageKHR renv._vulkanDevice._device renv._vulkanSwapChain._swapchain maxBound sem Vk.NULL_HANDLE
  pure (i, sem)

submitGraphicsQueue :: Vk.CommandBuffer ⊸ Vk.Semaphore ⊸ Vk.Semaphore ⊸ Vk.Fence ⊸ Renderer (Vk.CommandBuffer, Vk.Semaphore, Vk.Semaphore, Vk.Fence)
submitGraphicsQueue = Unsafe.toLinearN @4 \cb sem1 sem2 fence -> Linear.do
  let
    submitInfo = Vk.SubmitInfo { next = ()
                                -- We want to wait with writing colors to the image until it's available
                               , waitSemaphores = [sem1]
                               , waitDstStageMask = [Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
                                -- Semaphores to signal when we are done
                               , signalSemaphores = [sem2]
                               , commandBuffers = [ cb.commandBufferHandle ]
                               }
               -- this pattern should be used as unsafeAsk (make utils...)
  Ur gqueue <- renderer (Unsafe.toLinear $ \renv -> pure (Ur renv._vulkanDevice._graphicsQueue, renv))
  liftSystemIO $ Vk.queueSubmit gqueue [Vk.SomeStruct submitInfo] fence
  pure (cb, sem1, sem2, fence)


presentPresentQueue :: Vk.Semaphore ⊸ Int -> Renderer Vk.Semaphore
presentPresentQueue = Unsafe.toLinear \sem imageIndex -> Linear.do
  (Ur swpc, Ur presentQueue) <- renderer (Unsafe.toLinear $ \renv -> pure ((Ur renv._vulkanSwapChain._swapchain, Ur renv._vulkanDevice._presentQueue), renv))
  let presentInfo = Vk.PresentInfoKHR { next = ()
                                      , waitSemaphores = [sem]
                                      , swapchains = [swpc]
                                      , imageIndices = [fromIntegral imageIndex]
                                      , results = nullPtr
                                      }
  Ur _ <- liftSystemIOU $ Vk.queuePresentKHR presentQueue presentInfo
  pure sem

validationLayers :: Vector ByteString
validationLayers = [
-- We must be careful: if we're releasing our game bundled with the dynamic
-- libraries (e.g. using ghengin-dist-macos), we cannot use a validation layer
-- because those aren't bundled.
#ifdef DEVELOPMENT
                    "VK_LAYER_KHRONOS_validation"
#endif
                   ]

deviceExtensions :: Vector ByteString
deviceExtensions = [ Vk.KHR_SWAPCHAIN_EXTENSION_NAME

                     -- required at least from 1.3 with MoltenVk
                   , Vk.KHR_PORTABILITY_SUBSET_EXTENSION_NAME
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

  Prelude.pure $ do
    let s1 = if props.deviceType Prelude.== Vk.PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
               then 1000 else 0

        s2 = props.limits.maxImageDimension2D

        swapChainAdequate   = Prelude.not (Prelude.null surfaceFormats) Prelude.&& Prelude.not (null surfacePresentModes)
        extensionsSupported = Prelude.not $ Prelude.null $ L.intersect (V.toList deviceExtensions) (V.toList $ V.map (.extensionName) extensionProps)

    -- If the app couldn't function without geometry shaders
    -- guard feats.geometryShader

    (graphicsF, presentF) <- queueFamilies
    Control.Monad.guard swapChainAdequate
    Control.Monad.guard extensionsSupported
    Prelude.pure (s1 Prelude.+ Prelude.fromIntegral s2, graphicsF, presentF)

  where
    findQueueFamilies :: Vk.PhysicalDevice -> Vk.SurfaceKHR -> Prelude.IO (Maybe (Word32, Word32))
    findQueueFamilies pd sr = do
      props <- Vk.getPhysicalDeviceQueueFamilyProperties pd
      graphicsF <- findM (isSuitableGraphics Prelude.. snd) (V.indexed props)
      presentF  <- findM (isSuitablePresent  Prelude.. fst) (V.indexed props)
      Prelude.pure $ do
        ig <- graphicsF
        pg <- presentF
        Prelude.pure (fromIntegral $ fst ig, fromIntegral $ fst pg)

      where
        isSuitableGraphics :: Vk.QueueFamilyProperties -> Prelude.IO Bool
        isSuitableGraphics q = Prelude.pure $ q.queueFlags .&&. Vk.QUEUE_GRAPHICS_BIT

        isSuitablePresent :: Int -> Prelude.IO Bool
        isSuitablePresent  i = Vk.getPhysicalDeviceSurfaceSupportKHR pd (Prelude.fromIntegral i) sr


-- :| Utils |:

-- getRenderExtent :: Renderer Vk.Extent2D
-- getRenderExtent = asks (._vulkanSwapChain._surfaceExtent)

-- getDevice :: Renderer Vk.Device
-- getDevice = asks (._vulkanDevice._device)

-- rendererBracket :: Renderer a -> (a -> Renderer ext b) -> (a -> Renderer ext c) -> Renderer ext c
-- rendererBracket x f g = Renderer $ ReaderT $ \renv ->
--   bracket (runReaderT (unRenderer x) renv)
--           ((`runReaderT` renv) . unRenderer . f)
--           ((`runReaderT` renv) . unRenderer . g)


(.&&.) :: Bits a => a -> a -> Bool
x .&&. y = (Prelude./= zeroBits) (x .&. y)

-- | Returns the first element in a foldable structure for that the
-- monadic predicate holds true, and @Nothing@ if no such element
-- exists.
findM :: ∀ m t a. (Prelude.Monad m, Prelude.Foldable t)
      => (a -> m Bool) -> t a -> m (Maybe a)
findM p = Prelude.foldr go (Prelude.pure Nothing)
  where
    go :: a -> m (Maybe a) -> m (Maybe a)
    go x acc = do
      b <- p x
      if b then Prelude.pure (Just x) else acc

