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

import Control.Monad.Reader

import Data.ByteString (ByteString)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.List as L

import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan as Vk

import Ghengin.Vulkan.Device.Instance
import Ghengin.Vulkan.Device
import Ghengin.Vulkan.SwapChain
import Ghengin.Vulkan.Command
import Ghengin.Vulkan.GLFW.Window
import Ghengin.Utils

data RendererEnv = REnv { _vulkanDevice :: VulkanDevice
                        , _vulkanWindow :: VulkanWindow
                        , _vulkanSwapChain :: VulkanSwapChain
                        , _commandPool   :: Vk.CommandPool
                        , _commandBuffer :: Vk.CommandBuffer
                        }
newtype Renderer a = Renderer { unRenderer :: ReaderT RendererEnv IO a } deriving (Functor, Applicative, Monad, MonadIO, MonadReader RendererEnv)

runVulkanRenderer :: Renderer a -> IO a
runVulkanRenderer r =
  bracket
  (do

    inst <- createInstance validationLayers

    win  <- createVulkanWindow inst (800, 600) "Ghengin"

    device <- createVulkanDevice inst validationLayers deviceExtensions (rateFn win._surface)

    swapChain <- createSwapChain win device

    -- (For now) we allocate just one command pool and one command buffer
    commandPool <- createCommandPool device
    [commandBuffer] <- createCommandBuffers (device._device) commandPool

    pure (inst, REnv device win swapChain commandPool commandBuffer)

    )

  (\(inst, REnv device win swapchain commandPool _) -> do

    liftIO $ putStrLn "[Start] Clean up"

    destroyCommandPool device._device commandPool

    destroySwapChain device._device swapchain
    destroyVulkanDevice device
    destroyVulkanWindow inst win
    destroyInstance inst

    liftIO $ putStrLn "[Done] Clean up"

    )

  (\(_, renv) -> do

    runReaderT (unRenderer r) renv

    )

acquireNextImage :: Vk.Semaphore -> Renderer Int
acquireNextImage sem = ask >>= \renv ->
  fromIntegral . snd <$> Vk.acquireNextImageKHR renv._vulkanDevice._device renv._vulkanSwapChain._swapchain maxBound sem Vk.NULL_HANDLE

submitGraphicsQueue :: Vk.CommandBuffer -> Vk.Semaphore -> Vk.Semaphore -> Vk.Fence -> Renderer ()
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


presentPresentQueue :: Vk.Semaphore -> Int -> Renderer ()
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

getRenderExtent :: Renderer Vk.Extent2D
getRenderExtent = asks (._vulkanSwapChain._surfaceExtent)

getDevice :: Renderer Vk.Device
getDevice = asks (._vulkanDevice._device)

rendererBracket :: Renderer a -> (a -> Renderer b) -> (a -> Renderer c) -> Renderer c
rendererBracket x f g = Renderer $ ReaderT $ \renv ->
  bracket (runReaderT (unRenderer x) renv)
          ((`runReaderT` renv) . unRenderer . f)
          ((`runReaderT` renv) . unRenderer . g)
