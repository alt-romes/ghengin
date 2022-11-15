{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Ghengin.Vulkan where

import Data.Maybe
import Data.Word
import Control.Exception

import Control.Monad.Reader

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.List as L

import qualified Vulkan as Vk

import Ghengin.Vulkan.Device.Instance
import Ghengin.Vulkan.Device
import Ghengin.Vulkan.SwapChain
import Ghengin.Vulkan.GLFW.Window
import Ghengin.Utils

data RendererEnv = REnv { _vulkanDevice :: VulkanDevice
                        , _vulkanWindow :: VulkanWindow
                        , _vulkanSwapChain :: VulkanSwapChain
                        }
newtype Renderer a = Renderer { unRenderer :: ReaderT RendererEnv IO a }

runVulkanRenderer :: Renderer () -> IO ()
runVulkanRenderer r =
  bracket
  (do

    inst <- createInstance validationLayers

    win  <- createVulkanWindow inst (800, 600) "Ghengin"

    device <- createVulkanDevice inst validationLayers deviceExtensions (rateFn win._surface)

    swapChain <- createSwapChain win device

    pure (inst, REnv device win swapChain)

    )

  (\(_, renv) -> do

    runReaderT (unRenderer r) renv

    )

  (\(inst, REnv device win swapchain) -> do

    destroySwapChain device._device swapchain
    destroyVulkanDevice device
    destroyVulkanWindow inst win
    destroyInstance inst

    )


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


