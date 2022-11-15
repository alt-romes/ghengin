{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Ghengin.Vulkan where

import Data.Coerce
import Data.Word
import Control.Exception

import Control.Monad.Reader

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Vector (Vector)
import qualified Data.Vector as V

import qualified Vulkan as Vk

import Ghengin.Vulkan.Device.Instance
import Ghengin.Vulkan.Device
import Ghengin.Utils
import Ghengin.Vulkan.GLFW.Window

data RendererEnv = REnv { _vulkanDevice :: VulkanDevice
                        , _vulkanWindow :: VulkanWindow
                        }
newtype Renderer a = Renderer { unRenderer :: ReaderT RendererEnv IO a }

runVulkanRenderer :: Renderer () -> IO ()
runVulkanRenderer r =
  bracket
  (do

    inst <- createInstance validationLayers

    win  <- createVulkanWindow inst (800, 600) "Ghengin"

    device <- createVulkanDevice inst validationLayers deviceExtensions rateFn

    pure (inst, REnv device win)

    )

  (\(_, renv) -> do

    runReaderT (unRenderer r) renv

    )

  (\(inst, REnv device win) -> do

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

rateFn :: DeviceRateFunction
rateFn d = do
  props <- Vk.getPhysicalDeviceProperties d
  feats <- Vk.getPhysicalDeviceFeatures d
  (_, extensionProps) <- Vk.enumerateDeviceExtensionProperties d Nothing

  queueFamilies <- findQueueFamilies d sr
  -- (SCSD _ forms pmodes) <- querySwapChainSupport d sr

  let
      s1 = if props.deviceType == Vk.PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
             then 1000
             else 0

      s2 = props.limits.maxImageDimension2D

      c1 = isJust queueFamilies

      -- extensionsSupported = not . null $ L.intersect (V.toList deviceExtensions) (V.toList $ V.map (.extensionName) extensionProps)

      swapChainAdequate = not (null forms) && not (null pmodes)

     -- If the app couldn't function without geometry shaders
      _c8 = feats.geometryShader

  if c1 && extensionsSupported && swapChainAdequate
     then pure (s1 + fromIntegral s2, d)
     else pure (0, d)

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


