{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
module Ghengin.VulkanEngine.PhysicalDevice where

import Data.Maybe
import Data.Ord
import Data.List qualified as L
import Data.Vector qualified as V
import Data.ByteString qualified as BS

import Vulkan qualified as Vk

import Ghengin.VulkanEngine.SwapChain
import Ghengin.VulkanEngine.QueueFamilies

rateDevice :: V.Vector BS.ByteString -- ^ Device Extensions
           -> Vk.PhysicalDevice
           -> Vk.SurfaceKHR
           -> IO (Int, Vk.PhysicalDevice)
rateDevice deviceExtensions d sr = do
  props <- Vk.getPhysicalDeviceProperties d
  feats <- Vk.getPhysicalDeviceFeatures d
  (_, extensionProps) <- Vk.enumerateDeviceExtensionProperties d Nothing

  queueFamilies <- findQueueFamilies d sr
  (SCSD _ forms pmodes) <- querySwapChainSupport d sr

  let
      s1 = if props.deviceType == Vk.PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
             then 1000
             else 0

      s2 = props.limits.maxImageDimension2D

      c1 = isJust queueFamilies

      extensionsSupported = not . null $ L.intersect (V.toList deviceExtensions) (V.toList $ V.map (.extensionName) extensionProps)

      swapChainAdequate = not (null forms) && not (null pmodes)

     -- If the app couldn't function without geometry shaders
      _c8 = feats.geometryShader

  if c1 && extensionsSupported && swapChainAdequate
     then pure (s1 + fromIntegral s2, d)
     else pure (0, d)


pickPhysicalDevice :: V.Vector BS.ByteString -- ^ Device Extensions
                   -> Vk.Instance
                   -> Vk.SurfaceKHR
                   -> IO Vk.PhysicalDevice
pickPhysicalDevice deviceExtensions vkInst vkSurf = do
  (_, dvs) <- Vk.enumeratePhysicalDevices vkInst
  L.sortOn (Down . fst) . V.toList <$> traverse (\x -> rateDevice deviceExtensions x vkSurf) dvs >>= \case
    []  -> fail "Failed to find GPUs with Vulkan support!"
    (rating,device):_
      | rating < 1 -> fail "Failed to find a suitable GPU!"
      | otherwise  -> pure device

