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

import Vulkan qualified as Vk

import Ghengin.VulkanEngine.QueueFamilies

rateDevice :: Vk.PhysicalDevice -> IO (Int, Vk.PhysicalDevice)
rateDevice d = do
  props <- Vk.getPhysicalDeviceProperties d
  feats <- Vk.getPhysicalDeviceFeatures d
  queueFamilies <- findQueueFamilies d

  let
      s1 = if props.deviceType == Vk.PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
             then 1000
             else 0
      s2 = props.limits.maxImageDimension2D

      c1 = isJust queueFamilies

     -- If the app couldn't function without geometry shaders
      _c2 = feats.geometryShader

  if c1
     then pure (s1 + fromIntegral s2, d)
     else pure (0, d)


pickPhysicalDevice :: Vk.Instance -> IO Vk.PhysicalDevice
pickPhysicalDevice vkInst = do
  (_, dvs) <- Vk.enumeratePhysicalDevices vkInst
  L.sortOn (Down . fst) . V.toList <$> traverse rateDevice dvs >>= \case
    []  -> fail "Failed to find GPUs with Vulkan support!"
    (rating,device):_
      | rating < 1 -> fail "Failed to find a suitable GPU!"
      | otherwise  -> pure device

