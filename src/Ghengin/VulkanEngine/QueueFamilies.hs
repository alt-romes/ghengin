{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Ghengin.VulkanEngine.QueueFamilies where

import Data.Word
import Data.Vector qualified as V

import Vulkan qualified as Vk

import Ghengin.Utils

type QueueFamily = Word32

data QueueFamiliesIndices = QFI { _graphicsFamily :: QueueFamily, _presentFamily :: QueueFamily }

findQueueFamilies :: Vk.PhysicalDevice -> Vk.SurfaceKHR -> IO (Maybe QueueFamiliesIndices)
findQueueFamilies pd sr = do
  props <- Vk.getPhysicalDeviceQueueFamilyProperties pd
  graphicsF <- findM (isSuitableGraphics . snd) (V.indexed props)
  presentF  <- findM (isSuitablePresent  . fst) (V.indexed props)
  pure $ do
    ig <- graphicsF
    pg <- presentF
    pure $ QFI (fromIntegral $ fst ig) (fromIntegral $ fst pg)

  where
    isSuitableGraphics :: Vk.QueueFamilyProperties -> IO Bool
    isSuitableGraphics q = pure $ q.queueFlags .&&. Vk.QUEUE_GRAPHICS_BIT

    isSuitablePresent :: Int -> IO Bool
    isSuitablePresent  i = Vk.getPhysicalDeviceSurfaceSupportKHR pd (fromIntegral i) sr

