{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Ghengin.VulkanEngine.QueueFamilies where

import Data.Word
import Data.Vector qualified as V

import Vulkan qualified as Vk

import Ghengin.Utils

type QueueFamily = Word32

data QueueFamiliesIndices = QFI { _graphicsFamily :: QueueFamily }

findQueueFamilies :: Vk.PhysicalDevice -> IO (Maybe QueueFamiliesIndices)
findQueueFamilies pd = do
  props <- Vk.getPhysicalDeviceQueueFamilyProperties pd
  let suitable = V.filter (isQueueFamilySuitable . snd) (V.indexed props)
  if V.null suitable
     then pure Nothing
     else pure $ Just $ QFI $ fromIntegral $ fst $ V.head suitable

  where
    isQueueFamilySuitable :: Vk.QueueFamilyProperties -> Bool
    isQueueFamilySuitable q = q.queueFlags .&&. Vk.QUEUE_GRAPHICS_BIT


