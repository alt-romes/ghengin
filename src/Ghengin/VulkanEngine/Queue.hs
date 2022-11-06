{-# LANGUAGE ImportQualifiedPost #-}
module Ghengin.VulkanEngine.Queue where

import Data.Word
import Vulkan qualified as Vk

import Ghengin.VulkanEngine.QueueFamilies

getDeviceQueue :: Vk.Device
               -> QueueFamily -- Queue family index
               -> Word32      -- Queue ix
               -> IO Vk.Queue
getDeviceQueue d qf i = do
  Vk.getDeviceQueue d qf i

