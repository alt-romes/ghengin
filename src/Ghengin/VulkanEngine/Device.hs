{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Ghengin.VulkanEngine.Device where

import Data.Word
import Data.Set        qualified as S
import Data.Vector     qualified as V
import Data.ByteString qualified as BS

import Vulkan qualified as Vk
import Vulkan.CStruct.Extends qualified as VkC

import Ghengin.VulkanEngine.QueueFamilies

createLogicalDevice :: V.Vector (BS.ByteString) -- ^ Validation layers
                    -> V.Vector BS.ByteString -- ^ Device Extensions
                    -> Vk.PhysicalDevice
                    -> QueueFamiliesIndices
                    -> IO (Vk.Device)
createLogicalDevice validationLayers deviceExtensions physicalDevice qfi = do
  Vk.createDevice physicalDevice (dci (dqci qfi)) Nothing
    where
      dci :: V.Vector (VkC.SomeStruct Vk.DeviceQueueCreateInfo) -> Vk.DeviceCreateInfo '[]
      dci qci = Vk.DeviceCreateInfo {..} where
                next = ()
                flags = Vk.DeviceCreateFlags 0
                queueCreateInfos = qci
                enabledLayerNames = validationLayers
                enabledExtensionNames = deviceExtensions
                enabledFeatures = Nothing

      dqci :: QueueFamiliesIndices -> V.Vector (VkC.SomeStruct Vk.DeviceQueueCreateInfo)
      dqci (QFI ix1 ix2) = V.fromList $ map ix' $ S.toList ([ix1, ix2] :: S.Set Word32)
        where
          ix' ix = VkC.SomeStruct $ Vk.DeviceQueueCreateInfo {..} where
                next = ()
                flags = Vk.DeviceQueueCreateFlagBits 0
                queueFamilyIndex = ix
                queuePriorities  = [1]


destroyLogicalDevice :: Vk.Device -> IO ()
destroyLogicalDevice d = Vk.destroyDevice d Nothing


