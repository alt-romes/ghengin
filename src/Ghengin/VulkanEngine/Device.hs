{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Ghengin.VulkanEngine.Device where

import Data.Vector     qualified as V
import Data.ByteString qualified as BS

import Vulkan qualified as Vk
import Vulkan.CStruct.Extends qualified as VkC

import Ghengin.VulkanEngine.QueueFamilies

createLogicalDevice :: V.Vector (BS.ByteString) -- ^ Validation layers
                    -> Vk.PhysicalDevice
                    -> QueueFamiliesIndices
                    -> IO (Vk.Device)
createLogicalDevice validationLayers physicalDevice qfi = do
  Vk.createDevice physicalDevice (dci (dqci qfi)) Nothing
    where
      dci :: Vk.DeviceQueueCreateInfo '[] -> Vk.DeviceCreateInfo '[]
      dci qci = Vk.DeviceCreateInfo {..} where
                next = ()
                flags = Vk.DeviceCreateFlags 0
                queueCreateInfos = [VkC.SomeStruct qci]
                enabledLayerNames = validationLayers
                enabledExtensionNames = []
                enabledFeatures = Nothing

      dqci :: QueueFamiliesIndices -> Vk.DeviceQueueCreateInfo '[]
      dqci (QFI ix) = Vk.DeviceQueueCreateInfo {..} where
                next = ()
                flags = Vk.DeviceQueueCreateFlagBits 0
                queueFamilyIndex = ix
                queuePriorities  = [1]


destroyLogicalDevice :: Vk.Device -> IO ()
destroyLogicalDevice d = Vk.destroyDevice d Nothing


