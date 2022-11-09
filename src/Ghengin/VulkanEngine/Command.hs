{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
module Ghengin.VulkanEngine.Command where

import qualified Vulkan as Vk

import Ghengin.VulkanEngine.QueueFamilies

createCommandPool :: Vk.Device -> QueueFamiliesIndices -> IO Vk.CommandPool
createCommandPool device qfi = do

  let
    poolInfo = Vk.CommandPoolCreateInfo { flags = Vk.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
                                        , queueFamilyIndex = qfi._graphicsFamily
                                        }

  Vk.createCommandPool device poolInfo Nothing


destroyCommandPool :: Vk.Device -> Vk.CommandPool -> IO ()
destroyCommandPool dev pool = Vk.destroyCommandPool dev pool Nothing

-- createCommandBuffer :: 
