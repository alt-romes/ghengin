{-# LANGUAGE RecordWildCards #-}
module Ghengin.VulkanEngine.Synchronization where

import Vulkan.Zero (zero)
import qualified Vulkan as Vk

createSyncObjects :: Vk.Device -> IO (Vk.Semaphore, Vk.Semaphore, Vk.Fence)
createSyncObjects dev = do
  let
    semaphoreInfo = Vk.SemaphoreCreateInfo { next = (), flags = zero }
    fenceInfo = Vk.FenceCreateInfo { next = (), flags = Vk.FENCE_CREATE_SIGNALED_BIT }

  imageAvailableSem <- Vk.createSemaphore dev semaphoreInfo Nothing
  renderFinishedSem <- Vk.createSemaphore dev semaphoreInfo Nothing
  inFlightFence <- Vk.createFence dev fenceInfo Nothing
  pure (imageAvailableSem, renderFinishedSem, inFlightFence)

destroySem :: Vk.Device -> Vk.Semaphore -> IO ()
destroySem dev sem = Vk.destroySemaphore dev sem Nothing

destroyFence :: Vk.Device -> Vk.Fence -> IO ()
destroyFence dev sem = Vk.destroyFence dev sem Nothing
