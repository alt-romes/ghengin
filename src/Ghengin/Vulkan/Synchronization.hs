{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Ghengin.Vulkan.Synchronization where

import Control.Monad.IO.Class

import Vulkan.Zero (zero)
import qualified Vulkan as Vk

-- | Create a Semaphore
createSemaphore :: MonadIO m => Vk.Device -> m Vk.Semaphore
createSemaphore dev = 
  let semaphoreInfo = Vk.SemaphoreCreateInfo { next = (), flags = zero }
   in Vk.createSemaphore dev semaphoreInfo Nothing

-- | Create a Fence.
-- If the first argument is 'True' the fence is already signaled when created
createFence :: MonadIO m => Vk.Device -> Bool -> m Vk.Fence
createFence dev isSignaled =
  let fenceInfo = Vk.FenceCreateInfo { next = (), flags = if isSignaled then Vk.FENCE_CREATE_SIGNALED_BIT else zero }
   in Vk.createFence dev fenceInfo Nothing

destroySem :: MonadIO m => Vk.Device -> Vk.Semaphore -> m ()
destroySem dev sem = Vk.destroySemaphore dev sem Nothing

destroyFence :: MonadIO m => Vk.Device -> Vk.Fence -> m ()
destroyFence dev fen = Vk.destroyFence dev fen Nothing


