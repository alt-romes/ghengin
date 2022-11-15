{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Ghengin.Vulkan.Synchronization where

import Vulkan.Zero (zero)
import qualified Vulkan as Vk

import Ghengin.Vulkan

withSemaphore :: (Vk.Semaphore -> Renderer a) -> Renderer a
withSemaphore f = rendererBracket createSemaphore destroySem f

withFence :: Bool -> (Vk.Fence -> Renderer a) -> Renderer a
withFence isSignaled f = rendererBracket (createFence isSignaled) destroyFence f

-- | Create a Semaphore
createSemaphore :: Renderer Vk.Semaphore
createSemaphore = getDevice >>= \dev ->
  let semaphoreInfo = Vk.SemaphoreCreateInfo { next = (), flags = zero }
   in Vk.createSemaphore dev semaphoreInfo Nothing

-- | Create a Fence.
-- If the first argument is 'True' the fence is already signaled when created
createFence :: Bool -> Renderer Vk.Fence
createFence isSignaled = getDevice >>= \dev ->
  let fenceInfo = Vk.FenceCreateInfo { next = (), flags = if isSignaled then Vk.FENCE_CREATE_SIGNALED_BIT else zero }
   in Vk.createFence dev fenceInfo Nothing

destroySem :: Vk.Semaphore -> Renderer ()
destroySem sem = getDevice >>= \dev -> Vk.destroySemaphore dev sem Nothing

destroyFence :: Vk.Fence -> Renderer ()
destroyFence fen = getDevice >>= \dev -> Vk.destroyFence dev fen Nothing


