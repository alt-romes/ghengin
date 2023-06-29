{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE LinearTypes #-}
module Ghengin.Vulkan.Renderer.Synchronization where

import Prelude.Linear hiding (zero)
import Control.Functor.Linear
import Control.Monad.IO.Class.Linear

import Vulkan.Zero (zero)
import qualified Vulkan as Vk

import qualified Unsafe.Linear as Unsafe

import Ghengin.Vulkan.Renderer.Device

-- | Create a Semaphore
createSemaphore :: MonadIO m => VulkanDevice ⊸ m (Vk.Semaphore, VulkanDevice)
createSemaphore = Unsafe.toLinear $ \dev ->
  let semaphoreInfo = Vk.SemaphoreCreateInfo { next = (), flags = zero }
   in (,dev) <$> liftSystemIO (Vk.createSemaphore dev._device semaphoreInfo Nothing)

-- | Create a Fence.
-- If the first argument is 'True' the fence is already signaled when created
createFence :: MonadIO m => VulkanDevice ⊸ Bool -> m (Vk.Fence, VulkanDevice)
createFence = Unsafe.toLinear $ \dev isSignaled ->
  let fenceInfo = Vk.FenceCreateInfo { next = (), flags = if isSignaled then Vk.FENCE_CREATE_SIGNALED_BIT else zero }
   in (,dev) <$> liftSystemIO (Vk.createFence dev._device fenceInfo Nothing)

destroySem :: MonadIO m => VulkanDevice ⊸ Vk.Semaphore ⊸ m VulkanDevice
destroySem   = Unsafe.toLinear2 $ \dev sem -> dev <$ liftSystemIO (Vk.destroySemaphore dev._device sem Nothing)

destroyFence :: MonadIO m => VulkanDevice ⊸ Vk.Fence ⊸ m VulkanDevice
destroyFence = Unsafe.toLinear2 $ \dev fen -> dev <$ liftSystemIO (Vk.destroyFence dev._device fen Nothing)

