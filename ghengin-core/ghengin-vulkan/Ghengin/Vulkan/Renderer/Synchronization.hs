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

import Ghengin.Vulkan.Renderer.Context.Device

-- | Create a Semaphore
createSemaphore :: MonadIO m => VulkanContext ⊸ m (Vk.Semaphore, VulkanContext)
createSemaphore = Unsafe.toLinear $ \dev ->
  let semaphoreInfo = Vk.SemaphoreCreateInfo { next = (), flags = zero }
   in (,dev) <$> liftSystemIO (Vk.createSemaphore dev._device semaphoreInfo Nothing)

-- | Create a Fence.
-- If the first argument is 'True' the fence is already signaled when created
createFence :: MonadIO m => VulkanContext ⊸ Bool -> m (Vk.Fence, VulkanContext)
createFence = Unsafe.toLinear $ \dev isSignaled ->
  let fenceInfo = Vk.FenceCreateInfo { next = (), flags = if isSignaled then Vk.FENCE_CREATE_SIGNALED_BIT else zero }
   in (,dev) <$> liftSystemIO (Vk.createFence dev._device fenceInfo Nothing)

destroySem :: MonadIO m => VulkanContext ⊸ Vk.Semaphore ⊸ m VulkanContext
destroySem   = Unsafe.toLinear2 $ \dev sem -> dev <$ liftSystemIO (Vk.destroySemaphore dev._device sem Nothing)

destroyFence :: MonadIO m => VulkanContext ⊸ Vk.Fence ⊸ m VulkanContext
destroyFence = Unsafe.toLinear2 $ \dev fen -> dev <$ liftSystemIO (Vk.destroyFence dev._device fen Nothing)

