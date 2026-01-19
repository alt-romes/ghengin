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

import Ghengin.Vulkan.Renderer.Context

createSemaphore :: MonadIO m => VulkanContext ctx ⊸ m (Vk.Semaphore, VulkanContext ctx)
createSemaphore = Unsafe.toLinear $ \ctx ->
  let semaphoreInfo = Vk.SemaphoreCreateInfo { next = (), flags = zero }
   in (,ctx) <$> liftSystemIO (Vk.createSemaphore ctx.device semaphoreInfo Nothing)

-- | If the first argument is 'True' the fence is already signaled when created
createFence :: MonadIO m => VulkanContext ctx ⊸ Bool -> m (Vk.Fence, VulkanContext ctx)
createFence = Unsafe.toLinear $ \ctx isSignaled ->
  let fenceInfo = Vk.FenceCreateInfo { next = (), flags = if isSignaled then Vk.FENCE_CREATE_SIGNALED_BIT else zero }
   in (,ctx) <$> liftSystemIO (Vk.createFence ctx.device fenceInfo Nothing)

destroySem :: MonadIO m => VulkanContext ctx ⊸ Vk.Semaphore ⊸ m (VulkanContext ctx)
destroySem = Unsafe.toLinear2 $ \ctx sem -> ctx <$ liftSystemIO (Vk.destroySemaphore ctx.device sem Nothing)

destroyFence :: MonadIO m => VulkanContext ctx ⊸ Vk.Fence ⊸ m (VulkanContext ctx)
destroyFence = Unsafe.toLinear2 $ \ctx fen -> ctx <$ liftSystemIO (Vk.destroyFence ctx.device fen Nothing)

