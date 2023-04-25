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

-- | Create a Semaphore
createSemaphore :: MonadIO m => Vk.Device ⊸ m (Vk.Semaphore, Vk.Device)
createSemaphore = Unsafe.toLinear $ \dev ->
  let semaphoreInfo = Vk.SemaphoreCreateInfo { next = (), flags = zero }
   in (,dev) <$> liftSystemIO (Vk.createSemaphore dev semaphoreInfo Nothing)

-- | Create a Fence.
-- If the first argument is 'True' the fence is already signaled when created
createFence :: MonadIO m => Vk.Device ⊸ Bool -> m (Vk.Fence, Vk.Device)
createFence = Unsafe.toLinear $ \dev isSignaled ->
  let fenceInfo = Vk.FenceCreateInfo { next = (), flags = if isSignaled then Vk.FENCE_CREATE_SIGNALED_BIT else zero }
   in (,dev) <$> liftSystemIO (Vk.createFence dev fenceInfo Nothing)

destroySem :: MonadIO m => Vk.Device ⊸ Vk.Semaphore ⊸ m ()
destroySem = Unsafe.toLinear2 $ \dev sem -> liftSystemIO $ Vk.destroySemaphore dev sem Nothing

destroyFence :: MonadIO m => Vk.Device ⊸ Vk.Fence ⊸ m ()
destroyFence = Unsafe.toLinear2 $ \dev fen -> liftSystemIO $ Vk.destroyFence dev fen Nothing

