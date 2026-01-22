{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE LinearTypes #-}
module Ghengin.Vulkan.Renderer.ImmediateSubmit where

import qualified Prelude
import Prelude.Linear
import Control.Functor.Linear as Linear
import Control.Monad.IO.Class.Linear
import qualified Data.V.Linear as V

import qualified Unsafe.Linear as Unsafe

import qualified Vulkan.Zero as Vk
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan as Vk

import Ghengin.Vulkan.Renderer.Context
import Ghengin.Vulkan.Renderer.Command as Cmd
import Ghengin.Vulkan.Renderer.Synchronization

data ImmediateSubmitCtx = ImmediateSubmitCtx
  { uploadFence   :: !Vk.Fence
  , commandPool   :: !Vk.CommandPool
  , commandBuffer :: !Vk.CommandBuffer
  }

createImmediateSubmitCtx :: MonadIO m
                         => VulkanContext ctx
                          ⊸ m (ImmediateSubmitCtx, VulkanContext ctx)
createImmediateSubmitCtx ctx = Linear.do
  (fence, ctx) <- createFence ctx False
  (cpool0, ctx) <- createCommandPool ctx
  (bs, device3, cpool1) <- createCommandBuffers @1 ctx cpool0
  let elim' :: (Vk.CommandBuffer ⊸ ImmediateSubmitCtx) ⊸ V.V 1 Vk.CommandBuffer ⊸ ImmediateSubmitCtx = V.elim @1
  pure (elim' (\b -> ImmediateSubmitCtx fence cpool1 b) bs, device3)

destroyImmediateSubmitCtx :: MonadIO m => VulkanContext ctx ⊸ ImmediateSubmitCtx ⊸ m (VulkanContext ctx)
destroyImmediateSubmitCtx ctx (ImmediateSubmitCtx fence pool0 buffer) = Linear.do
  ctx <- destroyFence ctx fence
  (ctx, pool1) <- destroyCommandBuffers ctx pool0 (V.make @1 buffer)
  destroyCommandPool ctx pool1

-- | Submit a command to the immediate submit command buffer that synchronously
-- submits it to the graphics queue
immediateSubmit' :: MonadIO m
                => VulkanContext ctx
                 ⊸ ImmediateSubmitCtx
                 ⊸ CommandM m a
                 ⊸ m ((VulkanContext ctx, ImmediateSubmitCtx), a)
-- Submit a command on a newly created buffer to the Graphics Queue
immediateSubmit' ctx0 (ImmediateSubmitCtx fence pool buffer) cmd = Linear.do

  (buffer', x) <- Cmd.recordCommandOneShot buffer cmd

  r <- Unsafe.toLinear liftSystemIO $ (Unsafe.toLinearN @4 \ctx fence' pool' buffer'' -> do

    Vk.queueSubmit ctx.queue [Vk.SomeStruct $ Vk.SubmitInfo () [] [] [buffer''.commandBufferHandle] []] fence'
    _ <- Vk.waitForFences ctx.device [fence'] True maxBound
    Vk.resetFences ctx.device [fence']


    Vk.resetCommandPool ctx.device pool' Vk.zero

    Prelude.pure (ctx, ImmediateSubmitCtx fence' pool' buffer'')) ctx0 fence pool buffer'

  pure (r, x)

