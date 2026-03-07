{-# LANGUAGE ImpredicativeTypes #-}
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

import Ghengin.Core.Type.Utils
import Ghengin.Vulkan.Renderer.Context
import Ghengin.Vulkan.Renderer.Command as Cmd
import Ghengin.Vulkan.Renderer.Command.Buffer as Cmd
import Ghengin.Vulkan.Renderer.Synchronization
import Ghengin.Core.Prelude (withResource)

data ImmediateSubmitCtx = ImmediateSubmitCtx
  { uploadFence   :: !Vk.Fence
  , commandPool   :: !Vk.CommandPool
  , commandBuffer :: !(Some CommandBuffer)
  }

createImmediateSubmitCtx :: MonadIO m
                         => VulkanContext ctx
                          ⊸ m (ImmediateSubmitCtx, VulkanContext ctx)
createImmediateSubmitCtx ctx = Linear.do
  (fence, ctx) <- createFence ctx False
  (cpool0, ctx) <- createCommandPool ctx
  (bs, device3, cpool1) <- createCommandBuffers @1 ctx cpool0
  let elim' :: (CommandBuffer Initial ⊸ ImmediateSubmitCtx) ⊸ V.V 1 (CommandBuffer Initial) ⊸ ImmediateSubmitCtx = V.elim @1
  pure (elim' (\b -> ImmediateSubmitCtx fence cpool1 (Some b)) bs, device3)

destroyImmediateSubmitCtx :: MonadIO m => VulkanContext ctx ⊸ ImmediateSubmitCtx ⊸ m (VulkanContext ctx)
destroyImmediateSubmitCtx ctx (ImmediateSubmitCtx fence pool0 buffer) = Linear.do
  ctx <- destroyFence ctx fence
  (ctx, pool1) <- destroyCommandBuffers ctx pool0 (V.make @1 buffer)
  destroyCommandPool ctx pool1

-- | Submit a command to the immediate submit command buffer that synchronously
-- submits it to the graphics queue. That is, this function synchronously waits
-- for the command to be executed.
immediateSubmitSync
  :: MonadIO m
  => VulkanContext WithSwapchain
  {- TODO: This doesn't need to be fixed to 'WithSwapchain'. Use generic context. Will need to update few things wrt recording command buffers which also don't need to be fixed, and update VulkanContextM to take a ctx param -}
   ⊸ ImmediateSubmitCtx
   ⊸ CommandM m a
   ⊸ m ((VulkanContext WithSwapchain, ImmediateSubmitCtx), a)
-- Submit a command on a newly created buffer to the Graphics Queue
immediateSubmitSync = Unsafe.toLinear2 \ctx (ImmediateSubmitCtx fence pool (Some buffer0)) cmd -> Linear.do

  buffer <- Cmd.resetCommandBuffer buffer0
  (x, freeAliases, buffer') <- Cmd.recordCommand buffer cmd
  r <- Unsafe.toLinear (\buffer'' -> liftSystemIO $ do

    Vk.queueSubmit ctx.queue [Vk.SomeStruct $ Vk.SubmitInfo () [] [] [buffer''.unsafeGetCommandBuffer.commandBufferHandle] []] fence
    _ <- Vk.waitForFences ctx.device [fence] True maxBound
    Vk.resetFences ctx.device [fence]

    Vk.resetCommandPool ctx.device pool Vk.zero

    Prelude.pure ((Some buffer''))) buffer'

  ((), ctx) <- liftIO $ withResource ctx freeAliases

  pure ((ctx, ImmediateSubmitCtx fence pool r), x)
