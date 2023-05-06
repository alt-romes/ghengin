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

import Ghengin.Vulkan.Renderer.Device
import Ghengin.Vulkan.Renderer.Command as Cmd
import Ghengin.Vulkan.Renderer.Synchronization

data ImmediateSubmitCtx = ImmediateSubmitCtx { _uploadFence   :: !Vk.Fence
                                             , _commandPool   :: !Vk.CommandPool
                                             , _commandBuffer :: !Vk.CommandBuffer
                                             }

-- ROMES:TODO: Use an independent render pass (that can be done in parallel to the main one?)

-- :| Immediate Submit |:

-- Ah... I see, there's an ImmediateSubmitCtx in VulkanDevice, so it doesn't
-- make that much sense to receive it on creation... ach
-- ROMES:TODO...
createImmediateSubmitCtx :: MonadIO m
                         => VulkanDevice
                          ⊸ m (ImmediateSubmitCtx, VulkanDevice)
createImmediateSubmitCtx device0 = Linear.do
  (fence, device1) <- createFence device0 False
  (cpool0, device2) <- createCommandPool device1
  (bs, device3, cpool1) <- createCommandBuffers @1 device2 cpool0
  let elim' :: (Vk.CommandBuffer ⊸ ImmediateSubmitCtx) ⊸ V.V 1 Vk.CommandBuffer ⊸ ImmediateSubmitCtx = V.elim @1
  pure (elim' (\b -> ImmediateSubmitCtx fence cpool1 b) bs, device3)

destroyImmediateSubmitCtx :: MonadIO m => VulkanDevice ⊸ ImmediateSubmitCtx ⊸ m VulkanDevice
destroyImmediateSubmitCtx device0 (ImmediateSubmitCtx fence pool0 buffer) = Linear.do
  device1 <- destroyFence device0 fence
  (device2, pool1) <- destroyCommandBuffers device1 pool0 (V.make @1 buffer)
  device3 <- destroyCommandPool device2 pool1
  pure device3

-- | Submit a command to the immediate submit command buffer that synchronously
-- submits it to the graphics queue
immediateSubmit' :: MonadIO m
                => VulkanDevice
                 ⊸ ImmediateSubmitCtx
                 ⊸ Command m
                 ⊸ m (VulkanDevice, ImmediateSubmitCtx)
-- Submit a command on a newly created buffer to the Graphics Queue
immediateSubmit' device (ImmediateSubmitCtx fence pool buffer) cmd = Linear.do

  buffer' <- Cmd.recordCommandOneShot buffer cmd

  Unsafe.toLinear liftSystemIO $ (Unsafe.toLinearN @4 \dev fence' pool' buffer'' -> do

    Vk.queueSubmit dev._graphicsQueue [Vk.SomeStruct $ Vk.SubmitInfo () [] [] [buffer''.commandBufferHandle] []] fence'
    _ <- Vk.waitForFences dev._device [fence'] True maxBound
    Vk.resetFences dev._device [fence']


    Vk.resetCommandPool dev._device pool' Vk.zero

    Prelude.pure (dev, ImmediateSubmitCtx fence' pool' buffer'')) device fence pool buffer'

