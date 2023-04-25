{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE LinearTypes #-}
module Ghengin.Vulkan.Renderer.ImmediateSubmit where


import Control.Monad.IO.Class.Linear

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

-- TODO: Use an independent render pass (that can be done in parallel to the main one?)

-- :| Immediate Submit |:

createImmediateSubmitCtx :: MonadIO m
                         => VulkanDevice
                         -> m ImmediateSubmitCtx
createImmediateSubmitCtx device = do
  fence <- createFence device._device False
  cpool <- createCommandPool device
  createCommandBuffers device._device cpool 1 >>= \case
    [b] -> pure (ImmediateSubmitCtx fence cpool b)
    _ -> liftIO $ fail "Create command buffers expected 1 got something else"

destroyImmediateSubmitCtx :: Vk.Device -> ImmediateSubmitCtx -> IO ()
destroyImmediateSubmitCtx device (ImmediateSubmitCtx fence pool buffer) = do
  destroyFence device fence
  destroyCommandBuffers device pool [buffer]
  destroyCommandPool device pool
  

-- | Submit a command on a newly created buffer to the Graphics Queue
immediateSubmit :: MonadIO m
                => Vk.Device
                -> Vk.Queue -- ^ Graphics queue
                -> ImmediateSubmitCtx
                -> Command m
                -> m ()
immediateSubmit device graphicsQueue (ImmediateSubmitCtx fence pool buffer) cmd = do

  Cmd.recordCommandOneShot buffer cmd

  Vk.queueSubmit graphicsQueue [Vk.SomeStruct $ Vk.SubmitInfo () [] [] [buffer.commandBufferHandle] []] fence
  _ <- Vk.waitForFences device [fence] True maxBound
  Vk.resetFences device [fence]


  Vk.resetCommandPool device pool Vk.zero

