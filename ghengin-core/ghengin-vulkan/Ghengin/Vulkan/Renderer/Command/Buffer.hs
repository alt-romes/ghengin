{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Ghengin.Vulkan.Renderer.Command.Buffer where

import Prelude hiding (($), pure, return)
import Prelude.Linear (($))
import qualified Prelude.Linear as Linear ((.))

import GHC.TypeLits

import qualified Data.V.Linear as V
import qualified Data.V.Linear.Internal as VI
import qualified Data.Vector as Vector

import qualified Control.Functor.Linear as Linear
import qualified Data.Functor.Linear as Data.Linear
import qualified Control.Monad.IO.Class.Linear as Linear

import qualified Vulkan.Zero as Vk
import qualified Vulkan      as Vk

import Ghengin.Vulkan.Renderer.Context
import Ghengin.Core.Type.Utils (w32, Some(..))

import qualified Unsafe.Linear as Unsafe

-- | A wrapper around 'Vk.CommandBuffer' that indexes the state of the command buffer in the type system.
--
-- The state machine is described in the Vulkan spec: https://docs.vulkan.org/spec/latest/chapters/cmdbuffers.html#commandbuffers-lifecycle
newtype CommandBuffer (s :: CommandBufferState) = CommandBuffer { unsafeGetCommandBuffer :: Vk.CommandBuffer }

-- | The state of a command buffer.
data CommandBufferState
  = Initial
  | Recording
  | Executable
  | Pending
  | Invalid

-- | Create a command buffer in the 'Initial' state.
createCommandBuffers
  :: forall n ctx m. (KnownNat n, Linear.MonadIO m)
  => VulkanContext ctx ⊸ Vk.CommandPool ⊸ m (V.V n (CommandBuffer 'Initial), VulkanContext ctx, Vk.CommandPool)
createCommandBuffers = Unsafe.toLinear2 \dev cpool ->
  let allocInfo = Vk.CommandBufferAllocateInfo
        { commandPool = cpool
        , level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
        , commandBufferCount = w32 @n }
   in (,dev,cpool) Linear.. Data.Linear.fmap (CommandBuffer @Initial) Linear.. VI.V @n @Vk.CommandBuffer Linear.<$>
    Linear.liftSystemIO (Vk.allocateCommandBuffers dev.device allocInfo)

destroyCommandBuffers
  :: forall n ctx m. Linear.MonadIO m
  => VulkanContext ctx ⊸ Vk.CommandPool ⊸ V.V n (Some CommandBuffer) ⊸ m (VulkanContext ctx, Vk.CommandPool)
destroyCommandBuffers = Unsafe.toLinear3 \dev pool (VI.V bufs) -> (dev,pool) Linear.<$ Linear.liftSystemIO
  (Vk.freeCommandBuffers dev.device pool (Vector.map (\(Some b) -> b.unsafeGetCommandBuffer) bufs))

-- | Begin recording a command buffer.
--
-- The command buffer must be in the 'Initial' state.
-- It transitions to the 'Recording' state.
beginCommandBuffer :: Linear.MonadIO m => CommandBuffer 'Initial ⊸ Vk.CommandBufferUsageFlags -> m (CommandBuffer 'Recording)
beginCommandBuffer = Unsafe.toLinear \cb flags -> Linear.do
  let beginInfo = Vk.CommandBufferBeginInfo { next = (), flags = flags
                                            , inheritanceInfo = Nothing }
  Linear.liftSystemIO $ Vk.beginCommandBuffer (unsafeGetCommandBuffer cb) beginInfo
  Linear.pure (CommandBuffer (unsafeGetCommandBuffer cb))

-- | End recording a command buffer.
--
-- The command buffer must be in the 'Recording' state.
-- It transitions to the 'Executable' state.
endCommandBuffer :: Linear.MonadIO m => CommandBuffer 'Recording ⊸ m (CommandBuffer 'Executable)
endCommandBuffer = Unsafe.toLinear \cb -> Linear.do
  Linear.liftSystemIO $ Vk.endCommandBuffer (unsafeGetCommandBuffer cb)
  Linear.pure (CommandBuffer (unsafeGetCommandBuffer cb))

-- | Reset a command buffer.
resetCommandBuffer :: Linear.MonadIO m => CommandBuffer s ⊸ m (CommandBuffer 'Initial)
resetCommandBuffer = Unsafe.toLinear \cb -> Linear.do
  Linear.liftSystemIO $ Vk.resetCommandBuffer (unsafeGetCommandBuffer cb) Vk.zero
  Linear.pure (CommandBuffer (unsafeGetCommandBuffer cb))

-- | Submit a command buffer to a queue.
--
-- The command buffer must be in the 'Executable' state.
-- queueSubmit
--   :: Linear.MonadIO m 
--   => Vk.PipelineStageFlags
--   -- ^ The pipeline stage in waitDstStageMask will make that wait happen at
--   -- the color attachment output stage of the pipeline, so (in theory) the GPU
--   -- might already start doing work on parts of the pipeline that come before
--   -- this, e.g. fetching vertices.
--   -> Vk.Queue %1
--   -> CommandBuffer 'Executable %1
--   -> Vk.Semaphore %1
--   -- ^ Wait semaphore. Makes sure the submitted command buffer(s)
--   -- won't start execution before the presentation of the current
--   -- frame has finished.
--   -> Vk.Semaphore %1
--   -- ^ The signal semaphore in signalSemaphores on the other hand is a
--   -- semaphore that's signalled by the GPU once command buffer execution has
--   -- completed.
--   -> Vk.Fence %1
--   -> m (Vk.Queue, CommandBuffer 'Executable, Vk.Semaphore, Vk.Semaphore, Vk.Fence)
-- queueSubmit waitStage = Unsafe.toLinearN @5 \queue cb waitSem signalSem fence -> Linear.do
--   let submitInfo = Vk.SubmitInfo
--         { next = ()
--         , waitSemaphores = [waitSem]
--         , waitDstStageMask = [waitStage]
--         , signalSemaphores = [signalSem]
--         , commandBuffers = Vector.singleton (unsafeGetCommandBuffer cb)
--         }
--   Linear.liftSystemIO $ Vk.queueSubmit queue [Vk.SomeStruct submitInfo] fence
--   Linear.pure (queue, cb, waitSem, signalSem, fence)
--
