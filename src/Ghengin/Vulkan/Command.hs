{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Ghengin.Vulkan.Command
  ( Command
  , RenderPassCmd
  -- , CommandM
  -- , RenderPassCmdM
  , recordCommand
  , recordCommandOneShot
  , renderPass
  , bindGraphicsPipeline
  , bindComputePipeline
  , bindRayTracingPipeline
  , bindGraphicsDescriptorSets
  , setViewport
  , setScissor
  , bindVertexBuffers
  , bindIndex32Buffer
  , copyFullBuffer
  , pushConstants
  , draw
  , drawIndexed
  , withCmdBuffer
  , makeRenderPassCmd

  , createCommandPool
  , destroyCommandPool
  , createCommandBuffers
  , destroyCommandBuffers
  ) where

import Control.Monad.Reader
import Data.Word
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Data.Vector (Vector)
import qualified Vulkan.Zero as Vk
import qualified Vulkan      as Vk

import Ghengin.Vulkan.Device

-- | A command description: a language to describe what will be recorded in the buffer
-- 
-- You can compose command blueprints into a bigger command blueprint using do notation.
--
-- === Example
--
-- @
-- recordCommand eng.vkCommandBuffer $ do
--  renderPass eng.vkRenderPass (eng.vkSwapChainFramebuffers V.! i) eng.vkSwapChainExtent $ do
--    bindGraphicsPipeline eng.vkPipeline
--    setViewport viewport
--    setScissor  scissor
--    draw 3
-- @
type Command = CommandM ()

-- | A render pass command description: a language to describe the subset of commands to record in a render pass command
--
-- === Example
--
-- @
-- rpc :: RenderPassCmd
-- rpc = do
--    bindGraphicsPipeline eng.vkPipeline
--    setViewport viewport
--    setScissor  scissor
--    draw 3
-- @
type RenderPassCmd = RenderPassCmdM ()

newtype CommandM a = Command (ReaderT Vk.CommandBuffer IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

newtype RenderPassCmdM a = RenderPassCmd (ReaderT Vk.CommandBuffer IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Given a 'Vk.CommandBuffer' and the 'Command' to record in this buffer,
-- record the command in the buffer.
recordCommand :: MonadIO m => Vk.CommandBuffer -> Command -> m ()
recordCommand buf (Command cmds) = do
  let beginInfo = Vk.CommandBufferBeginInfo { next = (), flags = Vk.zero
                                            , inheritanceInfo = Nothing }

  -- Begin recording
  Vk.beginCommandBuffer buf beginInfo

  -- Record commands
  liftIO $ runReaderT cmds buf

  -- Finish recording
  Vk.endCommandBuffer buf
{-# INLINE recordCommand #-}

recordCommandOneShot :: MonadIO m => Vk.CommandBuffer -> Command -> m ()
recordCommandOneShot buf (Command cmds) = do
  let beginInfo = Vk.CommandBufferBeginInfo { next = (), flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT, inheritanceInfo = Nothing }
  Vk.beginCommandBuffer buf beginInfo
  liftIO $ runReaderT cmds buf
  Vk.endCommandBuffer buf
{-# INLINE recordCommandOneShot #-}

-- | Make a render pass part a command blueprint that can be further composed with other commands
renderPass :: Vk.RenderPass -> Vk.Framebuffer -> Vk.Extent2D -> RenderPassCmd -> Command
renderPass rpass frameBuffer renderAreaExtent (RenderPassCmd rpcmds) = Command $ ask >>= \buf -> do
  let
    renderPassInfo = Vk.RenderPassBeginInfo { next = ()
                                            , renderPass  = rpass
                                            , framebuffer = frameBuffer
                                            , renderArea  = Vk.Rect2D (Vk.Offset2D 0 0) renderAreaExtent
                                            , clearValues = [Vk.Color $ Vk.Float32 0.1 0.1 0.1 1, Vk.DepthStencil $ Vk.ClearDepthStencilValue 1 0]
                                            }

  Vk.cmdBeginRenderPass buf renderPassInfo Vk.SUBPASS_CONTENTS_INLINE

  rpcmds

  Vk.cmdEndRenderPass buf
{-# INLINE renderPass #-}

bindGraphicsPipeline :: Vk.Pipeline -> RenderPassCmd
bindGraphicsPipeline pp = RenderPassCmd $ ask >>= \buf -> Vk.cmdBindPipeline buf Vk.PIPELINE_BIND_POINT_GRAPHICS pp
{-# INLINE bindGraphicsPipeline #-}

bindComputePipeline :: Vk.Pipeline -> RenderPassCmd
bindComputePipeline pp = RenderPassCmd $ ask >>= \buf -> Vk.cmdBindPipeline buf Vk.PIPELINE_BIND_POINT_COMPUTE pp
{-# INLINE bindComputePipeline #-}

bindRayTracingPipeline :: Vk.Pipeline -> RenderPassCmd
bindRayTracingPipeline pp = RenderPassCmd $ ask >>= \buf -> Vk.cmdBindPipeline buf Vk.PIPELINE_BIND_POINT_RAY_TRACING_KHR pp
{-# INLINE bindRayTracingPipeline #-}

setViewport :: Vk.Viewport -> RenderPassCmd
setViewport viewport = RenderPassCmd $ ask >>= \buf -> Vk.cmdSetViewport buf 0 [viewport]
{-# INLINE setViewport #-}

setScissor :: Vk.Rect2D   -> RenderPassCmd
setScissor scissor = RenderPassCmd $ ask >>= \buf -> Vk.cmdSetScissor  buf 0 [scissor]
{-# INLINE setScissor #-}

bindVertexBuffers :: Word32 -> Vector Vk.Buffer -> Vector Vk.DeviceSize -> RenderPassCmd
bindVertexBuffers i bufs offsets = RenderPassCmd $ ask >>= \buf -> Vk.cmdBindVertexBuffers buf i bufs offsets
{-# INLINE bindVertexBuffers #-}

bindIndex32Buffer :: Vk.Buffer -- ^ Index buffer
                  -> Vk.DeviceSize -- ^ Offset into index buffer
                  -> RenderPassCmd
bindIndex32Buffer ibuffer offset = RenderPassCmd $ ask >>= \buf -> Vk.cmdBindIndexBuffer buf ibuffer offset Vk.INDEX_TYPE_UINT32
{-# INLINE bindIndex32Buffer #-}

draw :: Word32 -> RenderPassCmd
draw vertexCount = RenderPassCmd $ ask >>= \buf -> Vk.cmdDraw buf vertexCount 1 0 0
{-# INLINE draw #-}

drawIndexed :: Word32 -> RenderPassCmd
drawIndexed ixCount = RenderPassCmd $ ask >>= \buf -> Vk.cmdDrawIndexed buf ixCount 1 0 0 0
{-# INLINE drawIndexed #-}

copyFullBuffer :: Vk.Buffer -> Vk.Buffer -> Vk.DeviceSize -> Command
copyFullBuffer src dst size = Command $ ask >>= \buf -> do
  Vk.cmdCopyBuffer buf src dst [Vk.BufferCopy 0 0 size]
{-# INLINE copyFullBuffer #-}

pushConstants :: ∀ a. Storable a => Vk.PipelineLayout -> Vk.ShaderStageFlags -> a -> RenderPassCmd
pushConstants pipelineLayout stageFlags values =
  RenderPassCmd $ ask >>= \buf ->
    liftIO $ alloca @a $ \ptr -> do
      poke ptr values
      Vk.cmdPushConstants buf pipelineLayout stageFlags 0 (fromIntegral $ sizeOf values) (castPtr ptr)
{-# INLINE pushConstants #-}

bindGraphicsDescriptorSets :: Vk.PipelineLayout -> Vector Vk.DescriptorSet -> RenderPassCmd
bindGraphicsDescriptorSets pipelay dsets = RenderPassCmd $ ask >>= \buf -> do
  Vk.cmdBindDescriptorSets buf Vk.PIPELINE_BIND_POINT_GRAPHICS pipelay 0 dsets [] -- offsets array not used

-- | Lift a function that uses a command buffer to a Command
withCmdBuffer :: (Vk.CommandBuffer -> IO ()) -> Command
withCmdBuffer f = Command $ ask >>= lift . f

makeRenderPassCmd :: (Vk.CommandBuffer -> IO ()) -> RenderPassCmd
makeRenderPassCmd f = RenderPassCmd $ ask >>= lift . f

-- :| Creation and Destruction |:

-- | Creates a command pool for the graphics queue family
createCommandPool :: MonadIO m => VulkanDevice -> m Vk.CommandPool
createCommandPool vkDevice = do

  let
    poolInfo = Vk.CommandPoolCreateInfo { flags = Vk.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
                                        , queueFamilyIndex = vkDevice._graphicsQueueFamily
                                        }

  Vk.createCommandPool vkDevice._device poolInfo Nothing


destroyCommandPool :: MonadIO m => Vk.Device -> Vk.CommandPool -> m ()
destroyCommandPool dev pool = Vk.destroyCommandPool dev pool Nothing

createCommandBuffers :: MonadIO m => Vk.Device -> Vk.CommandPool -> Word32 -> m (Vector Vk.CommandBuffer)
createCommandBuffers dev cpool n = do
  let
    allocInfo = Vk.CommandBufferAllocateInfo { commandPool = cpool
                                             , level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
                                             , commandBufferCount = n
                                             }
  Vk.allocateCommandBuffers dev allocInfo

destroyCommandBuffers :: MonadIO m => Vk.Device -> Vk.CommandPool -> Vector Vk.CommandBuffer -> m ()
destroyCommandBuffers = Vk.freeCommandBuffers


