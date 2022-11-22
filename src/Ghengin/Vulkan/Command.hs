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
  , renderPass
  , bindGraphicsPipeline
  , bindComputePipeline
  , bindRayTracingPipeline
  , setViewport
  , setScissor
  , bindVertexBuffers
  , pushConstants
  , draw

  , createCommandPool
  , destroyCommandPool
  , createCommandBuffers
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

draw :: Word32 -> RenderPassCmd
draw vertexCount = RenderPassCmd $ ask >>= \buf -> Vk.cmdDraw buf vertexCount 1 0 0
{-# INLINE draw #-}

pushConstants :: forall a. Storable a => Vk.PipelineLayout -> Vk.ShaderStageFlags -> a -> RenderPassCmd
pushConstants pipelineLayout stageFlags values =
  RenderPassCmd $ ask >>= \buf ->
    liftIO $ alloca @a $ \ptr -> do
      poke ptr values
      Vk.cmdPushConstants buf pipelineLayout stageFlags 0 (fromIntegral $ sizeOf values) (castPtr ptr)
{-# INLINE pushConstants #-}

-- :| Creation and Destruction |:

createCommandPool :: VulkanDevice -> IO Vk.CommandPool
createCommandPool vkDevice = do

  let
    poolInfo = Vk.CommandPoolCreateInfo { flags = Vk.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
                                        , queueFamilyIndex = vkDevice._graphicsQueueFamily
                                        }

  Vk.createCommandPool vkDevice._device poolInfo Nothing


destroyCommandPool :: Vk.Device -> Vk.CommandPool -> IO ()
destroyCommandPool dev pool = Vk.destroyCommandPool dev pool Nothing

createCommandBuffers :: Vk.Device -> Vk.CommandPool -> Word32 -> IO (Vector Vk.CommandBuffer)
createCommandBuffers dev cpool n = do
  let
    allocInfo = Vk.CommandBufferAllocateInfo { commandPool = cpool
                                             , level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
                                             , commandBufferCount = n
                                             }
  Vk.allocateCommandBuffers dev allocInfo

