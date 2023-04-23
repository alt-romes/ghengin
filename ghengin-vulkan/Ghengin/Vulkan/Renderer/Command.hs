{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
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
-- | Better imported qualified as Cmd.
module Ghengin.Vulkan.Renderer.Command
  ( Command
  , RenderPassCmd
  -- , CommandM
  -- , RenderPassCmdM
  , recordCommand
  , recordCommandOneShot
  , renderPassCmd
  , bindGraphicsPipeline
  , bindComputePipeline
  , bindRayTracingPipeline
  , bindGraphicsDescriptorSet
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

  , embed

  -- * Images
  , copyFullBufferToImage
  , transitionImageLayout
  ) where

import Control.Monad.Reader
import Data.Word
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Data.Vector (Vector)
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Zero as Vk
import qualified Vulkan      as Vk

import Ghengin.Vulkan.Renderer.Device

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
type Command m = CommandM m ()

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
type RenderPassCmd m = RenderPassCmdM m ()

newtype CommandM m a = Command (ReaderT Vk.CommandBuffer m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

newtype RenderPassCmdM m a = RenderPassCmd (ReaderT Vk.CommandBuffer m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

-- | Given a 'Vk.CommandBuffer' and the 'Command' to record in this buffer,
-- record the command in the buffer.
recordCommand :: MonadIO m => Vk.CommandBuffer -> Command m -> m ()
recordCommand buf (Command cmds) = do
  let beginInfo = Vk.CommandBufferBeginInfo { next = (), flags = Vk.zero
                                            , inheritanceInfo = Nothing }

  -- Begin recording
  Vk.beginCommandBuffer buf beginInfo

  -- Record commands
  runReaderT cmds buf

  -- Finish recording
  Vk.endCommandBuffer buf
{-# INLINE recordCommand #-}

recordCommandOneShot :: MonadIO m => Vk.CommandBuffer -> Command m -> m ()
recordCommandOneShot buf (Command cmds) = do
  let beginInfo = Vk.CommandBufferBeginInfo { next = (), flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT, inheritanceInfo = Nothing }
  Vk.beginCommandBuffer buf beginInfo
  runReaderT cmds buf
  Vk.endCommandBuffer buf
{-# INLINE recordCommandOneShot #-}

-- | Make a render pass part a command blueprint that can be further composed with other commands
renderPassCmd :: MonadIO m => Vk.RenderPass -> Vk.Framebuffer -> Vk.Extent2D -> RenderPassCmd m -> Command m
renderPassCmd rpass frameBuffer renderAreaExtent (RenderPassCmd rpcmds) = Command $ ask >>= \buf -> do
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
{-# INLINE renderPassCmd #-}

bindGraphicsPipeline :: MonadIO m => Vk.Pipeline -> RenderPassCmd m
bindGraphicsPipeline pp = RenderPassCmd $ ask >>= \buf -> Vk.cmdBindPipeline buf Vk.PIPELINE_BIND_POINT_GRAPHICS pp
{-# INLINE bindGraphicsPipeline #-}

bindComputePipeline :: MonadIO m => Vk.Pipeline -> RenderPassCmd m
bindComputePipeline pp = RenderPassCmd $ ask >>= \buf -> Vk.cmdBindPipeline buf Vk.PIPELINE_BIND_POINT_COMPUTE pp
{-# INLINE bindComputePipeline #-}

bindRayTracingPipeline :: MonadIO m => Vk.Pipeline -> RenderPassCmd m
bindRayTracingPipeline pp = RenderPassCmd $ ask >>= \buf -> Vk.cmdBindPipeline buf Vk.PIPELINE_BIND_POINT_RAY_TRACING_KHR pp
{-# INLINE bindRayTracingPipeline #-}

setViewport :: MonadIO m => Vk.Viewport -> RenderPassCmd m
setViewport viewport = RenderPassCmd $ ask >>= \buf -> Vk.cmdSetViewport buf 0 [viewport]
{-# INLINE setViewport #-}

setScissor :: MonadIO m => Vk.Rect2D   -> RenderPassCmd m
setScissor scissor = RenderPassCmd $ ask >>= \buf -> Vk.cmdSetScissor  buf 0 [scissor]
{-# INLINE setScissor #-}

bindVertexBuffers :: MonadIO m => Word32 -> Vector Vk.Buffer -> Vector Vk.DeviceSize -> RenderPassCmd m
bindVertexBuffers i bufs offsets = RenderPassCmd $ ask >>= \buf -> Vk.cmdBindVertexBuffers buf i bufs offsets
{-# INLINE bindVertexBuffers #-}

bindIndex32Buffer :: MonadIO m
                  => Vk.Buffer -- ^ Index buffer
                  -> Vk.DeviceSize -- ^ Offset into index buffer
                  -> RenderPassCmd m
bindIndex32Buffer ibuffer offset = RenderPassCmd $ ask >>= \buf -> Vk.cmdBindIndexBuffer buf ibuffer offset Vk.INDEX_TYPE_UINT32
{-# INLINE bindIndex32Buffer #-}

draw :: MonadIO m => Word32 -> RenderPassCmd m
draw vertexCount = RenderPassCmd $ ask >>= \buf -> Vk.cmdDraw buf vertexCount 1 0 0
{-# INLINE draw #-}

drawIndexed :: MonadIO m => Word32 -> RenderPassCmd m
drawIndexed ixCount = RenderPassCmd $ ask >>= \buf -> Vk.cmdDrawIndexed buf ixCount 1 0 0 0
{-# INLINE drawIndexed #-}

copyFullBuffer :: MonadIO m => Vk.Buffer -> Vk.Buffer -> Vk.DeviceSize -> Command m
copyFullBuffer src dst size = Command $ ask >>= \buf -> do
  Vk.cmdCopyBuffer buf src dst [Vk.BufferCopy 0 0 size]
{-# INLINE copyFullBuffer #-}

pushConstants :: ∀ a m. (MonadIO m, Storable a) => Vk.PipelineLayout -> Vk.ShaderStageFlags -> a -> RenderPassCmd m
pushConstants pipelineLayout stageFlags values =
  RenderPassCmd $ ask >>= \buf ->
    liftIO $ alloca @a $ \ptr -> do
      poke ptr values
      Vk.cmdPushConstants buf pipelineLayout stageFlags 0 (fromIntegral $ sizeOf values) (castPtr ptr)
{-# INLINE pushConstants #-}

bindGraphicsDescriptorSet :: MonadIO m
                          => Vk.PipelineLayout
                          -> Word32 -- ^ Set index at which to bind the descriptor set
                          -> Vk.DescriptorSet -> RenderPassCmd m
bindGraphicsDescriptorSet pipelay ix dset = RenderPassCmd $ ask >>= \buf -> do
  Vk.cmdBindDescriptorSets buf Vk.PIPELINE_BIND_POINT_GRAPHICS pipelay ix [dset] [] -- offsets array not used

-- | Lift a function that uses a command buffer to a Command
withCmdBuffer :: MonadIO m => (Vk.CommandBuffer -> m ()) -> Command m
withCmdBuffer f = Command $ ask >>= lift . f

makeRenderPassCmd :: MonadIO m => (Vk.CommandBuffer -> m ()) -> RenderPassCmd m
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

-- | A peculiar function to be sure. Best used with 'cmapM' from apecs
embed :: MonadIO m => ((x -> m ()) -> m ()) -> ((x -> RenderPassCmd m) -> RenderPassCmd m)
embed g h = RenderPassCmd $ ask >>= \buf -> lift $ g (fmap (\case RenderPassCmd act -> runReaderT act buf) h)


-- :| Images |: --

-- | Assumes the layout of the image is Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL!
copyFullBufferToImage :: MonadIO μ
                      => Vk.Buffer -- ^ From
                      -> Vk.Image  -- ^ To
                      -> Vk.Extent3D
                      -> Command μ
copyFullBufferToImage buf img extent = Command $ ask >>= \cmd ->
  let
      subresourceRange = Vk.ImageSubresourceLayers { aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT
                                                   , mipLevel = 0
                                                   , baseArrayLayer = 0
                                                   , layerCount = 1
                                                   }
                                                  -- Currently ^ this matches createImageView by chance and other uses of subresourceRange
      region = Vk.BufferImageCopy { bufferOffset = 0
                                  , bufferRowLength = 0 -- Data is tighly packed according to image size, so 0 is good here
                                  , bufferImageHeight = 0 -- ^ As above
                                  , imageSubresource = subresourceRange
                                  , imageOffset = Vk.Offset3D 0 0 0
                                  , imageExtent = extent
                                  }
   in do
      Vk.cmdCopyBufferToImage cmd buf img Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL [region]

transitionImageLayout :: MonadIO μ
                      => Vk.Image -> Vk.Format
                      -> Vk.ImageLayout -- ^ Src layout
                      -> Vk.ImageLayout -- ^ Dst layout
                      -> Command μ
transitionImageLayout img format srcLayout dstLayout = Command $ ask >>= \buf -> do

  let

      -- Barrier stages and access flags
      (srcAccess, dstAccess, stageFrom, stageTo) =
        case (srcLayout, dstLayout) of
          (Vk.IMAGE_LAYOUT_UNDEFINED, Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL) -> (Vk.zero, Vk.ACCESS_TRANSFER_WRITE_BIT, Vk.PIPELINE_STAGE_TOP_OF_PIPE_BIT, Vk.PIPELINE_STAGE_TRANSFER_BIT)
          (Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL, Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL) -> (Vk.ACCESS_TRANSFER_WRITE_BIT, Vk.ACCESS_SHADER_READ_BIT, Vk.PIPELINE_STAGE_TRANSFER_BIT, Vk.PIPELINE_STAGE_FRAGMENT_SHADER_BIT)
          _ -> error $ "Unknown transition " <> show (srcLayout, dstLayout)


      subresourceRange = Vk.ImageSubresourceRange { aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT
                                                  , baseMipLevel = 0
                                                  , levelCount = 1
                                                  , baseArrayLayer = 0
                                                  , layerCount = 1
                                                  }
                                                  -- Currently ^ this matches createImageView by chance and other uses of subresourceRange

      layoutChangeUndefTransfer = Vk.ImageMemoryBarrier { next = ()
                                                        , srcAccessMask = srcAccess
                                                        , dstAccessMask = dstAccess
                                                        , oldLayout = srcLayout
                                                        , newLayout = dstLayout
                                                        , srcQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
                                                        , dstQueueFamilyIndex = Vk.QUEUE_FAMILY_IGNORED
                                                        , image = img
                                                        , subresourceRange = subresourceRange
                                                        }

  -- Possible synchronization in pipeline barriers table: ?
  -- https://registry.khronos.org/vulkan/specs/1.3-extensions/html/chap7.html#synchronization-access-types-supported
  Vk.cmdPipelineBarrier buf
                        stageFrom stageTo
                        Vk.zero -- Dependency flags
                        [] -- Memory barriers
                        [] -- Buffer barriers
                        [Vk.SomeStruct layoutChangeUndefTransfer] -- Image memory barriers
