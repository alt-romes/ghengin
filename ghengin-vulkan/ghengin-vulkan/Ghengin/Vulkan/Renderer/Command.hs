{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE BlockArguments #-}
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
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
-- | Better imported qualified as Cmd.
module Ghengin.Vulkan.Renderer.Command
  ( Command
  , RenderPassCmd
  , CommandM
  , RenderPassCmdM
  , CommandBuffer -- for backpack, re-export Vulkan's definition
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

  -- ROMES:TODO
  -- , embed

  -- * Images
  , copyFullBufferToImage
  , transitionImageLayout
  ) where

import GHC.TypeLits

import Prelude hiding (($))
import Prelude.Linear (($), Ur(..))
import qualified Prelude.Linear as Linear ((.))

import qualified Data.V.Linear as V
import qualified Data.V.Linear.Internal as VI

import Control.Monad.Reader
import qualified Control.Functor.Linear as Linear
import qualified Data.Functor.Linear as Data.Linear
import qualified Control.Monad.IO.Class.Linear as Linear
import Data.Word
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Zero as Vk
import qualified Vulkan      as Vk

import Ghengin.Vulkan.Renderer.Device
import {-# SOURCE #-} Ghengin.Vulkan.Renderer.RenderPass
import {-# SOURCE #-} Ghengin.Vulkan.Renderer.DescriptorSet
import {-# SOURCE #-} Ghengin.Vulkan.Renderer.Pipeline
import {-# SOURCE #-} Ghengin.Vulkan.Renderer.Kernel
import {-# SOURCE #-} Ghengin.Vulkan.Renderer.Buffer

import Ghengin.Core.Type.Utils (w32)
import Ghengin.Core.Log

import qualified Unsafe.Linear as Unsafe

-- TODO: We define these commands in terms of Vk.layouts and such, it'd be
-- better to define them in terms of RendererPipeline such that we can
-- eventually create an hsig for Commands...
-- To fix the module loop, we'd need an hs-boot file for the RendererPipeline definition

-- Re-think interface for Commands, and how the underlying monad could be
-- linear, and values threaded through instead of returned on the outside with
-- the command. Then again, the current design isn't bad either I think. (Good
-- to separate code submited to GPU from host)

-- TODO: Make Commands dupable? Such that one could use them twice if desired? Achhh

{-
Note [Commands and RenderPassCmds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A command-like monad allows recording and submition of command buffers?

Regarding linear types: Since we don't expose the internals of the Command and
RenderCommand implementations, we keep most of it as it is while providing a
linear interface -- which is safe enough.

A Command is an action run in an environment in which a command buffer is available

-}

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
  deriving (Data.Linear.Functor, Linear.Functor, Functor, Applicative, Monad, MonadIO, MonadTrans)

newtype RenderPassCmdM m a = RenderPassCmd (ReaderT Vk.CommandBuffer m a)
  deriving (Data.Linear.Functor, Linear.Functor, Functor, Applicative, Monad, MonadIO, MonadTrans)

-- This interface is safe because the only ways to record the command
-- (recordCommand, recordCommandOneShot) guarantee the command buffer is
-- returned, and command actions otherwise don't expose the command buffer,
-- making it impossible to free it.
--
-- Therefore, we can instance linear Applicative and Monad for them
instance Linear.Applicative m => Linear.Applicative (CommandM m) where
  pure x = Command $ ReaderT $ Unsafe.toLinear \r -> Linear.pure x
  Command (ReaderT fff) <*> Command (ReaderT ffa) = Command $ ReaderT $ Unsafe.toLinear \r -> fff r Linear.<*> ffa r
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Data.Linear.Applicative m => Data.Linear.Applicative (CommandM m) where
  pure x = Command $ ReaderT $ Unsafe.toLinear \r -> Data.Linear.pure x
  Command (ReaderT fff) <*> Command (ReaderT ffa) = Command $ ReaderT $ Unsafe.toLinear \r -> fff r Data.Linear.<*> ffa r
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Linear.Monad m => Linear.Monad (CommandM m) where
  Command (ReaderT fma) >>= f = Command $ ReaderT $ Unsafe.toLinear \r -> fma r Linear.>>= (\case (Command (ReaderT m)) -> m r) Linear.. f
  {-# INLINE (>>=) #-}

instance Linear.Applicative m => Linear.Applicative (RenderPassCmdM m) where
  pure x = RenderPassCmd $ ReaderT $ Unsafe.toLinear \r -> Linear.pure x
  RenderPassCmd (ReaderT fff) <*> RenderPassCmd (ReaderT ffa) = RenderPassCmd $ ReaderT $ Unsafe.toLinear \r -> fff r Linear.<*> ffa r
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Data.Linear.Applicative m => Data.Linear.Applicative (RenderPassCmdM m) where
  pure x = RenderPassCmd $ ReaderT $ Unsafe.toLinear \r -> Data.Linear.pure x
  RenderPassCmd (ReaderT fff) <*> RenderPassCmd (ReaderT ffa) = RenderPassCmd $ ReaderT $ Unsafe.toLinear \r -> fff r Data.Linear.<*> ffa r
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Linear.Monad m => Linear.Monad (RenderPassCmdM m) where
  RenderPassCmd (ReaderT fma) >>= f = RenderPassCmd $ ReaderT $ Unsafe.toLinear \r -> fma r Linear.>>= (\case (RenderPassCmd (ReaderT m)) -> m r) Linear.. f
  {-# INLINE (>>=) #-}

instance Linear.MonadTrans CommandM where
  lift x = Command $ ReaderT $ Unsafe.toLinear $ \r -> x
  {-# INLINE lift #-}

instance Linear.MonadTrans RenderPassCmdM where
  lift x = RenderPassCmd $ ReaderT $ Unsafe.toLinear $ \r -> x
  {-# INLINE lift #-}

instance Linear.MonadIO m => Linear.MonadIO (CommandM m) where
  liftIO x = Command $ ReaderT $ Unsafe.toLinear $ \r -> Linear.liftIO x
  {-# INLINE liftIO #-}

instance Linear.MonadIO m => Linear.MonadIO (RenderPassCmdM m) where
  liftIO x = RenderPassCmd $ ReaderT $ Unsafe.toLinear $ \r -> Linear.liftIO x
  {-# INLINE liftIO #-}

instance HasLogger m => HasLogger (CommandM m) where
  getLogger = Command $ ReaderT $ Unsafe.toLinear $ \r -> getLogger
  withLevelUp (Command (ReaderT fma)) = Command $ ReaderT \r -> withLevelUp (fma r)
  {-# INLINE getLogger #-}
  {-# INLINE withLevelUp #-}

-- Perhaps we can instance this generically for MonadTrans
instance HasLogger m => HasLogger (RenderPassCmdM m) where
  getLogger = RenderPassCmd $ ReaderT $ Unsafe.toLinear $ \r -> getLogger
  withLevelUp (RenderPassCmd (ReaderT fma)) = RenderPassCmd $ ReaderT \r -> withLevelUp (fma r)
  {-# INLINE getLogger #-}
  {-# INLINE withLevelUp #-}

-- | Given a 'Vk.CommandBuffer' and the 'Command' to record in this buffer,
-- record the command in the buffer.
recordCommand :: Linear.MonadIO m => Vk.CommandBuffer ⊸ Command m ⊸ m Vk.CommandBuffer
recordCommand = Unsafe.toLinear2 $ \buf (Command cmds) -> Linear.do
  let beginInfo = Vk.CommandBufferBeginInfo { next = (), flags = Vk.zero
                                            , inheritanceInfo = Nothing }

  -- Begin recording
  Linear.liftSystemIO $ Vk.beginCommandBuffer buf beginInfo

  -- Record commands
  runReaderT cmds buf

  -- Finish recording
  Linear.liftSystemIO $ Vk.endCommandBuffer buf

  Linear.pure buf
{-# INLINE recordCommand #-}

recordCommandOneShot :: Linear.MonadIO m => Vk.CommandBuffer ⊸ Command m ⊸ m Vk.CommandBuffer
recordCommandOneShot = Unsafe.toLinear2 \buf (Command cmds) -> Linear.do
  let beginInfo = Vk.CommandBufferBeginInfo { next = (), flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT, inheritanceInfo = Nothing }
  Linear.liftSystemIO $ Vk.beginCommandBuffer buf beginInfo
  runReaderT cmds buf
  Linear.liftSystemIO $ Vk.endCommandBuffer buf
  Linear.pure buf
{-# INLINE recordCommandOneShot #-}

-- | Make a render pass part a command blueprint that can be further composed with other commands
renderPassCmd' :: Linear.MonadIO m => Vk.RenderPass -> Vk.Framebuffer -> Vk.Extent2D -> RenderPassCmd m -> Command m
renderPassCmd' rpass frameBuffer renderAreaExtent (RenderPassCmd rpcmds) = Command $ ReaderT \buf -> Linear.do
  let
    renderPassInfo = Vk.RenderPassBeginInfo { next = ()
                                            , renderPass  = rpass
                                            , framebuffer = frameBuffer
                                            , renderArea  = Vk.Rect2D (Vk.Offset2D 0 0) renderAreaExtent
                                            , clearValues = [Vk.Color $ Vk.Float32 0.1 0.1 0.1 1, Vk.DepthStencil $ Vk.ClearDepthStencilValue 1 0]
                                            }

  Linear.liftSystemIO $ Vk.cmdBeginRenderPass buf renderPassInfo Vk.SUBPASS_CONTENTS_INLINE

  runReaderT rpcmds buf

  Linear.liftSystemIO $ Vk.cmdEndRenderPass buf
{-# INLINE renderPassCmd #-}

bindGraphicsPipeline' :: Linear.MonadIO m => Vk.Pipeline ⊸ RenderPassCmdM m Vk.Pipeline
bindGraphicsPipeline' pp = unsafeRenderPassCmd pp (\buf -> Vk.cmdBindPipeline buf Vk.PIPELINE_BIND_POINT_GRAPHICS)
{-# INLINE bindGraphicsPipeline' #-}

bindComputePipeline :: Linear.MonadIO m => Vk.Pipeline -> RenderPassCmdM m Vk.Pipeline
bindComputePipeline pp = unsafeRenderPassCmd pp (\buf -> Vk.cmdBindPipeline buf Vk.PIPELINE_BIND_POINT_COMPUTE)
{-# INLINE bindComputePipeline #-}

bindRayTracingPipeline :: Linear.MonadIO m => Vk.Pipeline -> RenderPassCmdM m Vk.Pipeline
bindRayTracingPipeline pp = unsafeRenderPassCmd pp (\buf -> Vk.cmdBindPipeline buf Vk.PIPELINE_BIND_POINT_RAY_TRACING_KHR)
{-# INLINE bindRayTracingPipeline #-}

setViewport :: Linear.MonadIO m => Vk.Viewport -> RenderPassCmd m
setViewport viewport = unsafeRenderPassCmd_ (\buf -> Vk.cmdSetViewport buf 0 [viewport])
{-# INLINE setViewport #-}

setScissor :: Linear.MonadIO m => Vk.Rect2D -> RenderPassCmd m
setScissor scissor = unsafeRenderPassCmd_ (\buf -> Vk.cmdSetScissor buf 0 [scissor])
{-# INLINE setScissor #-}

bindVertexBuffers :: Linear.MonadIO m => Word32 -> V.V n Vk.Buffer ⊸ V.V n Vk.DeviceSize -> RenderPassCmdM m (V.V n Vk.Buffer)
bindVertexBuffers i bufs (VI.V offsets) = unsafeRenderPassCmd bufs (\cmdbuf (VI.V bufs') -> Vk.cmdBindVertexBuffers cmdbuf i bufs' offsets)
{-# INLINE bindVertexBuffers #-}

bindIndex32Buffer :: Linear.MonadIO m
                  => Vk.Buffer -- ^ Index buffer
                   ⊸ Vk.DeviceSize -- ^ Offset into index buffer
                  -> RenderPassCmdM m Vk.Buffer
bindIndex32Buffer ibuffer offset = unsafeRenderPassCmd ibuffer (\buf ibuf -> Vk.cmdBindIndexBuffer buf ibuf offset Vk.INDEX_TYPE_UINT32)
{-# INLINE bindIndex32Buffer #-}

draw :: Linear.MonadIO m => Word32 -> RenderPassCmd m
draw vertexCount = unsafeRenderPassCmd_ (\buf -> Vk.cmdDraw buf vertexCount 1 0 0)
{-# INLINE draw #-}

drawIndexed :: Linear.MonadIO m => Word32 -> RenderPassCmd m
drawIndexed ixCount = unsafeRenderPassCmd_ $ \buf -> Vk.cmdDrawIndexed buf ixCount 1 0 0 0
{-# INLINE drawIndexed #-}

copyFullBuffer :: Linear.MonadIO m => Vk.Buffer ⊸ Vk.Buffer ⊸ Vk.DeviceSize -> (Command m, Vk.Buffer, Vk.Buffer)
copyFullBuffer src dst size = case unsafeCmd (src,dst) $ \buf (src',dst') -> Vk.cmdCopyBuffer buf src' dst' [Vk.BufferCopy 0 0 size] of
                                (Ur cmd, (src', dst')) -> (cmd, src', dst')
{-# INLINE copyFullBuffer #-}

pushConstants :: ∀ a m. (Linear.MonadIO m, Storable a) => Vk.PipelineLayout ⊸ Vk.ShaderStageFlags -> a -> RenderPassCmdM m Vk.PipelineLayout
pushConstants pipelineLayout stageFlags values = unsafeRenderPassCmd pipelineLayout $ \buf piplayout -> do
    liftIO $ alloca @a $ \ptr -> do
      poke ptr values
      Vk.cmdPushConstants buf piplayout stageFlags 0 (fromIntegral $ sizeOf values) (castPtr ptr)
{-# INLINE pushConstants #-}

bindGraphicsDescriptorSet' :: Linear.MonadIO m
                          => Vk.PipelineLayout
                          ⊸ Word32 -- ^ Set index at which to bind the descriptor set
                          -> Vk.DescriptorSet ⊸ RenderPassCmdM m (Vk.PipelineLayout, Vk.DescriptorSet)
bindGraphicsDescriptorSet' pipelay ix dset =
  unsafeRenderPassCmd (pipelay,dset) (\buf (pip',dset') -> Vk.cmdBindDescriptorSets buf Vk.PIPELINE_BIND_POINT_GRAPHICS pip' ix [dset'] []) -- offsets array not used
{-# INLINE bindGraphicsDescriptorSet' #-}

-- | Lift a function that uses a command buffer to a Command
-- Get back to this: not trivial with linearity. Probably unsafe will have to be called outside
-- Or this function will be called unsafeWithCmdBuffer
--
-- This is an unsafe function because the command buffer can be used
-- unrestrictedly! Make sure you don't free it.
withCmdBuffer :: Linear.MonadIO m => (Vk.CommandBuffer -> m ()) -> Command m
withCmdBuffer f = Command $ ReaderT f

-- As above
makeRenderPassCmd :: Linear.MonadIO m => (Vk.CommandBuffer -> m ()) -> RenderPassCmd m
makeRenderPassCmd f = RenderPassCmd $ ReaderT f


-- :| Creation and Destruction |:

-- | Creates a command pool for the graphics queue family
createCommandPool :: Linear.MonadIO m => VulkanDevice ⊸ m (Vk.CommandPool, VulkanDevice)
createCommandPool = Unsafe.toLinear $ \vkDevice ->
  let
    poolInfo = Vk.CommandPoolCreateInfo { flags = Vk.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
                                        , queueFamilyIndex = vkDevice._graphicsQueueFamily
                                        }
   in (,vkDevice) Linear.<$> (Linear.liftSystemIO $ Vk.createCommandPool vkDevice._device poolInfo Nothing)


destroyCommandPool :: Linear.MonadIO m => VulkanDevice ⊸ Vk.CommandPool ⊸ m VulkanDevice
destroyCommandPool = Unsafe.toLinear2 $ \dev pool -> dev Linear.<$ Linear.liftSystemIO (Vk.destroyCommandPool dev._device pool Nothing)


createCommandBuffers :: ∀ (n :: Nat) m. (KnownNat n, Linear.MonadIO m) => VulkanDevice ⊸ Vk.CommandPool ⊸ m (V.V n Vk.CommandBuffer, VulkanDevice, Vk.CommandPool)
createCommandBuffers = Unsafe.toLinear2 \dev cpool ->
  let
    allocInfo = Vk.CommandBufferAllocateInfo { commandPool = cpool
                                             , level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
                                             , commandBufferCount = w32 @n
                                             }
   in (,dev,cpool) Linear.. VI.V @n @Vk.CommandBuffer Linear.<$> Linear.liftSystemIO (Vk.allocateCommandBuffers dev._device allocInfo)

destroyCommandBuffers :: Linear.MonadIO m => VulkanDevice ⊸ Vk.CommandPool ⊸ V.V n Vk.CommandBuffer ⊸ m (VulkanDevice, Vk.CommandPool)
destroyCommandBuffers = Unsafe.toLinear3 \dev pool (VI.V bufs) -> (dev,pool) Linear.<$ Linear.liftSystemIO (Vk.freeCommandBuffers dev._device pool bufs)

-- | A peculiar function to be sure. Best used with 'cmapM' from apecs
-- Hard with linearity.... reconsider
-- embed :: Linear.MonadIO m => ((x -> m ()) -> m ()) -> ((x -> RenderPassCmd m) -> RenderPassCmd m)
-- embed g h = RenderPassCmd $ ask >>= \buf -> lift $ g (fmap (\case RenderPassCmd act -> runReaderT act buf) h)


-- :| Images |: --

-- | Assumes the layout of the image is Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL!
copyFullBufferToImage :: Linear.MonadIO μ
                      => Vk.Buffer -- ^ From
                       ⊸ Vk.Image  -- ^ To
                       ⊸ Vk.Extent3D
                      -> (Command μ, Vk.Buffer, Vk.Image)
copyFullBufferToImage buf img extent =
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
   in case unsafeCmd (buf,img) $ \cmdbuf (buf', img') -> Vk.cmdCopyBufferToImage cmdbuf buf' img' Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL [region] of
        (Ur cmd, (buf',img')) -> (cmd, buf', img')

transitionImageLayout :: forall μ
                       . Linear.MonadIO μ
                      => Vk.Image
                       ⊸ Vk.Format -- ^ ROMES:TODO: Being ignored?
                      -> Vk.ImageLayout -- ^ Src layout
                      -> Vk.ImageLayout -- ^ Dst layout
                      -> (Command μ, Vk.Image)
transitionImageLayout = Unsafe.toLinear $ \img format srcLayout dstLayout ->

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

   in ( unsafeCmd_ @μ (\buf ->
      -- Possible synchronization in pipeline barriers table: ?
      -- https://registry.khronos.org/vulkan/specs/1.3-extensions/html/chap7.html#synchronization-access-types-supported
      Vk.cmdPipelineBarrier buf
                            stageFrom stageTo
                            Vk.zero -- Dependency flags
                            [] -- Memory barriers
                            [] -- Buffer barriers
                            [Vk.SomeStruct layoutChangeUndefTransfer]) -- Image memory barriers
      , img )


----- More for the .hsig interface -------

-- While I don't know the best place to keep this, I keep it here:
--
-- Ultimately, I think it will be about a good abstraction for issuing/batching
-- draw call.


drawVertexBuffer :: MonadIO m => VertexBuffer ⊸ RenderPassCmdM m VertexBuffer
drawVertexBuffer (VertexBuffer (DeviceLocalBuffer buf mem) nverts) = Linear.do
  let offsets = V.make 0
  buffers' <- bindVertexBuffers 0 (V.make buf) offsets
  draw nverts
  pure (VertexBuffer (DeviceLocalBuffer (V.elim id buffers') mem) nverts)

drawVertexBufferIndexed :: MonadIO m => VertexBuffer ⊸ Index32Buffer ⊸ RenderPassCmdM m (VertexBuffer, Index32Buffer)
drawVertexBufferIndexed (VertexBuffer (DeviceLocalBuffer vbuf mem) nverts) (Index32Buffer (DeviceLocalBuffer ibuf imem) nixs) = Linear.do
  let offsets = V.make 0
  buffers' <- bindVertexBuffers 0 (V.make vbuf) offsets
  ibuf'    <- bindIndex32Buffer ibuf 0
  drawIndexed nixs
  pure ( VertexBuffer (DeviceLocalBuffer (V.elim id buffers') mem) nverts
       , Index32Buffer (DeviceLocalBuffer ibuf' imem) nixs
       )

bindGraphicsPipeline :: Linear.MonadIO m => RendererPipeline Graphics ⊸ RenderPassCmdM m (RendererPipeline Graphics)
bindGraphicsPipeline pp = bindGraphicsPipeline' pp._pipeline
{-# INLINE bindGraphicsPipeline #-}

bindGraphicsDescriptorSet :: Linear.MonadIO m
                          => RendererPipeline Graphics
                          ⊸ Word32 -- ^ Set index at which to bind the descriptor set
                          -> DescriptorSet ⊸ RenderPassCmdM m (RendererPipeline Graphics, DescriptorSet)
bindGraphicsDescriptorSet pipelay ix dset = bindGraphicsDescriptorSet' pipelay._pipelineLayout ix dset._descriptorSet
{-# INLINE bindGraphicsDescriptorSet #-}

renderPassCmd :: Linear.MonadIO m => Int -- ^ needs a good explanation...
              -> RenderPass -> Vk.Extent2D -> RenderPassCmd m -> Command m
renderPassCmd currentImage = Unsafe.toLinear \rpass renderAreaExtent rpcmds -> renderPassCmd' rpass._renderPass (rpass._framebuffers Vector.! currentImage) renderAreaExtent rpcmds

----- Linear Unsafe Utils

-- Note how `a` is used unrestrictedly in the function `f`. This is because
-- often this function will be a Vulkan function which isn't linear.

unsafeCmd :: Linear.MonadIO m => a ⊸ (Vk.CommandBuffer -> a -> IO ()) -> (Ur (Command m), a)
unsafeCmd = Unsafe.toLinear \a f -> (Ur $ Command $ ReaderT \buf -> Linear.liftSystemIO (f buf a), a)

unsafeCmd_ :: Linear.MonadIO m => (Vk.CommandBuffer -> IO ()) -> Command m
unsafeCmd_ = Unsafe.toLinear \f -> (Command $ ReaderT \buf -> Linear.liftSystemIO (f buf))

-- | Unsafe for lots of reasons
unsafeRenderPassCmd :: Linear.MonadIO m => a ⊸ (Vk.CommandBuffer -> a -> IO ()) -> RenderPassCmdM m a
unsafeRenderPassCmd = Unsafe.toLinear \a f -> (RenderPassCmd $ ReaderT \buf -> a Linear.<$ Linear.liftSystemIO (f buf a))

unsafeRenderPassCmd_ :: Linear.MonadIO m => (Vk.CommandBuffer -> IO ()) -> RenderPassCmd m
unsafeRenderPassCmd_ = Unsafe.toLinear \f -> (RenderPassCmd $ ReaderT \buf -> Linear.liftSystemIO (f buf))

-- ghengin-vulkan can't yet depend on ghengin-core because of backpack bugs
-- (TODO: Report that ghc bug)
