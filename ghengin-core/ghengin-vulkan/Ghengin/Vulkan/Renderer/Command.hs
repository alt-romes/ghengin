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
  , RenderCmd
  , CommandM
  , RenderCmdM
  , Vk.CommandBuffer -- for backpack, re-export Vulkan's definition
  , recordCommand
  , recordCommandOneShot

  -- * Pipeline Binding
  , bindGraphicsPipeline
  , bindComputePipeline
  , bindRayTracingPipeline
  , bindGraphicsDescriptorSet

  -- * Dynamic State (Vulkan 1.0)
  , setViewport
  , setScissor
  , setLineWidth
  , setDepthBias
  , setBlendConstants
  , setDepthBounds
  , setStencilCompareMask
  , setStencilWriteMask
  , setStencilReference

  -- * Dynamic State (Vulkan 1.3 promoted from VK_EXT_extended_dynamic_state)
  , setCullMode
  , setFrontFace
  , setPrimitiveTopology
  , setViewportWithCount
  , setScissorWithCount
  , setDepthTestEnable
  , setDepthWriteEnable
  , setDepthCompareOp
  , setDepthBoundsTestEnable
  , setStencilTestEnable
  , setStencilOp
  , setDepthBiasEnable
  , setPrimitiveRestartEnable
  , setRasterizerDiscardEnable

  -- * Vertex/Index Buffer Binding
  , bindVertexBuffers
  , bindVertexBuffers2
  , bindIndex32Buffer

  -- * Drawing Commands
  , draw
  , drawIndexed
  , drawVertexBuffer
  , drawVertexBufferIndexed
  , drawIndirect
  , drawIndexedIndirect
  , drawIndirectCount
  , drawIndexedIndirectCount

  -- * Compute Dispatch
  , dispatch
  , dispatchIndirect

  -- * Data Transfer
  , copyFullBuffer
  , pushConstants
  , fillBuffer
  , updateBuffer

  -- * Dynamic Rendering (Vulkan 1.3)
  , beginRendering

  -- * Images
  , copyFullBufferToImage
  , transitionImageLayout
  , clearColorImage
  , clearDepthStencilImage
  , clearAttachments
  , copyImage
  , blitImage
  , copyImageToBuffer
  , resolveImage

  -- * Synchronization
  , pipelineBarrier
  , setEvent
  , resetEvent
  , waitEvents
  , writeTimestamp
  -- ** Synchronization2 (Vulkan 1.3)
  , pipelineBarrier2
  , setEvent2
  , resetEvent2
  , waitEvents2
  , writeTimestamp2

  -- * Query Commands
  , beginQuery
  , endQuery
  , resetQueryPool
  , copyQueryPoolResults

  -- * Secondary Command Buffers
  , executeCommands

  -- * Command Pool Management
  , createCommandPool
  , destroyCommandPool
  , createCommandBuffers
  , destroyCommandBuffers

  -- * Unsafe bits
  , unsafeCmd
  , unsafeCmd_
  , unsafeRenderCmd
  , unsafeRenderCmd_
  ) where

import GHC.TypeLits

import Prelude hiding (($), pure, return)
import Prelude.Linear (($))
import qualified Prelude.Linear as Linear ((.))

import qualified Data.V.Linear as V
import qualified Data.V.Linear.Internal as VI

import Control.Monad.Reader
import Control.Functor.Linear (pure, return)
import qualified Control.Functor.Linear as Linear
import qualified Data.Functor.Linear as Data.Linear
import qualified Control.Monad.IO.Class.Linear as Linear
import Data.Word
import Foreign.Storable
import Foreign.Marshal.Alloc
import Foreign.Ptr
import qualified Data.Vector as Vector
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Zero as Vk
import qualified Vulkan      as Vk

import Ghengin.Vulkan.Renderer.Context
import {-# SOURCE #-} Ghengin.Vulkan.Renderer.DescriptorSet
import {-# SOURCE #-} Ghengin.Vulkan.Renderer.Pipeline
import {-# SOURCE #-} Ghengin.Vulkan.Renderer.Buffer

import Ghengin.Core.Type.Utils (w32)
import Ghengin.Core.Log

import qualified Data.Linear.Alias as Alias
import qualified Data.Linear.Alias.Unsafe as Unsafe

import qualified Unsafe.Linear as Unsafe

{-
Note [Commands and RenderCmds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
--  renderPassCmd eng.vkRenderPass (eng.vkSwapChainFramebuffers V.! i) eng.vkSwapChainExtent $ do
--    bindGraphicsPipeline eng.vkPipeline
--    setViewport viewport
--    setScissor  scissor
--    draw 3
-- @
type Command m = CommandM m ()

-- | A rendering command description: a language to describe the subset of commands to record between beginRendering and endRendering
--
-- === Example
--
-- @
-- rpc :: RenderCmd
-- rpc = do
--    bindGraphicsPipeline eng.vkPipeline
--    setViewport viewport
--    setScissor  scissor
--    draw 3
-- @
type RenderCmd m = RenderCmdM m ()

newtype CommandM m a = Command (ReaderT CmdInfo m a)
  deriving (Data.Linear.Functor, Linear.Functor)

newtype RenderCmdM m a = RenderCmd (CommandM m a)
  deriving (Data.Linear.Functor, Linear.Functor, Data.Linear.Applicative, Linear.Applicative, Linear.Monad, Linear.MonadTrans, Linear.MonadIO, HasLogger)

data CmdInfo = CmdInfo
      { buf :: Vk.CommandBuffer

      -- | Because we have multiple frames in flight in our swapchain, we need
      -- to index the corresponding auxiliary structures on the right frame
      -- index. For instance, we need this Ix to pick the right framebuffer to
      -- use in the renderpass command.
      , _currentImageIx :: Int
      }

-- This interface is safe because the only ways to record the command
-- (recordCommand, recordCommandOneShot) guarantee the command buffer is
-- returned, and command actions otherwise don't expose the command buffer,
-- making it impossible to free it.
--
-- Therefore, we can instance linear Applicative and Monad for them
instance Linear.Applicative m => Linear.Applicative (CommandM m) where
  pure x = Command $ ReaderT $ Unsafe.toLinear \_ -> Linear.pure x
  Command (ReaderT fff) <*> Command (ReaderT ffa) = Command $ ReaderT $ Unsafe.toLinear \r -> fff r Linear.<*> ffa r
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Data.Linear.Applicative m => Data.Linear.Applicative (CommandM m) where
  pure x = Command $ ReaderT $ Unsafe.toLinear \_ -> Data.Linear.pure x
  Command (ReaderT fff) <*> Command (ReaderT ffa) = Command $ ReaderT $ Unsafe.toLinear \r -> fff r Data.Linear.<*> ffa r
  {-# INLINE pure #-}
  {-# INLINE (<*>) #-}

instance Linear.Monad m => Linear.Monad (CommandM m) where
  Command (ReaderT fma) >>= f = Command $ ReaderT $ Unsafe.toLinear \r -> fma r Linear.>>= (\case (Command (ReaderT m)) -> m r) Linear.. f
  {-# INLINE (>>=) #-}

instance Linear.MonadTrans CommandM where
  lift x = Command $ ReaderT $ Unsafe.toLinear $ \_ -> x
  {-# INLINE lift #-}

instance Linear.MonadIO m => Linear.MonadIO (CommandM m) where
  liftIO x = Command $ ReaderT $ Unsafe.toLinear $ \_ -> Linear.liftIO x
  {-# INLINE liftIO #-}

instance HasLogger m => HasLogger (CommandM m) where
  getLogger = Command $ ReaderT $ Unsafe.toLinear $ \_ -> getLogger
  withLevelUp (Command (ReaderT fma)) = Command $ ReaderT \r -> withLevelUp (fma r)
  {-# INLINE getLogger #-}
  {-# INLINE withLevelUp #-}

-- | Given a 'Vk.CommandBuffer' and the 'Command' to record in this buffer,
-- record the command in the buffer.
recordCommand :: Linear.MonadIO m => Int {-^ Current frame index -} -> Vk.CommandBuffer ⊸ CommandM m a ⊸ m (a, Vk.CommandBuffer)
recordCommand ix = Unsafe.toLinear2 $ \buf (Command cmds) -> Linear.do
  let beginInfo = Vk.CommandBufferBeginInfo { next = (), flags = Vk.zero
                                            , inheritanceInfo = Nothing }

  -- Begin recording
  Linear.liftSystemIO $ Vk.beginCommandBuffer buf beginInfo

  -- Record commands
  a <- runReaderT cmds (CmdInfo buf ix)

  -- Finish recording
  Linear.liftSystemIO $ Vk.endCommandBuffer buf

  Linear.pure (a, buf)
{-# INLINE recordCommand #-}

recordCommandOneShot :: Linear.MonadIO m => Vk.CommandBuffer ⊸ CommandM m a ⊸ m (Vk.CommandBuffer, a)
recordCommandOneShot = Unsafe.toLinear2 \buf (Command cmds) -> Linear.do
  let beginInfo = Vk.CommandBufferBeginInfo { next = (), flags = Vk.COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT, inheritanceInfo = Nothing }
  Linear.liftSystemIO $ Vk.beginCommandBuffer buf beginInfo
  x <- runReaderT cmds (CmdInfo buf (error "one shot command tried to begin render pass but no frame is selected?"))
  Linear.liftSystemIO $ Vk.endCommandBuffer buf
  Linear.pure (buf, x)
{-# INLINE recordCommandOneShot #-}

bindGraphicsPipeline' :: Linear.MonadIO m => Vk.Pipeline ⊸ RenderCmdM m Vk.Pipeline
bindGraphicsPipeline' pp = unsafeRenderCmd pp (\buf -> Vk.cmdBindPipeline buf Vk.PIPELINE_BIND_POINT_GRAPHICS)
{-# INLINE bindGraphicsPipeline' #-}

bindComputePipeline :: Linear.MonadIO m => Vk.Pipeline -> CommandM m Vk.Pipeline
bindComputePipeline pp = unsafeCmd pp (\buf -> Vk.cmdBindPipeline buf Vk.PIPELINE_BIND_POINT_COMPUTE)
{-# INLINE bindComputePipeline #-}

bindRayTracingPipeline :: Linear.MonadIO m => Vk.Pipeline -> CommandM m Vk.Pipeline
bindRayTracingPipeline pp = unsafeCmd pp (\buf -> Vk.cmdBindPipeline buf Vk.PIPELINE_BIND_POINT_RAY_TRACING_KHR)
{-# INLINE bindRayTracingPipeline #-}

setViewport :: Linear.MonadIO m => Vk.Viewport -> RenderCmd m
setViewport viewport = unsafeRenderCmd_ (\buf -> Vk.cmdSetViewport buf 0 [viewport])
{-# INLINE setViewport #-}

setScissor :: Linear.MonadIO m => Vk.Rect2D -> RenderCmd m
setScissor scissor = unsafeRenderCmd_ (\buf -> Vk.cmdSetScissor buf 0 [scissor])
{-# INLINE setScissor #-}

-- | Set line width dynamically
setLineWidth :: Linear.MonadIO m => Float -> RenderCmd m
setLineWidth lineWidth = unsafeRenderCmd_ (\buf -> Vk.cmdSetLineWidth buf lineWidth)
{-# INLINE setLineWidth #-}

-- | Set depth bias dynamically
setDepthBias :: Linear.MonadIO m
             => Float -- ^ Depth bias constant factor
             -> Float -- ^ Depth bias clamp
             -> Float -- ^ Depth bias slope factor
             -> RenderCmd m
setDepthBias constant clamp slope = unsafeRenderCmd_ (\buf -> Vk.cmdSetDepthBias buf constant clamp slope)
{-# INLINE setDepthBias #-}

-- | Set blend constants dynamically
setBlendConstants :: Linear.MonadIO m => (Float, Float, Float, Float) -> RenderCmd m
setBlendConstants (r, g, b, a) = unsafeRenderCmd_ (\buf -> Vk.cmdSetBlendConstants buf (r, g, b, a))
{-# INLINE setBlendConstants #-}

-- | Set depth bounds dynamically
setDepthBounds :: Linear.MonadIO m
               => Float -- ^ Min depth bounds
               -> Float -- ^ Max depth bounds
               -> RenderCmd m
setDepthBounds minBound maxBound = unsafeRenderCmd_ (\buf -> Vk.cmdSetDepthBounds buf minBound maxBound)
{-# INLINE setDepthBounds #-}

-- | Set stencil compare mask dynamically
setStencilCompareMask :: Linear.MonadIO m => Vk.StencilFaceFlags -> Word32 -> RenderCmd m
setStencilCompareMask faceMask compareMask = unsafeRenderCmd_ (\buf -> Vk.cmdSetStencilCompareMask buf faceMask compareMask)
{-# INLINE setStencilCompareMask #-}

-- | Set stencil write mask dynamically
setStencilWriteMask :: Linear.MonadIO m => Vk.StencilFaceFlags -> Word32 -> RenderCmd m
setStencilWriteMask faceMask writeMask = unsafeRenderCmd_ (\buf -> Vk.cmdSetStencilWriteMask buf faceMask writeMask)
{-# INLINE setStencilWriteMask #-}

-- | Set stencil reference dynamically
setStencilReference :: Linear.MonadIO m => Vk.StencilFaceFlags -> Word32 -> RenderCmd m
setStencilReference faceMask reference = unsafeRenderCmd_ (\buf -> Vk.cmdSetStencilReference buf faceMask reference)
{-# INLINE setStencilReference #-}

-- | Set cull mode dynamically (Vulkan 1.3)
setCullMode :: Linear.MonadIO m => Vk.CullModeFlags -> RenderCmd m
setCullMode cullMode = unsafeRenderCmd_ (\buf -> Vk.cmdSetCullMode buf cullMode)
{-# INLINE setCullMode #-}

-- | Set front face dynamically (Vulkan 1.3)
setFrontFace :: Linear.MonadIO m => Vk.FrontFace -> RenderCmd m
setFrontFace frontFace = unsafeRenderCmd_ (\buf -> Vk.cmdSetFrontFace buf frontFace)
{-# INLINE setFrontFace #-}

-- | Set primitive topology dynamically (Vulkan 1.3)
setPrimitiveTopology :: Linear.MonadIO m => Vk.PrimitiveTopology -> RenderCmd m
setPrimitiveTopology topology = unsafeRenderCmd_ (\buf -> Vk.cmdSetPrimitiveTopology buf topology)
{-# INLINE setPrimitiveTopology #-}

-- | Set multiple viewports dynamically with count (Vulkan 1.3)
setViewportWithCount :: Linear.MonadIO m => [Vk.Viewport] -> RenderCmd m
setViewportWithCount viewports = unsafeRenderCmd_ (\buf -> Vk.cmdSetViewportWithCount buf (Vector.fromList viewports))
{-# INLINE setViewportWithCount #-}

-- | Set multiple scissors dynamically with count (Vulkan 1.3)
setScissorWithCount :: Linear.MonadIO m => [Vk.Rect2D] -> RenderCmd m
setScissorWithCount scissors = unsafeRenderCmd_ (\buf -> Vk.cmdSetScissorWithCount buf (Vector.fromList scissors))
{-# INLINE setScissorWithCount #-}

-- | Enable/disable depth test dynamically (Vulkan 1.3)
setDepthTestEnable :: Linear.MonadIO m => Bool -> RenderCmd m
setDepthTestEnable enable = unsafeRenderCmd_ (\buf -> Vk.cmdSetDepthTestEnable buf enable)
{-# INLINE setDepthTestEnable #-}

-- | Enable/disable depth write dynamically (Vulkan 1.3)
setDepthWriteEnable :: Linear.MonadIO m => Bool -> RenderCmd m
setDepthWriteEnable enable = unsafeRenderCmd_ (\buf -> Vk.cmdSetDepthWriteEnable buf enable)
{-# INLINE setDepthWriteEnable #-}

-- | Set depth compare operation dynamically (Vulkan 1.3)
setDepthCompareOp :: Linear.MonadIO m => Vk.CompareOp -> RenderCmd m
setDepthCompareOp compareOp = unsafeRenderCmd_ (\buf -> Vk.cmdSetDepthCompareOp buf compareOp)
{-# INLINE setDepthCompareOp #-}

-- | Enable/disable depth bounds test dynamically (Vulkan 1.3)
setDepthBoundsTestEnable :: Linear.MonadIO m => Bool -> RenderCmd m
setDepthBoundsTestEnable enable = unsafeRenderCmd_ (\buf -> Vk.cmdSetDepthBoundsTestEnable buf enable)
{-# INLINE setDepthBoundsTestEnable #-}

-- | Enable/disable stencil test dynamically (Vulkan 1.3)
setStencilTestEnable :: Linear.MonadIO m => Bool -> RenderCmd m
setStencilTestEnable enable = unsafeRenderCmd_ (\buf -> Vk.cmdSetStencilTestEnable buf enable)
{-# INLINE setStencilTestEnable #-}

-- | Set stencil operations dynamically (Vulkan 1.3)
setStencilOp :: Linear.MonadIO m
             => Vk.StencilFaceFlags -- ^ Face mask
             -> Vk.StencilOp        -- ^ Fail op
             -> Vk.StencilOp        -- ^ Pass op
             -> Vk.StencilOp        -- ^ Depth fail op
             -> Vk.CompareOp        -- ^ Compare op
             -> RenderCmd m
setStencilOp faceMask failOp passOp depthFailOp compareOp =
  unsafeRenderCmd_ (\buf -> Vk.cmdSetStencilOp buf faceMask failOp passOp depthFailOp compareOp)
{-# INLINE setStencilOp #-}

-- | Enable/disable depth bias dynamically (Vulkan 1.3)
setDepthBiasEnable :: Linear.MonadIO m => Bool -> RenderCmd m
setDepthBiasEnable enable = unsafeRenderCmd_ (\buf -> Vk.cmdSetDepthBiasEnable buf enable)
{-# INLINE setDepthBiasEnable #-}

-- | Enable/disable primitive restart dynamically (Vulkan 1.3)
setPrimitiveRestartEnable :: Linear.MonadIO m => Bool -> RenderCmd m
setPrimitiveRestartEnable enable = unsafeRenderCmd_ (\buf -> Vk.cmdSetPrimitiveRestartEnable buf enable)
{-# INLINE setPrimitiveRestartEnable #-}

-- | Enable/disable rasterizer discard dynamically (Vulkan 1.3)
setRasterizerDiscardEnable :: Linear.MonadIO m => Bool -> RenderCmd m
setRasterizerDiscardEnable enable = unsafeRenderCmd_ (\buf -> Vk.cmdSetRasterizerDiscardEnable buf enable)
{-# INLINE setRasterizerDiscardEnable #-}

bindVertexBuffers :: Linear.MonadIO m => Word32 -> V.V n Vk.Buffer ⊸ V.V n Vk.DeviceSize -> RenderCmdM m (V.V n Vk.Buffer)
bindVertexBuffers i bufs (VI.V offsets) = unsafeRenderCmd bufs (\cmdbuf (VI.V bufs') -> Vk.cmdBindVertexBuffers cmdbuf i bufs' offsets)
{-# INLINE bindVertexBuffers #-}

-- | Bind vertex buffers with extended parameters (Vulkan 1.3)
bindVertexBuffers2 :: Linear.MonadIO m
                   => Word32                   -- ^ First binding
                   -> V.V n Vk.Buffer          -- ^ Buffers
                   ⊸ V.V n Vk.DeviceSize       -- ^ Offsets
                   -> V.V n Vk.DeviceSize      -- ^ Sizes
                   -> V.V n Vk.DeviceSize      -- ^ Strides
                   -> RenderCmdM m (V.V n Vk.Buffer)
bindVertexBuffers2 firstBinding bufs (VI.V offsets) (VI.V sizes) (VI.V strides) =
  unsafeRenderCmd bufs (\cmdbuf (VI.V bufs') ->
    Vk.cmdBindVertexBuffers2 cmdbuf firstBinding bufs' offsets sizes strides)
{-# INLINE bindVertexBuffers2 #-}

bindIndex32Buffer :: Linear.MonadIO m
                  => Vk.Buffer -- ^ Index buffer
                   ⊸ Vk.DeviceSize -- ^ Offset into index buffer
                  -> RenderCmdM m Vk.Buffer
bindIndex32Buffer ibuffer offset = unsafeRenderCmd ibuffer (\buf ibuf -> Vk.cmdBindIndexBuffer buf ibuf offset Vk.INDEX_TYPE_UINT32)
{-# INLINE bindIndex32Buffer #-}

draw :: Linear.MonadIO m => Word32 -> Word32 -> RenderCmd m
draw vertexCount instanceCount = unsafeRenderCmd_ (\buf -> Vk.cmdDraw buf vertexCount instanceCount 0 0)
{-# INLINE draw #-}

drawIndexed :: Linear.MonadIO m => Word32 -> Word32 -> RenderCmd m
drawIndexed ixCount instanceCount = unsafeRenderCmd_ $ \buf -> Vk.cmdDrawIndexed buf ixCount instanceCount 0 0 0
{-# INLINE drawIndexed #-}

-- | Draw primitives with indirect parameters from a buffer
drawIndirect :: Linear.MonadIO m
             => Vk.Buffer         -- ^ Buffer containing draw parameters
              ⊸ Vk.DeviceSize     -- ^ Offset into buffer
             -> Word32            -- ^ Draw count
             -> Word32            -- ^ Stride
             -> RenderCmdM m Vk.Buffer
drawIndirect buffer offset drawCount stride =
  unsafeRenderCmd buffer (\buf buffer' -> Vk.cmdDrawIndirect buf buffer' offset drawCount stride)
{-# INLINE drawIndirect #-}

-- | Draw indexed primitives with indirect parameters from a buffer
drawIndexedIndirect :: Linear.MonadIO m
                    => Vk.Buffer       -- ^ Buffer containing draw parameters
                     ⊸ Vk.DeviceSize   -- ^ Offset into buffer
                    -> Word32          -- ^ Draw count
                    -> Word32          -- ^ Stride
                    -> RenderCmdM m Vk.Buffer
drawIndexedIndirect buffer offset drawCount stride =
  unsafeRenderCmd buffer (\buf buffer' -> Vk.cmdDrawIndexedIndirect buf buffer' offset drawCount stride)
{-# INLINE drawIndexedIndirect #-}

-- | Draw with indirect count from buffer (Vulkan 1.2)
drawIndirectCount :: Linear.MonadIO m
                  => Vk.Buffer         -- ^ Buffer containing draw parameters
                   ⊸ Vk.DeviceSize     -- ^ Offset into draw buffer
                  -> Vk.Buffer         -- ^ Count buffer
                   ⊸ Vk.DeviceSize     -- ^ Offset into count buffer
                  -> Word32            -- ^ Max draw count
                  -> Word32            -- ^ Stride
                  -> RenderCmdM m (Vk.Buffer, Vk.Buffer)
drawIndirectCount drawBuffer drawOffset countBuffer countOffset maxDrawCount stride =
  unsafeRenderCmd (drawBuffer, countBuffer) (\buf (drawBuf', countBuf') ->
    Vk.cmdDrawIndirectCount buf drawBuf' drawOffset countBuf' countOffset maxDrawCount stride)
{-# INLINE drawIndirectCount #-}

-- | Draw indexed with indirect count from buffer (Vulkan 1.2)
drawIndexedIndirectCount :: Linear.MonadIO m
                         => Vk.Buffer       -- ^ Buffer containing draw parameters
                          ⊸ Vk.DeviceSize   -- ^ Offset into draw buffer
                         -> Vk.Buffer       -- ^ Count buffer
                          ⊸ Vk.DeviceSize   -- ^ Offset into count buffer
                         -> Word32          -- ^ Max draw count
                         -> Word32          -- ^ Stride
                         -> RenderCmdM m (Vk.Buffer, Vk.Buffer)
drawIndexedIndirectCount drawBuffer drawOffset countBuffer countOffset maxDrawCount stride =
  unsafeRenderCmd (drawBuffer, countBuffer) (\buf (drawBuf', countBuf') ->
    Vk.cmdDrawIndexedIndirectCount buf drawBuf' drawOffset countBuf' countOffset maxDrawCount stride)
{-# INLINE drawIndexedIndirectCount #-}

-- | Dispatch compute work items
dispatch :: Linear.MonadIO m
         => Word32 -- ^ Group count X
         -> Word32 -- ^ Group count Y
         -> Word32 -- ^ Group count Z
         -> Command m
dispatch groupCountX groupCountY groupCountZ =
  unsafeCmd_ (\buf -> Vk.cmdDispatch buf groupCountX groupCountY groupCountZ)
{-# INLINE dispatch #-}

dispatchIndirect :: Linear.MonadIO m
                 => Vk.Buffer       -- ^ Buffer containing dispatch parameters
                  ⊸ Vk.DeviceSize   -- ^ Offset into buffer
                 -> CommandM m Vk.Buffer
dispatchIndirect buffer offset =
  unsafeCmd buffer (\buf buffer' -> Vk.cmdDispatchIndirect buf buffer' offset)
{-# INLINE dispatchIndirect #-}

-- :| Dynamic Rendering (Vulkan 1.3) |: --

-- | Begin dynamic rendering (Vulkan 1.3)
beginRendering :: Linear.MonadIO m => Vk.RenderingInfo '[] -> RenderCmdM m a ⊸ CommandM m a
beginRendering renderingInfo = Unsafe.toLinear $ \(RenderCmd (Command rpcmds)) -> Command $ ReaderT $ \info -> Linear.do
  Linear.liftSystemIO $ Vk.cmdBeginRendering (info.buf) renderingInfo
  a <- runReaderT rpcmds info
  Linear.liftSystemIO $ Vk.cmdEndRendering (info.buf)
  Linear.pure a
{-# INLINE beginRendering #-}

-- :| Buffer Data Commands |: --

-- | Fill a region of a buffer with a fixed value
fillBuffer :: Linear.MonadIO m
           => Vk.Buffer       -- ^ Destination buffer
            ⊸ Vk.DeviceSize   -- ^ Offset into buffer
           -> Vk.DeviceSize   -- ^ Size to fill (or Vk.WHOLE_SIZE)
           -> Word32          -- ^ Data to fill with
           -> CommandM m Vk.Buffer
fillBuffer buffer offset size dataValue =
  unsafeCmd buffer (\buf buffer' -> Vk.cmdFillBuffer buf buffer' offset size dataValue)
{-# INLINE fillBuffer #-}

-- | Update a buffer's contents from host memory
updateBuffer :: Linear.MonadIO m
             => Vk.Buffer       -- ^ Destination buffer
              ⊸ Vk.DeviceSize   -- ^ Offset into buffer
             -> Word64          -- ^ Data size
             -> Ptr ()          -- ^ Pointer to data
             -> CommandM m Vk.Buffer
updateBuffer buffer offset dataSize ptr =
  unsafeCmd buffer (\buf buffer' -> Vk.cmdUpdateBuffer buf buffer' offset dataSize ptr)
{-# INLINE updateBuffer #-}

copyFullBuffer :: Linear.MonadIO m => Vk.Buffer ⊸ Vk.Buffer ⊸ Vk.DeviceSize -> CommandM m (Vk.Buffer, Vk.Buffer)
copyFullBuffer src dst size =
  unsafeCmd (src,dst) $ \buf (src',dst') ->
    Vk.cmdCopyBuffer buf src' dst' [Vk.BufferCopy 0 0 size]
{-# INLINE copyFullBuffer #-}

pushConstants :: ∀ a m. (Linear.MonadIO m, Storable a) => Vk.PipelineLayout ⊸ Vk.ShaderStageFlags -> a -> RenderCmdM m Vk.PipelineLayout
pushConstants pipelineLayout stageFlags values = unsafeRenderCmd pipelineLayout $ \buf piplayout -> do
    liftIO $ alloca @a $ \ptr -> do
      poke ptr values
      Vk.cmdPushConstants buf piplayout stageFlags 0 (fromIntegral $ sizeOf values) (castPtr ptr)
{-# INLINE pushConstants #-}

bindGraphicsDescriptorSet' :: Linear.MonadIO m
                          => Vk.PipelineLayout
                          ⊸ Word32 -- ^ Set index at which to bind the descriptor set
                          -> Vk.DescriptorSet ⊸ RenderCmdM m (Vk.PipelineLayout, Vk.DescriptorSet)
bindGraphicsDescriptorSet' pipelay ix dset =
  unsafeRenderCmd (pipelay,dset) (\buf (pip',dset') -> Vk.cmdBindDescriptorSets buf Vk.PIPELINE_BIND_POINT_GRAPHICS pip' ix [dset'] []) -- offsets array not used
{-# INLINE bindGraphicsDescriptorSet' #-}

-- :| Creation and Destruction |:

-- | Creates a command pool for the graphics queue family
createCommandPool :: forall ctx m. Linear.MonadIO m => VulkanContext ctx ⊸ m (Vk.CommandPool, VulkanContext ctx)
createCommandPool = Unsafe.toLinear $ \vkCtx ->
  let poolInfo = Vk.CommandPoolCreateInfo
        { flags = Vk.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
        , queueFamilyIndex = fromIntegral vkCtx.queueFamilyIndex }
   in (,vkCtx) Linear.<$> (Linear.liftSystemIO $ Vk.createCommandPool vkCtx.device poolInfo Nothing)


destroyCommandPool :: forall ctx m. Linear.MonadIO m => VulkanContext ctx ⊸ Vk.CommandPool ⊸ m (VulkanContext ctx)
destroyCommandPool = Unsafe.toLinear2 $ \dev pool -> dev Linear.<$ Linear.liftSystemIO (Vk.destroyCommandPool dev.device pool Nothing)


createCommandBuffers :: forall n ctx m. (KnownNat n, Linear.MonadIO m) => VulkanContext ctx ⊸ Vk.CommandPool ⊸ m (V.V n Vk.CommandBuffer, VulkanContext ctx, Vk.CommandPool)
createCommandBuffers = Unsafe.toLinear2 \dev cpool ->
  let allocInfo = Vk.CommandBufferAllocateInfo
        { commandPool = cpool
        , level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
        , commandBufferCount = w32 @n }
   in (,dev,cpool) Linear.. VI.V @n @Vk.CommandBuffer Linear.<$>
    Linear.liftSystemIO (Vk.allocateCommandBuffers dev.device allocInfo)

destroyCommandBuffers :: forall n ctx m. Linear.MonadIO m => VulkanContext ctx ⊸ Vk.CommandPool ⊸ V.V n Vk.CommandBuffer ⊸ m (VulkanContext ctx, Vk.CommandPool)
destroyCommandBuffers = Unsafe.toLinear3 \dev pool (VI.V bufs) -> (dev,pool) Linear.<$ Linear.liftSystemIO (Vk.freeCommandBuffers dev.device pool bufs)


-- :| Images |: --

-- | Assumes the layout of the image is Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL!
copyFullBufferToImage :: Linear.MonadIO μ
                      => Vk.Buffer -- ^ From
                       ⊸ Vk.Image  -- ^ To
                       ⊸ Vk.Extent3D
                      -> CommandM μ (Vk.Buffer, Vk.Image)
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
   in unsafeCmd (buf,img) $ \cmdbuf (buf', img') ->
        Vk.cmdCopyBufferToImage cmdbuf buf' img' Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL [region]

transitionImageLayout :: forall μ
                       . Linear.MonadIO μ
                      => Vk.Image
                       ⊸ Vk.ImageLayout -- ^ Src layout
                      -> Vk.ImageLayout -- ^ Dst layout
                      -> CommandM μ Vk.Image
transitionImageLayout img srcLayout dstLayout =
  unsafeCmd img (\buf img' ->
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
                                                        , image = img'
                                                        , subresourceRange = subresourceRange
                                                        }
      -- Possible synchronization in pipeline barriers table: ?
      -- https://registry.khronos.org/vulkan/specs/1.3-extensions/html/chap7.html#synchronization-access-types-supported
     in Vk.cmdPipelineBarrier buf
                            stageFrom stageTo
                            Vk.zero -- Dependency flags
                            [] -- Memory barriers
                            [] -- Buffer barriers
                            [Vk.SomeStruct layoutChangeUndefTransfer]
                            ) -- Image memory barriers


----- More for the .hsig interface -------

-- While I don't know the best place to keep this, I keep it here:
--
-- Ultimately, I think it will be about a good abstraction for issuing/batching
-- draw call.


drawVertexBuffer :: Linear.MonadIO m => VertexBuffer ⊸ RenderCmdM m VertexBuffer
drawVertexBuffer (VertexBuffer (DeviceLocalBuffer buf mem) nverts) = Linear.do
  let offsets = V.make 0
  buffers' <- bindVertexBuffers 0 (V.make buf :: V.V 1 Vk.Buffer) offsets
  draw nverts 1
  pure (VertexBuffer (DeviceLocalBuffer (V.elim (\x -> x) buffers') mem) nverts)

drawVertexBufferIndexed :: Linear.MonadIO m => VertexBuffer ⊸ Index32Buffer ⊸ RenderCmdM m (VertexBuffer, Index32Buffer)
drawVertexBufferIndexed (VertexBuffer (DeviceLocalBuffer vbuf mem) nverts) (Index32Buffer (DeviceLocalBuffer ibuf imem) nixs) = Linear.do
  let offsets = V.make 0
  buffers' <- bindVertexBuffers 0 (V.make vbuf) offsets
  ibuf'    <- bindIndex32Buffer ibuf 0
  drawIndexed nixs 1
  pure ( VertexBuffer (DeviceLocalBuffer (V.elim (\x -> x) buffers') mem) nverts
       , Index32Buffer (DeviceLocalBuffer ibuf' imem) nixs
       )

bindGraphicsPipeline :: Linear.MonadIO m => RendererPipeline Graphics ⊸ RenderCmdM m (RendererPipeline Graphics)
bindGraphicsPipeline (VulkanPipeline pipeline layout) = Linear.do
  pipeline' <- bindGraphicsPipeline' pipeline
  return (VulkanPipeline pipeline' layout)
{-# INLINE bindGraphicsPipeline #-}

bindGraphicsDescriptorSet :: Linear.MonadIO m
                          => RendererPipeline Graphics
                          ⊸ Word32 -- ^ Set index at which to bind the descriptor set
                          -> DescriptorSet ⊸ RenderCmdM m (DescriptorSet, RendererPipeline Graphics)
bindGraphicsDescriptorSet (VulkanPipeline pipelay layout) ix (DescriptorSet dix dset) = Linear.do
  (layout', dset') <- bindGraphicsDescriptorSet' layout ix dset
  return (DescriptorSet dix dset', VulkanPipeline pipelay layout')
{-# INLINE bindGraphicsDescriptorSet #-}

clearColorImage :: Linear.MonadIO m => Vk.Image -> Float -> Float -> Float -> Float -> Command m
clearColorImage img r g b a = unsafeCmd_ $ \buf ->
    Vk.cmdClearColorImage buf img
      Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
      (Vk.Float32 r g b a)
      [Vk.ImageSubresourceRange
        { aspectMask = Vk.IMAGE_ASPECT_COLOR_BIT
        , baseMipLevel = 0
        , levelCount = Vk.REMAINING_MIP_LEVELS
        , baseArrayLayer = 0
        , layerCount = Vk.REMAINING_ARRAY_LAYERS
        }]

-- | Clear a depth/stencil image
clearDepthStencilImage :: Linear.MonadIO m
                       => Vk.Image
                       -> Vk.ImageLayout
                       -> Vk.ClearDepthStencilValue
                       -> [Vk.ImageSubresourceRange]
                       -> Command m
clearDepthStencilImage img layout clearValue ranges = unsafeCmd_ $ \buf ->
    Vk.cmdClearDepthStencilImage buf img layout clearValue (Vector.fromList ranges)
{-# INLINE clearDepthStencilImage #-}

-- | Clear regions of attachments within a render pass
clearAttachments :: Linear.MonadIO m
                 => [Vk.ClearAttachment]
                 -> [Vk.ClearRect]
                 -> RenderCmd m
clearAttachments attachments rects = unsafeRenderCmd_ $ \buf ->
    Vk.cmdClearAttachments buf (Vector.fromList attachments) (Vector.fromList rects)
{-# INLINE clearAttachments #-}

-- | Copy data between images
copyImage :: Linear.MonadIO m
          => Vk.Image         -- ^ Source image
           ⊸ Vk.ImageLayout   -- ^ Source image layout
          -> Vk.Image         -- ^ Destination image
           ⊸ Vk.ImageLayout   -- ^ Destination image layout
          -> [Vk.ImageCopy]
          -> CommandM m (Vk.Image, Vk.Image)
copyImage srcImage srcLayout dstImage dstLayout regions =
  unsafeCmd (srcImage, dstImage) $ \buf (src', dst') ->
    Vk.cmdCopyImage buf src' srcLayout dst' dstLayout (Vector.fromList regions)
{-# INLINE copyImage #-}

-- | Copy an image to another location, with possible format conversion and scaling
blitImage :: Linear.MonadIO m
          => Vk.Image         -- ^ Source image
           ⊸ Vk.ImageLayout   -- ^ Source image layout
          -> Vk.Image         -- ^ Destination image
           ⊸ Vk.ImageLayout   -- ^ Destination image layout
          -> [Vk.ImageBlit]
          -> Vk.Filter
          -> CommandM m (Vk.Image, Vk.Image)
blitImage srcImage srcLayout dstImage dstLayout regions filterMode =
  unsafeCmd (srcImage, dstImage) $ \buf (src', dst') ->
    Vk.cmdBlitImage buf src' srcLayout dst' dstLayout (Vector.fromList regions) filterMode
{-# INLINE blitImage #-}

-- | Copy data from an image to a buffer
copyImageToBuffer :: Linear.MonadIO m
                  => Vk.Image         -- ^ Source image
                   ⊸ Vk.ImageLayout   -- ^ Source image layout
                  -> Vk.Buffer        -- ^ Destination buffer
                   ⊸ [Vk.BufferImageCopy]
                  -> CommandM m (Vk.Image, Vk.Buffer)
copyImageToBuffer srcImage srcLayout dstBuffer regions =
  unsafeCmd (srcImage, dstBuffer) $ \buf (img', buffer') ->
    Vk.cmdCopyImageToBuffer buf img' srcLayout buffer' (Vector.fromList regions)
{-# INLINE copyImageToBuffer #-}

-- | Resolve a multisample image to a non-multisample image
resolveImage :: Linear.MonadIO m
             => Vk.Image         -- ^ Source (multisample) image
              ⊸ Vk.ImageLayout   -- ^ Source image layout
             -> Vk.Image         -- ^ Destination image
              ⊸ Vk.ImageLayout   -- ^ Destination image layout
             -> [Vk.ImageResolve]
             -> CommandM m (Vk.Image, Vk.Image)
resolveImage srcImage srcLayout dstImage dstLayout regions =
  unsafeCmd (srcImage, dstImage) $ \buf (src', dst') ->
    Vk.cmdResolveImage buf src' srcLayout dst' dstLayout (Vector.fromList regions)
{-# INLINE resolveImage #-}

-- :| Synchronization Commands |: --

-- | Insert a pipeline barrier
pipelineBarrier :: Linear.MonadIO m
                => Vk.PipelineStageFlags  -- ^ Source stage mask
                -> Vk.PipelineStageFlags  -- ^ Destination stage mask
                -> Vk.DependencyFlags
                -> [Vk.MemoryBarrier]
                -> [Vk.BufferMemoryBarrier '[]]
                -> [Vk.ImageMemoryBarrier '[]]
                -> Command m
pipelineBarrier srcStageMask dstStageMask depFlags memBarriers bufBarriers imgBarriers =
  unsafeCmd_ $ \buf ->
    Vk.cmdPipelineBarrier buf srcStageMask dstStageMask depFlags
      (Vector.fromList memBarriers)
      (Vector.fromList $ fmap Vk.SomeStruct bufBarriers)
      (Vector.fromList $ fmap Vk.SomeStruct imgBarriers)
{-# INLINE pipelineBarrier #-}

-- | Insert a pipeline barrier (Vulkan 1.3 synchronization2)
pipelineBarrier2 :: Linear.MonadIO m => Vk.DependencyInfo -> Command m
pipelineBarrier2 depInfo = unsafeCmd_ (\buf -> Vk.cmdPipelineBarrier2 buf depInfo)
{-# INLINE pipelineBarrier2 #-}

-- | Set an event
setEvent :: Linear.MonadIO m => Vk.Event -> Vk.PipelineStageFlags -> Command m
setEvent event stageMask = unsafeCmd_ (\buf -> Vk.cmdSetEvent buf event stageMask)
{-# INLINE setEvent #-}

-- | Reset an event
resetEvent :: Linear.MonadIO m => Vk.Event -> Vk.PipelineStageFlags -> Command m
resetEvent event stageMask = unsafeCmd_ (\buf -> Vk.cmdResetEvent buf event stageMask)
{-# INLINE resetEvent #-}

-- | Wait for one or more events
waitEvents :: Linear.MonadIO m
           => [Vk.Event]
           -> Vk.PipelineStageFlags  -- ^ Source stage mask
           -> Vk.PipelineStageFlags  -- ^ Destination stage mask
           -> [Vk.MemoryBarrier]
           -> [Vk.BufferMemoryBarrier '[]]
           -> [Vk.ImageMemoryBarrier '[]]
           -> Command m
waitEvents events srcStageMask dstStageMask memBarriers bufBarriers imgBarriers =
  unsafeCmd_ $ \buf ->
    Vk.cmdWaitEvents buf
      (Vector.fromList events)
      srcStageMask dstStageMask
      (Vector.fromList memBarriers)
      (Vector.fromList $ fmap Vk.SomeStruct bufBarriers)
      (Vector.fromList $ fmap Vk.SomeStruct imgBarriers)
{-# INLINE waitEvents #-}

-- | Write a device timestamp into a query pool (Vulkan 1.0)
writeTimestamp :: Linear.MonadIO m
               => Vk.PipelineStageFlagBits  -- ^ Pipeline stage
               -> Vk.QueryPool
               -> Word32                    -- ^ Query index
               -> Command m
writeTimestamp stage queryPool queryIndex =
  unsafeCmd_ (\buf -> Vk.cmdWriteTimestamp buf stage queryPool queryIndex)
{-# INLINE writeTimestamp #-}

-- | Set an event with extended parameters (Vulkan 1.3 synchronization2)
setEvent2 :: Linear.MonadIO m => Vk.Event -> Vk.DependencyInfo -> Command m
setEvent2 event depInfo = unsafeCmd_ (\buf -> Vk.cmdSetEvent2 buf event depInfo)
{-# INLINE setEvent2 #-}

-- | Reset an event (Vulkan 1.3 synchronization2)
resetEvent2 :: Linear.MonadIO m => Vk.Event -> Vk.PipelineStageFlags2 -> Command m
resetEvent2 event stageMask = unsafeCmd_ (\buf -> Vk.cmdResetEvent2 buf event stageMask)
{-# INLINE resetEvent2 #-}

-- | Wait for events (Vulkan 1.3 synchronization2)
waitEvents2 :: Linear.MonadIO m => [Vk.Event] -> [Vk.DependencyInfo] -> Command m
waitEvents2 events depInfos = unsafeCmd_ $ \buf ->
  Vk.cmdWaitEvents2 buf (Vector.fromList events) (Vector.fromList depInfos)
{-# INLINE waitEvents2 #-}

-- | Write a device timestamp (Vulkan 1.3 synchronization2)
writeTimestamp2 :: Linear.MonadIO m
                => Vk.PipelineStageFlags2  -- ^ Pipeline stage
                -> Vk.QueryPool
                -> Word32                  -- ^ Query index
                -> Command m
writeTimestamp2 stage queryPool queryIndex =
  unsafeCmd_ (\buf -> Vk.cmdWriteTimestamp2 buf stage queryPool queryIndex)
{-# INLINE writeTimestamp2 #-}

-- :| Query Commands |: --

-- | Begin a query
beginQuery :: Linear.MonadIO m
           => Vk.QueryPool
           -> Word32              -- ^ Query index
           -> Vk.QueryControlFlags
           -> Command m
beginQuery queryPool queryIndex flags =
  unsafeCmd_ (\buf -> Vk.cmdBeginQuery buf queryPool queryIndex flags)
{-# INLINE beginQuery #-}

-- | End a query
endQuery :: Linear.MonadIO m => Vk.QueryPool -> Word32 -> Command m
endQuery queryPool queryIndex =
  unsafeCmd_ (\buf -> Vk.cmdEndQuery buf queryPool queryIndex)
{-# INLINE endQuery #-}

-- | Reset a query pool
resetQueryPool :: Linear.MonadIO m
               => Vk.QueryPool
               -> Word32  -- ^ First query
               -> Word32  -- ^ Query count
               -> Command m
resetQueryPool queryPool firstQuery queryCount =
  unsafeCmd_ (\buf -> Vk.cmdResetQueryPool buf queryPool firstQuery queryCount)
{-# INLINE resetQueryPool #-}

-- | Copy query results to a buffer
copyQueryPoolResults :: Linear.MonadIO m
                     => Vk.QueryPool
                     -> Word32              -- ^ First query
                     -> Word32              -- ^ Query count
                     -> Vk.Buffer           -- ^ Destination buffer
                      ⊸ Vk.DeviceSize       -- ^ Destination offset
                     -> Vk.DeviceSize       -- ^ Stride
                     -> Vk.QueryResultFlags
                     -> CommandM m Vk.Buffer
copyQueryPoolResults queryPool firstQuery queryCount buffer dstOffset stride flags =
  unsafeCmd buffer $ \buf buffer' ->
    Vk.cmdCopyQueryPoolResults buf queryPool firstQuery queryCount buffer' dstOffset stride flags
{-# INLINE copyQueryPoolResults #-}

-- :| Secondary Command Buffers |: --

-- | Execute secondary command buffers from a primary command buffer
executeCommands :: Linear.MonadIO m => [Vk.CommandBuffer] -> Command m
executeCommands cmdBuffers = unsafeCmd_ $ \buf ->
  Vk.cmdExecuteCommands buf (Vector.fromList cmdBuffers)
{-# INLINE executeCommands #-}

----- Linear Unsafe Utils

-- Note how `a` is used unrestrictedly in the function `f`. This is because
-- often this function will be a Vulkan function which isn't linear.

unsafeCmd :: Linear.MonadIO m => a ⊸ (Vk.CommandBuffer -> a -> IO ()) -> CommandM m a
unsafeCmd = Unsafe.toLinear \a f -> (Command $ ReaderT \CmdInfo{buf} -> a Linear.<$ Linear.liftSystemIO (f buf a))

unsafeCmd_ :: Linear.MonadIO m => (Vk.CommandBuffer -> IO ()) -> Command m
unsafeCmd_ = Unsafe.toLinear \f -> (Command $ ReaderT \CmdInfo{buf} -> Linear.liftSystemIO (f buf))

-- | Unsafe for lots of reasons
unsafeRenderCmd :: Linear.MonadIO m => a ⊸ (Vk.CommandBuffer -> a -> IO ()) -> RenderCmdM m a
unsafeRenderCmd = Unsafe.toLinear \a f -> (RenderCmd $ Command $ ReaderT \CmdInfo{buf} -> a Linear.<$ Linear.liftSystemIO (f buf a))

unsafeRenderCmd_ :: Linear.MonadIO m => (Vk.CommandBuffer -> IO ()) -> RenderCmd m
unsafeRenderCmd_ = Unsafe.toLinear \f -> (RenderCmd $ Command $ ReaderT \CmdInfo{buf} -> Linear.liftSystemIO (f buf))

