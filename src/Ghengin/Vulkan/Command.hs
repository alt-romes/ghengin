{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
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
  , draw
  ) where

import Control.Monad.Reader
import Data.Word
import qualified Vulkan.Zero as Vk
import qualified Vulkan      as Vk

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
  deriving (Functor, Applicative, Monad)

newtype RenderPassCmdM a = RenderPassCmd (ReaderT Vk.CommandBuffer IO a)
  deriving (Functor, Applicative, Monad)

-- | Given a 'Vk.CommandBuffer' and the 'Command' to record in this buffer,
-- record the command in the buffer.
recordCommand :: Vk.CommandBuffer -> Command -> IO ()
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

-- | Make a render pass part a command blueprint that can be further composed with other commands
renderPass :: Vk.RenderPass -> Vk.Framebuffer -> Vk.Extent2D -> RenderPassCmd -> Command
renderPass rpass frameBuffer renderAreaExtent (RenderPassCmd rpcmds) = Command $ ask >>= \buf -> do
  let
    renderPassInfo = Vk.RenderPassBeginInfo { next = ()
                                            , renderPass  = rpass
                                            , framebuffer = frameBuffer
                                            , renderArea  = Vk.Rect2D (Vk.Offset2D 0 0) renderAreaExtent
                                            , clearValues = [Vk.Color $ Vk.Float32 1 1 1 1]
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

draw :: Word32 -> RenderPassCmd
draw vertexCount = RenderPassCmd $ ask >>= \buf -> Vk.cmdDraw buf vertexCount 1 0 0
{-# INLINE draw #-}
