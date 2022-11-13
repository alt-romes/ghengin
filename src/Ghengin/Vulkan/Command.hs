{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
module Ghengin.Vulkan.Command where

import Data.Word
import qualified Vulkan.Zero as Vk
import qualified Vulkan      as Vk

import Control.Monad.Free.Church
import Control.Monad.Free.TH

type Command = F CommandF ()

type RenderPassCmd = F RenderPassCmdF ()

data CommandF next where
  RenderPass :: Vk.RenderPass -> Vk.Framebuffer -> Vk.Extent2D -> RenderPassCmd -> next -> CommandF next
  deriving Functor

-- instance Functor CommandF where
--   fmap f = \case
--     RenderPass rpc n -> RenderPass rpc (f n)

data RenderPassCmdF next where
  BindGraphicsPipeline   :: Vk.Pipeline -> next -> RenderPassCmdF next
  BindComputePipeline    :: Vk.Pipeline -> next -> RenderPassCmdF next
  BindRayTracingPipeline :: Vk.Pipeline -> next -> RenderPassCmdF next
  SetViewport  :: Vk.Viewport -> next -> RenderPassCmdF next
  SetScissor   :: Vk.Rect2D   -> next -> RenderPassCmdF next
  Draw         :: Word32      -> next -> RenderPassCmdF next
  deriving Functor

-- instance Functor RenderPassCmdF where
--   fmap f = \case
--     BindPipeline x n -> BindPipeline x (f n)
--     SetViewport  x n -> SetViewport  x (f n)
--     SetScissor   x n -> SetScissor   x (f n)
--     Draw         x n -> Draw         x (f n)

makeFree_ ''CommandF

renderPass :: Vk.RenderPass -> Vk.Framebuffer -> Vk.Extent2D -> RenderPassCmd -> Command
{-# INLINE renderPass #-}

makeFree_ ''RenderPassCmdF

bindGraphicsPipeline :: Vk.Pipeline -> RenderPassCmd
bindComputePipeline :: Vk.Pipeline -> RenderPassCmd
bindRayTracingPipeline :: Vk.Pipeline -> RenderPassCmd
setViewport  :: Vk.Viewport -> RenderPassCmd
setScissor   :: Vk.Rect2D   -> RenderPassCmd
draw         :: Word32      -> RenderPassCmd
{-# INLINE bindGraphicsPipeline #-}
{-# INLINE bindComputePipeline #-}
{-# INLINE bindRayTracingPipeline #-}
{-# INLINE setViewport #-}
{-# INLINE setScissor #-}
{-# INLINE draw #-}


recordCommand :: Vk.CommandBuffer -> Command -> IO ()
recordCommand buf cmd = do
  let beginInfo = Vk.CommandBufferBeginInfo { next = (), flags = Vk.zero
                                            , inheritanceInfo = Nothing }

  -- Begin recording
  Vk.beginCommandBuffer buf beginInfo

  -- Record commands
  iterM interpretCommand cmd

  -- Finish recording
  Vk.endCommandBuffer buf

  where
    interpretCommand :: CommandF (IO ()) -> IO ()
    interpretCommand = \case
      RenderPass rpass frameBuffer renderAreaExtent rpcmd next -> do
        let
          renderPassInfo = Vk.RenderPassBeginInfo { next = ()
                                                  , renderPass  = rpass
                                                  , framebuffer = frameBuffer
                                                  , renderArea  = Vk.Rect2D (Vk.Offset2D 0 0) renderAreaExtent
                                                  , clearValues = [Vk.Color $ Vk.Float32 1 1 1 1]
                                                  }

        Vk.cmdBeginRenderPass buf renderPassInfo Vk.SUBPASS_CONTENTS_INLINE

        iterM interpretRenderPassCmd rpcmd
        
        next

        Vk.cmdEndRenderPass buf


    interpretRenderPassCmd :: RenderPassCmdF (IO ()) -> IO ()
    interpretRenderPassCmd = \case
      BindGraphicsPipeline   pp next -> Vk.cmdBindPipeline buf Vk.PIPELINE_BIND_POINT_GRAPHICS        pp >> next
      BindComputePipeline    pp next -> Vk.cmdBindPipeline buf Vk.PIPELINE_BIND_POINT_COMPUTE         pp >> next
      BindRayTracingPipeline pp next -> Vk.cmdBindPipeline buf Vk.PIPELINE_BIND_POINT_RAY_TRACING_KHR pp >> next
      SetViewport  viewport next -> Vk.cmdSetViewport buf 0 [viewport] >> next
      SetScissor   scissor  next -> Vk.cmdSetScissor  buf 0 [scissor]  >> next
      Draw      vertexCount next -> Vk.cmdDraw buf vertexCount 1 0 0   >> next

