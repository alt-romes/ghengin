{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import Ghengin.Core
import Ghengin.Core.Prelude as Linear
import Ghengin.Core.Render
import Ghengin.Core.Render.Pipeline
import Ghengin.Core.Render.Queue
import qualified Data.Monoid.Linear as LMon
import qualified Prelude
import qualified Data.Linear.Alias as Alias
import Shaders

import qualified Ghengin.DearImGui.Vulkan as ImGui

--------------------------------------------------------------------------------
-- Bufferless rendering as seen in:
--  https://www.saschawillems.de/blog/2016/08/13/vulkan-tutorial-on-rendering-a-fullscreen-quad-without-buffers/
--------------------------------------------------------------------------------

gameLoop :: RenderQueue () ⊸ Renderer (RenderQueue ())
gameLoop rq = Linear.do
 Ur should_close <- shouldCloseWindow
 if should_close then return rq else Linear.do
  pollWindowEvents

  -- Prepare Imgui data
  ImGui.withNewFrame $ do

    ImGui.showDemoWindow

  rq <- newFrame \frameIndex imageIndex ->
    renderWith frameIndex imageIndex $ \rinfo -> Linear.do
      Ur extent <- lift getRenderExtent
      let viewport = viewportFromExtent extent
          scissor  = scissorFromExtent extent

      beginRendering rinfo $ Linear.do
        setViewport viewport
        setScissor scissor

        rq <- renderQueueCmd rq
        draw 3 1

        -- Render Imgui data!
        ImGui.renderDrawData

        return rq

  gameLoop rq

main :: Prelude.IO ()
main = do
 withLinearIO $
  runRenderer (width, height) Linear.do

    -- Init imgui
    imctx <- ImGui.initImGui

    pipeline      <- makeRenderPipelineWith defaultGraphicsPipelineSettings{cullMode=CullFront} shaderPipelineSimple GHNil
    (rq, Ur pkey) <- pure (insertPipeline pipeline LMon.mempty)

    rq <- gameLoop rq

    freeRenderQueue rq
    -- Then destroy it
    ImGui.destroyImCtx imctx

    return (Ur ())



