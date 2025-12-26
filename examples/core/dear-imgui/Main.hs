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

gameLoop :: Alias RenderPass ⊸ RenderQueue () ⊸ Renderer (RenderQueue ())
gameLoop rp rq = Linear.do
 Ur should_close <- shouldCloseWindow
 if should_close then Alias.forget rp >> return rq else Linear.do
  pollWindowEvents

  -- Prepare Imgui data
  ImGui.withNewFrame $ do

    ImGui.showDemoWindow

  (rp, rq) <- renderWith $ Linear.do

    (rp1, rp2) <- lift (Alias.share rp)

    Ur extent <- lift getRenderExtent
    
    renderPassCmd extent rp1 $ Linear.do

      rq <- renderQueueCmd rq
      draw 3

      -- Render Imgui data!
      ImGui.renderDrawData

      return (rp2, rq)

  gameLoop rp rq

main :: Prelude.IO ()
main = do
 withLinearIO $
  runRenderer (width, height) Linear.do

    (rp1, rp2) <- Alias.share =<< createSimpleRenderPass

    -- Init imgui
    (rp1, imctx) <- Alias.useM rp1 ImGui.initImGui

    pipeline      <- makeRenderPipelineWith defaultGraphicsPipelineSettings{cullMode=CullFront} rp1 shaderPipelineSimple GHNil
    (rq, Ur pkey) <- pure (insertPipeline pipeline LMon.mempty)

    rq <- gameLoop rp2 rq

    freeRenderQueue rq
    -- Then destroy it
    ImGui.destroyImCtx imctx

    return (Ur ())



