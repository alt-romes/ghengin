{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import Geomancy.Vec3
import Ghengin.Core
import Ghengin.Core.Input
import Ghengin.Core.Mesh
import Ghengin.Core.Material
import Ghengin.Core.Prelude as Linear
import Ghengin.Core.Render
import Ghengin.Core.Shader.Data
import Ghengin.Core.Render.Property
import Ghengin.Core.Render.Pipeline
import Ghengin.Core.Render.Queue
import qualified Data.Monoid.Linear as LMon
import qualified Prelude
import qualified Data.Linear.Alias as Alias
import Shaders
import qualified FIR

--------------------------------------------------------------------------------
-- Oscilloscope Demo
--
-- Based on:
--  http://nicktasios.nl/posts/simulating-an-xy-oscilloscope-on-the-gpu.html#
--  https://m1el.github.io/woscope-how/
--  https://madebyevan.com/shaders/fast-rounded-rectangle-shadows/ (erf function)
-- Bufferless rendering as seen in:
--  https://www.saschawillems.de/blog/2016/08/13/vulkan-tutorial-on-rendering-a-fullscreen-quad-without-buffers/
--------------------------------------------------------------------------------

gameLoop :: PipelineKey a '[ InStruct "t" Float ] -> Alias RenderPass ⊸ RenderQueue () ⊸ Core (RenderQueue ())
gameLoop pkey rp rq = Linear.do
 Ur should_close <- shouldCloseWindow
 if should_close then Alias.forget rp >> return rq else Linear.do
  pollWindowEvents

  rq <- liftCore $
    editPipeline pkey rq $ propertyAt @0 @(InStruct "t" Float) $ \(Ur (InStruct time)) ->
      pure (Ur (InStruct (time+0.016)))

  (rp, rq) <- renderWith $ Linear.do

    (rp1, rp2) <- lift (Alias.share rp)

    Ur extent <- lift getRenderExtent
    
    renderPassCmd extent rp1 $ Linear.do

      rq <- renderQueueCmd rq

      draw 3

      return (rp2, rq)

  gameLoop pkey rp rq

main :: Prelude.IO ()
main = do
 withLinearIO $
  runRenderer (720, 720) Linear.do

    (rp1, rp2) <- (Alias.share =<< createRenderPassFromSettings RenderPassSettings{keepColor = True} ↑)

    rp1 <- clearImages rp1

    pipeline      <- (makeRenderPipelineWith defaultGraphicsPipelineSettings{cullMode=CullBack, blendMode=BlendAdd} rp1 shaderPipelineSimple (DynamicBinding (Ur (InStruct 0)) :## GHNil) ↑)
    (rq, Ur pkey) <- pure (insertPipeline pipeline LMon.mempty)

    rq <- gameLoop pkey rp2 rq

    (freeRenderQueue rq ↑)

    return (Ur ())

