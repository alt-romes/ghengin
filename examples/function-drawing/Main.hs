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
-- Book of Shader: Shaping Functions
--
-- Bufferless rendering as seen in:
--  https://www.saschawillems.de/blog/2016/08/13/vulkan-tutorial-on-rendering-a-fullscreen-quad-without-buffers/
--------------------------------------------------------------------------------

gameLoop :: Alias RenderPass ⊸ RenderQueue () ⊸ Core (RenderQueue ())
gameLoop rp rq = Linear.do
 should_close <- (shouldCloseWindow ↑)
 if should_close then (Alias.forget rp ↑) >> return rq else Linear.do
  (pollWindowEvents ↑)

  (rp, rq) <- renderWith $ Linear.do

    (rp1, rp2) <- lift (Alias.share rp)

    Ur extent <- lift getRenderExtent
    
    renderPassCmd extent rp1 $ Linear.do

      rq <- renderQueueCmd rq

      draw 3

      return (rp2, rq)

  gameLoop rp rq

main :: Prelude.IO ()
main = do
 withLinearIO $
  runCore (720, 720) Linear.do

    (rp1, rp2) <- (Alias.share =<< createSimpleRenderPass ↑)

    let sides = Sides {x=10, y=10, off_x=(-5), off_y=(-5)}

    pipeline      <- (makeRenderPipelineWith GPS{cullMode=CullBack} rp1 shaderPipelineSimple (DynamicBinding (Ur sides) :## GHNil) ↑)
    (rq, Ur pkey) <- pure (insertPipeline pipeline LMon.mempty)

    rq <- gameLoop rp2 rq

    (freeRenderQueue rq ↑)

    return (Ur ())

data Sides = Sides { x :: Float, y :: Float, off_x :: Float, off_y :: Float }
  deriving Generic
  deriving anyclass Block

instance ShaderData Sides where
  type FirType Sides
            = FIR.Struct '[ "x" 'FIR.:-> Float
                          , "y" 'FIR.:-> Float
                          , "off_x" 'FIR.:-> Float
                          , "off_y" 'FIR.:-> Float
                          ]
  

