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
-- Book of Shader: Shaping Functions
--
-- Bufferless rendering as seen in:
--  https://www.saschawillems.de/blog/2016/08/13/vulkan-tutorial-on-rendering-a-fullscreen-quad-without-buffers/
--------------------------------------------------------------------------------

gameLoop :: CharStream -> PipelineKey a '[ Sides, InStruct "t" Float ]
         -> Alias RenderPass ⊸ RenderQueue () ⊸ Core (RenderQueue ())
gameLoop cs pkey rp rq = Linear.do
 should_close <- (shouldCloseWindow ↑)
 if should_close then (Alias.forget rp ↑) >> return rq else Linear.do
  (pollWindowEvents ↑)

  Ur mci <- readCharInput cs
  rq <- case mci of

    Just '+' -> liftCore $
      editPipeline pkey rq $ propertyAt @0 @Sides $ \(Ur sides) ->
        pure (Ur sides{s=sides.s*2, off_x=sides.off_x*2, off_y=sides.off_y*2})

    Just '-' -> liftCore $
      editPipeline pkey rq $ propertyAt @0 @Sides $ \(Ur sides) ->
        pure (Ur sides{s=sides.s/2, off_x=sides.off_x/2, off_y=sides.off_y/2})

    _ -> pure rq

  rq <- liftCore $
    editPipeline pkey rq $ propertyAt @1 @(InStruct "t" Float) $ \(Ur (InStruct time)) ->
      pure (Ur (InStruct (time+0.016)))

  (rp, rq) <- renderWith $ Linear.do

    (rp1, rp2) <- lift (Alias.share rp)

    Ur extent <- lift getRenderExtent
    
    renderPassCmd extent rp1 $ Linear.do

      rq <- renderQueueCmd rq

      draw 3

      return (rp2, rq)

  gameLoop cs pkey rp rq

main :: Prelude.IO ()
main = do
 withLinearIO $
  runCore (720, 720) Linear.do
    Ur cs <- registerCharStream

    (rp1, rp2) <- (Alias.share =<< createRenderPassFromSettings RenderPassSettings{keepColor=True} ↑)

    let sides = Sides {s=2, off_x=(-1), off_y=(-1)}
        time = 0

    pipeline      <- (makeRenderPipelineWith defaultGraphicsPipelineSettings{cullMode=CullBack} rp1 shaderPipelineSimple (DynamicBinding (Ur sides) :## DynamicBinding (Ur (InStruct time)) :## GHNil) ↑)
    (rq, Ur pkey) <- pure (insertPipeline pipeline LMon.mempty)

    rq <- gameLoop cs pkey rp2 rq

    (freeRenderQueue rq ↑)

    return (Ur ())

data Sides = Sides { s :: Float, off_x :: Float, off_y :: Float }
  deriving Generic
  deriving anyclass Block

instance ShaderData Sides where
  type FirType Sides
            = FIR.Struct '[ "s" 'FIR.:-> Float
                          , "off_x" 'FIR.:-> Float
                          , "off_y" 'FIR.:-> Float
                          ]
  
  

