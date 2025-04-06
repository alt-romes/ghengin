{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import Geomancy
import Geomancy.Vulkan.Projection
import Ghengin.Core.Shader.Data
import Ghengin.Core
import Ghengin.Core.Mesh
import Ghengin.Core.Material
import Ghengin.Core.Prelude as Linear
import Ghengin.Core.Render
import Ghengin.Core.Render.Property
import Ghengin.Core.Render.Pipeline
import Ghengin.Core.Render.Queue
import qualified Data.Monoid.Linear as LMon
import qualified Prelude

import Shaders
import qualified FIR

width, height :: Num a => a
width = 1920
height = 1080

projection :: InStruct "proj" Mat4
projection = InStruct $ unTransform $ orthoOffCenter 0 100 width (height :: Int)

sampleVertices :: Float {-^ Start -} -> Float {-^ Increment -} -> [Vertex '[Vec3]] -- ToDo: could be a single Int (must check it works)
sampleVertices start increment = go start where
  go !i = Sin (vec3 i 0 0) : go (i+increment)

gameLoop :: RenderQueue () ⊸ Core (RenderQueue ())
gameLoop rq = Linear.do
 should_close <- (shouldCloseWindow ↑)
 if should_close then return rq else Linear.do
  (pollWindowEvents ↑)

  rq <- render rq

  gameLoop rq

main :: Prelude.IO ()
main = do

  let samples :: Num a => a
      samples = 100000

  -- Function to Plot
  let f x = (FIR.sin x) FIR.* x
  let g x = x FIR.* FIR.Lit 2 FIR.+ FIR.Lit 1
  let weierstrass x
        = Prelude.foldl' (\acc (FIR.Lit -> n) -> (a FIR.** n FIR.* FIR.cos (b FIR.** n FIR.* FIR.pi FIR.* x)) FIR.+ acc) (FIR.Lit 0) [0..10]
          where
            a = FIR.Lit 0.6
            b = FIR.Lit 11

  -- Line from -x to +x and -y to y
  let grid_x = [ Sin (vec3 (-width/2) 0 0)
               , Sin (vec3 (width/2) 0 0)
               ]
  let grid_y = [ Sin (vec3 0 (-height/2) 0)
               , Sin (vec3 0 (height/2) 0)
               ]

  let line_props = StaticBinding (Ur (InStruct @"c" $ vec3 0.4 0.76 0.33)) :##
                   StaticBinding (Ur (InStruct @"b" DO_APPLY)) :##
                   GHNil
  let grid_props = StaticBinding (Ur (InStruct @"c" $ vec3 1 1 1)) :##
                   StaticBinding (Ur (InStruct @"b" DONT_APPLY)) :##
                   GHNil
  let verts  = Prelude.take samples (sampleVertices (-width) (width*2/samples))
  let shader = shaderPipelineSimple @(FIR.Line FIR.Strip) g

  let pipeline_props = StaticBinding (Ur projection) :## GHNil

  withLinearIO $
   runCore (1920, 1080) Linear.do
     pipeline <- (makeRenderPipeline shader pipeline_props ↑)
     (emptyMat, pipeline) <- (material GHNil pipeline ↑)
     (gridMeshX, pipeline) <- (createMesh pipeline grid_props grid_x ↑)
     (gridMeshY, pipeline) <- (createMesh pipeline grid_props grid_y ↑)
     (mesh, pipeline) <- (createMesh pipeline line_props verts ↑)
     (rq, Ur pkey)    <- pure (insertPipeline pipeline LMon.mempty)
     (rq, Ur mkey)    <- pure (insertMaterial pkey emptyMat rq)
     (rq, Ur _gmk)    <- pure (insertMesh mkey gridMeshX rq)
     (rq, Ur _gmk)    <- pure (insertMesh mkey gridMeshY rq)
     (rq, Ur _mshk)   <- pure (insertMesh mkey mesh rq)

     rq <- gameLoop rq

     (freeRenderQueue rq ↑)

     return (Ur ())

