{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import Text.Read (readMaybe)
import System.Environment (getArgs)
import Geomancy.Vec3
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

sampleVertices :: Float {-^ Start -} -> Float {-^ Increment -} -> [Vertex '[Vec3]] -- ToDo: could be a single Int (must check it works)
sampleVertices start increment = go start where
  go !i = Sin (vec3 i 0 1) : go (i+increment)

gameLoop :: RenderQueue () ⊸ Core (RenderQueue ())
gameLoop rq = Linear.do
 should_close <- (shouldCloseWindow ↑)
 if should_close then return rq else Linear.do
  (pollWindowEvents ↑)

  rq <- render rq

  gameLoop rq

{-
We use Vulkan's clipping volume directly:

       z ∈ [0, 1]
      /
     /
    *------> x ∈ [-1, 1]
    |
    |
    v
    y ∈ [-1, 1]
-}


main :: Prelude.IO ()
main = do
  args <- getArgs

  let samples :: (Read a, Num a) => a
      samples = case args of
        (ns:_) 
          | Just n <- readMaybe ns
          -> n
        _ -> 10000

  -- Function to Plot
  let f x = (FIR.sin x) FIR.* x
  let xbound = 20
  let ybound = 20

  -- Line from -x to +x and -y to y
  let grid_x = [ Sin (vec3 (-xbound) 0 1)
               , Sin (vec3 xbound 0 1)
               ]
  let grid_y = [ Sin (vec3 0 (-ybound) 1)
               , Sin (vec3 0 (ybound) 1)
               ]

  let line_props = StaticBinding (Ur (InStruct @"c" $ vec3 0.4 0.76 0.33)) :##
                   StaticBinding (Ur (InStruct @"b" DO_APPLY)) :##
                   GHNil
  let grid_props = StaticBinding (Ur (InStruct @"c" $ vec3 1 1 1)) :##
                   StaticBinding (Ur (InStruct @"b" DONT_APPLY)) :##
                   GHNil
  let verts  = Prelude.take samples (sampleVertices (-xbound) (xbound*2/samples))
  let shader = shaderPipelineSimple @(FIR.Line FIR.Strip) xbound ybound f

  withLinearIO $
   runCore (1920, 1080) Linear.do
     pipeline <- (makeRenderPipeline shader GHNil ↑)
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

