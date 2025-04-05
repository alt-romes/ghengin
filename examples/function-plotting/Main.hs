{-# LANGUAGE OverloadedRecordDot #-}
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
import Ghengin.Core
import Ghengin.Core.Mesh
import Ghengin.Core.Material
import Ghengin.Core.Prelude as Linear
import Ghengin.Core.Render
import Ghengin.Core.Render.Pipeline
import Ghengin.Core.Render.Queue
import qualified Data.Monoid.Linear as LMon
import qualified Prelude

import Shaders
import qualified FIR
import qualified Math.Linear as FIR

sampleVertices :: Float {-^ Start -} -> Float {-^ Increment -} -> [Vertex '[Vec3]] -- ToDo: could be a single Int (must check it works)
sampleVertices start increment = go start where
  go !i = Sin (vec3 i 0 1) : go (i+increment)

gameLoop :: RenderQueue () ⊸ RenderQueue () ⊸ Core (RenderQueue (), RenderQueue ())
gameLoop rq grid_rq = Linear.do
 should_close <- (shouldCloseWindow ↑)
 if should_close then return (rq, grid_rq) else Linear.do
  (pollWindowEvents ↑)

  grid_rq <- render grid_rq
  rq <- render rq

  gameLoop rq grid_rq

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
  let grid = [
              Sin (vec3 (-xbound) 0 1)
             , Sin (vec3 xbound 0 1)
             , Sin (vec3 0 (-ybound) 1)
             , Sin (vec3 0 (ybound) 1)
             ]

  let verts  = Prelude.take samples (sampleVertices (-xbound) (xbound*2/samples))
  let shader = shaderPipelineSimple @(FIR.Line FIR.Strip) (FIR.V3 0 1 1) xbound ybound f

  -- The grid needs to be separate from the main pipeline because it's for
  -- drawing the grid. Note it uses different arguments despite being the same
  -- function to create the shader. We need to separate because of the Line Strip topology too.
  let grid_shader = gridShaderPipeline @(FIR.Line FIR.List) (FIR.V3 1 1 1) xbound ybound

  withLinearIO $
   runCore (1280, 720) Linear.do
     pipeline <- (makeRenderPipeline shader GHNil ↑)
     (emptyMat, pipeline) <- (material GHNil pipeline ↑)
     (mesh, pipeline) <- (createMesh pipeline GHNil verts ↑)
     (rq, Ur pkey)    <- pure (insertPipeline pipeline LMon.mempty)
     (rq, Ur mkey)    <- pure (insertMaterial pkey emptyMat rq)
     (rq, Ur _mshk)   <- pure (insertMesh mkey mesh rq)

     gridPipeline <- (makeRenderPipeline grid_shader GHNil ↑)
     (gridEmptyMat, gridPipeline) <- (material GHNil gridPipeline ↑)
     (gridMesh, gridPipeline) <- (createMesh gridPipeline GHNil grid ↑)
     (grid_rq, Ur gpkey)  <- pure (insertPipeline gridPipeline LMon.mempty)
     (grid_rq, Ur gmtk)    <- pure (insertMaterial gpkey gridEmptyMat grid_rq)
     (grid_rq, Ur _gmk)    <- pure (insertMesh gmtk gridMesh grid_rq)

     (rq, grid_rq) <- gameLoop rq grid_rq

     (freeRenderQueue rq ↑)
     (freeRenderQueue grid_rq ↑)

     return (Ur ())

