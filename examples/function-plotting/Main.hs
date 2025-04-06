{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import GHC.Float
import Geomancy
import Control.Monad.STM
import Control.Concurrent.STM.TChan
import qualified Geomancy.Transform as T
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
import qualified Prelude as P

import Shaders
import qualified FIR
import qualified Graphics.UI.GLFW as GLFW
import qualified Unsafe.Linear as Unsafe -- GLFW shenanigans

width, height :: Num a => a
width = 1920
height = 1080

projection :: InStruct "proj" Mat4
projection = InStruct $ unTransform $ orthoOffCenter 0 100 width (height :: Int)

sampleVertices :: Float {-^ Start -} -> Float {-^ Increment -} -> [Vertex '[Vec3]] -- ToDo: could be a single Int (must check it works)
sampleVertices start increment = go start where
  go !i = Sin (vec3 i 0 0) : go (i+increment)

gameLoop :: TChan Double -- ^ YScroll
         -> PipelineKey _ '[InStruct "proj" Mat4, InStruct "sproj" Mat4]
         -> RenderQueue ()
          ⊸ Core (RenderQueue ())
gameLoop keys pipkey rq = Linear.do
 should_close <- (shouldCloseWindow ↑)
 if should_close then return rq else Linear.do
  (pollWindowEvents ↑)

  Ur mkey <- liftSystemIOU $ atomically $ tryReadTChan keys
  rq <- case mkey of
    Just yscroll -> Linear.do
      (editPipeline pipkey rq (propertyAt @1 @(InStruct "sproj" Mat4) (\(Ur (InStruct pscale)) -> pure $ Ur $
        InStruct $ unTransform (T.scale (double2Float (if yscroll > 0 then yscroll else 1/yscroll))) <> pscale)) ↑)
    _        -> return rq

  rq <- render rq

  gameLoop keys pipkey rq

scrollBack :: TChan Double -> GLFW.ScrollCallback
scrollBack chan _ _ yoffset = do
  atomically $ writeTChan chan yoffset

main :: Prelude.IO ()
main = do
  chan <- newTChanIO

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

  let line_props = StaticBinding (Ur (InStruct @"c" $ vec3 0.4 0.76 0.33)) :##
                   StaticBinding (Ur (InStruct @"b" DO_APPLY)) :##
                   GHNil
  let grid_props = StaticBinding (Ur (InStruct @"c" $ vec3 0.2 0.2 0.2)) :##
                   StaticBinding (Ur (InStruct @"b" DONT_APPLY)) :##
                   GHNil
  let axis_props = StaticBinding (Ur (InStruct @"c" $ vec3 1 1 1)) :##
                   StaticBinding (Ur (InStruct @"b" DONT_APPLY)) :##
                   GHNil
  let verts  = Prelude.take samples (sampleVertices (-width) (width*2/samples))
  let shader = shaderPipelineSimple @(FIR.Line FIR.Strip) weierstrass

  let scaleN = 10
  let scaleProj = InStruct @"sproj" (unTransform $ T.scale scaleN)
  let pipeline_props = StaticBinding (Ur projection) :##
                       StaticBinding (Ur scaleProj) :##
                       GHNil

  -- let gw = ceilingFloatInt $ width / scaleN
  -- let gh = ceilingFloatInt $ height / scaleN
  -- no need ^; verts don't need to be in the bounding volume to be culled; the whole frag probably needs to be
  let gw = width
  let gh = height

  withLinearIO $
   runCore (width, height) Linear.do

     (withWindow (Unsafe.toLinear $ \w -> Linear.do
      liftSystemIO (GLFW.setScrollCallback w (Just (scrollBack chan)))
      return w) ↑)

     pipeline <- (makeRenderPipeline shader pipeline_props ↑)
     (emptyMat, pipeline) <- (material GHNil pipeline ↑)
     (gridMeshX, pipeline) <- (createMesh pipeline grid_props (gridVertsX gw gh) ↑)
     (gridMeshY, pipeline) <- (createMesh pipeline grid_props (gridVertsY gw gh) ↑)
     (axisMesh, pipeline) <- (createMesh pipeline axis_props (axisVerts width height) ↑)
     (functionMesh, pipeline) <- (createMesh pipeline line_props verts ↑)
     (rq, Ur pkey)    <- pure (insertPipeline pipeline LMon.mempty)
     (rq, Ur mkey)    <- pure (insertMaterial pkey emptyMat rq)
     (rq, Ur _gmk)    <- pure (insertMesh mkey gridMeshX rq)
     (rq, Ur _gmk)    <- pure (insertMesh mkey gridMeshY rq)
     (rq, Ur _gmk)    <- pure (insertMesh mkey axisMesh rq)
     (rq, Ur _mshk)    <- pure (insertMesh mkey functionMesh rq)

     rq <- gameLoop chan pkey rq

     (freeRenderQueue rq ↑)

     return (Ur ())

--------------------------------------------------------------------------------
-- Grid
--------------------------------------------------------------------------------

gridVertsX :: Int -> Int -> [Vertex '[Vec3]]
gridVertsX w h = do
  x <- [-w..w]
  (a,b) <-
    if P.even x then do
      [(x, -h), (x, h), (x, -h)]
    else do
      [(-x, -h), (-x, h), (-x, -h)]
  P.return (Sin (vec3 (fromIntegral a) (fromIntegral b) 1))

gridVertsY :: Int -> Int -> [Vertex '[Vec3]]
gridVertsY w h = do
  y <- [-h..h]
  (a,b) <-
    if P.even y then do
      [(-w, y), (w, y), (-w, y)]
    else do
      [(-w, -y), (w, -y), (-w, -y)]
  P.return (Sin (vec3 (fromIntegral a) (fromIntegral b) 1))

axisVerts :: Int -> Int -> [Vertex '[Vec3]]
axisVerts w h = do
  (a, b) <- [(-w, 0), (w, 0), (0,0), (0, h), (0, -h)]
  P.return (Sin (vec3 (fromIntegral a) (fromIntegral b) 0))
