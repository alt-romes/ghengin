{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import GHC.Float.RealFracMethods
import GHC.Float
import Geomancy
import Control.Monad.STM
import Control.Concurrent.STM.TChan
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
import qualified Data.Linear.Alias as Alias

import Shaders
import qualified FIR
import qualified Graphics.UI.GLFW as GLFW
import qualified Unsafe.Linear as Unsafe -- GLFW shenanigans

width, height :: Num a => a
width = 1920
height = 1080

projection :: Float {-^ zoom -} -> InStruct "proj" Mat4
projection zoom = InStruct $ unTransform $ orthoOffCenter 0 100 (roundFloatInt $ width/zoom) (roundFloatInt $ height/zoom)

sampleVertices :: Float {-^ Start -} -> Float {-^ Increment -} -> [Vertex '[Vec3]] -- ToDo: could be a single Int (must check it works)
sampleVertices start increment = go start where
  go !i = Sin (vec3 i 0 0) : go (i+increment)

gameLoop :: Float -- ^ Zoom
         -> TChan (Either Double GLFW.Key) -- ^ YScroll or Key
         -> PipelineKey _ '[InStruct "proj" Mat4, InStruct "x" Float]
         -> Alias RenderPass
          ⊸ RenderQueue ()
          ⊸ Core (RenderQueue ())
gameLoop zoom keys pipkey rp rq = Linear.do
 should_close <- (shouldCloseWindow ↑)
 if should_close then (Alias.forget rp ↑) >> return rq else Linear.do
  (pollWindowEvents ↑)

  Ur mkey <- liftSystemIOU $ atomically $ tryReadTChan keys
  (rq, Ur zoom) <- case mkey of
    Just (Left yscroll)
      | let zoom' = zoom + double2Float yscroll
      , zoom' P.>= 1
      , zoom' P.<= 100 -> Linear.do
        rq <- (editPipeline pipkey rq (propertyAt @0 @(InStruct "proj" Mat4) (\(Ur (InStruct _proj)) -> pure $ Ur $
          projection zoom')) ↑)
        return (rq, Ur zoom')
    Just (Right key)
      | let n = if zoom P.< 10 then 10 {- move faster at low zoom -} else 1
      , Just adjust <- case key of
          GLFW.Key'Right -> Just (+n)
          GLFW.Key'D     -> Just (+n)
          GLFW.Key'Left  -> Just (P.-n)
          GLFW.Key'A     -> Just (P.-n)
          _              -> Nothing
      -> Linear.do
        rq <- (editPipeline pipkey rq (propertyAt @1 @(InStruct "x" Float) (\(Ur (InStruct off)) -> pure $ Ur $
          InStruct $ adjust off)) ↑)
        return (rq, Ur zoom)
    _        -> return (rq, Ur zoom)

  (rp', rq) <- render rp rq

  gameLoop zoom keys pipkey rp' rq

scrollBack :: TChan (Either Double GLFW.Key) -> GLFW.ScrollCallback
scrollBack chan _ _ yoffset = do
  atomically $ writeTChan chan (Left yoffset)

keyPress :: TChan (Either Double GLFW.Key) -> GLFW.KeyCallback
keyPress keys _ key _ _ _ = do
  atomically $ writeTChan keys (Right key)

main :: Prelude.IO ()
main = do
  chan <- newTChanIO

  let samples :: Num a => a
      samples = 1000000

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
                   StaticBinding (Ur (InStruct @"bo" DO_APPLY_TO_FUN)) :##
                   GHNil
  let grid_props = StaticBinding (Ur (InStruct @"c" $ vec3 0.2 0.2 0.2)) :##
                   StaticBinding (Ur (InStruct @"b" DONT_APPLY)) :##
                   StaticBinding (Ur (InStruct @"bo" DONT_APPLY)) :##
                   GHNil
  let axis_props = StaticBinding (Ur (InStruct @"c" $ vec3 1 1 1)) :##
                   StaticBinding (Ur (InStruct @"b" DONT_APPLY)) :##
                   StaticBinding (Ur (InStruct @"bo" DO_APPLY_TO_VERT)) :##
                   GHNil
  let verts  = Prelude.take samples (sampleVertices (-width) (width*2/samples))
  let shader = shaderPipelineSimple @(FIR.Line FIR.Strip)

  let pipeline_props = StaticBinding (Ur (projection 10)) :##
                       StaticBinding (Ur (InStruct @"x" @Float 0)) :##
                       GHNil

  withLinearIO $
   runCore (width, height) Linear.do

     (withWindow (Unsafe.toLinear $ \w -> Linear.do
      liftSystemIO $ do
        GLFW.setScrollCallback w (Just (scrollBack chan))
        GLFW.setKeyCallback w (Just (keyPress chan)) 
      return w) ↑)

     rp <- (createSimpleRenderPass ↑)
     (rp1, rp2) <- (Alias.share rp ↑)
     pipeline <- (makeRenderPipeline (shader f) pipeline_props rp1 ↑)
     (emptyMat, pipeline) <- (material GHNil pipeline ↑)
     (gridMeshX, pipeline) <- (createMesh pipeline grid_props (gridVertsX width height) ↑)
     (gridMeshY, pipeline) <- (createMesh pipeline grid_props (gridVertsY width height) ↑)
     (axisMesh, pipeline) <- (createMesh pipeline axis_props (axisVerts width height) ↑)
     (functionMesh, pipeline) <- (createMesh pipeline line_props verts ↑)
     (rq, Ur pkey)    <- pure (insertPipeline pipeline LMon.mempty)
     (rq, Ur mkey)    <- pure (insertMaterial pkey emptyMat rq)
     (rq, Ur _gmk)    <- pure (insertMesh mkey gridMeshX rq)
     (rq, Ur _gmk)    <- pure (insertMesh mkey gridMeshY rq)
     (rq, Ur _gmk)    <- pure (insertMesh mkey axisMesh rq)
     (rq, Ur _mshk)    <- pure (insertMesh mkey functionMesh rq)

     rq <- gameLoop 1 chan pkey rp2 rq

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
