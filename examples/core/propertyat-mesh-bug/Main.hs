{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
-- |
-- Minimal repro for the propertyAt-mesh bug.
--
-- Two cube meshes A and B share one pipeline+material; each has a
-- @'[Transform]@ dynamic binding. On a frame timer we re-create one mesh
-- inside @editAtMeshesKey@, pull its old transform out via @puncons@, and
-- write it back to the fresh mesh with @propertyAt @0 @Transform@.
--
-- We exercise the bug-triggering pattern in a back-and-forth:
--   frame 30  — regen A
--   frame 60  — regen B (clobbers A's GPU buffer too)
--   frame 90  — regen A again; @old_tr@ printout shows B's transform
--               instead of A's, demonstrating the bug in text.
--
-- Run with @cabal run propertyat-mesh-bug-min -f-dev@.
module Main where

import Geomancy.Vec3
import Geomancy.Transform
import Ghengin.Core
import Ghengin.Core.Shader.Data
import Ghengin.Core.Mesh
import Ghengin.Core.Material
import Ghengin.Core.Prelude as Linear hiding (Eq(..))
import Ghengin.Core.Render
import Ghengin.Core.Render.Property
import Ghengin.Core.Render.Pipeline
import Ghengin.Core.Render.Queue
import qualified Data.IORef
import Data.IORef (IORef, atomicModifyIORef')
import Data.List.Linear ()
import qualified Data.Monoid.Linear as LMon
import qualified Prelude
import qualified Math.Linear as FIR
import qualified FIR
import Control.Concurrent
import Control.Exception

import Shaders

type CubeMesh = Mesh '[Vec3, Vec3] '[Transform]

cubeVertices :: [Vertex '[Vec3, Vec3]]
cubeVertices =
  [ vec3 (-0.5) (-0.5) (-0.5) :&: white
  , vec3 (-0.5) (-0.5) ( 0.5) :&: white
  , vec3 (-0.5) ( 0.5) ( 0.5) :&: white
  , vec3 (-0.5) ( 0.5) ( 0.5) :&: red
  , vec3 (-0.5) ( 0.5) (-0.5) :&: red
  , vec3 (-0.5) (-0.5) (-0.5) :&: red
  , vec3 (-0.5) (-0.5) (-0.5) :&: green
  , vec3 ( 0.5) ( 0.5) (-0.5) :&: green
  , vec3 ( 0.5) (-0.5) (-0.5) :&: green
  , vec3 (-0.5) (-0.5) (-0.5) :&: blue
  , vec3 (-0.5) ( 0.5) (-0.5) :&: blue
  , vec3 ( 0.5) ( 0.5) (-0.5) :&: blue ]
  where
    white = vec3 0.9 0.9 0.9
    red   = vec3 0.8 0.1 0.1
    green = vec3 0.1 0.8 0.1
    blue  = vec3 0.1 0.1 0.8

leftTr, rightTr :: Transform
leftTr  = translate (-0.5) 0 0.5 <> scale 0.3
rightTr = translate ( 0.5) 0 0.5 <> scale 0.15

-- | Re-create a mesh inside @editAtMeshesKey@ and re-set its Transform via
-- @propertyAt@. This is the bug-triggering pattern.
forceRegenerate
  :: _
  => Prelude.String
  -> MeshKey π p ma '[Vec3, Vec3] '[Transform]
  -> RenderQueue () ⊸ Renderer (RenderQueue ())
forceRegenerate tag key rq = Linear.do
  (rq, ()) <- editAtMeshesKey key rq $ \pipeline mat [(msh, x)] -> Linear.do
    (newMesh :: CubeMesh, pipeline) <-
      createMesh pipeline (DynamicBinding (Ur (mempty :: Transform)) :## GHNil) cubeVertices

    let !(DynamicBinding (Ur old_tr), msh') = puncons msh
    Linear.liftSystemIO $ Prelude.putStrLn
      ("[" Prelude.++ tag Prelude.++ "] old_tr=" Prelude.++ Prelude.show old_tr)
    freeMesh msh'

    newMesh <- propertyAt @0 @Transform (\(Ur _) -> pure (Ur old_tr)) newMesh

    let !(DynamicBinding (Ur new_tr), msh') = puncons newMesh
    Linear.liftSystemIO $ Prelude.putStrLn
      ("[" Prelude.++ tag Prelude.++ "] new_tr=" Prelude.++ Prelude.show new_tr)

    let newMesh = pcons (DynamicBinding (Ur new_tr)) msh'

    Linear.pure (pipeline, mat, [(newMesh, x)], ())
  Linear.pure rq

gameLoop
  :: _
  => IORef Prelude.Int
  -> MeshKey π p ma '[Vec3, Vec3] '[Transform]
  -> MeshKey π p ma '[Vec3, Vec3] '[Transform]
  -> RenderQueue () ⊸ Renderer (RenderQueue ())
gameLoop frameRef kA kB rq = Linear.do
  Ur should_close <- shouldCloseWindow
  if should_close then Linear.pure rq else Linear.do
    pollWindowEvents

    Ur n <- liftSystemIOU (atomicModifyIORef' frameRef (\m -> (m Prelude.+ 1, m)))
    rq <- case n of
      30  -> Linear.do
        liftSystemIO (Prelude.putStrLn "=== regen A (first) ===")
        forceRegenerate "regen A" kA rq
      60  -> Linear.do
        liftSystemIO (Prelude.putStrLn "=== regen B ===")
        forceRegenerate "regen B" kB rq
      90  -> Linear.do
        liftSystemIO (Prelude.putStrLn "=== regen A ===")
        forceRegenerate "regen A" kA rq
      _   -> Linear.pure rq

    rq <- newFrame \frameIndex imageIndex -> render frameIndex imageIndex rq
    gameLoop frameRef kA kB rq

-- non-compositional instance for "Transform", just for demo
instance ShaderData Transform where
  type FirType Transform = FIR.Struct '[ "m" 'FIR.:-> FIR.M 4 4 Float ]

main :: Prelude.IO ()
main = do
  x <- newEmptyMVar
  forkOS $ do
    main' `finally` putMVar x ()
  takeMVar x

main' :: Prelude.IO ()
main' = do
  frameRef <- Data.IORef.newIORef 0
  withLinearIO $
    runRenderer (640, 640) Linear.do
      pipeline <- makeRenderPipeline shaderPipeline GHNil

      (emptyMat, pipeline) <- material GHNil pipeline

      (meshA :: CubeMesh, pipeline) <-
        createMesh pipeline (DynamicBinding (Ur leftTr)  :## GHNil) cubeVertices
      (meshB :: CubeMesh, pipeline) <-
        createMesh pipeline (DynamicBinding (Ur rightTr) :## GHNil) cubeVertices

      (rq, Ur pkey) <- pure (insertPipeline pipeline LMon.mempty)
      (rq, Ur mkey) <- pure (insertMaterial pkey emptyMat rq)
      (rq, Ur kA)   <- pure (insertMesh mkey meshA rq)
      (rq, Ur kB)   <- pure (insertMesh mkey meshB rq)
      liftSystemIO (Prelude.putStrLn "=== initial state ===")
      liftSystemIO (Prelude.putStrLn "=== regen A (initial) ===")
      liftSystemIO (Prelude.print leftTr)
      liftSystemIO (Prelude.putStrLn "=== regen B (initial) ===")
      liftSystemIO (Prelude.print rightTr)

      rq <- gameLoop frameRef kA kB rq

      freeRenderQueue rq

      Linear.pure (Ur ())
