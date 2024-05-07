{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import GHC.Generics
import Geomancy.Vec3
import Geomancy.Mat4
import Geomancy.Transform
import Geomancy.Vulkan.View
import Geomancy.Vulkan.Projection
import Ghengin.Core
import Ghengin.Core.Shader.Data
import Ghengin.Core.Mesh
import Ghengin.Core.Material
import Ghengin.Core.Prelude as Linear
import Ghengin.Core.Render
import Ghengin.Core.Render.Property
import Ghengin.Core.Render.Pipeline
import Ghengin.Core.Render.Queue
import Data.List.Linear ()
import qualified Data.Monoid.Linear as LMon
import qualified Prelude
import qualified Math.Linear as FIR
import qualified FIR

import Shaders

type CubeMesh = Mesh '[Vec3, Vec3] '[Transform]

cubeVertices :: [Vertex '[Vec3, Vec3]]
cubeVertices = [

  -- left face (white)
  vec3 (-0.5) (-0.5) (-0.5) :&: white,
  vec3 (-0.5) (-0.5) (0.5)  :&: white,
  vec3 (-0.5) (0.5)  (0.5)  :&: white,
  vec3 (-0.5) (0.5) (0.5) :&: white,
  vec3 (-0.5) (0.5)  (-0.5)  :&: white,
  vec3 (-0.5) (-0.5)  (-0.5) :&: white,

  -- right face (yellow)
  vec3 (-0.5) (-0.5) (-0.5) :&: yellow,
  vec3 0.5 (0.5)  (-0.5)  :&: yellow,
  vec3 0.5 (-0.5) (-0.5)  :&: yellow,
  vec3 (-0.5) (-0.5) (-0.5) :&: yellow,
  vec3 (-0.5) (0.5)  (-0.5) :&: yellow,
  vec3 0.5 (0.5)  (-0.5)  :&: yellow,

  -- top face (orange, remember y axis points down)
  vec3 (-0.5) (-0.5) (-0.5) :&: orange,
  vec3 (0.5) (-0.5) (-0.5)  :&: orange,
  vec3 (0.5)  (-0.5) (0.5)  :&: orange,
  vec3 (-0.5) (-0.5) (-0.5) :&: orange,
  vec3 (0.5)  (-0.5) (0.5)  :&: orange,
  vec3 (-0.5)  (-0.5) (0.5) :&: orange,

  -- bottom face (red)
  vec3 (-0.5) (0.5) (-0.5) :&: red,
  vec3 (-0.5) (0.5) (0.5)  :&: red,
  vec3 (0.5)  (0.5) (0.5)  :&: red,
  vec3 (-0.5) (0.5) (-0.5) :&: red,
  vec3 (0.5)  (0.5) (0.5)  :&: red,
  vec3 (0.5)  (0.5) (-0.5) :&: red,

  -- nose face (blue)
  vec3 (0.5) (0.5) (-0.5) :&: blue,
  vec3 (0.5)  (0.5)  (0.5) :&: blue,
  vec3 (0.5) (-0.5)  (0.5) :&: blue,
  vec3 (0.5) (-0.5) (0.5) :&: blue,
  vec3 (0.5)  (-0.5) (-0.5) :&: blue,
  vec3 (0.5)  (0.5)  (-0.5) :&: blue,

  -- tail face (green)
  vec3 (-0.5) (0.5) (0.5) :&: green,
  vec3 (-0.5) (-0.5)  (0.5) :&: green,
  vec3 (0.5)  (0.5)  (0.5) :&: green,
  vec3 (-0.5) (-0.5) (0.5) :&: green,
  vec3 (0.5)  (-0.5)  (0.5) :&: green,
  vec3 (0.5)  (0.5) (0.5) :&: green]
  where
    white = vec3 0.9 0.9 0.9
    yellow = vec3 0.8 0.8 0.1
    orange = vec3 0.9 0.6 0.1
    red = vec3 0.8 0.1 0.1
    blue = vec3 0.1 0.1 0.8
    green = vec3 0.1 0.8 0.1

data Camera
  = Camera { view :: Mat4
           , proj :: Mat4
           }
           deriving Generic
           deriving anyclass Block


defaultCamera :: Camera
defaultCamera = Camera
  { view = unTransform $ lookAt (vec3 0 0 0) (vec3 0 0 1) (vec3 0 1 0)
  , proj = unTransform $ perspective 45 0.1 1000 640 480
  }


gameLoop :: PipelineKey _ '[Camera] -- ^ rq key to camera
         -> MeshKey _ _ _ _ '[Transform] -- ^ rq key to cube mesh
         -> Float -- ^ rotation
         -> RenderQueue ()
          ⊸ Core (RenderQueue ())
gameLoop ckey mkey rot rq = Linear.do
 should_close <- (shouldCloseWindow ↑)
 if should_close then return rq else Linear.do
  (pollWindowEvents ↑)

  rq <- render rq 
  rq <- (editMeshes mkey rq (traverse' $ propertyAt @0 (\(Ur tr) -> pure $ Ur $
    scale 5 <> rotateY rot <> rotateX (-rot) <> translate 0 0 10)) ↑)

  gameLoop ckey mkey (rot+0.01) rq

main :: Prelude.IO ()
main = do
 withLinearIO $
  runCore (640, 480) Linear.do

    pipeline <- (makeRenderPipeline shaderPipeline (StaticBinding (Ur defaultCamera) :## GHNil) ↑)
    (emptyMat, pipeline) <- (material GHNil pipeline ↑)
    (mesh :: CubeMesh, pipeline) <-
      (createMesh pipeline (DynamicBinding (Ur (rotateY (pi/4))) :## GHNil) cubeVertices ↑)
    (rq, Ur pkey)    <- pure (insertPipeline pipeline LMon.mempty)
    (rq, Ur mkey)    <- pure (insertMaterial pkey emptyMat rq)
    (rq, Ur mshkey)  <- pure (insertMesh mkey mesh rq)

    rq <- gameLoop pkey mshkey 0 rq

    (freeRenderQueue rq ↑)

    return (Ur ())


-- non-compositional instance for "Transform", just for demo
instance ShaderData Transform where
  type FirType Transform = FIR.Struct '[ "m" 'FIR.:-> FIR.M 4 4 Float ]

instance ShaderData Camera where
  type FirType Camera = FIR.Struct '[ "view_matrix" 'FIR.:-> FIR.M 4 4 Float
                                    , "proj_matrix" 'FIR.:-> FIR.M 4 4 Float ]
