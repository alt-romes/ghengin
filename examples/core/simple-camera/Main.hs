{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import qualified Language.Haskell.TH.Syntax as TH
import qualified Prelude
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
import qualified Math.Linear as FIR
import qualified FIR
import qualified Data.Linear.Alias as Alias

import Ghengin.Geometry.Cube (coloredCube)
import Shaders

type CubeMesh = Mesh '[Vec3, Vec3] '[Transform]

data Camera
  = Camera { view :: Mat4
           , proj :: Mat4
           }
           deriving Generic
           deriving anyclass Block

defaultCamera :: Camera
defaultCamera = 
  let
    up = vec3 0 (-1) 0
    forward = vec3 0 0 1
    eye = vec3 0 0 0
  in
    Camera
    { view = unTransform $ lookAtRH eye forward up 
    , proj = unTransform $ reverseDepthRH (pi / 2) 0.1 640 480
    }

gameLoop :: PipelineKey _ '[Camera] -- ^ rq key to camera
         -> MeshKey _ _ _ _ '[Transform] -- ^ rq key to cube mesh
         -> Float -- ^ rotation
         -> Alias RenderPass
          ⊸ RenderQueue ()
          ⊸ Renderer (RenderQueue ())
gameLoop ckey mkey rot rp rq = Linear.do
 Ur should_close <- shouldCloseWindow
 if should_close then Alias.forget rp >> return rq else Linear.do
  pollWindowEvents

  (rp, rq) <- render rp rq
  rq <- editMeshes mkey rq (traverse' $ propertyAt @0 (\(Ur tr) -> pure $ Ur $
    translate 0 0 10 <> rotateY rot <> rotateX (-rot) <> scale 5))

  gameLoop ckey mkey (rot+0.01) rp rq

main :: Prelude.IO ()
main = do
 withLinearIO $
  runRenderer (640, 480) Linear.do

    (rp1, rp2) <- Alias.share =<< createSimpleRenderPass
    pipeline <- makeRenderPipeline rp1 shaderPipeline (StaticBinding (Ur defaultCamera) :## GHNil)
    (emptyMat, pipeline) <- material GHNil pipeline
    (mesh :: CubeMesh, pipeline) <-
      -- Also displays how TH can be used to create procedural meshes at compile time when the parameters are statically known
      createMesh pipeline (DynamicBinding (Ur (rotateY (pi/4))) :## GHNil) ($$(TH.liftTyped coloredCube))
    (rq, Ur pkey)    <- pure (insertPipeline pipeline LMon.mempty)
    (rq, Ur mkey)    <- pure (insertMaterial pkey emptyMat rq)
    (rq, Ur mshkey)  <- pure (insertMesh mkey mesh rq)

    rq <- gameLoop pkey mshkey 0 rp2 rq

    freeRenderQueue rq

    return (Ur ())


-- non-compositional instance for "Transform", just for demo
instance ShaderData Transform where
  type FirType Transform = FIR.Struct '[ "m" 'FIR.:-> FIR.M 4 4 Float ]

instance ShaderData Camera where
  type FirType Camera = FIR.Struct '[ "view_matrix" 'FIR.:-> FIR.M 4 4 Float
                                    , "proj_matrix" 'FIR.:-> FIR.M 4 4 Float ]
