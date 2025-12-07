{-# LANGUAGE PartialTypeSignatures #-}
module Main where

import Ghengin.Core.Prelude as Linear
import qualified Prelude

import Geomancy
import Geomancy.Transform
import Ghengin.Core
import Ghengin.Core.Shader.Data
import Ghengin.Core.Mesh
import Ghengin.Core.Material
import Ghengin.Core.Render
import Ghengin.Core.Render.Property
import Ghengin.Core.Render.Pipeline
import Ghengin.Core.Render.Queue

import qualified FIR
import qualified Math.Linear as FIR

import qualified Data.Monoid.Linear as LMon
import qualified Data.Linear.Alias as Alias

-- ghengin
import Ghengin.Camera
import Ghengin.Geometry.Obj

import Shaders

main :: Prelude.IO ()
main = do
 withLinearIO $
  runCore (640, 480) Linear.do

    (rp1, rp2) <- (Alias.share =<< createSimpleRenderPass ↑)

    pipeline <- (makeRenderPipelineWith defaultGraphicsPipelineSettings{cullMode=CullFront, polygonMode=PolygonFill} rp1 shaderPipeline (StaticBinding (Ur camera) :## GHNil) ↑)
    (emptyMat, pipeline) <- (material GHNil pipeline ↑)
    (mesh, pipeline) <- (loadObjMesh "examples/teapot-obj/assets/teapot.obj" pipeline
    -- (mesh, pipeline) <- (loadObjMesh "examples/teapot-obj/assets/building-tower.obj" pipeline
                          (DynamicBinding (Ur (scale 1.2)) :## GHNil) ↑)

    let !(rq, Ur pkey) = insertPipeline pipeline LMon.mempty
    (rq, Ur mkey)    <- pure (insertMaterial pkey emptyMat rq)
    (rq, Ur mshkey)  <- pure (insertMesh mkey mesh rq)

    rq <- gameLoop mshkey rp2 rq

    (freeRenderQueue rq ↑)

    return (Ur ())

gameLoop :: MeshKey _ _ _ _ '[Transform] -- ^ rq key to cube mesh
         -> Alias RenderPass
          ⊸ RenderQueue ()
          ⊸ Core (RenderQueue ())
gameLoop mkey rp rq = Linear.do
 should_close <- (shouldCloseWindow ↑)
 if should_close then (Alias.forget rp ↑) >> return rq else Linear.do
  (pollWindowEvents ↑)

  (rp, rq) <- render rp rq
  rq <- (editMeshes mkey rq (traverse' $ propertyAt @0 (\(Ur tr) -> pure $ Ur $
    rotateY 0.01 <> tr)) ↑)

  gameLoop mkey rp rq

camera :: Camera "view_matrix" "proj_matrix"
camera = cameraLookAt (vec3 0 (-5) (-5){- move camera up and back-}) (vec3 0 0 0) (1280, 720)

-- non-compositional instance for "Transform", just for demo
instance ShaderData Transform where
  type FirType Transform = FIR.Struct '[ "m" 'FIR.:-> FIR.M 4 4 Float ]
