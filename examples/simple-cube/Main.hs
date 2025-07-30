{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import Geomancy.Vec3
import Geomancy.Mat4
import Geomancy.Transform
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
import qualified Data.Linear.Alias as Alias
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

gameLoop :: MeshKey _ _ _ _ '[Transform] -> Float -> Alias RenderPass ⊸ RenderQueue () ⊸ Core (RenderQueue ())
gameLoop mkey rot rp rq = Linear.do
 should_close <- (shouldCloseWindow ↑)
 if should_close then (Alias.forget rp ↑) >> return rq else Linear.do
  (pollWindowEvents ↑)

  -- (rp, rq) <- render rp rq

  (rp, rq) <- renderWith $ Linear.do

    (rp1, rp2) <- lift (Alias.share rp)

    Ur extent <- lift getRenderExtent
 
    renderPassCmd extent rp1 $ Linear.do

      rq <- renderQueueCmd rq

      return (rp2, rq)

  rq <- (editMeshes mkey rq (traverse' $ propertyAt @0 (\(Ur tr) -> pure $ Ur $
    -- We're not using any projection of sorts, so we need to make the cube fit
    -- in the xyz vulkan space, where x and y go from -1 to 1 but z goes from 0
    -- to 1
    scale 0.5 <> rotateY rot <> rotateX (-rot) <> translate 0 0 0.5)) ↑)

  gameLoop mkey (rot+0.01) rp rq

-- non-compositional instance for "Transform", just for demo
instance ShaderData Transform where
  type FirType Transform = FIR.Struct '[ "m" 'FIR.:-> FIR.M 4 4 Float ]

main :: Prelude.IO ()
main = do
 withLinearIO $
  runCore (640, 640) Linear.do
    (rp1, rp2) <- (Alias.share =<< createRenderPassFromSettings RenderPassSettings{keepColor=True} ↑)

    let clear_image rp1 = renderWith $ Linear.do
          (rp3, rp4) <- lift (Alias.share rp1)

          Ur extent <- lift getRenderExtent
          renderPassCmd extent rp3 $ Linear.do
            clearColorImage 0 0 0 1
            return rp4
    
    rp1 <- clear_image rp1
    rp1 <- clear_image rp1
    rp1 <- clear_image rp1
    rp1 <- clear_image rp1

    pipeline <- (makeRenderPipelineWith defaultGraphicsPipelineSettings{blendMode=BlendAlpha} rp1 shaderPipeline GHNil ↑)

    (emptyMat, pipeline) <- (material GHNil pipeline ↑)

    (mesh :: CubeMesh, pipeline) <-
      (createMesh pipeline (DynamicBinding (Ur (rotateY (pi/4))) :## GHNil) cubeVertices ↑)

    (rq, Ur pkey)    <- pure (insertPipeline pipeline LMon.mempty)
    (rq, Ur mkey)    <- pure (insertMaterial pkey emptyMat rq)
    (rq, Ur mshkey)  <- pure (insertMesh mkey mesh rq)

    rq <- gameLoop mshkey 0 rp2 rq

    (freeRenderQueue rq ↑)

    return (Ur ())

