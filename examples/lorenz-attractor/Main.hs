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

import Ghengin.Geometry.Sphere
import Shaders

type SphereMesh = Mesh '[Vec3, Vec3, Vec3] '[Transform]

data Camera
  = Camera { view :: Mat4
           , proj :: Mat4
           }
           deriving Generic
           deriving anyclass Block

defaultCamera :: Camera
defaultCamera = Camera
  { view = unTransform $ lookAt (vec3 0 0 0) (vec3 0 0 1) (vec3 0 1 0)
  , proj = unTransform $ perspective @Int 45 0.1 1000 640 480
  }

gameLoop :: PipelineKey _ '[Camera] -- ^ rq key to camera
         -> MeshKey _ _ _ _ '[Transform] -- ^ rq key to mesh
         -> Float -- ^ rotation
         -> Vec3 -- ^ last position
         -> Alias RenderPass
          ⊸ RenderQueue ()
          ⊸ Core (RenderQueue ())
gameLoop ckey mkey rot last rp rq = Linear.do
 should_close <- (shouldCloseWindow ↑)
 if should_close then (Alias.forget rp ↑) >> return rq else Linear.do
  (pollWindowEvents ↑)

  let
      (x',y',z') = lorenzIteration (x,y,z)
      WithVec3 x y z = last
      next_pos = vec3 x' y' z'

  (rp, rq) <- render rp rq
  rq <- (editMeshes mkey rq (traverse' $ propertyAt @0 (\(Ur tr) -> do

    pure $ Ur $ translate x' y' z' <>
      scale 1 <> rotateY rot <> rotateX (-rot) <> translate 0 0 100)) ↑)

  gameLoop ckey mkey (rot+0.01) next_pos rp rq

main :: Prelude.IO ()
main = do
 withLinearIO $
  runCore (640, 480) Linear.do

    (clearRenderImages 0 0 0 1 ↑)

    (rp1, rp2) <- (Alias.share =<< createRenderPassFromSettings RenderPassSettings{keepColor=True} ↑)

    pipeline <- (makeRenderPipelineWith defaultGraphicsPipelineSettings{blendMode=BlendAlpha}
                   rp1 shaderPipeline (StaticBinding (Ur defaultCamera) :## GHNil) ↑)
    (emptyMat, pipeline) <- (material GHNil pipeline ↑)
    let UnitSphere vs is = newUnitSphere 5 Nothing
    (mesh :: SphereMesh, pipeline) <-
      (createMeshWithIxs pipeline (DynamicBinding (Ur (rotateY (pi/4))) :## GHNil) vs is ↑)
    (rq, Ur pkey)    <- pure (insertPipeline pipeline LMon.mempty)
    (rq, Ur mkey)    <- pure (insertMaterial pkey emptyMat rq)
    (rq, Ur mshkey)  <- pure (insertMesh mkey mesh rq)

    rq <- gameLoop pkey mshkey 0 (vec3 5 5 5) rp2 rq

    (freeRenderQueue rq ↑)

    return (Ur ())


-- non-compositional instance for "Transform", just for demo
instance ShaderData Transform where
  type FirType Transform = FIR.Struct '[ "m" 'FIR.:-> FIR.M 4 4 Float ]

instance ShaderData Camera where
  type FirType Camera = FIR.Struct '[ "view_matrix" 'FIR.:-> FIR.M 4 4 Float
                                    , "proj_matrix" 'FIR.:-> FIR.M 4 4 Float ]

--------------------------------------------------------------------------------

lorenzIteration :: RealFrac a => (a, a, a) -> (a, a, a)
lorenzIteration (x, y, z) =
  -- constants from lorenz's paper (page 136)
  -- https://cdanfort.w3.uvm.edu/research/lorenz-1963.pdf
  let s = 10
      b = 8/3
      -- r = s*(s + b + 3)*(1 / (s - b - 1)) -- critical Rayleigh's number
      r = 28 -- "slightly supercritical", see same page

      dt = 0.01 -- x',y',z' are wrt dimensionless time that Lorenz simulates as 0.01

      x' = s * (y - x)     -- (25)
      y' = x * (r - z) - y -- (26)
      z' = x * y - b * z   -- (27)
   in (x + x'*dt, y + y'*dt, z + z'*dt)

