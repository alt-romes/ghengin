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

import Ghengin.Camera
import Shaders

type Points = Mesh '[Vec3] '[]

myCamera :: Camera "view_matrix" "proj_matrix"
myCamera = Camera
  { view = orthoFitScreen 1 1 1 1 -- noop
  , proj = unTransform $ orthoOffCenter @Int 0 50 50 50
  }

gameLoop :: PipelineKey _ '[Camera "view_matrix" "proj_matrix"] -- ^ rq key to camera
         -> MaterialKey _ _ _ -- ^ rq key to material
         -> MeshKey _ _ _ _ _ -- ^ rq key to mesh
         -> Vec3 -- ^ last position
         -> Alias RenderPass
          ⊸ RenderQueue ()
          ⊸ Core (RenderQueue ())
gameLoop ckey matkey mkey last rp rq = Linear.do
 should_close <- (shouldCloseWindow ↑)
 if should_close then (Alias.forget rp ↑) >> return rq else Linear.do
  (pollWindowEvents ↑)

  let
      (x',y',z') = lorenzInc (x,y,z)
      WithVec3 x y z = last
      next_pos = vec3 x' y' z'

  (rp, rq) <- render rp rq

  gameLoop ckey matkey mkey next_pos rp rq

main :: Prelude.IO ()
main = do
 withLinearIO $
  runCore (640, 480) Linear.do

    let start_points = [Sin (vec3 0 1 0), Sin (tupleToVec3 $ lorenzNext (0, 1, 0))]

    (clearRenderImages 0 0 0 1 ↑)

    (rp1, rp2) <- (Alias.share =<< createRenderPassFromSettings RenderPassSettings{keepColor=True} ↑)

    pipeline <- (makeRenderPipelineWith defaultGraphicsPipelineSettings{blendMode=BlendAdd}
                   rp1 shaderPipeline (StaticBinding (Ur myCamera) :## GHNil) ↑)
    (emptyMat, pipeline) <- (material GHNil pipeline ↑)

    (mesh :: Points, pipeline) <-
      (createMesh pipeline GHNil start_points ↑)

    (rq, Ur pkey)    <- pure (insertPipeline pipeline LMon.mempty)
    (rq, Ur mkey)    <- pure (insertMaterial pkey emptyMat rq)

    (rq, Ur mshkey)  <- pure (insertMesh mkey mesh rq)

    rq <- gameLoop pkey mkey mshkey (vec3 0 1 0) rp2 rq

    (freeRenderQueue rq ↑)

    return (Ur ())


-- non-compositional instance for "Transform", just for demo
instance ShaderData Transform where
  type FirType Transform = FIR.Struct '[ "m" 'FIR.:-> FIR.M 4 4 Float ]

--------------------------------------------------------------------------------

lorenzNext :: RealFrac a => (a, a, a) -> (a, a, a)
lorenzNext (x,y,z) =
  let (x',y',z') = lorenzInc (x,y,z)
   in (x+x',y+y',z+z')

lorenzInc :: RealFrac a => (a, a, a) -> (a, a, a)
lorenzInc (x, y, z) =
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
   in ( x'*dt, y'*dt, z'*dt)

tupleToVec3 (a,b,c) = vec3 a b c
