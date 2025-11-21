{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PostfixOperators #-}
{-|

The planets-core demo:

A planet (render packet) is defined through a mesh, material, and pipeline,
which in turn are made up of multiple things, as represented in the following picture.

┌─────────────┐┌─────────────┐┌────────────┐┌──────┐┌───────────┐
│Position (DP)││Colormap (TP)││Min Max (SP)││Shader││Camera (DP)│
└┬────────────┘└┬────────────┘└┬───────────┘└┬─────┘└┬──────────┘
┌▽───┐┌─────────▽──────────────▽┐┌───────────▽───────▽┐          
│Mesh││Material                 ││Pipeline            │          
└┬───┘└┬────────────────────────┘└┬───────────────────┘          
┌▽─────▽──────────────────────────▽┐                             
│Planet                            │                             
└──────────────────────────────────┘                             

The function `newPlanet` handles the creation of planet render packets...

-}
module Main where

import Data.Coerce
import Data.Time
import Foreign.Storable
import Geomancy.Mat4
import Geomancy.Transform
import Geomancy.Vec3
import Geomancy.Vec4
import Ghengin.Core
import Ghengin.Core.Log
import Ghengin.Core.Mesh
import Ghengin.Core.Material
import Ghengin.Core.Prelude as Linear
import Ghengin.Core.Render
import Ghengin.Core.Render.Packet
import Ghengin.Core.Render.Pipeline
import Ghengin.Core.Render.Property
import Ghengin.Vulkan.Renderer.Sampler
import Ghengin.Core.Render.Queue
import Ghengin.Core.Shader (StructVec3(..), StructMat4(..))
import Vulkan.Core10.FundamentalTypes (Extent2D(..))
import qualified Data.Monoid.Linear as LMon
import qualified FIR
import qualified Math.Linear as FIR
import qualified Prelude
import qualified Data.Linear.Alias as Alias

import Ghengin.Camera

-- planets!
import Shaders -- planet shaders
import Planet
import Planet.Noise

gameLoop :: _
         => UTCTime -> _ -> _ -> Alias RenderPass ⊸ RenderQueue () ⊸ Core (RenderQueue ())
gameLoop currentTime mkey rot rp rq = Linear.do
 logT "New frame" 
 should_close <- (shouldCloseWindow ↑)
 if should_close then (Alias.forget rp ↑) >> return rq else Linear.do
  (pollWindowEvents ↑)

  Ur newTime <- liftSystemIOU getCurrentTime

  -- Fix Your Timestep: A Very Hard Thing To Get Right. For now, the simplest approach:
  -- let frameTime = diffUTCTime newTime currentTime
  --     deltaTime = Prelude.min MAX_FRAME_TIME $ realToFrac frameTime

  -- Render the rendering queue!
  (rp, rq) <- render rp rq

  rq <- (editMeshes mkey rq (traverse' $ propertyAt @0 (\(Ur tr) -> pure $ Ur $
    rotateY rot <> rotateX (-rot))) ↑)

  -- Loop!
  gameLoop newTime mkey (rot+0.01) rp rq

main :: Prelude.IO ()
main = do
 currTime <- getCurrentTime
 withLinearIO $
  runCore (1280, 720) Linear.do
    -- sampler <- ( createSampler FILTER_NEAREST SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE ↑)
    -- tex     <- ( texture "assets/planet_gradient.png" sampler ↑)

    (rp1, rp2) <- (Alias.share =<< createSimpleRenderPass ↑)
    pipeline <- (makeRenderPipeline rp1 shaders (StaticBinding (Ur camera) :## GHNil) ↑)
    (p1mesh, pipeline) <- newPlanetMesh pipeline defaultPlanet
    (emptyMat, pipeline) <- (material GHNil pipeline ↑)
    -- (pmat, pipeline)    <- newPlanetMaterial minmax tex pipeline

    -- remember to provide helper function in ghengin to insert meshes with pipelines and mats, without needing to do this:
    (rq, Ur pkey)       <- pure (insertPipeline pipeline LMon.mempty)
    (rq, Ur mkey)       <- pure (insertMaterial pkey emptyMat rq)
    (rq, Ur mshkey)     <- pure (insertMesh mkey p1mesh rq)

    rq <- gameLoop currTime mshkey 0 rp2 rq

    (freeRenderQueue rq ↑)

    return (Ur ())

camera :: Camera "view_matrix" "proj_matrix"
camera = cameraLookAt (vec3 0 0 (-5){- move camera "back"-}) (vec3 0 0 0) (1280, 720)

defaultPlanet :: Planet
defaultPlanet = Planet
  { resolution = 100
  , planetShape = PlanetShape
      { planetRadius = 1
      , planetNoise  = LayersCoherentNoise
          { centre        = vec3 0 0 0
          , baseRoughness = 1
          , strength      = 1
          , numLayers     = 2
          , persistence   = 0.5
          , roughness     = 2
          }
      }
  }

