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
import Ghengin.Core.Type.Compatible
import qualified Ghengin.DearImGui.Vulkan as ImGui

-- planets!
import Shaders -- planet shaders
import Planet
import Planet.Noise
import Planet.UI

gameLoop :: Compatible '[Vec3, Vec3] '[Transform] '[MinMax, Texture2D (RGBA8 UNorm)] '[Camera "view_matrix" "proj_matrix"] π
         => UTCTime
         -> _
         -- -> MeshKey π _ _ _ _
         -> MeshKey π '[Camera "view_matrix" "proj_matrix"] '[MinMax, Texture2D (RGBA8 UNorm)] '[Vec3, Vec3] '[Transform]
         -> _
         -> Alias RenderPass ⊸ RenderQueue () ⊸ Core (RenderQueue ())
gameLoop currentTime planet mkey rot rp rq = Linear.do
 logT "New frame" 
 should_close <- (shouldCloseWindow ↑)
 if should_close then (Alias.forget rp ↑) >> return rq else Linear.do
  (pollWindowEvents ↑)

  Ur (newPlanet, changed) <- preparePlanetUI planet

  Ur newTime <- liftSystemIOU getCurrentTime

  -- Fix Your Timestep: A Very Hard Thing To Get Right. For now, the simplest approach:
  -- let frameTime = diffUTCTime newTime currentTime
  --     deltaTime = Prelude.min MAX_FRAME_TIME $ realToFrac frameTime

  (rp, rq) <- renderWith $ Linear.do

    (rp1, rp2) <- lift (Alias.share rp)
    Ur extent <- lift getRenderExtent
    
    renderPassCmd extent rp1 $ Linear.do

      rq <- renderQueueCmd rq

      -- Render Imgui data!
      ImGui.renderDrawData

      return (rp2, rq)


  rq <-
    if changed then
      (editAtMeshesKey mkey rq (\pipeline mat [(msh, x)] -> Linear.do
            ( (pmesh, pipeline),
              Ur minmax ) <- newPlanetMesh pipeline newPlanet 
            mat' <- propertyAt @0 @MinMax (\(Ur _oldMm) -> pure (Ur minmax)) mat
            freeMesh msh
            return (pipeline, (mat', [(pmesh, x)]))
          ) ↑)
    else
      pure rq

  rq <- (editMeshes mkey rq (traverse' $ propertyAt @0 (\(Ur tr) -> pure $ Ur $
    rotateY rot <> rotateX (-rot))) ↑)

  -- Loop!
  gameLoop newTime newPlanet mkey (rot+0.01) rp rq

main :: Prelude.IO ()
main = do
 currTime <- getCurrentTime
 withLinearIO $
  runCore (1280, 720) Linear.do
    sampler <- ( createSampler FILTER_NEAREST SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE ↑)
    tex     <- ( texture "examples/planets-core/assets/planet_gradient.png" sampler ↑)

    (rp1, rp2) <- (Alias.share =<< createSimpleRenderPass ↑)

    -- Init imgui
    (rp1, imctx) <- (Alias.useM rp1 ImGui.initImGui ↑)

    pipeline   <- (makeRenderPipeline rp1 shaders (StaticBinding (Ur camera) :## GHNil) ↑)
    -- todo: minmax should be per-mesh
    ( (pmesh, pipeline),
      Ur minmax )    <- (newPlanetMesh pipeline defaultPlanet ↑)
    (pmat, pipeline) <- (newPlanetMaterial minmax tex pipeline ↑)

    -- remember to provide helper function in ghengin to insert meshes with pipelines and mats, without needing to do this:
    (rq, Ur pkey)    <- pure (insertPipeline pipeline LMon.mempty)
    (rq, Ur mkey)    <- pure (insertMaterial pkey pmat rq)
    (rq, Ur mshkey)  <- pure (insertMesh mkey pmesh rq)

    rq <- gameLoop currTime defaultPlanet mshkey 0 rp2 rq

    (freeRenderQueue rq ↑)
    (ImGui.destroyImCtx imctx ↑)

    return (Ur ())

camera :: Camera "view_matrix" "proj_matrix"
camera = cameraLookAt (vec3 0 0 (-5){- move camera "back"-}) (vec3 0 0 0) (1280, 720)

defaultPlanet :: Planet
defaultPlanet = Planet
  { resolution = 100
  , planetShape = PlanetShape
      { planetRadius = 2.50
      , planetNoise  = AddNoiseMasked
          [ StrengthenNoise 0.12 $ MinValueNoise
            { minNoiseVal = 1.1
            , baseNoise   = LayersCoherentNoise
              { centre        = vec3 0 0 0
              , baseRoughness = 0.71
              , roughness     = 1.83
              , numLayers     = 5
              , persistence   = 0.54
              }
            }
          , StrengthenNoise 2.5 $ MinValueNoise
            { minNoiseVal = 0
            , baseNoise   = RidgedNoise
              { seed          = 123
              , octaves       = 5
              , scale         = 1
              , frequency     = 2
              , lacunarity    = 3
              }
            }
          ]
      }
  }

