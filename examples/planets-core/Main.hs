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
import Ghengin.Core.Input
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
import qualified Ghengin.DearImGui.UI as ImGui

-- planets!
import Shaders -- planet shaders
import Planet
import Planet.Noise
import Planet.UI

gameLoop :: Compatible '[Vec3, Vec3] '[Transform] '[MinMax, Texture2D (RGBA8 UNorm)] '[Camera "view_matrix" "proj_matrix"] π
         => CharStream
         -> _
         -> MeshKey π '[Camera "view_matrix" "proj_matrix"] '[MinMax, Texture2D (RGBA8 UNorm)] '[Vec3, Vec3] '[Transform]
         -> Alias RenderPass
          ⊸ RenderQueue ()
          ⊸ Core (RenderQueue ())
gameLoop charStream planet mkey rp rq = Linear.do
 logT "New frame" 
 should_close <- (shouldCloseWindow ↑)
 if should_close then (Alias.forget rp ↑) >> return rq else Linear.do
  (pollWindowEvents ↑)

  -- Update planet mesh according to UI
  Ur (newPlanet, changed) <- preparePlanetUI planet -- must happen before the first render
  rq <-
    if changed then
      (editAtMeshesKey mkey rq (\pipeline mat [(msh, x)] -> Linear.do
            ( (pmesh, pipeline),
              Ur minmax ) <- newPlanetMesh pipeline newPlanet 
            mat' <- propertyAt @0 @MinMax (\(Ur _) -> pure (Ur minmax)) mat
            freeMesh msh
            return (pipeline, (mat', [(pmesh, x)]))
          ) ↑)
    else
      pure rq

  Ur mb_in <- readCharInput charStream
  rq <- case mb_in of
    Just ch -> Linear.do
      let doRotate 'd' = rotateY 0.01
          doRotate 'a' = rotateY (-0.01)
          doRotate 'w' = rotateX (0.01)
          doRotate 's' = rotateX (-0.01)
          doRotate _   = mempty
      (editMeshes mkey rq (traverse' $ propertyAt @0 (\(Ur tr) -> pure $ Ur $ doRotate ch <> tr)) ↑)
    Nothing -> pure rq

  -- Render!
  (rp, rq) <- renderWith $ Linear.do

    (rp1, rp2) <- lift (Alias.share rp)
    Ur extent <- lift getRenderExtent
    
    renderPassCmd extent rp1 $ Linear.do

      rq <- renderQueueCmd rq

      -- Render Imgui data!
      ImGui.renderDrawData

      return (rp2, rq)

  -- Loop!
  gameLoop charStream newPlanet mkey rp rq

main :: Prelude.IO ()
main = do
 withLinearIO $
  runCore (1280, 720) Linear.do
    Ur charStream <- registerCharStream

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

    rq <- gameLoop charStream defaultPlanet mshkey rp2 rq

    (freeRenderQueue rq ↑)
    (ImGui.destroyImCtx imctx ↑)

    return (Ur ())

camera :: Camera "view_matrix" "proj_matrix"
camera = cameraLookAt (vec3 0 0 (-5){- move camera "back"-}) (vec3 0 0 0) (1280, 720)

defaultPlanet :: Planet
defaultPlanet = Planet
  { resolution = 100
  , planetShape = PlanetShape
      { planetRadius = 3
      , planetNoise  = ImGui.Collapsible $ AddNoiseMasked
          [ StrengthenNoise 0.110 $ MinValueNoise
            { minNoiseVal = 0.930
            , baseNoise   = LayersCoherentNoise
              -- { centre        = ImGui.WithTooltip $ ImGui.Color $ vec3 0 0 0
              { centre        = ImGui.WithTooltip $ ImGui.Color $ vec3 255 147 0
              , baseRoughness = 1.5
              , roughness     = 2.5
              , numLayers     = 20
              , persistence   = 0.4
              }
            }
          , StrengthenNoise 5 $ MinValueNoise
            { minNoiseVal = 0.120
            , baseNoise   = RidgedNoise
              { seed          = 25
              , octaves       = 10
              , scale         = 0.59
              , frequency     = 2
              , lacunarity    = 5.2
              }
            }
          ]
      }
  }

