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
import Ghengin.Core.Type.Compatible.Pixel
import qualified Ghengin.DearImGui.Vulkan as ImGui
import qualified Ghengin.DearImGui.UI as ImGui

-- planets!
import Shaders -- planet shaders
import Planet
import Planet.Noise
import Planet.UI

data UrGameData π = GameData
  { charStream      :: CharStream
  , mouseDragStream :: MouseDragStream
  , planetMeshKey   :: MeshKey π '[Camera "view_matrix" "proj_matrix"] '[MinMax, Texture2D (RGBA8 UNorm)] '[Vec3, Vec3] '[Transform]
  , planet          :: Planet
  }

gameLoop :: Compatible '[Vec3, Vec3] '[Transform] '[MinMax, Texture2D (RGBA8 UNorm)] '[Camera "view_matrix" "proj_matrix"] π
         => UrGameData π -> Alias RenderPass ⊸ RenderQueue () ⊸ Renderer (RenderQueue ())
gameLoop GameData{..} rp rq = Linear.do
 logT "New frame" 
 should_close <- (shouldCloseWindow)
 if should_close then (Alias.forget rp) >> return rq else Linear.do
  (pollWindowEvents)

  -- Update planet mesh according to UI
  Ur (newPlanet, changedShape, changedColor) <- preparePlanetUI planet -- must happen before the first render
  rq <-
    if changedShape then
      (editAtMeshesKey planetMeshKey rq (\pipeline mat [(msh, x)] -> Linear.do
            ( (pmesh, pipeline),
              Ur minmax ) <- newPlanetMesh pipeline newPlanet 
            mat <- propertyAt @0 @MinMax (\(Ur _) -> pure (Ur minmax)) mat
            freeMesh msh
            return (pipeline, (mat, [(pmesh, x)]))
          ))
    else if changedColor then
      -- TODO: EditAtMaterialKey (using a materialKeyOfMeshKey)
      (editAtMeshesKey planetMeshKey rq (\pipeline mat [(msh, x)] -> Linear.do
            mat <- propertyAt @1 @_ (\tex -> Alias.forget tex >> planetTexture (planetColor newPlanet)) mat
            return (pipeline, (mat, [(msh, x)]))
          ))
    else
      pure rq

  -- Handle mouse drag rotation
  Ur mbDrag <- readMouseDrag mouseDragStream
  rq <- case mbDrag of
    Just (MouseDrag deltaX deltaY) -> Linear.do
      let sensitivity = 0.002
          yawDelta = -(realToFrac deltaX * sensitivity)
          pitchDelta = realToFrac deltaY * sensitivity
      (editMeshes planetMeshKey rq (traverse' $ propertyAt @0 (\(Ur tr) ->
            pure $ Ur $ rotateY yawDelta <> rotateX pitchDelta <> tr)))
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
  gameLoop GameData{planet=newPlanet,..} rp rq

dimensions :: Num a => (a, a)
dimensions = (1920, 1080)

main :: Prelude.IO ()
main = do
 withLinearIO $
  runRenderer dimensions Linear.do
    Ur charStream <- registerCharStream
    Ur mouseDragStream <- registerMouseDragStream $ do
      -- Is drag allowed? not if imgui is using the mouse
      Prelude.not Prelude.<$> ImGui.wantCaptureMouse

    (rp1, rp2) <- (Alias.share =<< createSimpleRenderPass)

    -- Init imgui
    (rp1, imctx) <- (Alias.useM rp1 ImGui.initImGui)

    pipeline   <- (makeRenderPipeline rp1 shaders (StaticBinding (Ur camera) :## GHNil))
    -- todo: minmax should be per-mesh
    ( (pmesh, pipeline),
      Ur minmax )    <- (newPlanetMesh pipeline defaultPlanet)
    (pmat, pipeline) <- (newPlanetMaterial minmax pipeline defaultPlanet)

    -- remember to provide helper function in ghengin to insert meshes with pipelines and mats, without needing to do this:
    (rq, Ur pkey)    <- pure (insertPipeline pipeline LMon.mempty)
    (rq, Ur mkey)    <- pure (insertMaterial pkey pmat rq)
    (rq, Ur mshkey)  <- pure (insertMesh mkey pmesh rq)

    rq <- gameLoop GameData{planet=defaultPlanet, planetMeshKey=mshkey, ..} rp2 rq

    (freeRenderQueue rq)
    (ImGui.destroyImCtx imctx)

    return (Ur ())

camera :: Camera "view_matrix" "proj_matrix"
camera = cameraLookAt (vec3 0 0 (-5){- move camera "back"-}) (vec3 0 0 0) dimensions

defaultPlanet :: Planet
defaultPlanet = Planet
  { planetShape = PlanetShape
      { planetResolution = 50
      , planetRadius = 3
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
  , planetColor = PlanetColor
    { planetColors =
      Prelude.map
        (\(bnd, WithVec3 rn gn bn) -> (ImGui.InRange bnd, ImGui.Color (vec3 (rn/255) (gn/255) (bn/255))))
        [ (1, vec3 0 83 255)
        , (2, vec3 255 218 0)
        , (5, vec3 255 120 0)
        , (10, vec3 60 255 0)
        , (20, vec3 27 183 0)
        , (30, vec3 10 163 0)
        , (40, vec3 158 37 0)
        , (85, vec3 108 13 0)
        , (100, vec3 231 231 231)
        ]
    }
  }

