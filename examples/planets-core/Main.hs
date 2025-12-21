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
  , planetMeshKey   :: MeshKey π '[Camera "view_matrix" "proj_matrix"] PlanetMaterialAttrs PlanetMeshVerts PlanetMeshAttrs
  , planet          :: Planet
  }

gameLoop :: Compatible PlanetMeshVerts PlanetMeshAttrs PlanetMaterialAttrs '[Camera "view_matrix" "proj_matrix"] π
         => UrGameData π -> Alias RenderPass ⊸ RenderQueue () ⊸ Renderer (RenderQueue ())
gameLoop GameData{..} rp rq = Linear.do
 logT "New frame" 
 should_close <- (shouldCloseWindow)
 if should_close then (Alias.forget rp) >> return rq else Linear.do
  (pollWindowEvents)

  -- Update planet mesh according to UI
  Ur (newPlanet, changedShape, changedColor) <- preparePlanetUI planet -- must happen before the first render
  rq <-
    if changedShape || changedColor then Linear.do -- TODO: If only the colors changed, no problem. We only have to regenerate the mesh if any of the biomes noise or start height changed.
      rq <- (editAtMeshesKey planetMeshKey rq (\pipeline mat [(msh, x)] -> Linear.do
            ( (pmesh, pipeline),
              Ur minmax ) <- newPlanetMesh pipeline newPlanet 
            mat <- propertyAt @0 @MinMax (\(Ur _) -> pure (Ur minmax)) mat

            -- Re-use old transform and free old mesh
            let !(DynamicBinding (Ur prv_tr), msh') = puncons msh
            freeMesh msh'

            pmesh' <- propertyAt @0 @Transform (\(Ur _) -> pure (Ur prv_tr)) pmesh

            return (pipeline, (mat, [(pmesh', x)]))
          ))

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

  Ur c_input <- readCharInput charStream
  case c_input of
    Just 'p' -> Linear.do
      -- Save new planet configuration to file
      Ur time <- liftSystemIOU getCurrentTime
      let filename = "planet-" ++ show time ++ ".hs"
      liftSystemIO $ writeFile filename (show newPlanet)
    _ -> return ()

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

    -- What planet?
    let planet = pinkPlanet

    (rp1, rp2) <- (Alias.share =<< createSimpleRenderPass)

    -- Init imgui
    (rp1, imctx) <- (Alias.useM rp1 ImGui.initImGui)

    pipeline   <- (makeRenderPipeline rp1 shaders (StaticBinding (Ur camera) :## GHNil))
    -- todo: minmax should be per-mesh
    ( (pmesh, pipeline),
      Ur minmax )    <- (newPlanetMesh pipeline planet)
    (pmat, pipeline) <- (newPlanetMaterial minmax pipeline planet)

    -- remember to provide helper function in ghengin to insert meshes with pipelines and mats, without needing to do this:
    (rq, Ur pkey)    <- pure (insertPipeline pipeline LMon.mempty)
    (rq, Ur mkey)    <- pure (insertMaterial pkey pmat rq)
    (rq, Ur mshkey)  <- pure (insertMesh mkey pmesh rq)

    rq <- gameLoop GameData{planet, planetMeshKey=mshkey, ..} rp2 rq

    (freeRenderQueue rq)
    (ImGui.destroyImCtx imctx)

    return (Ur ())

camera :: Camera "view_matrix" "proj_matrix"
camera = cameraLookAt (vec3 0 0 (-5){- move camera "back"-}) (vec3 0 0 0) dimensions

defaultPlanet :: Planet
pinkPlanet  :: Planet
xenonSulphur  :: Planet

defaultPlanet = Planet
  { planetShape = PlanetShape
      { planetResolution = 65
      , planetRadius = 2.2
      , planetNoise  = ImGui.Collapsible $ AddNoiseMasked
          [ StrengthenNoise 0.110 $ MinValueNoise
            { minNoiseVal = 0.930
            , baseNoise   = LayersCoherentNoise
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
    { planetBiomes =
      [ ImGui.Collapsible PlanetBiome
        { biomeColors = mkColors
          [ (1,   vec3 255 248 205)
          , (5,   vec3 255 234 234)
          , (15,  vec3 225 225 225)
          , (75,  vec3 195 195 195)
          , (100, vec3 255 255 255)
          ]
        , biomeOceanColors = mkColors
          [ (1,   vec3 8   40  70)   -- Deep ocean trenches
          , (30,  vec3 12  60  100)  -- Deep water
          , (60,  vec3 20  80  130)  -- Mid-depth ocean
          , (85,  vec3 30  110 160)  -- Continental shelf
          , (100, vec3 24  150 183)  -- Shallow coastal waters (matches terrain color 1)
          ]
        , biomeStartHeight = 0
        , biomeTint = ImGui.Color (vec3 1 0 1)
        , biomeTintPercent = 0
        }
      , ImGui.Collapsible PlanetBiome
        { biomeColors = mkColors
          [ (1, vec3 255 218 0)
          , (5, vec3 255 120 0)
          , (10, vec3 60 255 0)
          , (20, vec3 27 183 0)
          , (30, vec3 10 163 0)
          , (40, vec3 158 37 0)
          , (85, vec3 108 13 0)
          , (100, vec3 231 231 231)
          ]
        , biomeOceanColors = mkColors
          [ (1,   vec3 10  50  85)   -- Deep ocean
          , (50,  vec3 0   68  255)  -- Mid ocean
          , (100, vec3 0   83  255)  -- Coastal waters (matches terrain color 1)
          ]
        , biomeStartHeight = 0.38
        , biomeTint = ImGui.Color (vec3 0 1 1)
        , biomeTintPercent = 0
        }
      , ImGui.Collapsible PlanetBiome
        { biomeColors = mkColors
          [ (1, vec3 255 80 0)      -- Glowing lava at shore
          , (5, vec3 200 40 0)      -- Cooling lava flows
          , (10, vec3 120 20 0)     -- Dark red volcanic rock
          , (20, vec3 80 15 10)     -- Deep red-brown slopes
          , (30, vec3 90 25 15)     -- Iron-rich volcanic stone
          , (40, vec3 140 30 0)     -- Oxidized red rock
          , (60, vec3 180 35 0)     -- Glowing red basalt
          , (85, vec3 220 50 0)     -- Bright red-orange ridges
          , (100, vec3 255 100 0)   -- Molten orange peaks
          ]
        , biomeOceanColors = mkColors
          [ (1,   vec3 5   25  45)   -- Very deep dark blue
          , (50,  vec3 11  22  33)   -- Deep midnight blue
          , (100, vec3 15  25  35)   -- Dark ocean blue (matches terrain color 1)
          ]
        , biomeStartHeight = 0.96
        , biomeTint = ImGui.Color (vec3 0 1 0)
        , biomeTintPercent = 0
        }
      ]
    , biomesNoise = ImGui.Collapsible $ StrengthenNoise 0.05 $
        LayersCoherentNoise
        { centre        = ImGui.WithTooltip $ ImGui.Color $ vec3 0 0 0
        , baseRoughness = 1.0
        , roughness     = 2.0
        , numLayers     = 3
        , persistence   = 2
        }
    , biomeBlendAmount = 0.2
    , biomeNoiseOffset = 0
    , planetColorsInterpolate = False
    }
  }
  where
    mkColors = Prelude.map $ \(bnd, WithVec3 rn gn bn) ->
      (ImGui.InRange bnd, ImGui.Color (vec3 (rn/255) (gn/255) (bn/255)))

--------------------------------------------------------------------------------
-- More Planets
--------------------------------------------------------------------------------

pinkPlanet = Planet
  { planetShape = PlanetShape
      { planetResolution = 65
      , planetRadius = 2.2
      , planetNoise  = ImGui.Collapsible $ AddNoiseMasked
          [ StrengthenNoise 0.110 $ MinValueNoise
            { minNoiseVal = 0.930
            , baseNoise   = LayersCoherentNoise
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
    { planetBiomes =
      [ ImGui.Collapsible PlanetBiome
        { biomeColors = mkColors
          [ (1,   vec3 15  45  75)   -- Deep navy blue
          , (2,   vec3 200 230 255)  -- Pale ice blue
          , (5,   vec3 230 245 255)  -- Almost white ice
          , (15,  vec3 240 240 240)  -- Light grey
          , (75,  vec3 210 210 210)  -- Medium grey
          , (100, vec3 255 255 255)  -- Pure white
          ]
        , biomeOceanColors = mkColors
          [ (1,   vec3 10  30  60)   -- Deep midnight blue ocean
          , (50,  vec3 15  45  75)   -- Navy depths
          , (100, vec3 25  60  95)   -- Lighter blue shallows
          ]
        , biomeStartHeight = 0
        , biomeTint = ImGui.Color (vec3 1 0 1)
        , biomeTintPercent = 0
        }
      , ImGui.Collapsible PlanetBiome
        { biomeColors = mkColors
          [ (1,   vec3 120 80  200)  -- Purple coastal waters
          , (2,   vec3 255 180 220)  -- Pink beaches
          , (5,   vec3 255 100 150)  -- Hot pink lowlands
          , (10,  vec3 200 50  255)  -- Magenta plains
          , (20,  vec3 150 30  200)  -- Deep purple forests
          , (30,  vec3 100 20  150)  -- Dark violet jungle
          , (40,  vec3 200 120 50)   -- Orange-brown plateaus
          , (85,  vec3 150 80  30)   -- Rust highlands
          , (100, vec3 255 220 180)  -- Cream peaks
          ]
        , biomeOceanColors = mkColors
          [ (1,   vec3 80  40  150)  -- Deep purple ocean
          , (50,  vec3 120 80  200)  -- Medium purple
          , (100, vec3 160 120 230)  -- Light purple shallows
          ]
        , biomeStartHeight = 0.38
        , biomeTint = ImGui.Color (vec3 0 1 1)
        , biomeTintPercent = 0
        }
      , ImGui.Collapsible PlanetBiome
        { biomeColors = mkColors
          [ (1,   vec3 0   40  50)   -- Dark teal water
          , (2,   vec3 0   255 200)  -- Bright cyan lava shores
          , (5,   vec3 0   220 180)  -- Turquoise flows
          , (10,  vec3 0   180 140)  -- Teal volcanic rock
          , (20,  vec3 0   140 120)  -- Dark cyan slopes
          , (30,  vec3 50  200 180)  -- Aqua stone
          , (40,  vec3 100 230 210)  -- Mint basalt
          , (60,  vec3 150 250 230)  -- Pale cyan ridges
          , (85,  vec3 200 255 245)  -- Ice cyan peaks
          , (100, vec3 240 255 255)  -- White-cyan summit
          ]
        , biomeOceanColors = mkColors
          [ (1,   vec3 0   20  30)   -- Deep dark teal
          , (50,  vec3 0   40  50)   -- Navy teal
          , (100, vec3 0   60  70)   -- Lighter teal
          ]
        , biomeStartHeight = 0.96
        , biomeTint = ImGui.Color (vec3 0 1 0)
        , biomeTintPercent = 0
        }
      ]
    , biomesNoise = ImGui.Collapsible $ StrengthenNoise 0.05 $
        LayersCoherentNoise
        { centre        = ImGui.WithTooltip $ ImGui.Color $ vec3 0 0 0
        , baseRoughness = 1.0
        , roughness     = 2.0
        , numLayers     = 3
        , persistence   = 2
        }
    , biomeBlendAmount = 0.2
    , biomeNoiseOffset = 0
    , planetColorsInterpolate = False
    }
  }
  where
    mkColors = Prelude.map $ \(bnd, WithVec3 rn gn bn) ->
      (ImGui.InRange bnd, ImGui.Color (vec3 (rn/255) (gn/255) (bn/255)))

xenonSulphur = Planet
  { planetShape = PlanetShape
      { planetResolution = 100  -- Increased for sharper jagged peaks
      , planetRadius = 2.2      -- A larger super-earth
      , planetNoise  = ImGui.Collapsible $ AddNoiseMasked
          [ -- Base Layer: Rolling alien hills
            StrengthenNoise 0.08 $ MinValueNoise
            { minNoiseVal = 0.82
            , baseNoise   = LayersCoherentNoise
              { centre        = ImGui.WithTooltip $ ImGui.Color $ vec3 100 0 255
              , baseRoughness = 1.2
              , roughness     = 2.2
              , numLayers     = 8
              , persistence   = 0.5
              }
            }
          , -- Detail Layer: Sharp, crystalline ridges
            StrengthenNoise 1.2 $ MinValueNoise
            { minNoiseVal = 0.05
            , baseNoise   = RidgedNoise
              { seed        = 1337
              , octaves     = 12
              , scale       = 0.8
              , frequency   = 1.8
              , lacunarity  = 2.4 -- Low lacunarity makes the ridges look thicker/blockier
              }
            }
          ]
      }
  , planetColor = PlanetColor
    { planetBiomes =
      [ -- BIOME 1: The Obsidian Lowlands (Dark & Moody)
        ImGui.Collapsible PlanetBiome
        { biomeColors = mkColors
          [ (1,   vec3 5   5   10)   -- Vantablack rock
          , (10,  vec3 30  30  35)   -- Dark charcoal
          , (30,  vec3 60  20  80)   -- Faint purple mineral deposits
          , (60,  vec3 20  20  20)   -- Grey stone
          , (85,  vec3 100 100 100)  -- Ash grey
          , (100, vec3 150 150 150)  -- White ash peaks
          ]
        , biomeOceanColors = mkColors
          [ (1,   vec3 10  0   20)   -- Almost black violet depths
          , (50,  vec3 40  0   80)   -- Deep indigo
          , (100, vec3 80  10  160)  -- Glowing electric purple shallows
          ]
        , biomeStartHeight = 0
        , biomeTint = ImGui.Color (vec3 0.5 0 1)
        , biomeTintPercent = 0.1
        }
      , -- BIOME 2: The Crimson Iron Flats (Vibrant Contrast)
        ImGui.Collapsible PlanetBiome
        { biomeColors = mkColors
          [ (1,   vec3 40  0   0)    -- Dried blood red
          , (20,  vec3 120 10  10)   -- Rust red
          , (40,  vec3 180 40  10)   -- Iron oxide orange
          , (70,  vec3 255 80  0)    -- Bright ember orange
          , (90,  vec3 255 150 50)   -- Glowing heat
          , (100, vec3 255 200 150)  -- White-hot peaks
          ]
        , biomeOceanColors = mkColors
          [ (1,   vec3 30  5   0)    -- Dark maroon
          , (50,  vec3 100 20  0)    -- Blood red fluid
          , (100, vec3 200 50  0)    -- Bright red coast
          ]
        , biomeStartHeight = 0.45
        , biomeTint = ImGui.Color (vec3 1 0 0)
        , biomeTintPercent = 0.2
        }
      , -- BIOME 3: The Toxic Sulfur Highlands (Neon Highlights)
        ImGui.Collapsible PlanetBiome
        { biomeColors = mkColors
          [ (1,   vec3 20  40  0)    -- Dark swamp green
          , (15,  vec3 50  80  0)    -- Olive
          , (30,  vec3 100 150 0)    -- Acid green
          , (50,  vec3 180 220 0)    -- Lime
          , (75,  vec3 220 255 0)    -- Neon Yellow (Sulfur)
          , (100, vec3 255 255 200)  -- Pale yellow crystal spires
          ]
        , biomeOceanColors = mkColors
          [ (1,   vec3 0   20  10)   -- Dark sludge
          , (50,  vec3 10  60  30)   -- Toxic waste green
          , (100, vec3 50  200 100)  -- Radioactive bright green
          ]
        , biomeStartHeight = 0.8
        , biomeTint = ImGui.Color (vec3 0.8 1 0)
        , biomeTintPercent = 0
        }
      ]
    , biomesNoise = ImGui.Collapsible $ StrengthenNoise 0.5 $
        LayersCoherentNoise
        { centre        = ImGui.WithTooltip $ ImGui.Color $ vec3 255 255 255
        , baseRoughness = 0.8
        , roughness     = 1.5
        , numLayers     = 4
        , persistence   = 0.5
        }
    , biomeBlendAmount = 0.15 -- Sharp transitions between the alien zones
    , biomeNoiseOffset = 0
    , planetColorsInterpolate = True -- Smooth gradients within biomes
    }
  }
  where
    mkColors = Prelude.map $ \(bnd, WithVec3 rn gn bn) ->
      (ImGui.InRange bnd, ImGui.Color (vec3 (rn/255) (gn/255) (bn/255)))

