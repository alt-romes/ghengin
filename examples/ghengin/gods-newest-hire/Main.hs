{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import qualified Ghengin.Core as Core
import Control.Monad
import Data.Coerce
import Data.Time
import Foreign.Storable
import Geomancy.Mat4
import Geomancy.Transform
import Geomancy.Vec3
import Geomancy.Vec4
import Ghengin.Core.Log
import Ghengin.Core.Mesh
import Ghengin.Core.Material
import qualified Ghengin.Core.Prelude as Linear
import Ghengin.Core.Render
import Ghengin.Core.Render.Pipeline
import Ghengin.Core.Render.Property
import Ghengin.Vulkan.Renderer.Sampler
import Ghengin.Core.Render.Queue
import Ghengin.Input
import Ghengin.Core.Shader (StructVec3(..), StructMat4(..))
import Vulkan.Core10.FundamentalTypes (Extent2D(..))
import qualified Data.Monoid.Linear as LMon
import qualified FIR
import qualified Math.Linear as FIR
import Ghengin.Prelude
import qualified Data.Linear.Alias as Alias

import Ghengin.Camera
import Ghengin.Core.Type.Compatible
import Ghengin.Core.Type.Compatible.Pixel
import qualified Ghengin.DearImGui.Vulkan as ImGui
import qualified Ghengin.DearImGui.UI as ImGui

import Ghengin.Monad

-- planets!
import Shaders -- planet shaders
import Planet
import Planet.Noise
import Planet.UI

data GameData π = GameData
  { planetMeshKey   :: MeshKey π '[Camera "view_matrix" "proj_matrix"] PlanetMaterialAttrs PlanetMeshVerts PlanetMeshAttrs
  , planet          :: Planet
  }

gameStep :: Linear.KnownNat swpImgs => Compatible PlanetMeshVerts PlanetMeshAttrs PlanetMaterialAttrs '[Camera "view_matrix" "proj_matrix"] π
         => GameData π
         -> Linear.Finite FramesInFlight
         -> Linear.Finite swpImgs
         -> Ghengin (GameData π)
gameStep GameData{..} frameIx imageIx = do

  -- Update planet mesh according to UI
  (newPlanet, changedShape, changedColor) <- preparePlanetUI planet -- must happen before the first render
  when (changedShape || changedColor) $ do

    -- Only regen when vertex data must change
    when (newPlanet.planetShape /= planet.planetShape
       || map (.unCollapsible.biomeStartHeight) newPlanet.planetColor.planetBiomes
          /= map (.unCollapsible.biomeStartHeight) planet.planetColor.planetBiomes) do

      editRenderQueue $ \rq ->
        editAtMeshesKey planetMeshKey rq $ \pipeline mat [(msh, x)] -> Linear.do
          ( (pmesh, pipeline),
            Ur minmax ) <- newPlanetMesh pipeline newPlanet
          mat <- propertyAt @0 @MinMax (\(Ur _) -> Linear.pure (Ur minmax)) mat

          -- Re-use old transform and free old mesh
          let !(DynamicBinding (Ur old_tr), msh') = puncons msh
          freeMesh msh'

          pmesh' <- propertyAt @0 @Transform (\(Ur _) -> Linear.pure (Ur old_tr)) pmesh

          Linear.pure (pipeline, (mat, [(pmesh', x)]))

    -- On any change
    editRenderQueue $ \rq ->
      editMaterial (meshKey2MatKey planetMeshKey) rq $ \mat -> Linear.do
        propertyAt @1 @_ (\tex -> Alias.forget tex Linear.>> planetTexture (planetColor newPlanet)) mat

  handleMouseDrag =<< readMouseDrag
  maybeSavePlanet =<< readCharInput

  -- Render! TODO: Store frameIx and imageIx in RenderState and make
  -- 'renderWith' in Ghengin.Monad for which the continuation already takes the
  -- render queue
  editRenderQueue $ \rq ->
    Core.renderWith frameIx imageIx $ \rinfo -> Linear.do
      Ur extent <- Linear.lift getRenderExtent
      let viewport = viewportFromExtent extent
          scissor  = scissorFromExtent extent

      beginRendering rinfo $ Linear.do
        setViewport viewport
        setScissor scissor

        rq <- Core.renderQueueCmd rq

        -- Render Imgui data!
        ImGui.renderDrawData

        Linear.pure rq

  -- Loop!
  return GameData{planet=newPlanet,..}

savePlanet :: Maybe Char -> Ghengin ()
savePlanet (Just 'p') = liftIO $ do
  -- Save new planet configuration to file
  time <- getCurrentTime
  let filename = "planet-" ++ show time ++ ".hs"
  writeFile filename (show newPlanet)
savePlanet  _  = pure ()

handleMouseDrag :: Maybe MouseDrag -> Ghengin ()
handleMouseDrag Nothing = pure ()
handleMouseDrag (Just (MouseDrag deltaX deltaY)) = do
  let sensitivity = 0.002
      yawDelta = -(realToFrac deltaX * sensitivity)
      pitchDelta = realToFrac deltaY * sensitivity
  editRenderQueue $ \rq ->
    editMeshes planetMeshKey rq $
      Linear.traverse' $ propertyAt @0 $ \(Ur tr) ->
        Linear.pure $ Ur $ rotateY yawDelta <> rotateX pitchDelta <> tr

dimensions :: Num a => (a, a)
dimensions = (1920, 1080)

main :: IO ()
main = do
  -- TODO: Read ghenginConf from optparse options
  runGhengin defaultGhenginConf{frameWidth=fst dimensions, frameHeight=snd dimensions} $ do

    let 
      camera :: Camera "view_matrix" "proj_matrix"
      camera = cameraLookAt (vec3 0 0 (-5){- move camera "back"-}) (vec3 0 0 0) dimensions

      planet = defaultPlanet

    mshkey <- renderState $ \RenderState{..} -> Linear.do

      pipeline         <- makeRenderPipeline shaders $
                            StaticBinding (Ur camera) :## GHNil
      ( (pmesh, pipeline),
        Ur minmax )    <- newPlanetMesh pipeline planet
      (pmat, pipeline) <- newPlanetMaterial minmax pipeline planet

      let !(rq0, Ur pkey)   = insertPipeline pipeline renderQueue
      let !(rq1, Ur mkey)   = insertMaterial pkey pmat rq0
      let !(rq2, Ur mshkey) = insertMesh mkey pmesh rq1

      Linear.return (Ur mshkey, RenderState{renderQueue=rq2})

    runGameLoop gameStep GameData{planet, planetMeshKey=mshkey}

  return ()

--------------------------------------------------------------------------------

defaultPlanet :: Planet
defaultPlanet = Planet
  { planetShape = PlanetShape
      { planetResolution = 65
      , planetRadius = 2.2
      , planetNoise  = ImGui.Collapsible $ AddNoiseMasked
          [ StrengthenNoise 0.110 $ MinValueNoise
            { minNoiseVal = 0.87
            , baseNoise   = LayersCoherentNoise
              { centre        = ImGui.WithTooltip $ ImGui.Color $ vec3 (194/255) (129/255) (41/255)
              , baseRoughness = 1.5
              , roughness     = 2.5
              , numLayers     = 20
              , persistence   = 0.4
              }
            }
          , StrengthenNoise 5 $ MinValueNoise
            { minNoiseVal = 0.120
            , baseNoise   = RidgedNoise
              { seed          = 349
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
    mkColors = map $ \(bnd, WithVec3 rn gn bn) ->
      (ImGui.InRange bnd, ImGui.Color (vec3 (rn/255) (gn/255) (bn/255)))

