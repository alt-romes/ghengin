{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import qualified Ghengin.Core as Core
import Control.Monad
import Data.List (sort)
import Data.Time
import Geomancy.Transform as Tr
import Geomancy.Vec3
import Ghengin.Core.Mesh
import System.Random
import qualified Ghengin.Core.Prelude as Linear
import Ghengin.Core.Render
import Ghengin.Core.Render.Pipeline
import Ghengin.Core.Render.Property
import Ghengin.Core.Render.Queue
import Ghengin.Input
import Ghengin.Prelude
import qualified Data.Linear.Alias as Alias

import Ghengin.Camera
import Ghengin.Core.Type.Compatible
import qualified Ghengin.DearImGui.Vulkan as ImGui
import qualified Ghengin.DearImGui.UI as ImGui

import Ghengin.Monad

-- planets!
import Shaders -- planet shaders
import Planet
import Planet.Noise
import Planet.UI

--------------------------------------------------------------------------------

data Body π = Body
  { rqkey  :: MeshKey π '[Camera "view_matrix" "proj_matrix"] PlanetMaterialAttrs PlanetMeshVerts PlanetMeshAttrs
  , planet :: Planet
  }

updateBody :: _ => Body π -> Ghengin (Body π)
updateBody Body{..} = do

  (newPlanet, changedShape, changedColor) <- liftRenderer (preparePlanetUI planet)

  -- Update rendered planet if it changed
  when (changedShape || changedColor) $ do

    -- Only regen when vertex data must change
    when (newPlanet.planetShape /= planet.planetShape
       || map (.unCollapsible.biomeStartHeight) newPlanet.planetColor.planetBiomes
          /= map (.unCollapsible.biomeStartHeight) planet.planetColor.planetBiomes) do

      editRenderQueue $ \rq -> Linear.do
        (rq, ()) <- editAtMeshesKey rqkey rq $ \pipeline mat [(msh, x)] -> Linear.do
          ( (pmesh, pipeline),
            Ur minmax ) <- newPlanetMesh pipeline newPlanet
          mat <- propertyAt @0 @MinMax (\(Ur _) -> Linear.pure (Ur minmax)) mat

          -- Re-use old transform and free old mesh
          let !(DynamicBinding (Ur old_tr), msh') = puncons msh
          freeMesh msh'

          pmesh' <- propertyAt @0 @Transform (\(Ur _) -> Linear.pure (Ur old_tr)) pmesh

          Linear.pure (pipeline, mat, [(pmesh', x)], ())
        Linear.pure rq

    -- On any change
    editRenderQueue $ \rq ->
      editMaterial (meshKey2MatKey rqkey) rq $ \mat -> Linear.do
        propertyAt @1 @_ (\tex -> Alias.forget tex Linear.>> planetTexture (planetColor newPlanet)) mat


  return Body{planet=newPlanet,..}

createBody :: _ => Planet -> Transform -> PipelineKey _ _ -> Ghengin (Body _)
createBody planet tr pkey = do
  rqkey <- renderState $ \RenderState{..} -> Linear.do

    (rq0, (pmat, pmesh)) <- editAtPipelineKey pkey renderQueue $ \pipeline matmap -> Linear.do

      ( (pmesh, pipeline),
        Ur minmax )     <- newPlanetMesh pipeline planet
      pmesh             <- propertyAt @0 @Transform
                              (\(Ur _) -> Linear.pure (Ur tr)) pmesh
      (pmat, pipeline)  <- newPlanetMaterial minmax pipeline planet
      Linear.return (pipeline, matmap, (pmat, pmesh))

    let !(rq1, Ur matkey) = insertMaterial pkey pmat rq0
    let !(rq2, Ur mshkey) = insertMesh matkey pmesh rq1

    Linear.return (Ur mshkey, RenderState{renderQueue=rq2})

  pure Body{rqkey, planet}

--------------------------------------------------------------------------------

data GameData π = GameData
  { bodies :: [Body π]
  }

gameStep :: Linear.KnownNat swpImgs => Compatible PlanetMeshVerts PlanetMeshAttrs PlanetMaterialAttrs '[Camera "view_matrix" "proj_matrix"] π
         => GameData π
         -> Linear.Finite FramesInFlight
         -> Linear.Finite swpImgs
         -> Ghengin (GameData π)
gameStep GameData{..} frameIx imageIx = do

  drag <- readMouseDrag
  bodies' <- forM bodies $ \body -> do
    body' <- updateBody body
    handleMouseDrag body'.rqkey drag
    return body'

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

  return GameData{bodies=bodies',..}

handleMouseDrag :: _ => _ -> Maybe MouseDrag -> Ghengin ()
handleMouseDrag _ Nothing = pure ()
handleMouseDrag planetMeshKey (Just (MouseDrag deltaX deltaY)) = do
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
  planet1 <- randomPlanet
  planet2 <- randomPlanet

  runGhengin defaultGhenginConf{frameWidth=fst dimensions, frameHeight=snd dimensions} $ do

    let
      camera :: Camera "view_matrix" "proj_matrix"
      camera = cameraLookAt (vec3 0 0 (-12){- move camera "back"-}) (vec3 0 0 0) dimensions

    pipkey <- renderState $ \RenderState{..} -> Linear.do

      pipeline <- makeRenderPipeline shaders $ StaticBinding (Ur camera) :## GHNil
      let !(rq0, Ur pkey)    = insertPipeline pipeline renderQueue
      Linear.pure (Ur pkey, RenderState{renderQueue=rq0,..})

    b1 <- createBody planet1 (translate (-3) 0 0) pipkey
    b2 <- createBody planet2 (translate 3 0 0 <> Tr.scale 0.5) pipkey

    _ <- runGameLoop gameStep GameData{bodies = [b1, b2]}

    return ()

--------------------------------------------------------------------------------
-- * Random planet generation
--------------------------------------------------------------------------------

randomPlanet :: IO Planet
randomPlanet = do
  shape <- randomPlanetShape
  color <- randomPlanetColor
  pure Planet{planetShape=shape, planetColor=color}

-- Random shape mirrors the structure of 'defaultPlanet': a fine-detail mask
-- (small strength, very high minVal) as the first AddNoiseMasked layer, then
-- a ridged "big features" layer (large strength, low minVal) that only
-- contributes where the mask is positive. Both layers' parameters jitter
-- around the defaults so the overall scale of features stays comparable.
randomPlanetShape :: IO PlanetShape
randomPlanetShape = do
  radius  <- jitter 2.2 0.1
  mask    <- randomMaskLayer
  ridges  <- randomRidgesLayer
  biomesN <- randomBiomesNoise
  blend   <- jitter 0.2 0.05
  pure PlanetShape
    { planetResolution = ImGui.InRange 65
    , planetRadius     = ImGui.InRange radius
    , planetNoise      = ImGui.Collapsible $ AddNoiseMasked [mask, ridges]
    , biomesNoise      = ImGui.Collapsible biomesN
    , biomeBlendAmount = ImGui.InRange blend
    , biomeNoiseOffset = 0
    }

-- | First layer: gates the second. Defaults: strength 0.110, minVal 0.87,
-- coherent fBM with 20 layers, persistence 0.4, baseRoughness 1.5, roughness 2.5.
randomMaskLayer :: IO Noise
randomMaskLayer = do
  strength <- jitter 0.110 0.03
  minVal   <- jitter 0.87  0.04
  c        <- randomCentre
  baseR    <- jitter 1.5 0.3
  rough    <- jitter 2.5 0.3
  nL       <- randomRIO (16, 22 :: Int)
  pers     <- jitter 0.4 0.05
  pure $ StrengthenNoise strength $ MinValueNoise minVal LayersCoherentNoise
    { centre = ImGui.WithTooltip c, baseRoughness = baseR
    , roughness = rough, numLayers = ImGui.InRange nL, persistence = pers
    }

-- | Second layer: big ridged features, masked by the first. Defaults:
-- strength 5, minVal 0.12, ridged with octaves 10, scale 0.59, freq 2, lac 5.2.
randomRidgesLayer :: IO Noise
randomRidgesLayer = do
  strength <- jitter 5.0  1.0
  minVal   <- jitter 0.12 0.03
  seed     <- randomRIO (1, 10_000 :: Int)
  oct      <- randomRIO (8, 12 :: Int)
  scl      <- jitter 0.59 0.10
  freq     <- jitter 2.0  0.4
  lac      <- jitter 5.2  0.6
  pure $ StrengthenNoise strength $ MinValueNoise minVal RidgedNoise
    { seed = seed, octaves = ImGui.InRange oct
    , scale = scl, frequency = freq, lacunarity = lac
    }

randomCentre :: IO ImGui.Color
randomCentre = ImGui.Color <$> randomVec3 (-1) 1

-- | Defaults: StrengthenNoise 0.05 over coherent (baseR 1.0, rough 2.0, 3 layers, pers 2).
randomBiomesNoise :: IO Noise
randomBiomesNoise = do
  strength <- jitter 0.05 0.015
  c        <- randomCentre
  baseR    <- jitter 1.0 0.2
  rough    <- jitter 2.0 0.3
  pers     <- jitter 2.0 0.3
  pure $ StrengthenNoise strength LayersCoherentNoise
    { centre = ImGui.WithTooltip c, baseRoughness = baseR
    , roughness = rough, numLayers = ImGui.InRange 3, persistence = pers
    }

-- | Sample uniformly from [centre - radius, centre + radius].
jitter :: (Random a, Num a) => a -> a -> IO a
jitter c r = randomRIO (c - r, c + r)

randomPlanetColor :: IO PlanetColor
randomPlanetColor = do
  nBiomes <- randomRIO (2, 4 :: Int)
  -- Spread biome start heights evenly in [0, 1) with small jitter, then sort.
  startHeights <- fmap sort $ replicateM nBiomes (randomRIO (0, 1))
  -- Pick this planet's land and ocean hue anchors independently per planet so
  -- two planets generated in sequence pick unrelated palettes.
  landHue  <- randomRIO (0, 1)
  oceanHue <- randomRIO (0, 1)
  biomes   <- mapM (randomBiome landHue oceanHue) startHeights
  interp       <- (> (0.5 :: Float)) <$> randomRIO (0, 1)
  pure PlanetColor
    { planetBiomes            = biomes
    , planetColorsInterpolate = interp
    }

randomBiome
  :: Float  -- ^ Land hue anchor
  -> Float  -- ^ Ocean hue anchor
  -> Float  -- ^ Biome start height
  -> IO (ImGui.Collapsible "Biome Settings" PlanetBiome)
randomBiome landHue oceanHue startHeight = do
  nLand   <- randomRIO (3, 6 :: Int)
  nOcean  <- randomRIO (2, 4 :: Int)
  -- Each biome picks a small hue offset from the planet's anchor, keeping the
  -- overall palette coherent across biomes.
  landOff   <- randomRIO (-0.08, 0.08)
  oceanOff  <- randomRIO (-0.05, 0.05)
  land    <- randomColorRamp nLand  (wrapHue (landHue  + landOff))  0.55 0.95
  ocean   <- randomColorRamp nOcean (wrapHue (oceanHue + oceanOff)) 0.70 0.55
  tintHue <- randomRIO (0, 1)
  tintPct <- randomRIO (0, 0.3)
  pure $ ImGui.Collapsible PlanetBiome
    { biomeColors      = land
    , biomeOceanColors = ocean
    , biomeStartHeight = ImGui.InRange startHeight
    , biomeTint        = ImGui.Color (hsvToRgb tintHue 0.6 0.8)
    , biomeTintPercent = ImGui.InRange tintPct
    }

-- | Build a color ramp of N stops along the [1, 100] range. The hue stays near
-- 'baseHue' while saturation and value vary, so the ramp reads as shades of
-- one color family rather than a rainbow.
randomColorRamp
  :: Int    -- ^ Number of stops
  -> Float  -- ^ Base hue
  -> Float  -- ^ Saturation centre
  -> Float  -- ^ Value centre
  -> IO [(ImGui.InRange 0 100 Int, ImGui.Color)]
randomColorRamp n baseHue baseSat baseVal = do
  let stops = [ round (fromIntegral i * (99 :: Double) / fromIntegral (n - 1)) + 1 | i <- [0 .. n - 1] ]
  forM stops $ \s -> do
    hOff <- randomRIO (-0.04, 0.04)
    sOff <- randomRIO (-0.2, 0.1)
    vOff <- randomRIO (-0.25, 0.15)
    let h = wrapHue (baseHue + hOff)
        sat = clamp01 (baseSat + sOff)
        val = clamp01 (baseVal + vOff)
    pure (ImGui.InRange s, ImGui.Color (hsvToRgb h sat val))

randomVec3 :: Float -> Float -> IO Vec3
randomVec3 lo hi = vec3 <$> randomRIO (lo, hi) <*> randomRIO (lo, hi) <*> randomRIO (lo, hi)

clamp01 :: Float -> Float
clamp01 x = max 0 (min 1 x)

-- | Wrap a hue value into the [0, 1) range.
wrapHue :: Float -> Float
wrapHue h = let f = h - fromIntegral (floor h :: Int) in if f < 0 then f + 1 else f

-- | HSV → RGB with all components in [0, 1].
hsvToRgb :: Float -> Float -> Float -> Vec3
hsvToRgb h s v =
  let c = v * s
      h' = wrapHue h * 6
      x = c * (1 - abs (fracMod2 h' - 1))
      m = v - c
      (r, g, b)
        | h' < 1 = (c, x, 0)
        | h' < 2 = (x, c, 0)
        | h' < 3 = (0, c, x)
        | h' < 4 = (0, x, c)
        | h' < 5 = (x, 0, c)
        | otherwise = (c, 0, x)
  in vec3 (r + m) (g + m) (b + m)
  where
    fracMod2 z = z - 2 * fromIntegral (floor (z / 2) :: Int)

