{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
module Planet where

import qualified GHC.Generics as GHC
import qualified Prelude as P
import Ghengin.Core.Prelude as Linear hiding (All, foldl', Generic(..))
import Ghengin.Core.Render
import Ghengin.Core.Render.Pipeline
import Ghengin.Core.Render.Property
import Ghengin.Core.Material
import Ghengin.Core.Type.Compatible

import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV

import Geomancy.Transform

import Ghengin.Core
import Ghengin.Core.Mesh
import Geomancy.Vec3 as V3
import Geomancy.Vec4 as V4

import Ghengin.Geometry.Sphere
import Ghengin.Geometry.Normals

import qualified FIR
import qualified Math.Linear as FIR
import Ghengin.DearImGui.UI

import Ghengin.Core.Shader.Data
import Ghengin.Vulkan.Renderer.Sampler
import Ghengin.Vulkan.Renderer.Texture

import Generics.SOP
-- JuicyPixels
import Codec.Picture

import Planet.Noise

--------------------------------------------------------------------------------
-- * Planet
--------------------------------------------------------------------------------

data Planet = Planet { planetShape :: !PlanetShape
                     , planetColor :: !PlanetColor
                     }
                     deriving GHC.Generic
                     deriving anyclass Generic
                     deriving anyclass HasDatatypeInfo
                     deriving anyclass Widget

data MinMax = MinMax !Float !Float
  deriving (P.Eq, Show, GHC.Generic)
  deriving Block

instance ShaderData MinMax where
  type FirType MinMax = FIR.Struct '["min" 'FIR.:-> Float, "max" 'FIR.:-> Float]

--------------------------------------------------------------------------------
-- * Mesh
--------------------------------------------------------------------------------

type PlanetMeshAttrs = '[Transform]
type PlanetMeshVerts = '[Vec4, Vec3]
type PlanetMesh      = Mesh PlanetMeshVerts PlanetMeshAttrs

data PlanetShape = PlanetShape
  { planetResolution :: !(InRange 2 256 Int)
  , planetRadius     :: !(InRange 0 100 Float)
  , planetNoise      :: !(Collapsible "Planet Noise" Noise)
  }
  deriving GHC.Generic
  deriving anyclass Generic
  deriving anyclass HasDatatypeInfo
  deriving anyclass Widget
  deriving anyclass Default

-- | Make the point on a planet for the given point on a unit sphere
--
-- Returns the updated point and the elevation of that point
pointOnPlanet :: PlanetShape -> Vec3 -> (Vec3, Float)
pointOnPlanet PlanetShape{..} pointOnUnitSphere =
  let elevation = evalNoise (unCollapsible planetNoise) pointOnUnitSphere
      finalElevation = inRangeVal planetRadius * (1+elevation)
   in (pointOnUnitSphere V3.^* finalElevation, finalElevation)

-- | The Biome to pick (from float 0 to 1) at the given point on a unit sphere
biomeOnPoint :: PlanetColor -> Vec3 -> Float
biomeOnPoint PlanetColor{..} pointOnUnitSphere =
  let noise = evalNoise (unCollapsible biomesNoise) pointOnUnitSphere
      heightPercent = ((pointOnUnitSphere.y + 1) / 2) + (noise - biomeNoiseOffset)
      blendRange = inRangeVal biomeBlendAmount / 2
      biomeVal =
        V.ifoldl' (\acc i (Collapsible b) -> let
            dst = heightPercent - (inRangeVal b.biomeStartHeight)
            weight = invLerp dst (-blendRange) blendRange
           in acc * (1 - weight) + fromIntegral i * weight
          ) 0 (V.fromList planetBiomes)

   in biomeVal / (P.max 1 (fromIntegral (P.length planetBiomes - 1)))

-- | Construct the planet mesh and return the minimum and maximum elevation points on the planet
newPlanetMesh :: _ -- more constraints
              => CompatibleVertex PlanetMeshVerts π
              => CompatibleMesh PlanetMeshAttrs π
              => RenderPipeline π bs
               ⊸ Planet
              -> Renderer ((PlanetMesh, RenderPipeline π bs), Ur MinMax)
newPlanetMesh rp Planet{..} = Linear.do

  let UnitSphere us is0 = newUnitCubeSphere (inRangeVal planetShape.planetResolution)

      (planetPs, elevations)
               = V.unzip $ V.map (\(p :&: _) -> pointOnPlanet planetShape p) (V.convert us)
      planetBiomes -- TODO: When this changes, we don't have to update the whole mesh. Just recompute this and poke it into the VertexBuffer directly at the right stride.
               = V.map (\(p :&: _) -> biomeOnPoint planetColor p) (V.convert us)
      planetNs = computeNormals (SV.map fromIntegral is) planetPs
      planetVs = V.zipWith3 (\(WithVec3 x y z) biome n -> vec4 x y z biome :&: n) planetPs planetBiomes planetNs
      is       = weldVertices planetPs (SV.map fromIntegral is0)

      minmax = MinMax (P.minimum elevations) (P.maximum elevations)

   in (, Ur minmax) <$> createMeshWithIxsSV rp (DynamicBinding (Ur mempty) :## GHNil) (V.convert planetVs) (SV.map fromIntegral is)

--------------------------------------------------------------------------------
-- * Material
--------------------------------------------------------------------------------

type PlanetMaterialAttrs = '[MinMax, Texture2D (RGBA8 UNorm)]
type PlanetMaterial = Material PlanetMaterialAttrs

data PlanetColor = PlanetColor
  { biomesNoise  :: Collapsible "Biome Noise" Noise
  -- ^ Noise for the biomes
  , biomeNoiseOffset :: Float
  , planetColorsInterpolate :: Bool
  , planetBiomes :: ![Collapsible "Biome Settings" PlanetBiome]
  -- ^ A list of a percentage value and the color to use up to that percentage
  , biomeBlendAmount :: InRange 0 1 Float
  }
  deriving stock GHC.Generic
  deriving anyclass Generic
  deriving anyclass HasDatatypeInfo
  deriving anyclass Widget
  deriving anyclass Default

data PlanetBiome = PlanetBiome
  { biomeStartHeight :: InRange 0 1 Float
  , biomeTint        :: Color
  , biomeTintPercent :: InRange 0 1 Float
  , biomeColors      :: [(InRange 0 100 Int, Color)]
  }
  deriving stock GHC.Generic
  deriving anyclass Generic
  deriving anyclass HasDatatypeInfo
  deriving anyclass Widget
  deriving anyclass Default

newPlanetMaterial :: forall π p
                   . CompatibleMaterial '[MinMax, Texture2D (RGBA8 UNorm)] π
                  => MinMax
                  -> RenderPipeline π p
                   ⊸ Planet
                  -> Renderer (PlanetMaterial, RenderPipeline π p)
newPlanetMaterial mm pl planet = Linear.do
  tex <- planetTexture (planetColor planet)
  material @_ @π (StaticBinding (Ur mm) :## Texture2DBinding tex :## GHNil) pl

-- | Make a Texture from the planet color
planetTexture :: PlanetColor -> Renderer (Alias (Texture2D (RGBA8 UNorm)))
planetTexture PlanetColor{planetBiomes, planetColorsInterpolate} = Linear.do
  sampler <- createSampler FILTER_LINEAR{-FILTER_NEAREST-} SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE

  -- Generate a gradient image. One width per percent of a color in the biome gradient. One height per biome (where a biome of size 2 counts as 2 biomes)
  let gradientImg = generateImage pixelPaint
                      100{-width=100%-}
                      (P.length planetBiomes){-height=N pixels for a N biomes-}
      pixelPaint x y =
        let Collapsible biome = planetBiomes P.!! y
            tintPercent = inRangeVal biome.biomeTintPercent
            tintColor = let (Color c) = biome.biomeTint in c
            gradColor = evaluateGradient planetColorsInterpolate x (biomeColors biome)
            WithVec3 r g b
              = gradColor V3.^* (1 - tintPercent)
                + tintColor V3.^* tintPercent
         in PixelRGBA8 (round (r*255)) (round (g*255)) (round (b*255)) 255
  liftSystemIO $ savePngImage "my_generated_gradient_texture.png" (ImageRGBA8 gradientImg)
  newTexture gradientImg sampler


-- | Given an int ∈ [0, 100] and a list of colors with a value in the same
-- range, return the color with the matching closest int key to the given int.
evaluateGradient :: Bool -> Int -> [(InRange 0 100 Int, Color)] -> Vec3
evaluateGradient interpolate value colors = do
  if interpolate then
    case find (\(InRange k, _) -> value <= k) colors of
      Nothing -> case colors of
        [] -> vec3 0 0 0
        (_, Color c):_  -> c
      Just (InRange k2, Color c2) ->
        let (InRange k1, Color c1) = case reverse $ P.takeWhile (\(InRange k, _) -> value > k) colors of
              [] -> case colors of
                [] -> (InRange 0, Color (vec3 0 0 0))
                x:_ -> x
              x:_ -> x
            t = invLerp (fromIntegral value) (fromIntegral k1) (fromIntegral k2)
         in c1 V3.^* (1 - t) + c2 V3.^* t
  else
    case find (\(InRange k, _) -> value <= k) colors of
      Nothing -> case colors of
        [] -> vec3 0 0 0
        (_, Color c):_  -> c
      Just (_, Color c) -> c

--------------------------------------------------------------------------------

-- non-compositional instance for "Transform", just for demo
instance ShaderData Transform where
  type FirType Transform = FIR.Struct '[ "m" 'FIR.:-> FIR.M 4 4 Float ]

invLerp :: Float -> Float -> Float -> Float
invLerp value from to = P.max 0 (P.min 1 ((value - from) / (to - from)))
