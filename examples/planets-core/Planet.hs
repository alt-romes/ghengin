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
import Geomancy.Vec3

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
type PlanetMeshVerts = '[Vec3, Vec3, Float]
type PlanetMesh      = Mesh PlanetMeshVerts PlanetMeshAttrs

data PlanetShape = PlanetShape
  { planetResolution :: !(InRange 2 512 Int)
  , planetRadius     :: !(InRange 0 100 Float)
  , planetNoise      :: !(Collapsible "Noise section" Noise)
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
   in (pointOnUnitSphere ^* finalElevation, finalElevation)

-- | Construct the planet mesh and return the minimum and maximum elevation points on the planet
newPlanetMesh :: _ -- more constraints
              => CompatibleVertex PlanetMeshVerts π
              => CompatibleMesh PlanetMeshAttrs π
              => RenderPipeline π bs
               ⊸ Planet
              -> Renderer ((PlanetMesh, RenderPipeline π bs), Ur MinMax)
newPlanetMesh rp Planet{..} = Linear.do

  let UnitSphere us is0 = newUnitSphere (inRangeVal planetShape.planetResolution)

      (planetPs, elevations)
               = V.unzip $ V.map (\(p :&: _) -> pointOnPlanet planetShape p) (V.convert us)
      planetNs = computeNormals (SV.map fromIntegral is) planetPs
      planetVs = V.zipWith (\x y -> x :& y :&: 0) (planetPs) planetNs
      is       = weldVertices planetPs (SV.map fromIntegral is0)

      minmax = MinMax (P.minimum elevations) (P.maximum elevations)

   in (, Ur minmax) <$> createMeshWithIxsSV rp (DynamicBinding (Ur mempty) :## GHNil) (V.convert planetVs) (SV.map fromIntegral is)

--------------------------------------------------------------------------------
-- * Material
--------------------------------------------------------------------------------

type PlanetMaterialAttrs = '[MinMax, Texture2D (RGBA8 UNorm)]
type PlanetMaterial = Material PlanetMaterialAttrs

data PlanetColor = PlanetColor
  { planetBiomes :: [PlanetBiome]
  -- ^ A list of a percentage value and the color to use up to that percentage
  }
  deriving stock GHC.Generic
  deriving anyclass Generic
  deriving anyclass HasDatatypeInfo
  deriving anyclass Widget
  deriving anyclass Default

data PlanetBiome = PlanetBiome
  { biomeSize   :: InRange 0 10 Int
  , biomeColors :: [(InRange 0 100 Int, Color)]
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
planetTexture PlanetColor{planetBiomes} = Linear.do
  sampler <- createSampler FILTER_NEAREST SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE

  -- Expand a list of biomes with arbitrary size into a list of biomes all with size=1
  let expBiomes = concat $ P.map (\b -> P.replicate (inRangeVal (biomeSize b)) b) planetBiomes

  -- Generate a gradient image. One width per percent of a color in the biome gradient. One height per biome (where a biome of size 2 counts as 2 biomes)
  let gradientImg = generateImage pixelPaint
                      100{-width=100%-}
                      (P.length expBiomes){-height=N pixels for a biome of size N-}
      pixelPaint x y = case find (\(InRange limit, _) -> x < limit) (biomeColors (expBiomes P.!! y)) of
        Nothing -> PixelRGBA8 255 255 255 255
        Just (_, Color (WithVec3 r g b)) -> PixelRGBA8 (round (r*255)) (round (g*255)) (round (b*255)) 255
  liftSystemIO $ savePngImage "my_generated_gradient_texture.png" (ImageRGBA8 gradientImg)
  newTexture gradientImg sampler

--------------------------------------------------------------------------------

-- non-compositional instance for "Transform", just for demo
instance ShaderData Transform where
  type FirType Transform = FIR.Struct '[ "m" 'FIR.:-> FIR.M 4 4 Float ]

