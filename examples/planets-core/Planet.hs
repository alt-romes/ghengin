{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}
module Planet where

import qualified Prelude as P
import Ghengin.Core.Prelude as Linear hiding (All, foldl')
import Ghengin.Core.Render
import Ghengin.Core.Render.Pipeline
import Ghengin.Core.Render.Property
import Ghengin.Core.Material
import Ghengin.Core.Type.Compatible
import Ghengin.Core.Log

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

import Ghengin.Core.Shader.Data

import Planet.Noise

--------------------------------------------------------------------------------
-- * Planet
--------------------------------------------------------------------------------

data Planet = Planet { resolution  :: !Int
                     , planetShape :: !PlanetShape
                     }

data MinMax = MinMax !Float !Float
  deriving (P.Eq, Show, Generic)
  deriving Block

instance ShaderData MinMax where
  type FirType MinMax = FIR.Struct '["min" 'FIR.:-> Float, "max" 'FIR.:-> Float]

--------------------------------------------------------------------------------
-- * Mesh
--------------------------------------------------------------------------------

type PlanetMesh = Mesh '[Vec3, Vec3] '[Transform]

data PlanetShape = PlanetShape
  { planetRadius :: !Float
  , planetNoise  :: !Noise
  }

-- | Make the point on a planet for the given point on a unit sphere
--
-- Returns the updated point and the elevation of that point
pointOnPlanet :: PlanetShape -> Vec3 -> (Vec3, Float)
pointOnPlanet PlanetShape{..} pointOnUnitSphere =
  let elevation = evalNoise planetNoise pointOnUnitSphere
      finalElevation = planetRadius * (1+elevation)
   in (pointOnUnitSphere ^* finalElevation, finalElevation)

-- | Construct the planet mesh and return the minimum and maximum elevation points on the planet
newPlanetMesh :: _ -- more constraints
              => CompatibleVertex '[Vec3, Vec3] π
              => CompatibleMesh '[Transform] π
              => RenderPipeline π bs
               ⊸ Planet
              -> Core ((PlanetMesh, RenderPipeline π bs), Ur MinMax)
newPlanetMesh rp Planet{..} = enterD "newPlanetMesh" $ Linear.do

  let UnitSphere us is = newUnitSphere resolution

      (planetPs, elevations)
               = V.unzip $ V.map (\(p :&: _) -> pointOnPlanet planetShape p) (V.convert us)
      planetNs = computeNormals (SV.map fromIntegral is) (V.convert planetPs)
      planetVs = SV.zipWith (:&:) (V.convert planetPs) planetNs

      minmax = MinMax (P.minimum elevations) (P.maximum elevations)

   in (, Ur minmax) <$> (createMeshWithIxsSV rp (DynamicBinding (Ur mempty) :## GHNil) planetVs is ↑)

--------------------------------------------------------------------------------
-- * Material
--------------------------------------------------------------------------------

type PlanetMaterial = Material '[MinMax, Texture2D (RGBA8 UNorm)]

newPlanetMaterial :: forall π p
                   . CompatibleMaterial '[MinMax, Texture2D (RGBA8 UNorm)] π
                  => MinMax
                  -> Alias (Texture2D (RGBA8 UNorm))
                   ⊸ RenderPipeline π p
                   ⊸ Core (PlanetMaterial, RenderPipeline π p)
newPlanetMaterial mm t pl = ( material @_ @π (StaticBinding (Ur mm) :## Texture2DBinding t :## GHNil) pl ↑)

--------------------------------------------------------------------------------

-- non-compositional instance for "Transform", just for demo
instance ShaderData Transform where
  type FirType Transform = FIR.Struct '[ "m" 'FIR.:-> FIR.M 4 4 Float ]
