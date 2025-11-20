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
import Data.List (foldl')
import Ghengin.Core.Log

import GHC.Float
import Numeric.Noise hiding (Noise)
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V

import Geomancy.Transform

import Ghengin.Core
import Ghengin.Core.Mesh
import Geomancy (VectorSpace(..))
import Geomancy.Vec3

import Ghengin.Geometry.Sphere
import Ghengin.Geometry.Normals

import qualified FIR
import qualified Math.Linear as FIR

import Ghengin.Core.Shader.Data

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
pointOnPlanet :: PlanetShape -> Vec3 -> Vec3
pointOnPlanet PlanetShape{..} pointOnUnitSphere =
  let elevation = evalNoise planetNoise pointOnUnitSphere
   in pointOnUnitSphere ^* planetRadius ^* (1+elevation)

newPlanetMesh :: _ -- more constraints
              => CompatibleVertex '[Vec3, Vec3] π
              => CompatibleMesh '[Transform] π
              => RenderPipeline π bs
               ⊸ Planet
              -> Core (PlanetMesh, RenderPipeline π bs)
newPlanetMesh rp Planet{..} = enterD "newPlanetMesh" $ Linear.do

  let UnitSphere us is = newUnitSphere resolution

      planetVxs = P.map (\(p :&: n) -> pointOnPlanet planetShape p :&: n) us

      -- (ps', elevations) = P.unzip $ (`map` vs) \(p :& _) ->
      --   case nss of
      --     ns NE.:| nss' ->
      --       let initialElevation = evalNoise ns p
      --           mask = if enableMask then initialElevation else 1
      --           noiseElevation = foldl' (\acc ns' -> acc + (evalNoise ns' p)*mask) initialElevation nss'
      --           elevation = ra * (1 + noiseElevation)
      --        in (p ^* (elevation), elevation)
      --
      -- ns'  = V.toList $ computeNormals (V.fromList (P.map fromIntegral is)) (V.fromList ps')
      -- vs'' = P.zipWith3 (\a b c -> a :& b :&: c) ps' ns' (map (\(_ :& _ :&: c) -> c) vs)

      -- minmax = MinMax (P.minimum elevations) (P.maximum elevations)

   in (createMeshWithIxs rp (DynamicBinding (Ur mempty) :## GHNil) planetVxs is ↑)

--------------------------------------------------------------------------------
-- * Material
--------------------------------------------------------------------------------

newPlanetMaterial :: forall π p
                   . CompatibleMaterial '[MinMax,Texture2D] π
                  => MinMax
                  -> Alias Texture2D
                   ⊸ RenderPipeline π p
                   ⊸ Core (Material '[MinMax,Texture2D], RenderPipeline π p)
newPlanetMaterial mm t pl = ( material @_ @π (StaticBinding (Ur mm) :## Texture2DBinding t :## GHNil) pl ↑)

--------------------------------------------------------------------------------
-- * Noise
--------------------------------------------------------------------------------

data Noise = CoherentNoise
      { centre    :: !Vec3
      , roughness :: !Float
      , strength  :: !Float
      }

evalNoise :: Noise -> Vec3 -> Float
evalNoise CoherentNoise{..} point = double2Float
  let seed = 123456
      WithVec3 px py pz = point ^* roughness ^+^ centre
      noiseValue  = coherentNoise seed (float2Double px, float2Double py, float2Double pz)
      noiseScaled = (noiseValue + 1) * 0.5 {-from [-1 to 1] to [0 to 1] -}
   in noiseScaled * float2Double strength

--------------------------------------------------------------------------------

-- non-compositional instance for "Transform", just for demo
instance ShaderData Transform where
  type FirType Transform = FIR.Struct '[ "m" 'FIR.:-> FIR.M 4 4 Float ]
