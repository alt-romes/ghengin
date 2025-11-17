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

import Ghengin.Core
import Ghengin.Core.Mesh
import Ghengin.Core.Shader () -- instance Syntactic Float
import Geomancy.Vec3

import Ghengin.Geometry.Sphere

import qualified FIR
import FIR.Generics (FromGenericProduct(..))
import qualified Math.Linear as FIR
import qualified Generics.SOP as SOP

import Ghengin.Core.Shader.Data

--------------------------------------------------------------------------------
-- * Planet
--------------------------------------------------------------------------------

data PlanetSettings = PlanetSettings { resolution :: !Int
                                     , radius     :: !Float
                                     , color      :: !Vec3
                                     , useFirstLayerAsMask :: !Bool
                                     , noiseSettings :: !(NE.NonEmpty NoiseSettings)
                                     -- , gradient :: ImGradient
                                     }

data MinMax = MinMax !Float !Float
  deriving (P.Eq, Show, Generic)
  deriving Block

instance ShaderData MinMax where
  type FirType MinMax = FIR.Struct '["min" 'FIR.:-> Float, "max" 'FIR.:-> Float]

newPlanetMesh :: _ -- more constraints
              => CompatibleVertex '[Vec3, Vec3, Vec3] π
              => RenderPipeline π bs
               ⊸ PlanetSettings -> Core (Mesh '[Vec3, Vec3, Vec3] '[], RenderPipeline π bs)
newPlanetMesh rp (PlanetSettings re ra co enableMask nss) = enterD "newPlanetMesh" $ Linear.do

  let UnitSphere vs is = newUnitSphere re (Just co)

      -- (ps', elevations) = P.unzip $ (`map` vs) \(p :& _) ->
      --   case nss of
      --     ns NE.:| nss' ->
      --       let initialElevation = evalNoise ns p
      --           mask = if enableMask then initialElevation else 1
      --           noiseElevation = foldl' (\acc ns' -> acc + (evalNoise ns' p)*mask) initialElevation nss'
      --           elevation = ra * (1 + noiseElevation)
      --        in (p ^* elevation, elevation)
      --
      -- ns' = calculateSmoothNormals is ps'
      -- cs  = P.map (\(_ :& _ :&: c) -> c) vs
      -- vs'' = P.zipWith3 (\a b c -> a :& b :&: c) ps' ns' cs
      --
      -- minmax = MinMax (P.minimum elevations) (P.maximum elevations)
   in (createMeshWithIxs rp GHNil vs is ↑)

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


data NoiseType = SimpleNoise | RigidNoise deriving Show

data NoiseSettings = NoiseSettings { numLayers :: !Int
                                   , strength  :: !Float
                                   , roughness :: !Float
                                   , baseRoughness :: !Float
                                   , persistence   :: !Float
                                   , center    :: !Vec3
                                   , minValue  :: !Float
                                   , enabled   :: !Bool
                                   , type'     :: !NoiseType
                                   }

evalNoise :: NoiseSettings -> Vec3 -> Float
evalNoise (NoiseSettings nl st ro br ps ce mv en nt) p =
  case nt of
    SimpleNoise -> evalSimpleNoise nl st ro br ps ce mv en p
    RigidNoise  -> evalRigidNoise nl st ro br ps ce mv en p
  where

    evalSimpleNoise nlayers stren rough baseRoughness (float2Double -> persi) cent minVal enabled point =
      -- Accumulator is noiseValue, frequency, and amplitude, and get updated for each layer
      let (finalVal,_,_) = foldl' (\(noiseVal, freq, amplitude) _ ->
                                    let v          = noiseValue' (point ^* freq + cent)
                                        noiseVal'  = (v + 1)*0.5*amplitude + noiseVal
                                        freq'      = freq*rough -- >1 roughness will increase roughness as the layer increases
                                        amplitude' = amplitude*persi -- <1 persistence implies amplitude decreases with each layer
                                     in (noiseVal', freq', amplitude')
                                    ) (0,baseRoughness,1) ([1..nlayers] :: [Int])
       in if enabled then P.max (double2Float finalVal - minVal) 0 * stren else 0

    evalRigidNoise nlayers stren rough baseRoughness (float2Double -> persi) cent minVal enabled point =
      -- Accumulator is noiseValue, frequency, amplitude, and weight, and get updated for each layer
      let (finalVal,_,_,_) = foldl' (\(noiseVal, freq, amplitude, weight) _ ->
                                    let v          = 1 - abs(noiseValue' (point ^* freq + cent))
                                        v'         = v*v*weight
                                        noiseVal'  = v'*amplitude + noiseVal
                                        freq'      = freq*rough -- >1 roughness will increase roughness as the layer increases
                                        amplitude' = amplitude*persi -- <1 persistence implies amplitude decreases with each layer
                                        weight'    = v' -- weight starts at 1 is set to the value for each iteration
                                     in (noiseVal', freq', amplitude', weight')
                                    ) (0,baseRoughness,1,1) ([1..nlayers] :: [Int])
       in if enabled then P.max (double2Float finalVal - minVal) 0 * stren else 0

    noiseValue' (WithVec3 x y z) = coherentNoise 2 (float2Double x, float2Double y, float2Double z)

defaultPlanetSettings :: PlanetSettings
defaultPlanetSettings
  = PlanetSettings 5 1 (vec3 1 0 0) False
                   [ NoiseSettings 1 1 1 2 0.5 (vec3 0 0 0) 0 True SimpleNoise
                   , NoiseSettings 1 1 1 2 0.5 (vec3 0 0 0) 0 True SimpleNoise
                   ]
