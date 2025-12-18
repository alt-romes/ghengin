{-# LANGUAGE DeriveAnyClass #-}
module Planet.Noise where

import Prelude

import Geomancy (VectorSpace(..))
import Geomancy.Vec3

import GHC.Float
import qualified GHC.Generics as GHC
import Generics.SOP

import Numeric.Noise hiding (Noise)
import Numeric.Noise.Ridged as Ridged
import qualified Data.List.NonEmpty as NonEmpty
import Ghengin.DearImGui.UI

--------------------------------------------------------------------------------
-- * Noise
--------------------------------------------------------------------------------

data Noise
  -- | Level of detail noise created by repeated application of coherent noise with
  -- geometrically increasing roughness and decreasing amplitude.
  --
  -- You can have simple coherent noise by having numLayers = 1
  = LayersCoherentNoise
      { centre        :: !(WithTooltip "A seed" Color)
        -- Offset noise point
      , baseRoughness :: !Float
        -- ^ Roughness for the first layer
      , numLayers     :: !(InRange 0 20 Int)
        -- ^ How many layers of detail
      , persistence   :: !Double
        -- ^ Apply multiplier to cummulative strength per layer
      , roughness     :: !Float
        -- ^ Apply multiplier to cummulative roughness per layer
      }
  -- | Ridged multi-fractal noise
  | RidgedNoise
      { seed          :: !Int
      , octaves       :: !(InRange 0 64 Int)
      , scale         :: !Double
      , frequency     :: !Double
      , lacunarity    :: !Double
      }
  -- | A noise value which evaluates the base noise value and makes sure it is
  -- at least the given min value
  | MinValueNoise
      { minNoiseVal :: !Float
        -- ^ Value is increased to at least this much
      , baseNoise   :: !Noise
        -- ^ The base noise value
      }
  | StrengthenNoise
      { strength  :: !Float
        -- ^ Multiplies by noise val
      , baseNoise :: !Noise
        -- ^ To compute the noise val
      }
  -- | Add noise filters using the first layer to mask all following ones
  | AddNoiseMasked
      { noiseLayers :: ![Noise]
      }
  -- | Add noise filters unconditionally
  | AddNoiseLayers
      { noiseLayers :: ![Noise]
      }
  deriving Eq
  deriving Show
  deriving GHC.Generic
  deriving anyclass Generic
  deriving anyclass HasDatatypeInfo
  deriving anyclass Widget
  deriving anyclass Default

-- | Evaluate given 'Noise' at a 3D point
evalNoise :: Noise -> Vec3 -> Float
evalNoise LayersCoherentNoise{..} p
  = double2Float noiseVal
  where
    seed = 123456
    frequencies = baseRoughness : map (*roughness) frequencies
    amplitudes  = 1 : map (*persistence) amplitudes
    layerNoise frequency amplitude =
      let v = coherentNoise seed (vec3Point (p ^* frequency ^+^ (colorVec (unTooltip centre))))
       in (v + 1) * 0.5 * amplitude {-from [-1 to 1] to [0 to 1], then * amplitude-}
    noiseVal =
      sum $ take (inRangeVal numLayers) $
      zipWith layerNoise frequencies amplitudes
evalNoise StrengthenNoise{..} p = evalNoise baseNoise p * strength
evalNoise MinValueNoise{..}   p = max 0 (evalNoise baseNoise p - minNoiseVal)
evalNoise RidgedNoise{..}     p = double2Float $
  Ridged.noiseValue (ridged seed (inRangeVal octaves) scale frequency lacunarity) (vec3Point p)
evalNoise AddNoiseMasked{..}  p =
  case NonEmpty.nonEmpty noiseLayers of
    Nothing -> 0
    Just (firstLayer NonEmpty.:| otherLayers) ->
      let firstLayerVal = evalNoise firstLayer p
       in firstLayerVal + sum (map ((*firstLayerVal{-mask-}) . (`evalNoise` p)) otherLayers)
evalNoise AddNoiseLayers{..}  p = sum (map (`evalNoise` p) noiseLayers)

vec3Point :: Vec3 -> Point
vec3Point (WithVec3 px py pz) = (float2Double px, float2Double py, float2Double pz)
