module Planet.Noise where

import Prelude

import Geomancy (VectorSpace(..))
import Geomancy.Vec3

import GHC.Float
import Numeric.Noise hiding (Noise)

--------------------------------------------------------------------------------
-- * Noise
--------------------------------------------------------------------------------

data Noise
  -- | Level of detail noise created by repeated application of coherent noise with
  -- geometrically increasing roughness and decreasing amplitude.
  --
  -- You can have simple coherent noise by having numLayers = 1
  = LayersCoherentNoise
      { centre        :: !Vec3
        -- Offset noise point
      , baseRoughness :: !Float
        -- ^ Roughness for the first layer
      , strength      :: !Double
        -- ^ Amplitude multiplier for first layer
      , numLayers     :: !Int
        -- ^ How many layers of detail
      , persistence   :: !Double
        -- ^ Apply multiplier to cummulative strength per layer
      , roughness     :: !Float
        -- ^ Apply multiplier to cummulative roughness per layer
      }
  -- | Ridged multi-fractal noise
  | RidgedNoise
      {
      }
  -- | Add @moreNoise@ to @baseNoise@ if @baseNoise > 0@
  | AddNoiseMasked
      { baseNoise :: !Noise
      -- ^ Acts as the mask
      , moreNoise :: !Noise
      -- ^ Add this noise to @'baseNoise'@ if @'baseNoise'@ is > 0
      }
  -- | Add two noise filters unconditionally
  | AddNoise
      { baseNoise :: !Noise
      , moreNoise :: !Noise
      }

evalNoise :: Noise -> Vec3 -> Float
evalNoise LayersCoherentNoise{..} p
  = double2Float (noiseValue * strength)
  where
    seed = 123456
    frequencies = baseRoughness : map (*roughness) frequencies
    amplitudes  = 1 : map (*persistence) amplitudes
    layerNoise frequency amplitude =
      let v = coherentNoise seed (vec3Point (p ^* frequency ^+^ centre))
       in (v + 1) * 0.5 * amplitude {-from [-1 to 1] to [0 to 1], then * amplitude-}
    noiseValue =
      sum $ take numLayers $
      zipWith layerNoise frequencies amplitudes
evalNoise RidgedNoise{..}    p = undefined
evalNoise AddNoiseMasked{..} p = undefined
evalNoise AddNoise{..}       p = evalNoise baseNoise p + evalNoise moreNoise p

vec3Point :: Vec3 -> Point
vec3Point (WithVec3 px py pz) = (float2Double px, float2Double py, float2Double pz)
