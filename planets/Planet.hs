{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
module Planet where

import Data.String
import Data.List (foldl')
import GHC.Float
import Data.IORef
import Control.Monad

import Numeric.Noise
import Numeric.Noise.Perlin

import Ghengin hiding (get)
import Ghengin.Utils
import Ghengin.Vulkan
import Ghengin.Component.Mesh.Sphere
import Ghengin.Component.Mesh
import Ghengin.Component.UI

data PlanetSettings = PlanetSettings { resolution :: !(IORef Int)
                                     , radius     :: !(IORef Float)
                                     , color      :: !(IORef Vec3)
                                     , noiseSettings :: ![NoiseSettings]
                                     }
data NoiseSettings = NoiseSettings { numLayers :: !(IORef Int)
                                   , strength  :: !(IORef Float)
                                   , roughness :: !(IORef Float)
                                   , baseRoughness :: !(IORef Float)
                                   , persistence   :: !(IORef Float)
                                   , center    :: !(IORef Vec3)
                                   , minValue  :: !(IORef Float)
                                   , enabled   :: !(IORef Bool)
                                   }

instance UISettings NoiseSettings where
  makeSettings = do
    numLayersR  <- newIORef 1
    strengthR  <- newIORef 1
    baseRoughnessR <- newIORef 1
    roughnessR <- newIORef 2
    persistenceR <- newIORef 0.5
    centerR    <- newIORef (vec3 0 0 0)
    minValR <- newIORef 0
    enabledR <- newIORef True
    pure $ NoiseSettings numLayersR strengthR roughnessR baseRoughnessR persistenceR centerR minValR enabledR

  makeComponents (NoiseSettings nl st ro br ps ce mv cb) =
    -- What an amazing bug. If I increase one more letter from the first two
    -- following UI components the game will crash.
    [ Checkbox "Enabled" cb
    , SliderInt "Num Layers" nl 1 8
    , SliderFloat "Strength" st 0 2
    , SliderFloat "Roughness" ro 0 5
    , SliderFloat "Base Roughn" br 0 5
    , SliderFloat "Persistence" ps 0 2
    , SliderVec3  "Center" ce 0 5
    , SliderFloat "Minval" mv 0 5
    ]


instance UISettings PlanetSettings where
  makeSettings = do
    resR   <- newIORef 5
    radR   <- newIORef 1
    colorR <- newIORef (vec3 1 0 0)
    ns1    <- makeSettings @NoiseSettings
    ns2    <- makeSettings @NoiseSettings
    pure $ PlanetSettings resR radR colorR [ns1, ns2]

  makeComponents (PlanetSettings re ra co nss) =
    [ WithTree "Planet" [ SliderInt "Resolution" re 2 200
                        , SliderFloat "Radius" ra 0 3
                        , ColorPicker "Color" co
                        ]
    ]
    -- Careful! The tree's cannot have the same Id otherwise they will behave
    -- the same.
    <> map (\(ns, i) -> WithTree ("Layer " <> fromString (show i)) $ makeComponents ns) (zip nss [1..])
    -- <> concatMap makeComponents nss

newPlanet :: PlanetSettings -> Renderer Mesh
newPlanet (PlanetSettings re ra co nss) = do
  re' <- get re
  ra' <- get ra
  co' <- get co


  let UnitSphere vs is = newUnitSphere re' (Just co')

  ps' <- forM vs $ \(Vertex p _ _) -> do
    noiseElevation <- foldM (\acc (NoiseSettings nl st ro bro pers ce mv en) -> do
                                    nv <- evaluateNoise <$> get nl <*> get st <*> get ro <*> get bro <*> get pers <*> get ce <*> get mv <*> get en <*> pure p
                                    pure $ nv + acc
                            ) 0 nss
    pure $ p ^* (ra' * (1 + noiseElevation))

  let
      ns' = calculateSmoothNormals is ps'
      cs  = map (\(Vertex _ _ c) -> c) vs
      vs'' = zipWith3 Vertex ps' ns' cs
   in createMeshWithIxs vs'' is

-- :| Noise |:

toPoint :: Vec3 -> Point
toPoint (WithVec3 x y z) = (float2Double x, float2Double y, float2Double z)

defPerlin :: Perlin
defPerlin = perlin 2 5 1 0.5

-- | Evaluate Noise at a certain point
evaluateNoise :: Int   -- ^ Num layers
              -> Float -- ^ Strength
              -> Float -- ^ Roughness
              -> Float -- ^ Base Roughness
              -> Float -- ^ Persistence
              -> Vec3  -- ^ Center
              -> Float -- ^ Min value
              -> Bool  -- ^ Enabled
              -> Vec3  -- ^ Point
              -> Float
evaluateNoise nlayers stren rough baseRoughness (float2Double -> persi) cent minVal enabled point =
  -- Accumulator is noiseValue, frequency, and amplitude, and get updated for each layer
  let (finalVal,_,_) = foldl' (\(noiseVal, freq, amplitude) _ ->
                                let v          = noiseValue defPerlin (toPoint (point ^* freq + cent))
                                    noiseVal'  = (v + 1)*0.5*amplitude + noiseVal
                                    freq'      = freq*rough -- >1 roughness will increase roughness as the layer increases
                                    amplitude' = amplitude*persi -- <1 persistence implies amplitude decreases with each layer
                                 in (noiseVal', freq', amplitude')
                                ) (0,baseRoughness,1) [1..nlayers]
   in if enabled then max (double2Float finalVal - minVal) 0 * stren else 0



