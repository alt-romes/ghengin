{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
module Planet where

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
                                     , noiseSettings :: !NoiseSettings
                                     }
data NoiseSettings = NoiseSettings { strength  :: !(IORef Float)
                                   , roughness :: !(IORef Float)
                                   , center    :: !(IORef Vec3)
                                   }

planetSettings :: PlanetSettings
               -> [UIComponent]
planetSettings (PlanetSettings re ra co (NoiseSettings st ro ce)) =
  [ SliderInt "Resolution" re 2 200
  , SliderFloat "Radius" ra 0 3
  , ColorPicker "Color" co
  -- What an amazing bug. If I increase one more letter from these two
  -- following UI components the game will crash.
  , SliderFloat "Noise Roughnes" ro 0 2
  , SliderFloat "Noise Strength" st 0 2
  , ColorPicker "Noise Center" ce
  ]

newNoiseSettings :: IO NoiseSettings
newNoiseSettings = do
  strengthR  <- newIORef 1
  roughnessR <- newIORef 1
  centerR    <- newIORef (vec3 0 0 0)
  pure $ NoiseSettings strengthR roughnessR centerR

newPlanetSettings :: IO PlanetSettings
newPlanetSettings = do
  resR   <- newIORef 5
  radR   <- newIORef 1
  colorR <- newIORef (vec3 1 0 0)
  ns     <- newNoiseSettings
  pure $ PlanetSettings resR radR colorR ns


newPlanet :: PlanetSettings -> Renderer Mesh
newPlanet (PlanetSettings re ra co ns@(NoiseSettings st ro ce)) = do
  re' <- liftIO $ readIORef re
  ra' <- liftIO $ readIORef ra
  co' <- liftIO $ readIORef co
  st' <- liftIO $ readIORef st
  ro' <- liftIO $ readIORef ro
  ce' <- liftIO $ readIORef ce
  let UnitSphere vs is = newUnitSphere re' (Just co')
      noiseElevation = evaluateNoise st' ro' ce'
      -- TODO better code but oh well
      ps' = map (\(Vertex p n c) -> p ^* (ra' * (1 + noiseElevation p))) vs
      ns' = calculateSmoothNormals is ps'
      cs  = map (\(Vertex p n c) -> c) vs
      vs'' = zipWith3 Vertex ps' ns' cs
   in createMeshWithIxs vs'' is

-- :| Noise |:

toPoint :: Vec3 -> Point
toPoint (WithVec3 x y z) = (float2Double x, float2Double y, float2Double z)

defPerlin :: Perlin
defPerlin = perlin 2 5 1 1

-- | Evaluate Noise at a certain point
evaluateNoise :: Float -- ^ Strength
              -> Float -- ^ Roughness
              -> Vec3  -- ^ Center
              -> Vec3
              -> Float
evaluateNoise stren rough cent v =
  let val = ((noiseValue defPerlin (toPoint (v ^* rough + cent))) + 1)*0.5
   in double2Float val * stren



