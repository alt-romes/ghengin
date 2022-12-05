{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
module Planet where

import GHC.Float
import Data.IORef
import Control.Monad

import Numeric.Noise
import Numeric.Noise.Perlin

import Ghengin
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
  [ SliderInt "Resolution" re 2 256
  , SliderFloat "Radius" ra 0 200
  , ColorPicker "Color" co
  , SliderFloat "Noise Strength" st 0 20000
  -- , SliderFloat "Noise Roughness" ro 0 20000
  -- , ColorPicker "Noise Center" ce 0 200
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
newPlanet (PlanetSettings re ra co (NoiseSettings st ro ce)) = do
  re' <- liftIO $ readIORef re
  ra' <- liftIO $ readIORef ra
  co' <- liftIO $ readIORef co
  st' <- liftIO $ readIORef st
  ro' <- liftIO $ readIORef ro
  ce' <- liftIO $ readIORef ce
  let UnitSphere vs is = newUnitSphere re' (Just co')
      noiseElevation x = evaluateNoise x
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
evaluateNoise :: Vec3 -> Float
evaluateNoise v = double2Float $ ((noiseValue defPerlin (toPoint v)) + 1)*0.5



