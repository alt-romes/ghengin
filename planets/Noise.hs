{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Noise where

import Data.List (foldl')
import GHC.Float

import Data.IORef
import Control.Monad.IO.Class
import Ghengin hiding (get)
import Ghengin.Utils
import Ghengin.Component.UI

import Numeric.Noise hiding (Noise)
import Numeric.Noise.Perlin hiding (Noise)


data NoiseType = SimpleNoise | RigidNoise
  deriving Show

data NoiseSettings = NoiseSettings { numLayers :: !(IORef Int)
                                   , strength  :: !(IORef Float)
                                   , roughness :: !(IORef Float)
                                   , baseRoughness :: !(IORef Float)
                                   , persistence   :: !(IORef Float)
                                   , center    :: !(IORef Vec3)
                                   , minValue  :: !(IORef Float)
                                   , enabled   :: !(IORef Bool)
                                   , type'     :: !(IOSelectRef NoiseType)
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
    ntR <- newIOSelectRef SimpleNoise
    pure $ NoiseSettings numLayersR strengthR roughnessR baseRoughnessR persistenceR centerR minValR enabledR ntR

  makeComponents (NoiseSettings nl st ro br ps ce mv cb nt) = do
    -- TODO: What an amazing bug. If I increase one more letter from some of
    -- the following UI components the game will crash.
    b1 <- checkBox "Enabled" cb
    b2 <- withCombo "Type" nt [SimpleNoise, RigidNoise]
    b3 <- sliderInt "Num Layers" nl 1 8
    b4 <- sliderFloat "Strength" st 0 2
    b5 <- sliderFloat "Roughness" ro 0 5
    b6 <- sliderFloat "Base Roughn" br 0 5
    b7 <- sliderFloat "Persistence" ps 0 2
    b8 <- sliderVec3  "Center" ce 0 5
    b9 <- sliderFloat "Minval" mv 0 5
    -- 
    pure $ or ([b1,b2,b3,b4,b5,b6,b7,b8,b9] :: [Bool])

evalNoise :: MonadIO m => NoiseSettings -> Vec3 -> m Float
evalNoise (NoiseSettings nl st ro br ps ce mv en nt) p =
  readIOSelectRef nt >>= \case
    SimpleNoise -> evalSimpleNoise <$> get nl <*> get st <*> get ro <*> get br <*> get ps <*> get ce <*> get mv <*> get en <*> pure p
    RigidNoise  -> evalRigidNoise  <$> get nl <*> get st <*> get ro <*> get br <*> get ps <*> get ce <*> get mv <*> get en <*> pure p
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
       in if enabled then max (double2Float finalVal - minVal) 0 * stren else 0

    evalRigidNoise nlayers stren rough baseRoughness (float2Double -> persi) cent minVal enabled point =
      -- Accumulator is noiseValue, frequency, amplitude, and weight, and get updated for each layer
      let (finalVal,_,_,_) = foldl' (\(noiseVal, freq, amplitude, weight) _ ->
                                    let v          = 1 - abs(noiseValue' (point ^* freq + cent))
                                        v'         = v*v*weight
                                        noiseVal'  = v'*amplitude + noiseVal
                                        freq'      = freq*rough -- >1 roughness will increase roughness as the layer increases
                                        amplitude' = amplitude*persi -- <1 persistence implies amplitude decreases with each layer
                                        weight'    = v' -- weight starts at 1 is set to the value for each iteration
                                     in (noiseVal', freq', amplitude', weight)
                                    ) (0,baseRoughness,1,1) ([1..nlayers] :: [Int])
       in if enabled then max (double2Float finalVal - minVal) 0 * stren else 0

    noiseValue' (WithVec3 x y z) = noiseValue (perlin 2 5 1 0.5) (float2Double x, float2Double y, float2Double z)

