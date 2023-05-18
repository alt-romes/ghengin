{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}
module Noise where

import Ghengin.Core.Log
import Prelude.Linear (Ur(Ur), Dupable(..), ($))
import Prelude hiding (($))
import Data.List (foldl')
import GHC.Float

import Data.IORef
import qualified Control.Functor.Linear as Linear
import qualified Data.Unrestricted.Linear as Linear
import qualified Control.Monad.IO.Class.Linear as Linear
import qualified System.IO.Linear as Linear
import Ghengin hiding (get)
import Ghengin.Utils
import Ghengin.Component.UI

import Numeric.Noise hiding (Noise)
import Numeric.Noise hiding (Noise)


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
  type ReactivityInput NoiseSettings = ()
  type ReactivityOutput NoiseSettings = ()
  type ReactivityConstraints _ w = Dupable w

  makeSettings = Linear.do
    Ur numLayersR     <- Linear.newIORef 1
    Ur strengthR      <- Linear.newIORef 1
    Ur baseRoughnessR <- Linear.newIORef 1
    Ur roughnessR     <- Linear.newIORef 2
    Ur persistenceR   <- Linear.newIORef 0.5
    Ur centerR        <- Linear.newIORef (vec3 0 0 0)
    Ur minValR        <- Linear.newIORef 0
    Ur enabledR       <- Linear.newIORef True
    Ur ntR            <- newIOSelectRef SimpleNoise
    Linear.pure $ Ur $ NoiseSettings numLayersR strengthR roughnessR baseRoughnessR persistenceR centerR minValR enabledR ntR

  makeComponents (NoiseSettings nl st ro br ps ce mv cb nt) () = Linear.do

    Ur b1 <- checkBox "Enabled" cb
    Ur b2 <- withCombo "Type" nt [SimpleNoise, RigidNoise]
    Ur b3 <- sliderInt "Num Layers" nl 1 8
    Ur b4 <- sliderFloat "Strength" st 0 2
    Ur b5 <- sliderFloat "Roughness" ro 0 5
    Ur b6 <- sliderFloat "Base Roughn" br 0 5
    Ur b7 <- sliderFloat "Persistence" ps 0 2
    Ur b8 <- sliderVec3  "Center" ce 0 5
    Ur b9 <- sliderFloat "Minval" mv 0 5

    -- pure $ or ([b1,b2,b3,b4,b5,b6,b7,b8,b9] :: [Bool])
    Linear.pure ()

evalNoise :: HasLogger m => NoiseSettings -> Vec3 -> Linear.UrT m Float
evalNoise (NoiseSettings nl st ro br ps ce mv en nt) p = Linear.UrT $ Linear.do
  logT "Eval noise"
  readIOSelectRef nt Linear.>>= \case
    Ur SimpleNoise -> Linear.do
      logT "Simple noise"
      (Ur !x) <- Linear.liftSystemIOU $ evalSimpleNoise <$> get nl <*> get st <*> get ro <*> get br <*> get ps <*> get ce <*> get mv <*> get en <*> pure p
      logT "Done"
      logT ("Made: " <> toLogStr (show x))
      Linear.pure (Ur x)
    Ur RigidNoise  -> Linear.do
      logT "Rigid noise"
      !x <- Linear.liftSystemIOU $ evalRigidNoise  <$> get nl <*> get st <*> get ro <*> get br <*> get ps <*> get ce <*> get mv <*> get en <*> pure p
      logT "Done"
      Linear.pure x
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

    noiseValue' (WithVec3 x y z) = coherentNoise 2 (float2Double x, float2Double y, float2Double z)

