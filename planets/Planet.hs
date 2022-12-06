{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
module Planet where

import qualified Data.List.NonEmpty as NE
import Data.String
import Data.List (foldl')
import GHC.Float
import Data.IORef
import Control.Monad

import Ghengin hiding (get)
import Ghengin.Utils
import Ghengin.Vulkan
import Ghengin.Component.Mesh.Sphere
import Ghengin.Component.Mesh
import Ghengin.Component.UI

import Noise

data PlanetSettings = PlanetSettings { resolution :: !(IORef Int)
                                     , radius     :: !(IORef Float)
                                     , color      :: !(IORef Vec3)
                                     , useFirstLayerAsMask :: !(IORef Bool)
                                     , noiseSettings :: !(NE.NonEmpty NoiseSettings)
                                     }
instance UISettings PlanetSettings where
  makeSettings = do
    resR   <- newIORef 5
    radR   <- newIORef 1
    colorR <- newIORef (vec3 1 0 0)
    boolR  <- newIORef False
    ns1    <- makeSettings @NoiseSettings
    ns2    <- makeSettings @NoiseSettings
    pure $ PlanetSettings resR radR colorR boolR [ns1, ns2]

  makeComponents (PlanetSettings re ra co bo nss) =
    [ WithTree "Planet" [ SliderInt "Resolution" re 2 200
                        , SliderFloat "Radius" ra 0 3
                        , ColorPicker "Color" co
                        , Checkbox "Mask" bo
                        ]
    ]
    -- Careful! The tree's cannot have the same Id otherwise they will behave
    -- the same.
    <> NE.toList (fmap (\(ns, i) -> WithTree ("Layer " <> fromString (show i)) $ makeComponents ns) (NE.zip nss [1..]))
    -- <> concatMap makeComponents nss

newPlanet :: PlanetSettings -> Renderer Mesh
newPlanet (PlanetSettings re ra co bo nss) = do
  re' <- get re
  ra' <- get ra
  co' <- get co
  enableMask <- get bo


  let UnitSphere vs is = newUnitSphere re' (Just co')

  ps' <- forM vs $ \(Vertex p _ _) -> do
    case nss of
      ns NE.:| nss' -> do
        initialElevation <- evalNoise ns p
        let mask = if enableMask then initialElevation else 1
        noiseElevation <- foldM (\acc ns' -> evalNoise ns' p >>= pure . (+acc) . (* mask)) initialElevation nss'
        pure $ p ^* (ra' * (1 + noiseElevation))

  let
      ns' = calculateSmoothNormals is ps'
      cs  = map (\(Vertex _ _ c) -> c) vs
      vs'' = zipWith3 Vertex ps' ns' cs
   in createMeshWithIxs vs'' is



