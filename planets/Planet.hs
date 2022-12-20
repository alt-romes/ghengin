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

data MinMax = MinMax Float Float

-- type MinMaxMaterial = Material [ 0 :-> MinMax ]

-- instance Material (Material [ 0 :-> MinMax ]) where
--   writeBindings


data PlanetSettings = PlanetSettings { resolution :: !(IORef Int)
                                     , radius     :: !(IORef Float)
                                     , color      :: !(IORef Vec3)
                                     , useFirstLayerAsMask :: !(IORef Bool)
                                     , noiseSettings :: !(NE.NonEmpty NoiseSettings)
                                     , displayFace   :: !(IOSelectRef DisplayFace)
                                     }

data DisplayFace = All | FaceUp | FaceRight deriving Show

instance UISettings PlanetSettings where
  makeSettings = do
    resR   <- newIORef 5
    radR   <- newIORef 1
    colorR <- newIORef (vec3 1 0 0)
    boolR  <- newIORef False
    ns1    <- makeSettings @NoiseSettings
    ns2    <- makeSettings @NoiseSettings
    ns3    <- makeSettings @NoiseSettings
    df     <- newIOSelectRef All
    pure $ PlanetSettings resR radR colorR boolR [ns1, ns2, ns3] df

  makeComponents (PlanetSettings re ra co bo nss df) = do
    b1 <- withTree "Planet" do
      b1 <- sliderInt "Resolution" re 2 200
      b2 <- sliderFloat "Radius" ra 0 3
      b3 <- colorPicker "Color" co
      b4 <- checkBox "Mask" bo
      b5 <- withCombo "Faces" df [All, FaceUp, FaceRight]
      pure $ or ([b1,b2,b3,b4,b5] :: [Bool])
      -- Careful! The components cannot have the same Id otherwise they will behave
      -- the same.
    bs <- mapM (\(ns, i) -> withTree ("Layer " <> fromString (show i)) $
                                makeComponents ns) (NE.zip nss [1..])
    pure $ b1 || or bs

newPlanet :: PlanetSettings -> Renderer e (Mesh, MinMax)
newPlanet (PlanetSettings re ra co bo nss df) = do
  re' <- get re
  ra' <- get ra
  co' <- get co
  df' <- get df
  enableMask <- get bo


  let (vs, is) = case df' of
                   All -> let UnitSphere v i = newUnitSphere re' (Just co') in (v, i)
                   FaceUp -> let UF v i = newUnitFace re' (vec3 0 (-1) 0)
                              in (zipWith3 Vertex v (calculateSmoothNormals i v) (repeat co'),i)
                   FaceRight -> let UF v i = newUnitFace re' (vec3 1 0 0)
                              in (zipWith3 Vertex v (calculateSmoothNormals i v) (repeat co'),i)

  (ps', elevations) <- unzip <$> forM vs \(Vertex p _ _) -> do
    case nss of
      ns NE.:| nss' -> do
        initialElevation <- evalNoise ns p
        let mask = if enableMask then initialElevation else 1
        noiseElevation <- foldM (\acc ns' -> evalNoise ns' p >>= pure . (+acc) . (* mask)) initialElevation nss'
        let elevation = ra' * (1 + noiseElevation)
        pure $ (p ^* elevation, elevation)

  let
      ns' = calculateSmoothNormals is ps'
      cs  = map (\(Vertex _ _ c) -> c) vs
      vs'' = zipWith3 Vertex ps' ns' cs

      minmax = MinMax (minimum elevations) (maximum elevations)
   in (,minmax) <$> createMeshWithIxs vs'' is



