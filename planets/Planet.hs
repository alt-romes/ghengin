{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
module Planet where

import GHC.TypeLits
import GHC.Generics
import Control.Monad.Trans
import qualified Data.List.NonEmpty as NE
import Data.String
import Data.List (foldl')
import GHC.Float
import Data.IORef
import Control.Monad

import qualified Foreign.Storable as S

import Ghengin hiding (get)
import Ghengin.Utils
import Ghengin.Vulkan
import Ghengin.Vulkan.DescriptorSet
import Ghengin.Component.Mesh.Sphere
import Ghengin.Component.Mesh
import Ghengin.Component.Material
import Ghengin.Component.UI
import Foreign.Ptr

import Noise

data MinMax = MinMax Float Float
  deriving (Eq, Generic, Show)

instance Hashable MinMax
instance GStorable MinMax
-- instance S.Storable MinMax where
--   sizeOf _ = 2*S.sizeOf @Float undefined
--   alignment _ = 2*S.sizeOf @Float undefined
--   peek (castPtr -> p) = do
--     mi <- S.peekElemOff p 0
--     ma <- S.peekElemOff p 1
--     pure $ MinMax mi ma
--   poke (castPtr -> p) (MinMax mi ma) = S.pokeElemOff p 0 mi >> S.pokeElemOff p 1 ma

instance Sized MinMax where
  type SizeOf MinMax = 2 * SizeOf Float

-- instance Poke MinMax α where
--   type SizeOf α MinMax = 8
--   type Alignment α MinMax = 8
--   poke = S.poke

-- Why can't I imprt material
-- type Material' α = DescriptorSet -> Material α

makeMinMaxMaterial :: Vec3 -> MinMax -> Material' '[Vec3, MinMax]
makeMinMaxMaterial v x = DynamicBinding v . DynamicBinding x . Done

type MinMaxMaterial = Material '[ Vec3, MinMax ]

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
    _ns3    <- makeSettings @NoiseSettings
    df     <- newIOSelectRef All
    pure $ PlanetSettings resR radR colorR boolR [ns1, ns2] df

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

newPlanet :: PlanetSettings -> Ghengin w (Mesh, MinMax)
newPlanet (PlanetSettings re ra co bo nss df) = lift $ do
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



