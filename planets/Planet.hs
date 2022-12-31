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
import GHC.Records
import System.Random
import Control.Monad.Trans
import qualified Data.List.NonEmpty as NE
import Data.String
import Data.List (foldl')
import GHC.Float
import Data.IORef
import Control.Monad
import Unsafe.Coerce

import qualified Foreign.Storable as S

import Ghengin hiding (get)
import Ghengin.Asset.Texture
import Ghengin.Utils
import Ghengin.Vulkan
import Ghengin.Vulkan.DescriptorSet
import Ghengin.Component (Entity, cmapM)
import qualified Ghengin.Component as C
import Ghengin.Component.Mesh.Sphere
import Ghengin.Component.Mesh
import Ghengin.Component.Material
import Ghengin.Component.UI
import Foreign.Ptr
import qualified Shader
import Ghengin.Shaders
import Ghengin.Render.Packet

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

makeMinMaxMaterial :: Vec3 -> MinMax -> Texture2D -> Material' '[Texture2D, Vec3, MinMax]
makeMinMaxMaterial v x t = Texture2DBinding t . StaticBinding v . StaticBinding x . Done

data PlanetSettings = PlanetSettings { resolution :: !(IORef Int)
                                     , radius     :: !(IORef Float)
                                     , color      :: !(IORef Vec3)
                                     , useFirstLayerAsMask :: !(IORef Bool)
                                     , noiseSettings :: !(NE.NonEmpty NoiseSettings)
                                     , displayFace   :: !(IOSelectRef DisplayFace)
                                     }

data DisplayFace = All | FaceUp | FaceRight deriving Show

instance UISettings PlanetSettings where

  type ReactivityInput PlanetSettings = (Entity, Texture2D)
  type ReactivityOutput PlanetSettings = ()
  type ReactivityConstraints PlanetSettings w = (HasField "renderPackets" w (C.Storage RenderPacket))

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

  makeComponents ps@(PlanetSettings re ra co bo nss df) (planetEntity, tex) = do

    withTree "Planet" do
      _b1 <- sliderInt "Resolution" re 2 200
      _b2 <- sliderFloat "Radius" ra 0 3

      whenM (colorPicker "Color" co) $ do
        -- TODO: When the color changes we update the mesh right away
        pure ()

      _b4 <- checkBox "Mask" bo
      _b5 <- withCombo "Faces" df [All, FaceUp, FaceRight]
      pure ()

    mapM (\(ns, i) ->
      withTree ("Layer " <> fromString (show i)) $ makeComponents ns ()) (NE.zip nss [1..])

    -- TODO: Button to generate that updates the mesh
    whenM (button "Generate") $ do
       (RenderPacket oldMesh mat pp _) <- C.get planetEntity
       (newMesh,newMinMax) <- newPlanet ps
       lift $ do
         freeMesh (oldMesh) -- Can we hide/enforce this somehow? Meshes aren't automatically freed when switched! We should make "switching" explicit?
       case Shader.shaderPipeline of
         (spp :: GShaderPipeline i)
              -- GIGANTIC:TODO: For some reason I have yet to better
              -- understand, the pipeline associated to the render packet
              -- can't be used to validate Compatibility with a new material again.
              -- It's somehow related to being an existential type and therefore the type not carrying enough information?
              -- How can I make the existential type carry enough information to pass Compatible again?
         --
         --
         -- The Solution might be defining a function that edits the content of dynamic
         -- bindings (by comparing Typeable instances?) because (and this is the key) if
         -- the pipeline was already created then it was already compatible, and
         -- therefore changing the value of the dynamic binding will not affect
         -- compatibility
         --
         -- Also: TODO: With the typeable constraint, we are able to inspect at runtime the material type (as if it were a simple tag) and depending on the value updating the material
          -> do
            (x,y,z) <- liftIO randomIO
            -- TODO: Free previous material?
            mx <- lift $ material (makeMinMaxMaterial (vec3 x y z) newMinMax tex) pp
            -- TODO: The great modify upgrade...
            C.set planetEntity (renderPacket @_ @i newMesh mx (unsafeCoerce pp))


    pure ()

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


whenM :: Monad m => m Bool -> m () -> m ()
whenM c t = (`when` t) =<< c

