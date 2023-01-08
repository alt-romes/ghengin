{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
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

import Data.Typeable
import Ghengin.DearImGui.Gradient
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
import Ghengin.Vulkan.Sampler

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

type Planet = RenderPacket
type PlanetProps = '[Texture2D,MinMax]

data MinMax = MinMax Float Float
  deriving (Eq, Generic, Show)

instance Hashable MinMax
instance GStorable MinMax
instance Sized MinMax where
  type SizeOf MinMax = 2 * SizeOf Float

planetMaterial :: MinMax -> Texture2D -> Material' PlanetProps
planetMaterial mm t = Texture2DBinding t . StaticBinding mm . Done

data PlanetSettings = PlanetSettings { resolution :: !(IORef Int)
                                     , radius     :: !(IORef Float)
                                     , color      :: !(IORef Vec3)
                                     , useFirstLayerAsMask :: !(IORef Bool)
                                     , noiseSettings :: !(NE.NonEmpty NoiseSettings)
                                     , displayFace   :: !(IOSelectRef DisplayFace)
                                     -- , gradient :: (ImGradient, IORef ImGradientMark, IORef ImGradientMark)
                                     , gradient :: ImGradient
                                     }

data DisplayFace = All | FaceUp | FaceRight deriving Show

instance UISettings PlanetSettings where

  type ReactivityInput PlanetSettings = Entity
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
    grad <- newGradient (vec3 0 0 0) (vec3 1 1 1)
    -- m1   <- newIORef (ImGradientMark 0 0 0 1 0)
    -- m2   <- newIORef (ImGradientMark 0 0 0 1 0)
    pure $ PlanetSettings resR radR colorR boolR [ns1, ns2] df grad

  makeComponents ps@(PlanetSettings re ra co bo nss df grad) planetEntity = do

    (RenderPacket oldMesh (mat :: Material mt) pp _) <- C.get planetEntity

    -- Local equality for this render packets being a PlanetMaterial by
    -- comparing the runtime tag of this render packet.
    --
    -- > Therefore, GADT matches simply act as a rigid “type information diode”:
    --   the stuff inside can never be used to determine types on the outside,
    --   like that the case block as a whole should be IO ().
    --
    -- TODO: Some nice workaround to avoid this?
    () <- case eqT @mt @PlanetProps of
      Nothing -> error "Not a planet material BOOM"
      Just Refl -> do

        withTree "Planet" do
          _b1 <- sliderInt "Resolution" re 2 200
          _b2 <- sliderFloat "Radius" ra 0 3

          whenM (gradientEditor grad) $ do
            let oldTex = case mat of Texture2DBinding oldTex' _ -> oldTex'

            -- Because we introduced the equality constraint we can now edit
            -- the material AND we still keep the 'Compatible' instance because
            -- we simply introduced an equality to edit the material, and
            -- didn't remove the original compatibility between the existential
            -- material and pipeline
            newTex <- textureFromGradient grad
            newMat <- lift $ medit @1 @PlanetProps mat $ \_ -> newTex
            C.set planetEntity =<< renderPacket oldMesh newMat pp

            -- We can free the previous texture here because we only ever
            -- allocate one per planet, and because after we call medit the
            -- material has been written with the new texture and the previous
            -- one is no longer being used
            -- Be careful because textures are shared...
            device <- lift $ getDevice
            liftIO $ freeTexture device oldTex

            pure ()

          _b4 <- checkBox "Mask" bo
          _b5 <- withCombo "Faces" df [All, FaceUp, FaceRight]
          pure ()

        mapM (\(ns, i) ->
          withTree ("Layer " <> fromString (show i)) $ makeComponents ns ()) (NE.zip nss [1..])

        whenM (button "Generate") $ do

           (newMesh,newMinMax) <- newPlanetMesh ps

           lift $ freeMesh oldMesh -- Can we hide/enforce this somehow?
                                   -- Meshes aren't automatically freed when switched! We should make "switching" explicit?
                                   -- ^ Perhaps pattern matching on a mesh pattern synonym of the mesh would free it

           -- Edit multiple material properties at the same time
           -- newMaterial <- lift $ medits @[0,1] @PlanetProps mat $ (\_oldMinMax -> newMinMax) :-# pure :+# HFNil
           newMaterial <- lift $ medit @0 @PlanetProps mat (\_oldMinMax -> newMinMax)

           C.set planetEntity =<< renderPacket newMesh newMaterial pp

    pure ()

textureFromGradient :: ImGradient -> Ghengin w Texture2D
textureFromGradient grad = do
  let img = generateImage (\x _y -> normVec3ToRGB8 $ colorAt grad (fromIntegral x/(50-1))) 50 1
  sampler <- lift $ createSampler FILTER_NEAREST SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
  lift $ textureFromImage (ImageRGB8 img) sampler


newPlanet :: ∀ a w. Compatible PlanetProps a => PlanetSettings -> RenderPipeline a -> Ghengin w Planet
newPlanet ps@(PlanetSettings re ra co bo nss df grad) pipeline = do
  (mesh,minmax) <- newPlanetMesh ps
  tex <- textureFromGradient grad
  mat <- lift $ material (planetMaterial minmax tex) pipeline
  pure $ renderPacket @PlanetProps @a mesh mat pipeline

newPlanetMesh :: PlanetSettings -> Ghengin w (Mesh, MinMax)
newPlanetMesh (PlanetSettings re ra co bo nss df grad) = lift $ do
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

