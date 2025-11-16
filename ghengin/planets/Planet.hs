{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
module Planet where

import Apecs (Entity(..), cmapM)
import Ghengin.Core.Log
import Data.Unrestricted.Linear (liftUrT, runUrT, UrT(..))
import qualified Control.Monad as M
import Control.Functor.Linear as Linear
import Control.Monad.IO.Class.Linear as Linear
import qualified Data.Linear.Alias as Alias
import Data.IORef (IORef)
import Data.List (foldl')
import Data.String
import Data.Typeable
import Foreign.Ptr
import Foreign.Storable.Generic
import GHC.Float
import GHC.Generics
import GHC.Records
import GHC.TypeLits
import Ghengin hiding (get)
import Ghengin.Component.Mesh.Sphere
import Ghengin.Component.UI
import Ghengin.Core.Material
import Ghengin.Core.Mesh
import Ghengin.Core.Mesh.Vertex
import Ghengin.Core.Render.Packet
import Ghengin.Core.Render.Property
import Ghengin.Core.Type.Compatible
import Ghengin.Core.Type.Sized (Sized(..))
import Ghengin.DearImGui.Gradient
import Ghengin.Shader.FIR as FIR ((:->)(..), Struct, Syntactic(..))
import Ghengin.Vulkan.Renderer
import Ghengin.Vulkan.Renderer.Kernel
import Ghengin.Vulkan.Renderer.DescriptorSet
import Ghengin.Vulkan.Renderer.Sampler
import Ghengin.Vulkan.Renderer.Texture
import Prelude ((*), (+), (-), (/))
import Prelude.Linear hiding (All, IO, get, (*), (+), (-), (/))
import System.IO.Linear
import System.Random
import Unsafe.Coerce
import qualified Unsafe.Linear as Unsafe
import qualified Apecs as C
import qualified Data.List.NonEmpty as NE
import qualified Foreign.Storable as S
import qualified Ghengin.Shader.FIR as FIR
import qualified Prelude

import Noise
import Shader (CameraProperty)
import qualified Shader

-- type PlanetProps = '[MinMax,Texture2D]

data MinMax = MinMax !Float !Float
  deriving Show

instance ShaderData MinMax where
  type FirType MinMax = FIR.Val (Struct '[ "min" ':-> Float, "max" ':-> Float ])

planetPBS :: MinMax -> Alias Texture2D ⊸ PropertyBindings PlanetProps
planetPBS mm t = StaticBinding (Ur mm) :## Texture2DBinding t :## GHNil

-- data PlanetSettings = PlanetSettings { resolution :: !(IORef Int)
--                                      , radius     :: !(IORef Float)
--                                      , color      :: !(IORef Vec3)
--                                      , useFirstLayerAsMask :: !(IORef Bool)
--                                      , noiseSettings :: !(NE.NonEmpty NoiseSettings)
--                                      , displayFace   :: !(IOSelectRef DisplayFace)
--                                      , gradient :: ImGradient
--                                      }

-- textureFromGradient :: Dupable w => ImGradient -> Ghengin w (Alias Texture2D)
-- textureFromGradient grad = Linear.do
--   let img = generateImage (\x _y -> normVec3ToRGB8 $ colorAt grad (fromIntegral x/(50-1))) 50 1
--   sampler <- lift $ createSampler FILTER_NEAREST SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
--   lift $ textureFromImage (ImageRGB8 img) sampler

newPlanetMesh :: Ghengin w (Mesh '[Vec3, Vec3, Vec3], Ur MinMax)
newPlanetMesh = enterD "newPlanetMesh" $ lift $ Linear.do
  Ur !re' <- liftIO $ readIORef re
  Ur !ra' <- liftIO $ readIORef ra
  Ur !co' <- liftIO $ readIORef co
  Ur !df' <- liftIO $ readIOSelectRef df
  Ur !enableMask <- liftIO $ readIORef bo

  let UnitSphere vs is = newUnitSphere re' (Just co')

  Ur (ps', elevations) <- runUrT $ Prelude.unzip Prelude.<$> M.forM vs \(p :& _) -> liftUrT $
    case nss of
      ns NE.:| nss' -> Linear.do
        Ur !initialElevation <- runUrT $ evalNoise ns p
        let mask = if enableMask then initialElevation else 1
        Ur !noiseElevation <- runUrT $ M.foldM (\acc ns' -> (\x -> acc+x*mask) Prelude.<$> evalNoise ns' p) initialElevation nss'
        let elevation = ra' * (1 + noiseElevation)
        pure (p ^* elevation, elevation)

  let
      ns' = calculateSmoothNormals is ps'
      cs  = Prelude.map (\(_ :& _ :&: c) -> c) vs
      vs'' = Prelude.zipWith3 (\a b c -> a :& b :&: c) ps' ns' cs

      minmax = MinMax (Prelude.minimum elevations) (Prelude.maximum elevations)
   in (,Ur minmax) <$> createMeshWithIxs vs'' is

-- Utilities

whenM :: Monad m => m Bool -> m () -> m ()
whenM c t = Linear.do
  b <- c
  if b then t
       else pure ()

