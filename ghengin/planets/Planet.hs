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

-- textureFromGradient :: Dupable w => ImGradient -> Ghengin w (Alias Texture2D)
-- textureFromGradient grad = Linear.do
--   let img = generateImage (\x _y -> normVec3ToRGB8 $ colorAt grad (fromIntegral x/(50-1))) 50 1
--   sampler <- lift $ createSampler FILTER_NEAREST SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
--   lift $ textureFromImage (ImageRGB8 img) sampler

