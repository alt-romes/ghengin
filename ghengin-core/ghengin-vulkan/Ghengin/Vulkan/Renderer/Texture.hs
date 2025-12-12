{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
module Ghengin.Vulkan.Renderer.Texture
  (
    module Ghengin.Vulkan.Renderer.Texture

  -- * Generating textures
  , generateImage
  , DynamicImage(..)

  -- * Re-exports from FIR
  , ImageFormat(..), Component(..)
  , SNorm, UNorm, F, I, UI
  , pattern SNorm, pattern UNorm, pattern F, pattern I, pattern UI
  ) where

import GHC.TypeNats
import Data.Typeable
import Ghengin.Core.Log
import Ghengin.Core.Prelude as Linear
import qualified Prelude
import qualified Unsafe.Linear as Unsafe
import qualified Vulkan as Vk

import qualified FIR
import qualified FIR.Prim.Image
import SPIRV.Image

-- import System.Mem.Weak
import Codec.Picture

import Data.Bits
import Foreign.Storable
import Ghengin.Vulkan.Renderer.Buffer
import Ghengin.Vulkan.Renderer.Command
import Ghengin.Vulkan.Renderer.Image
import Ghengin.Vulkan.Renderer.Kernel
import Ghengin.Vulkan.Renderer.Sampler
import qualified Data.Linear.Alias as Alias

import Ghengin.Core.Type.Compatible.Pixel
import qualified Ghengin.Core.Shader.Data as Shader

-- TODO: More generally, we could have 1D and 3D textures too. There's also Images...
-- See FIR.Syntax.Synonyms
type Texture2D :: ImageFormat Nat -> Type
data Texture2D (fmt :: ImageFormat Nat)
  = Texture2D { image   :: VulkanImage
              , sampler :: Alias Sampler
              }

-- | Load a texture from a file directly and convert it to a texture using 'textureFromDynamicImage'.
texture :: FilePath -> Alias Sampler ⊸ Renderer (Alias (Texture2D (RGBA8 UNorm)))
texture fp sampler = enterD "Creating a texture" Linear.do
  liftSystemIOU (readImage fp) >>= \case
    Ur (Left e      ) -> Alias.forget sampler >> liftSystemIO (Prelude.fail e)
    Ur (Right dimage) -> textureFromDynamicImage dimage sampler

freeTexture :: Texture2D fmt ⊸ Renderer ()
freeTexture = Unsafe.toLinear $ \(Texture2D img sampler) -> enterD "freeTexture" Linear.do
  -- ROMES:tODO: fix Image.hs so that this definition doesn't need to be unsafe.
  withDevice (\dev -> ((),) <$> (destroyImage dev img))
  Alias.forget sampler


-- | Make a texture from a dynamic image by converting the image to RGBA8 first
textureFromDynamicImage :: DynamicImage
                        -> Alias Sampler
                         ⊸ Renderer (Alias (Texture2D (RGBA8 UNorm)))
textureFromDynamicImage dimage = newTexture (convertRGBA8 dimage)

-- | Make a new texture from an 'Image', provided the Image's pixel's are
-- compatible with the texture format.
newTexture :: (Pixel px, Typeable px, CompatiblePixel px fmt)
           => Codec.Picture.Image px
           -> Alias Sampler
            ⊸ Renderer (Alias (Texture2D fmt))
newTexture img sampler' =
   withStagingBuffer (img.imageData) $ \stagingBuffer _bufferSize -> enterD "textureFromImage" Linear.do

    (VulkanImage image devMem imgView)
        <- useVulkanDevice (\device ->
                  createImage device
                         (imagePixelFormat img)
                         (imageExtent img)
                         Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT -- Where to allocate the memory
                         (Vk.IMAGE_USAGE_TRANSFER_DST_BIT .|. Vk.IMAGE_USAGE_SAMPLED_BIT) -- For the texture to be used in the shader, and to transfer data to it
                         Vk.IMAGE_ASPECT_COLOR_BIT)

    -- The image starts with an undefined layout:
    --
    -- (1) we change to layout to transfer optimal,
    -- (2) we transfer from the staging buffer to the image
    -- (3) we change the layout to shader read-only optimal

    -- TODO: the use of unsafe to linear caused a segfault here. the staging
    -- buffer was captured in one of the commands which were only used later in
    -- "immediate submit".

    (stagingBuffer, image) <- immediateSubmit $ Linear.do

      -- (1) 
      image <- transitionImageLayout image Vk.IMAGE_LAYOUT_UNDEFINED Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL

      -- (2)
      (stagingBuffer, image) <- copyFullBufferToImage stagingBuffer image (imageExtent img)

      -- (3)
      (image) <- transitionImageLayout image Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL

      pure (stagingBuffer, image)

    destroyBuffer stagingBuffer

    Alias.newAlias freeTexture (Texture2D (VulkanImage image devMem imgView) sampler')


-- Not needed :(
dynamicSize :: DynamicImage -> Int
dynamicSize = \case
  ImageY8 img     -> img.imageWidth * img.imageHeight * sizeOf @(PixelBaseComponent Pixel8     ) undefined
  ImageY16 img    -> img.imageWidth * img.imageHeight * sizeOf @(PixelBaseComponent Pixel16    ) undefined
  ImageY32 img    -> img.imageWidth * img.imageHeight * sizeOf @(PixelBaseComponent Pixel32    ) undefined
  ImageYF img     -> img.imageWidth * img.imageHeight * sizeOf @(PixelBaseComponent PixelF     ) undefined
  ImageYA8 img    -> img.imageWidth * img.imageHeight * sizeOf @(PixelBaseComponent PixelYA8   ) undefined
  ImageYA16 img   -> img.imageWidth * img.imageHeight * sizeOf @(PixelBaseComponent PixelYA16  ) undefined
  ImageRGB8 img   -> img.imageWidth * img.imageHeight * sizeOf @(PixelBaseComponent PixelRGB8  ) undefined
  ImageRGB16 img  -> img.imageWidth * img.imageHeight * sizeOf @(PixelBaseComponent PixelRGB16 ) undefined
  ImageRGBF img   -> img.imageWidth * img.imageHeight * sizeOf @(PixelBaseComponent PixelRGBF  ) undefined
  ImageRGBA8 img  -> img.imageWidth * img.imageHeight * sizeOf @(PixelBaseComponent PixelRGBA8 ) undefined
  ImageRGBA16 img -> img.imageWidth * img.imageHeight * sizeOf @(PixelBaseComponent PixelRGBA16) undefined
  ImageYCbCr8 img -> img.imageWidth * img.imageHeight * sizeOf @(PixelBaseComponent PixelYCbCr8) undefined
  ImageCMYK8 img  -> img.imageWidth * img.imageHeight * sizeOf @(PixelBaseComponent PixelCMYK8 ) undefined
  ImageCMYK16 img -> img.imageWidth * img.imageHeight * sizeOf @(PixelBaseComponent PixelCMYK16) undefined


-- Vs. UNORM vs SRGB, which one do I want why?
-- TODO: CompatiblePixel must be consistent with this.
-- TODO: The easiest would be for the Vk.Format to be a function of `fmt`, not `px`.
-- Then, it would always match what the shader expected (UNorm vs SNorm) for RGBA8
imagePixelFormat :: forall px. (Pixel px, Typeable px) => Codec.Picture.Image px -> Vk.Format
imagePixelFormat _
  | Just Refl <- eqT @px @Pixel8      = Vk.FORMAT_R8_UNORM
  | Just Refl <- eqT @px @Pixel16     = Vk.FORMAT_R16_UNORM
  | Just Refl <- eqT @px @Pixel32     = Vk.FORMAT_R32_UINT
  | Just Refl <- eqT @px @PixelF      = Vk.FORMAT_R32_SFLOAT
  | Just Refl <- eqT @px @PixelYA8    = Vk.FORMAT_R8G8_UNORM  -- Y as R, A as G
  | Just Refl <- eqT @px @PixelYA16   = Vk.FORMAT_R16G16_UNORM
  | Just Refl <- eqT @px @PixelRGB8   = Vk.FORMAT_R8G8B8_UNORM
  | Just Refl <- eqT @px @PixelRGB16  = Vk.FORMAT_R16G16B16_UNORM
  | Just Refl <- eqT @px @PixelRGBF   = Vk.FORMAT_R32G32B32_SFLOAT
  | Just Refl <- eqT @px @PixelRGBA8  = Vk.FORMAT_R8G8B8A8_UNORM
  | Just Refl <- eqT @px @PixelRGBA16 = Vk.FORMAT_R16G16B16A16_UNORM
  | Just Refl <- eqT @px @PixelYCbCr8 = undefined
  | Just Refl <- eqT @px @PixelCMYK8  = undefined
  | Just Refl <- eqT @px @PixelCMYK16 = undefined
  | otherwise = error "impossible"

imageExtent :: Codec.Picture.Image px -> Vk.Extent3D
imageExtent img = Vk.Extent3D { width = Prelude.fromIntegral img.imageWidth
                               , height = Prelude.fromIntegral img.imageHeight
                               , depth = 1
                               }

--------------------------------------------------------------------------------
-- * Shader Data
--------------------------------------------------------------------------------

instance Shader.ShaderData (Texture2D fmt) where
  type FirType (Texture2D fmt) =
        FIR.Image (FIR.Properties FIR.Prim.Image.FloatingPointCoordinates Float FIR.TwoD (Just FIR.NotDepthImage) FIR.NonArrayed FIR.SingleSampled FIR.Sampled (Just fmt))

