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
import qualified Vulkan as Vk

import qualified FIR
import qualified FIR.Prim.Image
import SPIRV.Image

import Codec.Picture

import Data.Bits
import Foreign.Storable
import Ghengin.Vulkan.Renderer.Buffer
import Ghengin.Vulkan.Renderer.Context
import Ghengin.Vulkan.Renderer.Command
import Ghengin.Vulkan.Renderer.Image
import Ghengin.Vulkan.Renderer.Kernel
import Ghengin.Vulkan.Renderer.Sampler
import qualified Data.Linear.Alias as Alias

import Ghengin.Core.Type.Compatible.Pixel
import qualified Ghengin.Core.Shader.Data as Shader

type Texture2D :: ImageFormat Nat -> Type
data Texture2D (fmt :: ImageFormat Nat)
  = Texture2D { image   :: VulkanImage WithView
              , sampler :: Alias Sampler
              }

-- | Load a texture from a file directly and convert it to a texture using 'textureFromDynamicImage'.
texture :: FilePath -> Alias Sampler ⊸ Renderer (Alias (Texture2D (RGBA8 UNorm)))
texture fp sampler = enterD "Creating a texture" Linear.do
  liftSystemIOU (readImage fp) >>= \case
    Ur (Left e      ) -> Alias.forget sampler >> liftSystemIO (Prelude.fail e)
    Ur (Right dimage) -> textureFromDynamicImage dimage sampler

freeTexture :: Texture2D fmt ⊸ Renderer ()
freeTexture = undefined -- Unsafe.toLinear $ \(Texture2D img sampler) -> enterD "freeTexture" Linear.do
  -- withVkContext (\ctx -> ((),) <$> (destroyImage ctx img))
  -- Alias.forget sampler

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
newTexture img sampler' = Linear.do
   withStagingBuffer (img.imageData) $ \stagingBuffer _bufferSize -> enterD "textureFromImage" Linear.do

    (VulkanImage image devMem imgView) <- withVulkanContext $
      \vkContext ->
        createImage vkContext
          (Default2DImageInfo (juicyImageExtent img) (imagePixelFormat img)
              -- For the texture to be used in the shader, and to transfer data to it:
              (Vk.IMAGE_USAGE_TRANSFER_DST_BIT .|. Vk.IMAGE_USAGE_SAMPLED_BIT))
          (WithViewInfo Vk.IMAGE_VIEW_TYPE_2D Vk.IMAGE_ASPECT_COLOR_BIT)
          Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT
    (image1, imageA) <- Alias.share image
    (image2, imageB) <- Alias.share imageA
    (image3, image4) <- Alias.share imageB

    -- The image starts with an undefined layout:
    --
    -- (1) we change to layout to transfer optimal,
    -- (2) we transfer from the staging buffer to the image
    -- (3) we change the layout to shader read-only optimal

    -- The use of unsafe to linear caused a segfault here. the staging
    -- buffer was captured in one of the commands which were only used later in
    -- "immediate submit".
    --
    -- Gladly, we've updated the command recording API to use Aliases for resources whose
    -- freeing actions are collected and all run after the buffer is submitted!
    -- That's why the Cmd API no longer threads through any resources which are captured.

    stagingBufferA <- Alias.newAlias destroyBuffer stagingBuffer
    immediateSubmit $ Linear.do

      -- (1)
      transitionImageLayout image1 Vk.IMAGE_LAYOUT_UNDEFINED Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL

      -- (2)
      copyFullBufferToImage (juicyImageExtent img) stagingBufferA image2

      -- (3)
      transitionImageLayout image3 Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL

    Alias.newAlias freeTexture (Texture2D (VulkanImage image4 devMem imgView) sampler')


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

juicyImageExtent :: Codec.Picture.Image px -> Vk.Extent3D
juicyImageExtent img = Vk.Extent3D
  { width = Prelude.fromIntegral img.imageWidth
  , height = Prelude.fromIntegral img.imageHeight
  , depth = 1
  }

--------------------------------------------------------------------------------
-- * Shader Data
--------------------------------------------------------------------------------

instance Shader.ShaderData (Texture2D fmt) where
  type FirType (Texture2D fmt) =
        FIR.Image (FIR.Properties FIR.Prim.Image.FloatingPointCoordinates Float FIR.TwoD (Just FIR.NotDepthImage) FIR.NonArrayed FIR.SingleSampled FIR.Sampled (Just fmt))

