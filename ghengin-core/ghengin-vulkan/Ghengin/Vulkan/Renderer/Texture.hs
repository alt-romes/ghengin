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
  ) where

import Ghengin.Core.Log
import Ghengin.Core.Prelude as Linear
import qualified Prelude
import qualified Unsafe.Linear as Unsafe
import qualified Vulkan as Vk

import qualified FIR
import qualified FIR.Prim.Image as FIR
import qualified SPIRV.Image as SPIRV
import qualified SPIRV.ScalarTy

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

data Texture2D = Texture2D { image          :: VulkanImage
                           , sampler        :: Alias Sampler
                           } deriving Generic

instance Aliasable Texture2D where
  countedFields (Texture2D _ s) = [SomeAlias s]

-- TODO: This isntance sholuldn't exist. just temporary... if you find this here later try to remove it. it's currenty being used to instance hashable to create the render key...
instance Prelude.Eq Texture2D where
  (==) _ _ = False

texture :: FilePath -> Alias Sampler ⊸ Renderer (Alias Texture2D)
texture fp sampler = enterD "Creating a texture" Linear.do
  liftSystemIOU (readImage fp) >>= \case
    Ur (Left e      ) -> Alias.forget sampler >> liftSystemIO (Prelude.fail e)
    Ur (Right dimage) -> textureFromImage dimage sampler

freeTexture :: Texture2D ⊸ Renderer ()
freeTexture = Unsafe.toLinear $ \(Texture2D img sampler) -> enterD "freeTexture" Linear.do
  -- ROMES:tODO: fix Image.hs so that this definition doesn't need to be unsafe.
  useDevice (\dev -> ((),) <$> (destroyImage dev img))
  Alias.forget sampler

textureFromImage :: DynamicImage
                 -> Alias Sampler
                  ⊸ Renderer (Alias Texture2D)
-- (For now) we convert the image to RGBA8 at all costs, which is a bit of a hack
textureFromImage (ImageRGBA8 Prelude.. convertRGBA8 -> dimage) = \sampler' ->
  let wsb = case dimage of
              ImageY8     img -> withStagingBuffer (img.imageData)
              ImageY16    img -> withStagingBuffer (img.imageData)
              ImageY32    img -> withStagingBuffer (img.imageData)
              ImageYF     img -> withStagingBuffer (img.imageData)
              ImageYA8    img -> withStagingBuffer (img.imageData)
              ImageYA16   img -> withStagingBuffer (img.imageData)
              ImageRGB8   img -> withStagingBuffer (img.imageData)
              ImageRGB16  img -> withStagingBuffer (img.imageData)
              ImageRGBF   img -> withStagingBuffer (img.imageData)
              ImageRGBA8  img -> withStagingBuffer (img.imageData)
              ImageRGBA16 img -> withStagingBuffer (img.imageData)
              ImageYCbCr8 img -> withStagingBuffer (img.imageData)
              ImageCMYK8  img -> withStagingBuffer (img.imageData)
              ImageCMYK16 img -> withStagingBuffer (img.imageData)

   in wsb $ \stagingBuffer _bufferSize -> Linear.do

    (VulkanImage image devMem imgView)
        <- useVulkanDevice (\device ->
                  createImage device
                         (dynamicFormat dimage)
                         (dynamicExtent dimage)
                         Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT -- Where to allocate the memory
                         (Vk.IMAGE_USAGE_TRANSFER_DST_BIT .|. Vk.IMAGE_USAGE_SAMPLED_BIT) -- For the texture to be used in the shader, and to transfer data to it
                         Vk.IMAGE_ASPECT_COLOR_BIT)

    -- The image starts with an undefined layout:
    --
    -- (1) we change to layout to transfer optimal,
    -- (2) we transfer from the staging buffer to the image
    -- (3) we change the layout to shader read-only optimal

    -- TODO: Make this the default setting in those functions, and move it there.

    -- (1) 
    (cmd1, image) <- pure $ transitionImageLayout image (dynamicFormat dimage) Vk.IMAGE_LAYOUT_UNDEFINED Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL

    -- (2)
    (cmd2, stagingBuffer, image) <- pure $ copyFullBufferToImage stagingBuffer image (dynamicExtent dimage)

    destroyBuffer stagingBuffer
    
    -- (3)
    (cmd3, image) <- pure $ transitionImageLayout image (dynamicFormat dimage) Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL

    -- TODO: Re-imagine Commands, and probably make it so that we can use the monad completely, not just over ()
    -- then again it's also good to think about the submission as something executing on GPU not interleaved with CPU code?
    -- Ach, think about this all, but not now.
    -- Write thoughts on Command module.
    immediateSubmit $ cmd1 >> cmd2 >> cmd3

    Alias.newAlias freeTexture (Texture2D (VulkanImage image devMem imgView) sampler')


-- | Convert a vec3 with values between 0-1 and convert it into a pixelrgb8
-- with values between 0-255 
normVec3ToRGB8 :: (Float,Float,Float) -> PixelRGB8
--   Geomancy: (WithVec3 x y z)
normVec3ToRGB8 (x, y, z) = PixelRGB8 (round $ x Prelude.* 255) (round $ y Prelude.* 255) (round $ z Prelude.* 255)

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
dynamicFormat :: DynamicImage -> Vk.Format
dynamicFormat = \case
  ImageY8     _ -> undefined 
  ImageY16    _ -> undefined
  ImageY32    _ -> undefined
  ImageYF     _ -> undefined
  ImageYA8    _ -> undefined
  ImageYA16   _ -> undefined
  ImageRGB8   _ -> Vk.FORMAT_R8G8B8_SRGB
  ImageRGB16  _ -> undefined
  ImageRGBF   _ -> undefined
  ImageRGBA8  _ -> Vk.FORMAT_R8G8B8A8_SRGB
  ImageRGBA16 _ -> Vk.FORMAT_R8G8B8A8_SRGB
  ImageYCbCr8 _ -> undefined
  ImageCMYK8  _ -> undefined
  ImageCMYK16 _ -> undefined

dynamicExtent :: DynamicImage -> Vk.Extent3D
dynamicExtent dimg = Vk.Extent3D { width = dynamicMap (Prelude.fromIntegral Prelude.. (.imageWidth)) dimg
                                 , height = dynamicMap (Prelude.fromIntegral Prelude.. (.imageHeight)) dimg
                                 , depth = 1
                                 }

-- TODO
instance FIR.Syntactic Texture2D where
  type Internal Texture2D = FIR.Val ((FIR.Image ('FIR.Properties 'FIR.FloatingPointCoordinates Float 'SPIRV.TwoD ('Just 'SPIRV.NotDepthImage) 'SPIRV.NonArrayed 'SPIRV.SingleSampled 'SPIRV.Sampled ('Just ('SPIRV.ImageFormat ('SPIRV.Integer 'SPIRV.Normalised 'SPIRV.ScalarTy.Unsigned) '[8,8,8,8])))))
  toAST = FIR.undefined
  fromAST = FIR.undefined

