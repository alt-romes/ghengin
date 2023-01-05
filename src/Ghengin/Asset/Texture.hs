{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
module Ghengin.Asset.Texture
  (
    module Ghengin.Asset.Texture

  -- * Generating textures
  , generateImage
  , DynamicImage(..)
  ) where

import Data.IORef
import System.Mem.Weak
import Geomancy.Vec3
import Control.Logger.Simple
import Control.Monad.Reader
import Codec.Picture

import qualified Vulkan as Vk
import Ghengin.Utils
import Ghengin.Vulkan
import Ghengin.Vulkan.Sampler
import Ghengin.Vulkan.Command
import Ghengin.Vulkan.Image
import Ghengin.Vulkan.Buffer

data Texture2D = Texture2D VulkanImage Vk.Sampler (IORef ()) -- ^ Dummy IO Ref for garbage collection

-- TODO: This isntance sholuldn't exist. just temporary... if you find this here later try to remove it. it's currenty being used to instance hashable to create the render key...
instance Eq Texture2D where
  (==) _ _ = False

texture :: FilePath -> Vk.Sampler -> Renderer χ Texture2D
texture fp sampler = do
  liftIO (readImage fp) >>= \case
    Left e -> liftIO (fail e)
    Right dimage ->
      textureFromImage dimage sampler

freeTexture :: Vk.Device -> Texture2D -> IO ()
freeTexture dev (Texture2D img sampler _) = do
  logInfo "Freeing texture..."
  destroyImage dev img
  destroySampler dev sampler

textureFromImage :: DynamicImage
                 -> Vk.Sampler
                 -> Renderer χ Texture2D
-- (For now) we convert the image to RGBA8 at all costs, which is a bit of a hack
textureFromImage (ImageRGBA8 . convertRGBA8 -> dimage) sampler =
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

   in wsb $ \stagingBuffer _bufferSize -> do

    device <- asks (._vulkanDevice)
    image <- liftIO $ createImage device
                         (dynamicFormat dimage)
                         (dynamicExtent dimage)
                         Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT -- Where to allocate the memory
                         (Vk.IMAGE_USAGE_TRANSFER_DST_BIT .|. Vk.IMAGE_USAGE_SAMPLED_BIT) -- For the texture to be used in the shader, and to transfer data to it
                         Vk.IMAGE_ASPECT_COLOR_BIT

    -- The image starts with an undefined layout:
    --
    -- (1) we change to layout to transfer optimal,
    -- (2) we transfer from the staging buffer to the image
    -- (3) we change the layout to shader read-only optimal
    immediateSubmit $ do

      -- TODO: Make this the default setting in those functions, and move it there.

      -- (1) 
      transitionImageLayout image._image (dynamicFormat dimage) Vk.IMAGE_LAYOUT_UNDEFINED Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL

      -- (2)
      copyFullBufferToImage stagingBuffer image._image (dynamicExtent dimage)

      -- (3)
      transitionImageLayout image._image (dynamicFormat dimage) Vk.IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL

    dev <- getDevice

    -- Use haskell's garbage collector to free the texture
    tracker <- liftIO $ newIORef ()
    _ <- liftIO $ mkWeakIORef tracker (putStrLn "Fired!" >> destroyImage dev image >> destroySampler dev sampler)

    let tex2d = Texture2D image sampler tracker

    -- liftIO $ addFinalizer tex2d (freeTexture device tex2d)

    -- destroyImage dev img
    -- destroySampler dev sampler

    pure tex2d


-- | Convert a vec3 with values between 0-1 and convert it into a pixelrgb8
-- with values between 0-255 
normVec3ToRGB8 :: Vec3 -> PixelRGB8
normVec3ToRGB8 (WithVec3 x y z) = PixelRGB8 (round $ x * 255) (round $ y * 255) (round $ z * 255)

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
dynamicExtent dimg = Vk.Extent3D { width = dynamicMap (fromIntegral . (.imageWidth)) dimg
                                 , height = dynamicMap (fromIntegral . (.imageHeight)) dimg
                                 , depth = 1
                                 }

