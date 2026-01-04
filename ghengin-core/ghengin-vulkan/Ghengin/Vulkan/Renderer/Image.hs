{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LinearTypes #-}
module Ghengin.Vulkan.Renderer.Image where

import Data.Word
import GHC.Generics
import qualified Prelude as Ur
import qualified Data.Vector as V
import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk

import Ghengin.Core.Prelude as Linear
import Ghengin.Vulkan.Renderer.Context
import FIR.Vulkan.Memory

import qualified Unsafe.Linear as Unsafe

--------------------------------------------------------------------------------
-- ** Image Info
--------------------------------------------------------------------------------
-- (from fir-examples)
data ImageInfo
  = ImageInfo
  { imageType        :: Vk.ImageType
  , imageExtent      :: Vk.Extent3D
  , imageFormat      :: Vk.Format
  , imageLayout      :: Vk.ImageLayout
  , imageMipLevels   :: Word32
  , imageArrayLayers :: Word32
  , imageSamples     :: Vk.SampleCountFlagBits
  , imageTiling      :: Vk.ImageTiling
  , imageUsage       :: Vk.ImageUsageFlags
  }

pattern Default2DImageInfo :: Vk.Extent3D -> Vk.Format -> Vk.ImageUsageFlags -> ImageInfo
pattern Default2DImageInfo extent3D fmt usage
  = ImageInfo
  { imageType        = Vk.IMAGE_TYPE_2D
  , imageExtent      = extent3D
  , imageFormat      = fmt
  , imageLayout      = Vk.IMAGE_LAYOUT_UNDEFINED
  , imageMipLevels   = 1
  , imageArrayLayers = 1
  , imageSamples     = Vk.SAMPLE_COUNT_1_BIT
  , imageTiling      = Vk.IMAGE_TILING_OPTIMAL
  , imageUsage       = usage
  }

--------------------------------------------------------------------------------
-- ** Image View Info/Context
--------------------------------------------------------------------------------

data ImageViewContext
  = NoView
  | WithView 

data ImageView ( ctx :: ImageViewContext ) where
  NoImageView :: ImageView NoView
  ImageView   :: Vk.ImageView %1 -> ImageView WithView

data ImageViewInfo ( ctx :: ImageViewContext ) where
  NoViewInfo   :: ImageViewInfo NoView
  WithViewInfo :: Vk.ImageViewType %1 -> Vk.ImageAspectFlags %1 -> ImageViewInfo WithView

--------------------------------------------------------------------------------
-- * Images
--------------------------------------------------------------------------------

data VulkanImage (viewCtx :: ImageViewContext) = VulkanImage
  { image     :: Vk.Image
  , devMem    :: Vk.DeviceMemory
  , imageView :: ImageView viewCtx
  } deriving Generic

createImage
  :: Linear.MonadIO m
  => VulkanContext rCtx %1
  -> ImageInfo
  -> ImageViewInfo viewCtx
  -> Vk.MemoryPropertyFlags
  -> m (VulkanImage viewCtx, VulkanContext rCtx)
createImage = Unsafe.toLinear \vkContext ImageInfo{ .. } viewInfo reqs ->
  let imgCreateInfo :: Vk.ImageCreateInfo '[]
      imgCreateInfo =
        Vk.ImageCreateInfo
          { next               = ()
          , flags              = Vk.zero
          , imageType          = imageType
          , format             = imageFormat
          , extent             = imageExtent
          , mipLevels          = imageMipLevels
          , arrayLayers        = imageArrayLayers
          , samples            = imageSamples
          , tiling             = imageTiling
          , usage              = imageUsage
          , sharingMode        = Vk.SHARING_MODE_EXCLUSIVE
          , queueFamilyIndices = V.empty
          , initialLayout      = imageLayout
          }
  in liftSystemIO $ do
    image   <- Vk.createImage vkContext.device imgCreateInfo Nothing
    memReqs <- Vk.getImageMemoryRequirements vkContext.device image

    -- TODO: When we want to bind memory to an image, we needn't create a new
    -- memory object each time. It would be more optimal to create a small number of
    -- larger memory objects and bind parts of them by providing a proper offset
    -- value.
    ( devMem, physicalDevice, device ) <- Linear.withLinearIO $ fmap (Unsafe.toLinear Ur) $
      allocateMemory vkContext.physicalDevice vkContext.device memReqs reqs Vk.zero
    Vk.bindImageMemory vkContext.device image devMem 0

    vkImage <- case viewInfo of
      NoViewInfo ->
        Ur.pure VulkanImage{image, devMem, imageView = NoImageView}
      WithViewInfo viewType aspect -> do
        let
          components :: Vk.ComponentMapping
          components =
            Vk.ComponentMapping
              { Vk.r = Vk.COMPONENT_SWIZZLE_IDENTITY
              , Vk.g = Vk.COMPONENT_SWIZZLE_IDENTITY
              , Vk.b = Vk.COMPONENT_SWIZZLE_IDENTITY
              , Vk.a = Vk.COMPONENT_SWIZZLE_IDENTITY
              }

          subResourceRange :: Vk.ImageSubresourceRange
          subResourceRange =
            Vk.ImageSubresourceRange
              { Vk.aspectMask     = aspect
              , Vk.baseMipLevel   = 0
              , Vk.levelCount     = 1
              , Vk.baseArrayLayer = 0
              , Vk.layerCount     = 1
              }

          viewCreateInfo :: Vk.ImageViewCreateInfo '[]
          viewCreateInfo =
            Vk.ImageViewCreateInfo
              { Vk.next             = ()
              , Vk.flags            = Vk.zero
              , Vk.image            = image
              , Vk.viewType         = viewType
              , Vk.format           = imageFormat
              , Vk.components       = components
              , Vk.subresourceRange = subResourceRange
              }
        imageView <- Vk.createImageView vkContext.device viewCreateInfo Nothing
        Ur.pure VulkanImage{image, devMem, imageView = ImageView imageView}
    Ur.pure (vkImage, vkContext { physicalDevice, device })

destroyImage :: Linear.MonadIO m => Vk.Device ⊸ VulkanImage viewCtx ⊸ m Vk.Device
destroyImage = Unsafe.toLinear2 $ \device VulkanImage{..} -> liftSystemIO $ do
  Vk.destroyImage device image Nothing
  Vk.freeMemory device devMem Nothing
  case imageView of
    NoImageView -> Ur.pure ()
    ImageView vkImgView ->
      Vk.destroyImageView device vkImgView Nothing
  Ur.pure device
