{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LinearTypes #-}
module Ghengin.Vulkan.Renderer.Image where

import Data.Word
import qualified Prelude as Ur
import qualified Data.Vector as V
import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk

import Ghengin.Core.Prelude as Linear
import Ghengin.Vulkan.Renderer.Context
import FIR.Vulkan.Memory
import Data.Linear.Alias as Alias

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
  ImageView   :: Alias.Alias VulkanContextM Vk.ImageView %1 -> ImageView WithView

data ImageViewInfo ( ctx :: ImageViewContext ) where
  NoViewInfo   :: ImageViewInfo NoView
  WithViewInfo :: Vk.ImageViewType %1 -> Vk.ImageAspectFlags %1 -> ImageViewInfo WithView

--------------------------------------------------------------------------------
-- * Images
--------------------------------------------------------------------------------

data VulkanImage (viewCtx :: ImageViewContext) = VulkanImage
  { image     :: Alias.Alias VulkanContextM Vk.Image
  , devMem    :: Vk.DeviceMemory
  , imageView :: ImageView viewCtx
  }

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
  in Linear.do
    (Ur vk_img, devMem) <- liftSystemIO $ do
      image   <- Vk.createImage vkContext.device imgCreateInfo Nothing
      memReqs <- Vk.getImageMemoryRequirements vkContext.device image

      -- TODO: When we want to bind memory to an image, we needn't create a new
      -- memory object each time. It would be more optimal to create a small number of
      -- larger memory objects and bind parts of them by providing a proper offset
      -- value.
      devMem  <- allocateMemory vkContext.physicalDevice vkContext.device memReqs reqs Vk.zero
      Vk.bindImageMemory vkContext.device image devMem 0
      Ur.pure (Ur image, devMem)

    imageView <- case viewInfo of
      NoViewInfo -> pure NoImageView
      WithViewInfo viewType aspect -> Linear.do
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
              , Vk.image            = vk_img
              , Vk.viewType         = viewType
              , Vk.format           = imageFormat
              , Vk.components       = components
              , Vk.subresourceRange = subResourceRange
              }
        imageView <- liftSystemIO $ Vk.createImageView vkContext.device viewCreateInfo Nothing
        imageViewAlias <- Alias.newAlias destroy_vk_img_view imageView
        pure (ImageView imageViewAlias)
    image <- Alias.newAlias destroy_vk_img vk_img
    pure (VulkanImage{..}, vkContext)

destroyImage :: VulkanImage viewCtx ⊸ VulkanContextM ()
destroyImage = Unsafe.toLinear $ \VulkanImage{..} -> Linear.do
  Alias.forget image
  withVulkanContext $ Unsafe.toLinear \ctx -> liftSystemIO $ do
    Vk.freeMemory ctx.device devMem Nothing
    Ur.pure ((), ctx)
  case imageView of
    NoImageView -> pure ()
    ImageView vkImgView ->
      Alias.forget vkImgView

--------------------------------------------------------------------------------
-- * Internal
--------------------------------------------------------------------------------

destroy_vk_img :: HasVulkanContext m => Vk.Image ⊸ m ()
destroy_vk_img = Unsafe.toLinear $ \img -> withDevice $ Unsafe.toLinear \dev -> liftSystemIO $ do
  Vk.destroyImage dev img Nothing
  Ur.pure ((), dev)

destroy_vk_img_view :: HasVulkanContext m => Vk.ImageView ⊸ m ()
destroy_vk_img_view = Unsafe.toLinear $ \imgView -> withDevice $ Unsafe.toLinear \dev -> liftSystemIO $ do
  Vk.destroyImageView dev imgView Nothing
  Ur.pure ((), dev)
