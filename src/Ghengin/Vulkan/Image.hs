{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
module Ghengin.Vulkan.Image where

import Vulkan.Zero (zero)
import qualified Vulkan as Vk

import Ghengin.Vulkan.Device

data VulkanImage = VulkanImage { _image :: Vk.Image
                               , _devMem :: Vk.DeviceMemory
                               , _imageView :: Vk.ImageView
                               }

createImage :: VulkanDevice -> Vk.Format -> Vk.Extent3D -> Vk.MemoryPropertyFlags -> Vk.ImageUsageFlagBits -> Vk.ImageAspectFlags -> IO VulkanImage
createImage device format extent properties usage aspect = do
  let
      imageInfo = Vk.ImageCreateInfo { imageType = Vk.IMAGE_TYPE_2D
                                     , extent    = extent
                                     , mipLevels = 1
                                     , arrayLayers = 1
                                     , format      = format
                                     , tiling      = Vk.IMAGE_TILING_OPTIMAL
                                     , initialLayout = Vk.IMAGE_LAYOUT_UNDEFINED
                                     , usage         = usage
                                     , samples       = Vk.SAMPLE_COUNT_1_BIT
                                     , sharingMode   = Vk.SHARING_MODE_EXCLUSIVE
                                     , queueFamilyIndices = []
                                     , flags = zero
                                     , next = ()
                                     }
  img  <- Vk.createImage device._device imageInfo Nothing

  memReq       <- Vk.getImageMemoryRequirements device._device img
  memTypeIndex <- findMemoryType memReq.memoryTypeBits properties device._physicalDevice
  let
      memAllocInfo = Vk.MemoryAllocateInfo { next = ()
                                           , allocationSize  = memReq.size
                                           , memoryTypeIndex = memTypeIndex
                                           }

  -- TODO: Of course, when we want to bind a memory to an image, we don’t need
  -- to create a new memory object each time. It is more optimal to create a
  -- small number of larger memory objects and bind parts of them by providing
  -- a proper offset value.
  imgMem <- Vk.allocateMemory device._device memAllocInfo Nothing

  Vk.bindImageMemory device._device img imgMem 0

  imgView <- createImageView device._device format aspect img

  pure $ VulkanImage img imgMem imgView

createImageView :: Vk.Device -> Vk.Format -> Vk.ImageAspectFlags -> Vk.Image -> IO Vk.ImageView
createImageView dev format aspect img = do
  Vk.createImageView dev config Nothing
    where
      config =
        Vk.ImageViewCreateInfo
          { next = ()
          , flags = Vk.ImageViewCreateFlagBits 0
          , image = img
          , viewType = Vk.IMAGE_VIEW_TYPE_2D
          , format = format
            -- The next parameter is cool: could make hard color changes
          , components = Vk.ComponentMapping { r = Vk.COMPONENT_SWIZZLE_IDENTITY
                                             , g = Vk.COMPONENT_SWIZZLE_IDENTITY
                                             , b = Vk.COMPONENT_SWIZZLE_IDENTITY
                                             , a = Vk.COMPONENT_SWIZZLE_IDENTITY
                                             }
          , subresourceRange = Vk.ImageSubresourceRange { aspectMask     = aspect
                                                        , baseMipLevel   = 0
                                                        , levelCount     = 1
                                                        , baseArrayLayer = 0
                                                        , layerCount     = 1
                                                        }
          }

destroyImageView :: Vk.Device -> Vk.ImageView -> IO ()
destroyImageView d i = Vk.destroyImageView d i Nothing

-- TODO: Destroy Image isn't being called for the earlier images we were creating!?!
destroyImage :: Vk.Device -> VulkanImage -> IO ()
destroyImage d (VulkanImage im mem view) = do
  Vk.destroyImage d im Nothing
  Vk.freeMemory d mem Nothing
  Vk.destroyImageView d view Nothing

