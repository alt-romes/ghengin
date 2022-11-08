{-# LANGUAGE RecordWildCards #-}
module Ghengin.VulkanEngine.ImageView where

import qualified Vulkan as Vk

createImageView :: Vk.Device -> Vk.Format -> Vk.Image -> IO Vk.ImageView
createImageView dev swpcSurfaceFormat img = do
  Vk.createImageView dev config Nothing
    where
      config = Vk.ImageViewCreateInfo {..} where
                 next = ()
                 flags = Vk.ImageViewCreateFlagBits 0
                 image = img
                 viewType = Vk.IMAGE_VIEW_TYPE_2D
                 format = swpcSurfaceFormat
                 -- The next parameter is cool: could make hard color changes
                 components = Vk.ComponentMapping { r = Vk.COMPONENT_SWIZZLE_IDENTITY
                                                  , g = Vk.COMPONENT_SWIZZLE_IDENTITY
                                                  , b = Vk.COMPONENT_SWIZZLE_IDENTITY
                                                  , a = Vk.COMPONENT_SWIZZLE_IDENTITY
                                                  }
                 subresourceRange = Vk.ImageSubresourceRange { aspectMask     = Vk.IMAGE_ASPECT_COLOR_BIT
                                                             , baseMipLevel   = 0
                                                             , levelCount     = 1
                                                             , baseArrayLayer = 0
                                                             , layerCount     = 1
                                                             }


destroyImageView :: Vk.Device -> Vk.ImageView -> IO ()
destroyImageView d i = Vk.destroyImageView d i Nothing
