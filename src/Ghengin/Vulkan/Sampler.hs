{-# LANGUAGE RecordWildCards #-}
module Ghengin.Vulkan.Sampler where

import qualified Vulkan.Zero as Vk
import qualified Vulkan as Vk
import Ghengin.Vulkan

createSampler :: Renderer χ Vk.Sampler
createSampler = do
  device <- getDevice

  let
      -- TODO: Make more flexible as needed
      info = Vk.SamplerCreateInfo { magFilter = Vk.FILTER_LINEAR
                                  , minFilter = Vk.FILTER_LINEAR
                                  
                                  -- VK_SAMPLER_ADDRESS_MODE_REPEAT: Repeat the texture when going beyond the image dimensions.
                                  -- VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT: Like repeat, but inverts the coordinates to mirror the image when going beyond the dimensions.
                                  -- VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE: Take the color of the edge closest to the coordinate beyond the image dimensions.
                                  -- VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE: Like clamp to edge, but instead uses the edge opposite to the closest edge.
                                  -- VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER: Return a solid color when sampling beyond the dimensions of the image.
                                  , addressModeU = Vk.SAMPLER_ADDRESS_MODE_REPEAT
                                  , addressModeV = Vk.SAMPLER_ADDRESS_MODE_REPEAT
                                  , addressModeW = Vk.SAMPLER_ADDRESS_MODE_REPEAT

                                  , anisotropyEnable = False
                                  , maxAnisotropy = 1 -- Could query for MAX

                                  , borderColor = Vk.BORDER_COLOR_INT_OPAQUE_BLACK
                                  , unnormalizedCoordinates = False
                                  , compareEnable = False
                                  , compareOp = Vk.COMPARE_OP_ALWAYS

                                  , mipmapMode = Vk.SAMPLER_MIPMAP_MODE_LINEAR
                                  , mipLodBias = 0
                                  , minLod = 0
                                  , maxLod = 0

                                  , flags = Vk.zero
                                  , next = ()
                                  }

  Vk.createSampler device info Nothing

destroySampler :: Vk.Sampler -> Renderer χ ()
destroySampler s = do
  dev <- getDevice
  Vk.destroySampler dev s Nothing
