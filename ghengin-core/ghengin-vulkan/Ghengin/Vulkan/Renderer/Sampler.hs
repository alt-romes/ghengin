{-# LANGUAGE OverloadedStrings #-}
module Ghengin.Vulkan.Renderer.Sampler
  (
  -- * Sampler
    Sampler(..)
  , createSampler
  , destroySampler

  -- * Abstract filter types
  , SamplerFilter(..)
  , filterToVk

  -- * Abstract address mode types
  , SamplerAddressMode(..)
  , addressModeToVk

  ) where

import Ghengin.Core.Log
import Ghengin.Core.Prelude as Linear

import qualified Vulkan.Zero as Vk
import qualified Vulkan as Vk
import qualified Unsafe.Linear as Unsafe

import Ghengin.Vulkan.Renderer.Kernel

import qualified Data.Linear.Alias as Alias

data SamplerFilter
  = FilterNearest
  | FilterLinear

-- | Convert abstract filter to Vulkan filter
filterToVk :: SamplerFilter -> Vk.Filter
filterToVk FilterNearest = Vk.FILTER_NEAREST
filterToVk FilterLinear = Vk.FILTER_LINEAR

data SamplerAddressMode
  = SamplerRepeat
  | SamplerMirroredRepeat
  | SamplerClampToEdge
  | SamplerMirrorClampToEdge
  | SamplerClampToBorder

-- | Convert abstract address mode to Vulkan address mode
addressModeToVk :: SamplerAddressMode -> Vk.SamplerAddressMode
addressModeToVk SamplerRepeat = Vk.SAMPLER_ADDRESS_MODE_REPEAT
addressModeToVk SamplerMirroredRepeat = Vk.SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT
addressModeToVk SamplerClampToEdge = Vk.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
addressModeToVk SamplerMirrorClampToEdge = Vk.SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE
addressModeToVk SamplerClampToBorder = Vk.SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER

newtype Sampler = Sampler { sampler :: Vk.Sampler }

-- | Create a sampler with the given filter and sampler address mode
--
-- * VK_FILTER_NEAREST
-- * VK_FILTER_LINEAR
--
-- * VK_SAMPLER_ADDRESS_MODE_REPEAT: Repeat the texture when going beyond the image dimensions.
-- * VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT: Like repeat, but inverts the coordinates to mirror the image when going beyond the dimensions.
-- * VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE: Take the color of the edge closest to the coordinate beyond the image dimensions.
-- * VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE: Like clamp to edge, but instead uses the edge opposite to the closest edge.
-- * VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER: Return a solid color when sampling beyond the dimensions of the image.
createSampler :: SamplerFilter -> SamplerAddressMode -> Renderer (Alias Sampler)
createSampler filter addrMode = enterD "Creating Sampler" Linear.do
  let
      -- TODO: Make more flexible as needed
      info = Vk.SamplerCreateInfo { magFilter = filterToVk filter
                                  , minFilter = filterToVk filter

                                  , addressModeU = addressModeToVk addrMode
                                  , addressModeV = addressModeToVk addrMode
                                  , addressModeW = addressModeToVk addrMode

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

  vkSampler <- unsafeUseDevice (\dev -> Vk.createSampler dev info Nothing)
  refc <- Alias.newAlias destroySampler (Sampler vkSampler)
  pure refc


destroySampler :: Sampler ⊸ Renderer ()
destroySampler (Sampler s) = Unsafe.toLinear (\s' -> enterD "destroySampler" $ unsafeUseDevice (\dev -> Vk.destroySampler dev s' Nothing)) s

