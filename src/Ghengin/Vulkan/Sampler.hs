{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Ghengin.Vulkan.Sampler
  (
  -- * Sampler
    Sampler(..)
  , createSampler
  , destroySampler

  -- * Filters
  , Vk.Filter
    ( Vk.FILTER_NEAREST
    , Vk.FILTER_LINEAR
    )

  -- * Address modes
  , Vk.SamplerAddressMode
    ( Vk.SAMPLER_ADDRESS_MODE_REPEAT
    , Vk.SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT
    , Vk.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
    , Vk.SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE
    , Vk.SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER
    )

  ) where

import Control.Monad
import Data.StateVar
import Control.Logger.Simple
import Prelude hiding (filter)
import Control.Monad.IO.Class
import qualified Vulkan.Zero as Vk
import qualified Vulkan as Vk
import Ghengin.Vulkan
import Data.IORef
import Ghengin.Utils (decRefCount)

data Sampler = Sampler { sampler        :: Vk.Sampler
                       , referenceCount :: IORef Int
                       }

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
createSampler :: Vk.Filter -> Vk.SamplerAddressMode -> Renderer χ Sampler
createSampler filter addrMode = do
  device <- getDevice

  let
      -- TODO: Make more flexible as needed
      info = Vk.SamplerCreateInfo { magFilter = filter
                                  , minFilter = filter
                                  
                                  , addressModeU = addrMode
                                  , addressModeV = addrMode
                                  , addressModeW = addrMode

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

  vkSampler <- Vk.createSampler device info Nothing
  refCount <- liftIO $ newIORef 0
  pure $ Sampler vkSampler refCount


destroySampler :: Sampler -> Renderer χ ()
destroySampler vs@(Sampler s refs) = do
  dev <- getDevice

  () <- decRefCount vs

  count <- liftIO $ get refs
  when (count == 0) $ do
    logDebug "Freeing sampler..."
    -- If ref count were -1 then we could have already freed it too many times.
    -- It shouldn't happen in reality because it would mean the sampler wasn't
    -- assigned to a material and the free was called directly
    Vk.destroySampler dev s Nothing

  -- TODO: Don't include this when built for production somehow
  when (count < 0) $ do
    logError "Destroying sampler more times than the number of assignments..."

