module Ghengin.Vulkan.Config where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Vector (Vector)
import qualified Data.Vector as V

import qualified Vulkan as Vk

-- data VulkanConfig = VulkanConfig { validationLayers :: Vector ByteString
--                                  , deviceExtensions :: Vector ByteString
--                                  }

-- validationLayers :: V.Vector BS.ByteString
-- validationLayers = [ "VK_LAYER_KHRONOS_validation"
--                    ]

-- deviceExtensions :: V.Vector BS.ByteString
-- deviceExtensions = [ Vk.KHR_SWAPCHAIN_EXTENSION_NAME
--                    ]

