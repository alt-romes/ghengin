module Ghengin.Render.Pipeline where

import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Vector (Vector)
import Foreign.Storable

import qualified Vulkan as Vk
import Ghengin.Shaders
import Ghengin.Utils
import Ghengin.Vulkan.RenderPass
import Ghengin.Vulkan.Pipeline
import Ghengin.Vulkan.DescriptorSet
import Ghengin.Vulkan

-- | A render pipeline consists of the descriptor sets and a graphics pipeline
-- required to render certain 'RenderPacket's
data RenderPipeline info = RenderPipeline { _graphicsPipeline  :: VulkanPipeline
                                          , _renderPass        :: VulkanRenderPass
                                          , _descriptorSetsSet :: NonEmpty (Vector DescriptorSet, Vk.DescriptorPool) -- We need descriptor sets for each frame in flight
                                          , _shaderPipeline    :: GShaderPipeline info
                                          }

