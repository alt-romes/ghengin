module Ghengin.Vulkan.Renderer.Pipeline where

import qualified Vulkan as Vk

data RendererPipeline (t :: PipelineType)
  = VulkanPipeline { _pipeline :: Vk.Pipeline
                   , _pipelineLayout :: Vk.PipelineLayout
                   }

-- TODO: Use type data
data PipelineType = Graphics | Compute

