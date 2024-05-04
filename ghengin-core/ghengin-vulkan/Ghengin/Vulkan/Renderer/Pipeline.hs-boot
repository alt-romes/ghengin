module Ghengin.Vulkan.Renderer.Pipeline where

import qualified Vulkan as Vk

data RendererPipeline (t :: PipelineType)
  = VulkanPipeline { _pipeline :: Vk.Pipeline
                   , _pipelineLayout :: Vk.PipelineLayout
                   }

-- ROMES:TODO: Type data
data PipelineType = Graphics | Compute

