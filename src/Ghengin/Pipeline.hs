module Ghengin.Pipeline (Pipeline, createPipeline) where

-- import qualified Vulkan as VK

import Ghengin.Shaders

data Pipeline = MkPipeline ShaderByteCode ShaderByteCode

-- | Create a pipeline given a vertex shader and a fragment shader (in this
-- order)
createPipeline :: ShaderByteCode -> ShaderByteCode -> IO Pipeline
createPipeline vert frag = pure $ MkPipeline vert frag


