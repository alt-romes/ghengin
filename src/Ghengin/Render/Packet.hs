module Ghengin.Render.Packet where

import Ghengin.Shaders
-- import Ghengin.Shaders.FIR

-- class Vertex
-- class Mesh
-- class Material

import Ghengin.Vulkan

data RenderPacket
data Mesh

-- TODO: PushConstants must also be inferred from the shader code
newtype PushConstantData = PushConstantData { pos_offset :: Mat4 } deriving Storable

-- TODO: Ensure mesh type matches vertex input
-- TODO: Shader pipeline and buffers should only be created once and reused
-- across render packets that use the same one
-- TODO: Currently we assume all our descriptor sets are Uniform buffers and
-- our buffers too but eventually Uniform will be just a constructor of a more
-- general Buffer and we should select the correct type of buffer individually.
makeRenderPacket :: Mesh -> ShaderPipeline () -> Renderer RenderPacket
makeRenderPacket mesh shaderPipeline = do

  simpleRenderPass     <- createSimpleRenderPass

  -- Create the descriptor sets layout and graphics pipeline based on the shader
  -- pipeline
  descriptorSets       <- createDescriptorSets shaderPipeline
  pipeline             <- createGraphicsPipeline ssp simpleRenderPass._renderPass (descriptorSets) [Vk.PushConstantRange { offset = 0 , size   = fromIntegral $ sizeOf @PushConstantData undefined , stageFlags = Vk.SHADER_STAGE_VERTEX_BIT }]

  -- (1) Create the uniform buffers and the mapped memory
  -- (2) Create the descriptor sets from the descriptor set layout
  -- (3) Update the descriptor sets with the buffers information
  --
  -- We need to do (1) (2) and (3) as many times as there are frames in flight.
  uniformBuffers <- mapM (const createMappedUniformBuffer) [1..MAX_FRAMES_IN_FLIGHT]
  let createMappedUniformBuffer
  (dsets, dpool)       <- createUniformBufferDescriptorSets [descriptorSetLayouts | _ <- [1..MAX_FRAMES_IN_FLIGHT]]

  V.zipWithM_ writeUniformBufferDescriptorSet (fmap (.buffer) objUBs) dsets

  _
