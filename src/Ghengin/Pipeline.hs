{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Ghengin.Pipeline where

import qualified Data.ByteString as BS
import qualified Data.Vector as V
import Data.Coerce

import qualified Vulkan as Vk

import Ghengin.Shaders

data Pipeline = MkPipeline ShaderByteCode ShaderByteCode

dynamicStates :: V.Vector Vk.DynamicState
dynamicStates = [ Vk.DYNAMIC_STATE_VIEWPORT
                , Vk.DYNAMIC_STATE_SCISSOR ]

-- | Create a pipeline given a vertex shader and a fragment shader (in this
-- order)
createGraphicsPipeline :: Vk.Device -> Vk.Extent2D -> ShaderByteCode -> ShaderByteCode -> IO Pipeline
createGraphicsPipeline dev swapChainExtent vert frag = do
  vertSM <- createShaderModule dev vert
  fragSM <- createShaderModule dev frag

  let
    vertShaderStageInfo = Vk.PipelineShaderStageCreateInfo {..} where
                            next = ()
                            flags = Vk.PipelineShaderStageCreateFlagBits 0
                            stage   = Vk.SHADER_STAGE_VERTEX_BIT
                            module' = vertSM
                            name    = "main"
                            -- allows to specify values for shader constants,
                            -- and potentially use the same shader with
                            -- different values for each created pipeline
                            specializationInfo = Nothing

    fragShaderStageInfo = Vk.PipelineShaderStageCreateInfo {..} where
                            next = ()
                            flags = Vk.PipelineShaderStageCreateFlagBits 0
                            stage   = Vk.SHADER_STAGE_FRAGMENT_BIT
                            module' = fragSM
                            name    = "main"
                            specializationInfo = Nothing

    shaderStages = [vertShaderStageInfo, fragShaderStageInfo] :: V.Vector (Vk.PipelineShaderStageCreateInfo '[])

    dynamicState = Vk.PipelineDynamicStateCreateInfo (Vk.PipelineDynamicStateCreateFlags 0) dynamicStates

    vertexInputInfo = Vk.PipelineVertexInputStateCreateInfo {..} where
                        next = ()
                        flags = Vk.PipelineVertexInputStateCreateFlags 0
                        vertexBindingDescriptions = []
                        vertexAttributeDescriptions = []

    inputAssembly = Vk.PipelineInputAssemblyStateCreateInfo {..} where
                        next = ()
                        flags = Vk.PipelineInputAssemblyStateCreateFlags 0
                        topology = Vk.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST -- 3 vertices = triangle with no reuse.
                        primitiveRestartEnable = False -- Whether 0xFFFF and 0xFFFFFFFF are special values to break _STRIP topology variants

    -- The region of the framebuffer that the output will be rendered to. We
    -- render from (0,0) to (width, height) i.e. the whole framebuffer
    -- Defines a transformation from image to framebuffer
    viewport = Vk.Viewport {..} where
                 x = 0.0
                 y = 0.0
                 width  = fromIntegral $ swapChainExtent.width
                 height = fromIntegral $ swapChainExtent.height
                 minDepth = 0
                 maxDepth = 1

    -- Defines the region in which pixels will actually be stored. Any pixels
    -- outside of the scissor will be discarded. We keep it as the whole viewport
    scissor = Vk.Rect2D (Vk.Offset2D 0 0) swapChainExtent

    -- Both viewport and scissor can be dynamically changed in the pipeline, so
    -- we only need to specify their amount
    viewportState = Vk.PipelineViewportStateCreateInfo {..} where
                      next = ()
                      flags = Vk.PipelineViewportStateCreateFlags 0
                      viewportCount = 1
                      scissorCount  = 1
                      viewports     = []
                      scissors      = []


  destroyShaderModule dev vertSM
  destroyShaderModule dev fragSM
  pure $ MkPipeline vert frag




createShaderModule :: Vk.Device -> ShaderByteCode -> IO Vk.ShaderModule
createShaderModule dev sbc = do
  Vk.createShaderModule dev createInfo Nothing
  
  where
    createInfo = Vk.ShaderModuleCreateInfo {..} where
                   next = ()
                   flags = Vk.ShaderModuleCreateFlags 0
                   code = BS.toStrict $ coerce sbc


destroyShaderModule :: Vk.Device -> Vk.ShaderModule -> IO ()
destroyShaderModule d sm = Vk.destroyShaderModule d sm Nothing
