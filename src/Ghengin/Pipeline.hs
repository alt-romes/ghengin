{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Ghengin.Pipeline where

import qualified Data.ByteString as BS
import qualified Data.Vector as V
import Data.Coerce
import Data.Bits ((.|.))

import qualified Vulkan as Vk

import Ghengin.Shaders

dynamicStates :: V.Vector Vk.DynamicState
dynamicStates = [ Vk.DYNAMIC_STATE_VIEWPORT
                , Vk.DYNAMIC_STATE_SCISSOR ]

-- | Create a pipeline given a vertex shader and a fragment shader (in this
-- order)
createGraphicsPipeline :: Vk.Device -> Vk.Extent2D -> ShaderByteCode -> ShaderByteCode -> IO Vk.PipelineLayout
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

    -- Fixed functions configuration

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

    rasterizer = Vk.PipelineRasterizationStateCreateInfo {..} where
                    next = ()
                    flags = Vk.PipelineRasterizationStateCreateFlags 0
                    depthClampEnable = False -- Whether fragments that are beyond the near and far planes are clamped to them as opposed to discarding them. Requires a GPU feature.
                    rasterizerDiscardEnable = False -- If set to True, geometry never passes through the rasterizer stage. Basically disables output to the framebuffer
                    polygonMode = Vk.POLYGON_MODE_FILL -- Fill the area of the polygon with fragments; VK_POLYGON_MODE_LINE: polygon edges drawn as lines; VK_POLYGON_MODE_POINT: polygon vertices are drawn as points (other modes require GPU feature)
                    lineWidth = 1 -- Thickness of lines in terms of number of fragments (Any >1 requires wideLines feature)
                    -- Face culling: https://learnopengl.com/Advanced-OpenGL/Face-culling
                    cullMode = Vk.CULL_MODE_BACK_BIT    -- Cull back faces (polygons that from the viewer perspective are counterclockwise which means we are facing their back)
                    frontFace = Vk.FRONT_FACE_CLOCKWISE -- Default vertice front face to be defined clock wise
                    depthBiasEnable = False -- Biasing depth values based on a fragment's slope (could be used for shadow mapping)
                    depthBiasConstantFactor = 0
                    depthBiasClamp = 0
                    depthBiasSlopeFactor = 0

    multisampling = Vk.PipelineMultisampleStateCreateInfo {..} where
                      -- Configures multisampling (a way to do anti-aliasing)
                      -- Disabling for now...
                      next = ()
                      flags = Vk.PipelineMultisampleStateCreateFlags 0
                      sampleShadingEnable = False
                      rasterizationSamples = Vk.SAMPLE_COUNT_1_BIT
                      minSampleShading = 1
                      sampleMask = []
                      alphaToCoverageEnable = False
                      alphaToOneEnable = False

    -- Depth and stencil testing currently ignored and a nullptr is passed
    
    -- Color blending

    -- The most common way to use color blending is to implement alpha blending,
    -- where we want the new color to be blended with the old color based on its
    -- opacity.
    -- We're not doing this but the parameters for it are in the tutorial

    -- Disabled color blending
    colorBlendAttachment = Vk.PipelineColorBlendAttachmentState {..} where
                              colorWriteMask = Vk.COLOR_COMPONENT_R_BIT .|. Vk.COLOR_COMPONENT_G_BIT .|. Vk.COLOR_COMPONENT_B_BIT .|. Vk.COLOR_COMPONENT_A_BIT
                              blendEnable = False
                              srcColorBlendFactor = Vk.BLEND_FACTOR_ONE
                              dstColorBlendFactor = Vk.BLEND_FACTOR_ZERO
                              colorBlendOp = Vk.BLEND_OP_ADD
                              srcAlphaBlendFactor = Vk.BLEND_FACTOR_ONE
                              dstAlphaBlendFactor = Vk.BLEND_FACTOR_ZERO
                              alphaBlendOp = Vk.BLEND_OP_ADD

    colorBlending = Vk.PipelineColorBlendStateCreateInfo {..} where
                      next = ()
                      flags = Vk.PipelineColorBlendStateCreateFlagBits 0
                      logicOpEnable = False
                      logicOp = Vk.LOGIC_OP_COPY
                      attachmentCount = 1
                      attachments = [colorBlendAttachment]
                      blendConstants = (0,0,0,0)


    -- In this config we can specify uniform values and push constants (other
    -- way of passing dynamic values to shaders)
    pipelineLayoutInfo = Vk.PipelineLayoutCreateInfo {..} where
                           flags = Vk.PipelineLayoutCreateFlagBits 0
                           setLayouts = []
                           pushConstantRanges = []


  pipelineLayout <- Vk.createPipelineLayout dev pipelineLayoutInfo Nothing

  destroyShaderModule dev vertSM
  destroyShaderModule dev fragSM
  pure pipelineLayout


destroyPipelineLayout :: Vk.Device -> Vk.PipelineLayout -> IO ()
destroyPipelineLayout d pl = Vk.destroyPipelineLayout d pl Nothing



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
