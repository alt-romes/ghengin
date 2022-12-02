{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Ghengin.Vulkan.Pipeline where

import Control.Exception 
import Control.Monad.Reader
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import Data.Coerce
import Data.Bits ((.|.))
import Foreign.Storable
import Geomancy

import Vulkan.Zero (zero)
import qualified Vulkan.CStruct.Extends as VkC
import qualified Vulkan as Vk

import Ghengin.Shaders
import Ghengin.Vulkan
import Ghengin.Vulkan.Device

-- Oof... read comment on vertexInput
import Ghengin.Component.Mesh

data VulkanPipeline = VulkanPipeline { _pipeline :: Vk.Pipeline
                                     , _pipelineLayout :: Vk.PipelineLayout
                                     }

dynamicStates :: V.Vector Vk.DynamicState
dynamicStates = [ Vk.DYNAMIC_STATE_VIEWPORT
                , Vk.DYNAMIC_STATE_SCISSOR ]

-- Isto seria fixe num per-renderer basis
newtype PushConstantData = PushConstantData { pos_offset :: Mat4 } deriving Storable

withGraphicsPipeline :: ShaderByteCode -> ShaderByteCode -> Vk.RenderPass -> V.Vector Vk.DescriptorSetLayout -> (VulkanPipeline -> Renderer a) -> Renderer a
withGraphicsPipeline vert frag rp sls f = Renderer $ ReaderT $ \renv ->
                                          bracket
                                            (runReaderT (unRenderer $ createGraphicsPipeline vert frag rp sls) renv)
                                            ((`runReaderT` renv) . unRenderer . destroyPipeline)
                                            ((`runReaderT` renv) . unRenderer . f)

createGraphicsPipeline :: ShaderByteCode -> ShaderByteCode -> Vk.RenderPass -> V.Vector Vk.DescriptorSetLayout -> Renderer VulkanPipeline
createGraphicsPipeline v f rp sls = Renderer $ ReaderT (\renv -> createGraphicsPipeline' (renv._vulkanDevice._device) v f rp sls)

-- | Create a pipeline given a vertex shader and a fragment shader (in this
-- order)
createGraphicsPipeline' :: Vk.Device -> ShaderByteCode -> ShaderByteCode -> Vk.RenderPass -> V.Vector Vk.DescriptorSetLayout -> IO VulkanPipeline
createGraphicsPipeline' dev vert frag renderP descriptorSetLayouts = do

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

    shaderStages = [VkC.SomeStruct vertShaderStageInfo, VkC.SomeStruct fragShaderStageInfo] :: V.Vector (VkC.SomeStruct Vk.PipelineShaderStageCreateInfo)

    -- Fixed functions configuration

    dynamicStateInfo = Vk.PipelineDynamicStateCreateInfo (Vk.PipelineDynamicStateCreateFlags 0) dynamicStates

    vertexInputInfo = Vk.PipelineVertexInputStateCreateInfo {..} where
                        next = ()
                        flags = Vk.PipelineVertexInputStateCreateFlags 0
                        -- ROMES:TODO: Hardcoded for now. Later we might have
                        -- graphics pipelines for different types of vertices
                        vertexBindingDescriptions = [vertexInputBindingDescription]
                        vertexAttributeDescriptions = vertexInputAttributeDescriptions

    inputAssembly = Vk.PipelineInputAssemblyStateCreateInfo {..} where
                        flags = Vk.PipelineInputAssemblyStateCreateFlags 0
                        topology = Vk.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST -- _STRIP -- 3 vertices = triangle with no reuse.
                        primitiveRestartEnable = False -- Whether 0xFFFF and 0xFFFFFFFF are special values to break _STRIP topology variants

    -- Both viewport and scissor can be dynamically changed in the pipeline, so
    -- we only need to specify their amount
    viewportStateInfo = Vk.PipelineViewportStateCreateInfo {..} where
                          next = ()
                          flags = Vk.PipelineViewportStateCreateFlags 0
                          viewportCount = 1
                          scissorCount  = 1
                          viewports     = [] -- Empty because it is dynamic
                          scissors      = [] -- Empty because it is dynamic

    rasterizerInfo = Vk.PipelineRasterizationStateCreateInfo {..} where
                      next = ()
                      flags = Vk.PipelineRasterizationStateCreateFlags 0
                      depthClampEnable = False -- Whether fragments that are beyond the near and far planes are clamped to them as opposed to discarding them. Requires a GPU feature.
                      rasterizerDiscardEnable = False -- If set to True, geometry never passes through the rasterizer stage. Basically disables output to the framebuffer
                      polygonMode = Vk.POLYGON_MODE_FILL -- Fill the area of the polygon with fragments; VK_POLYGON_MODE_LINE: polygon edges drawn as lines (WIREFRAME?); VK_POLYGON_MODE_POINT: polygon vertices are drawn as points (other modes require GPU feature)
                      lineWidth = 1 -- Thickness of lines in terms of number of fragments (Any >1 requires wideLines feature)
                      -- Face culling: https://learnopengl.com/Advanced-OpenGL/Face-culling
                      -- cullMode = Vk.CULL_MODE_BACK_BIT    -- Cull back faces (polygons that from the viewer perspective are counterclockwise which means we are facing their back)
                      cullMode = Vk.CULL_MODE_NONE
                      -- frontFace = Vk.FRONT_FACE_COUNTER_CLOCKWISE -- Default vertice front face to be defined clock wise
                      frontFace = Vk.FRONT_FACE_CLOCKWISE
                      depthBiasEnable = False -- Biasing depth values based on a fragment's slope (could be used for shadow mapping)
                      depthBiasConstantFactor = 0
                      depthBiasClamp = 0
                      depthBiasSlopeFactor = 0

    multisamplingInfo = Vk.PipelineMultisampleStateCreateInfo {..} where
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

    colorBlendingInfo = Vk.PipelineColorBlendStateCreateInfo {..} where
                          next = ()
                          flags = Vk.PipelineColorBlendStateCreateFlagBits 0
                          logicOpEnable = False
                          logicOp = Vk.LOGIC_OP_COPY
                          attachmentCount = 1
                          attachments = [colorBlendAttachment]
                          blendConstants = (0,0,0,0)

    depthStencilInfo  = Vk.PipelineDepthStencilStateCreateInfo
                            { flags = zero
                            , depthTestEnable = True
                            , depthWriteEnable = True
                            , depthCompareOp = Vk.COMPARE_OP_LESS
                            
                            -- For the optional depth bound testing. Unused for now
                            , depthBoundsTestEnable = False
                            , minDepthBounds = 0
                            , maxDepthBounds = 1

                            -- Currently not using stencil testing
                            , stencilTestEnable = False
                            , front = zero
                            , back  = zero
                            }


    -- In this config we can specify uniform values and push constants (other
    -- way of passing dynamic values to shaders)
    pipelineLayoutInfo = Vk.PipelineLayoutCreateInfo {..} where
                           flags = Vk.PipelineLayoutCreateFlagBits 0
                           setLayouts = descriptorSetLayouts
                           pushConstantRanges = [Vk.PushConstantRange { offset = 0
                                                                      , size   = fromIntegral $ sizeOf @PushConstantData undefined
                                                                      , stageFlags = Vk.SHADER_STAGE_VERTEX_BIT
                                                                      }]

  pipelineLayout <- Vk.createPipelineLayout dev pipelineLayoutInfo Nothing

  let 
    pipelineInfo = Vk.GraphicsPipelineCreateInfo { next = ()
                                                 , flags = Vk.PipelineCreateFlagBits 0
                                                 , stageCount = 2
                                                 , stages = shaderStages
                                                 , vertexInputState = Just (VkC.SomeStruct vertexInputInfo)
                                                 , inputAssemblyState = Just inputAssembly
                                                 , tessellationState = Nothing
                                                 , viewportState = Just (VkC.SomeStruct viewportStateInfo)
                                                 , rasterizationState = Just (VkC.SomeStruct rasterizerInfo)
                                                 , multisampleState = Just (VkC.SomeStruct multisamplingInfo)
                                                 , depthStencilState = Just depthStencilInfo
                                                 , colorBlendState = Just (VkC.SomeStruct colorBlendingInfo)
                                                 , dynamicState = Just dynamicStateInfo
                                                 , layout = pipelineLayout
                                                 , renderPass = renderP
                                                 , subpass = 0 -- the index of the subpass in the render pass where this pipeline will be used.
                                                 , basePipelineHandle = Vk.NULL_HANDLE
                                                 , basePipelineIndex = -1
                                                 }

  (_, [pipeline]) <- Vk.createGraphicsPipelines dev Vk.NULL_HANDLE [VkC.SomeStruct pipelineInfo] Nothing

  destroyShaderModule dev vertSM
  destroyShaderModule dev fragSM

  pure $ VulkanPipeline pipeline pipelineLayout


destroyPipeline :: VulkanPipeline -> Renderer ()
destroyPipeline (VulkanPipeline pipeline pipelineLayout) = getDevice >>= \d -> do
  Vk.destroyPipeline d pipeline Nothing
  Vk.destroyPipelineLayout d pipelineLayout Nothing


-- :| Shader Modules |:

createShaderModule :: Vk.Device -> ShaderByteCode -> IO Vk.ShaderModule
createShaderModule dev sbc = do
  Vk.createShaderModule dev createInfo Nothing where
    createInfo = Vk.ShaderModuleCreateInfo {..} where
                   next = ()
                   flags = Vk.ShaderModuleCreateFlags 0
                   code = BS.toStrict $ coerce sbc

destroyShaderModule :: Vk.Device -> Vk.ShaderModule -> IO ()
destroyShaderModule d sm = Vk.destroyShaderModule d sm Nothing


