{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Ghengin.Vulkan.Renderer.Pipeline where

import qualified Prelude as Ur
import Ghengin.Core.Log
import Prelude.Linear hiding (zero, fromMaybe, IO)
import Control.Functor.Linear as Linear
import Control.Monad.IO.Class.Linear

import Control.Exception (assert)

import Data.Bits ((.|.))
import Data.Coerce
import FIR
  ( BindingStrides, VertexLocationDescriptions
  , GetVertexInputInfo
  , Known, knownValue
  , PipelineInfo
  , Shader(..)
  )
import FIR.Validation.Pipeline (ValidPipelineInfo)
import GHC.TypeNats ( Nat )
import Vulkan.Zero (zero)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IntMap as IM
import qualified Data.Vector as V
import qualified FIR
import qualified Vulkan as Vk
import qualified Vulkan as Vk.Surface
  ( SurfaceFormatKHR(..) )
import qualified Vulkan as Vk.Rendering
  ( PipelineRenderingCreateInfo(..) )
import qualified Vulkan.Zero as Vk
import qualified Vulkan.CStruct.Extends as VkC

import qualified Unsafe.Linear as Unsafe

import Ghengin.Core.Shader.Pipeline
import Ghengin.Vulkan.Renderer.Kernel
import Ghengin.Vulkan.Renderer.Descriptor.Pool
import Ghengin.Vulkan.Renderer.Command as Command

import FIR.Vulkan.Pipeline
import Ghengin.Vulkan.Renderer.Context
import Ghengin.Vulkan.Renderer.Context.Swapchain

#ifdef DEBUG_WRITE_SHADERS
import System.IO.Temp (writeSystemTempFile)
import Data.Text.Lazy.Encoding (decodeUtf8, unpack)
#endif

--------------------------------------------------------------------------------
-- * Commands
--------------------------------------------------------------------------------

bindGraphicsPipeline :: MonadIO m => RendererPipeline Graphics ⊸ RenderCmdM m (RendererPipeline Graphics)
bindGraphicsPipeline (VulkanPipeline pipeline layout) = Linear.do
  pipeline' <- Command.bindGraphicsPipeline' pipeline
  return (VulkanPipeline pipeline' layout)
{-# INLINE bindGraphicsPipeline #-}

--------------------------------------------------------------------------------

data RendererPipeline (t :: PipelineType)
  = VulkanPipeline { _pipeline :: Vk.Pipeline
                   , _pipelineLayout :: Vk.PipelineLayout
                   }

-- TODO: Use type data
data PipelineType = Graphics | Compute

--------------------------------------------------------------------------------

data GraphicsPipelineSettings = GPS
      { cullMode  :: CullMode
      , blendMode :: BlendMode
      , polygonMode :: PolygonMode
      }

data CullMode = CullBack | CullFront | CullNone

data BlendMode = BlendAdd | BlendAlpha | BlendNone

data PolygonMode = PolygonFill
                 -- ^ Fill the area of the polygon with fragments (default)
                 | PolygonLine
                 -- ^ polygon edges drawn as lines (WIREFRAME)
                 | PolygonPoint
                 -- ^ polygon vertices are drawn as points

defaultGraphicsPipelineSettings :: GraphicsPipelineSettings
defaultGraphicsPipelineSettings = GPS CullBack BlendNone PolygonFill

--------------------------------------------------------------------------------

dynamicStates :: V.Vector Vk.DynamicState
dynamicStates = [ Vk.DYNAMIC_STATE_VIEWPORT
                , Vk.DYNAMIC_STATE_SCISSOR ]

-- todo: check which depth attachment supported format is best,
-- see https://www.howtovulkan.com/#depth-attachment
-- (hardcoded for now)
depthFmt :: Vk.Format
depthFmt = Vk.FORMAT_D32_SFLOAT

type PipelineConstraints info top descs strides =
          ( ValidPipelineInfo info
          , '(top, descs, strides) ~ GetVertexInputInfo info
          , Known (PrimitiveTopology Nat)    top
          , Known VertexLocationDescriptions descs
          , Known BindingStrides             strides
          )

-- | Create a graphics pipeline based off of a given 'ShaderPipeline'
--
-- Note that the returned vulkan pipeline must be managed in a structure that
-- ensures each pipeline renders all related items in sequence instead of
-- jumping in between pipeline
createGraphicsPipeline  ::
                        ∀  ( info    :: PipelineInfo               )
                           ( top     :: PrimitiveTopology Nat      )
                           ( descs   :: VertexLocationDescriptions )
                           ( strides :: BindingStrides             )
                        .  PipelineConstraints info top descs strides
                        => GraphicsPipelineSettings
                        -> ShaderPipeline info
                        -> DescriptorPool
                         ⊸ Renderer (RendererPipeline Graphics, DescriptorPool)
createGraphicsPipeline gps (ShaderPipeline ppstages) = Unsafe.toLinear \dpool -> enterD "createGraphicsPipeline" $ Linear.do

  withVulkanContext $ Unsafe.toLinear \vkContext@VulkanContext
    { aSwapchainInfo = ASwapchainInfo SwapchainInfo
        { surfaceFormat = Ur surfaceFormat }
    , ..} -> liftSystemIO do

      let
        -- unsafe aliasing between them here and in the DescriptorPool?
        descriptorSetLayouts = V.fromList $ IM.elems dpool.set_bindings

         
        pipelineShaders :: [(FIR.Shader, Vk.ShaderModule)]
                        -> FIR.PipelineStages info2 ()
                        -> Ur.IO [(FIR.Shader, Vk.ShaderModule)]
        pipelineShaders acc (FIR.VertexInput) = Ur.pure $ reverse acc
        pipelineShaders acc (info FIR.:>-> (sm@(FIR.ShaderModule _ :: FIR.ShaderModule name shader defs endState), ()) )
          = do vksm <- compileFIRShader sm Ur.>>= createShaderModule device
               pipelineShaders ( (knownValue @shader, vksm) : acc) info

      !shaders <- pipelineShaders [] ppstages

      let
        shaderStageInfos = [ shaderInfo s sm | (s, sm) <- shaders ]
        shaderModules    = [ sm | (_, sm) <- shaders ]

        shaderStages = V.fromList $ map VkC.SomeStruct shaderStageInfos :: V.Vector (VkC.SomeStruct Vk.PipelineShaderStageCreateInfo)

        (primTop, vertexInputInfo) = topologyAndVertexInputStateInfo @info

        assemblyStateInfo = assemblyInfo primTop
        mbTessellationStateInfo = tessellationInfo primTop

        -- Fixed functions configuration
        dynamicStateInfo = Vk.PipelineDynamicStateCreateInfo zero dynamicStates
        
        -- Both viewport and scissor can be dynamically changed in the pipeline, so
        -- we only need to specify their amount
        viewportStateInfo = Vk.PipelineViewportStateCreateInfo
                            { next  = ()
                            , flags = zero
                            , viewportCount = 1
                            , scissorCount  = 1
                            , viewports     = [] -- Empty because it is dynamic
                            , scissors      = [] -- Empty because it is dynamic
                            }

        rasterizerInfo = Vk.PipelineRasterizationStateCreateInfo
                         { next  = ()
                         , flags = zero
                         , depthClampEnable = False -- Whether fragments that are beyond the near and far planes are clamped to them as opposed to discarding them. Requires a GPU feature.
                         , rasterizerDiscardEnable = False -- If set to True, geometry never passes through the rasterizer stage. Basically disables output to the framebuffer
                         , polygonMode = case gps.polygonMode of
                              PolygonFill -> Vk.POLYGON_MODE_FILL
                              PolygonLine -> Vk.POLYGON_MODE_LINE
                              PolygonPoint -> Vk.POLYGON_MODE_POINT
                              -- (other modes require GPU feature) ?
                         , lineWidth = 1 -- Thickness of lines in terms of number of fragments (Any >1 requires wideLines feature)
                           -- Face culling: https://learnopengl.com/Advanced-OpenGL/Face-culling
                         -- Cull back faces (polygons that from the viewer perspective are counterclockwise which means we are facing their back)
                         , cullMode = case gps.cullMode of
                                        CullBack  -> Vk.CULL_MODE_BACK_BIT
                                        CullFront -> Vk.CULL_MODE_FRONT_BIT
                                        CullNone  -> Vk.CULL_MODE_NONE
                         , frontFace = Vk.FRONT_FACE_COUNTER_CLOCKWISE -- Default vertice front face to be defined counter clock wise
                         -- , frontFace = Vk.FRONT_FACE_CLOCKWISE
                         , depthBiasEnable = False -- Biasing depth values based on a fragment's slope (could be used for shadow mapping)
                         , depthBiasConstantFactor = 0
                         , depthBiasClamp = 0
                         , depthBiasSlopeFactor = 0
                         }

        multisamplingInfo = Vk.PipelineMultisampleStateCreateInfo
                              -- Configures multisampling (a way to do anti-aliasing)
                              -- Disabling for now...
                            { next  = ()
                            , flags = zero
                            , sampleShadingEnable = False
                            , rasterizationSamples = Vk.SAMPLE_COUNT_1_BIT
                            , minSampleShading = 1
                            , sampleMask = []
                            , alphaToCoverageEnable = False
                            , alphaToOneEnable = False
                            }

        -- Stencil testing currently ignored and a nullptr is passed
        
        -- Color blending

        -- The most common way to use color blending is to implement alpha blending,
        -- where we want the new color to be blended with the old color based on its
        -- opacity.

        colorBlendingInfo = Vk.PipelineColorBlendStateCreateInfo
                            { next  = ()
                            , flags = zero
                            , logicOpEnable = False
                            , logicOp = Vk.LOGIC_OP_COPY
                            , attachmentCount = 1
                            , attachments = [colorBlendAttachment (blendMode gps)]
                            , blendConstants = (0,0,0,0)
                            }

        depthStencilInfo  = Vk.PipelineDepthStencilStateCreateInfo
                            { flags = zero
                            , depthTestEnable = True
                            , depthWriteEnable = True
                            , depthCompareOp = Vk.COMPARE_OP_GREATER
                            
                            -- For the optional depth bound testing. Unused for now
                            , depthBoundsTestEnable = False
                            , minDepthBounds = 0
                            , maxDepthBounds = 1

                            -- Currently not using stencil testing
                            , stencilTestEnable = False
                            , front = zero
                            , back  = zero
                            }


        pipelineLayoutInfo = Vk.PipelineLayoutCreateInfo
                             { flags = zero
                             , setLayouts = descriptorSetLayouts
                             , pushConstantRanges = []
                             }

      pipelineLayout <- Vk.createPipelineLayout device pipelineLayoutInfo Nothing

      let 
        -- Dynamic rendering with Vulkan 1.3 
        -- (No more render passes!!)
        renderingInfo :: Vk.PipelineRenderingCreateInfo
        renderingInfo = Vk.zero
          { Vk.Rendering.colorAttachmentFormats = [ Vk.Surface.format surfaceFormat ]
          , Vk.Rendering.depthAttachmentFormat  = depthFmt
          }
        pipelineInfo = Vk.GraphicsPipelineCreateInfo
          { next = (renderingInfo, ())
          , flags = Vk.zero
          , stageCount = fromIntegral $ V.length shaderStages
          , stages = shaderStages
          , vertexInputState = Just (VkC.SomeStruct vertexInputInfo)
          , inputAssemblyState = Just assemblyStateInfo
          , tessellationState = VkC.SomeStruct Ur.<$> mbTessellationStateInfo
          , viewportState = Just (VkC.SomeStruct viewportStateInfo)
          , rasterizationState = Just (VkC.SomeStruct rasterizerInfo)
          , multisampleState = Just (VkC.SomeStruct multisamplingInfo)
          , depthStencilState = Just depthStencilInfo
          , colorBlendState = Just (VkC.SomeStruct colorBlendingInfo)
          , dynamicState = Just dynamicStateInfo
          , layout = pipelineLayout
          , renderPass = Vk.zero
          , subpass    = Vk.zero
          , basePipelineHandle = Vk.NULL_HANDLE
          , basePipelineIndex = -1
          }

      (_, pipelines) <- Vk.createGraphicsPipelines device Vk.NULL_HANDLE [VkC.SomeStruct pipelineInfo] Nothing
      let pipeline = assert (V.length pipelines == 1) $ V.unsafeHead pipelines

      -- Destroy shader modules after creating the pipeline
      Ur.mapM_ (\sm -> Vk.destroyShaderModule device sm Nothing) shaderModules

      Ur.pure ((VulkanPipeline pipeline pipelineLayout, dpool), vkContext)

-- TODO: createComputePipeline

destroyPipeline :: RendererPipeline t ⊸ Renderer ()
destroyPipeline = Unsafe.toLinear \(VulkanPipeline pipeline pipelineLayout) -> unsafeUseDevice \dev -> do
  Vk.destroyPipeline dev pipeline Nothing
  Vk.destroyPipelineLayout dev pipelineLayout Nothing


createShaderModule :: Vk.Device -> ShaderByteCode -> Ur.IO Vk.ShaderModule
createShaderModule = Unsafe.toLinear $ \dev sbc ->
  let createInfo = Vk.ShaderModuleCreateInfo
                 { next = ()
                 , flags = zero
                 , code = BS.toStrict $ coerce sbc
                 }
   in Vk.createShaderModule dev createInfo Nothing

newtype ShaderByteCode = SBC LBS.ByteString

-- | Compile a shader program into SPIR-V bytecode.
--
-- === Example
--
-- @
-- vertexShaderByteCode <- compileShader SimpleShader.vertex
-- fragShaderByteCode   <- compileShader SimpleShader.fragment
-- @
compileFIRShader :: FIR.CompilableProgram prog => prog -> Ur.IO ShaderByteCode
compileFIRShader m = do
  FIR.compile [] m Ur.>>= \case
#ifdef DEBUG_WRITE_SHADERS
    Right (Just (FIR.ModuleBinary bs), _) -> Ur.do
      writeSystemTempFile "GhenginShader.spv" (((unpack . decodeUtf8)) bs)
      Ur.pure (SBC bs)
#else
    Right (Just (FIR.ModuleBinary bs), _) -> Ur.pure (SBC bs)
#endif
    Left e -> error $ "Failed to compiler Shader:\n" ++ show e
    _      -> error "Couldn't generate module binary when compiling"

colorBlendAttachment :: BlendMode -> Vk.PipelineColorBlendAttachmentState
colorBlendAttachment BlendNone = (colorBlendAttachment BlendAdd){Vk.blendEnable = False}
colorBlendAttachment BlendAdd =
  Vk.PipelineColorBlendAttachmentState
     { colorWriteMask = Vk.COLOR_COMPONENT_R_BIT .|. Vk.COLOR_COMPONENT_G_BIT .|. Vk.COLOR_COMPONENT_B_BIT .|. Vk.COLOR_COMPONENT_A_BIT
     , blendEnable = True
     , srcColorBlendFactor = Vk.BLEND_FACTOR_SRC_ALPHA
     , dstColorBlendFactor = Vk.BLEND_FACTOR_ONE
     , colorBlendOp = Vk.BLEND_OP_ADD
     , srcAlphaBlendFactor = Vk.BLEND_FACTOR_ONE
     , dstAlphaBlendFactor = Vk.BLEND_FACTOR_ZERO
     , alphaBlendOp = Vk.BLEND_OP_ADD
     }
colorBlendAttachment BlendAlpha =
  Vk.PipelineColorBlendAttachmentState
     { colorWriteMask = Vk.COLOR_COMPONENT_R_BIT .|. Vk.COLOR_COMPONENT_G_BIT .|. Vk.COLOR_COMPONENT_B_BIT .|. Vk.COLOR_COMPONENT_A_BIT
     , blendEnable = True
     , srcColorBlendFactor = Vk.BLEND_FACTOR_SRC_ALPHA
     , dstColorBlendFactor = Vk.BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
     , colorBlendOp = Vk.BLEND_OP_ADD
     , srcAlphaBlendFactor = Vk.BLEND_FACTOR_ONE
     , dstAlphaBlendFactor = Vk.BLEND_FACTOR_ZERO
     , alphaBlendOp = Vk.BLEND_OP_ADD
     }

shaderInfo
  :: FIR.Shader
  -> Vk.ShaderModule
  -> Vk.PipelineShaderStageCreateInfo '[]
shaderInfo shaderStage shaderModule =
  Vk.PipelineShaderStageCreateInfo
    { Vk.next               = ()
    , Vk.flags              = zero
    , Vk.name               = "main"
    , Vk.module'            = shaderModule
    , Vk.stage              = stageFlag shaderStage
    , Vk.specializationInfo = Nothing
    }

