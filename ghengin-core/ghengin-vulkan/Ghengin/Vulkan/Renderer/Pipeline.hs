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

import qualified Prelude
import Ghengin.Core.Log
import Prelude.Linear hiding (zero, fromMaybe, IO)
import Control.Functor.Linear as Linear
import qualified Data.Functor.Linear as Data.Linear
import Control.Monad.IO.Class.Linear
import System.IO.Linear
import Data.Bifunctor.Linear

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
import qualified Vulkan.CStruct.Extends as VkC

import qualified Unsafe.Linear as Unsafe

import Ghengin.Core.Shader.Pipeline
import Ghengin.Vulkan.Renderer.Kernel
import Ghengin.Vulkan.Renderer.Descriptor.Pool
import Ghengin.Vulkan.Renderer.Command as Command

import FIR.Vulkan.Pipeline

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
-- TODO: unfortunately this has to be duplicated in the hsig.

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
dynamicStates = [ Vk.DYNAMIC_STATE_VIEWPORT -- TODO: Eventually only the viewport needs to be dynamic right?
                , Vk.DYNAMIC_STATE_SCISSOR ]

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
  -- TODO: seems truly unsafe to alias the descriptor set layouts like this.
  -- we give an alias to the pipeline layout, and apparently forget about it...
  -- do we ever need the descriptor set layouts again outside of pipeline layout?
  Ur descriptorSetLayouts <- liftSystemIOU $ Prelude.pure $ V.fromList $ IM.elems dpool.set_bindings

  Ur dev <- unsafeGetDevice

  let
    pipelineShaders :: [(FIR.Shader, Vk.ShaderModule)]
                     ⊸ FIR.PipelineStages info2 ()
                    -> IO [(FIR.Shader, Vk.ShaderModule)]
    pipelineShaders acc (FIR.VertexInput) = pure $ reverse acc
    pipelineShaders acc (info FIR.:>-> (sm@(FIR.ShaderModule _ :: FIR.ShaderModule name shader defs endState), ()) )
      = Linear.do
        (vksm, dev') <- compileFIRShader sm >>= Unsafe.toLinear2 createShaderModule dev
        Unsafe.toLinear (\_ -> pure ()) dev' -- forget dev' alias
        pipelineShaders ( (knownValue @shader, vksm) : acc) info

  logT "Make pipeline shaders"
  !shaders <- liftIO $ pipelineShaders [] ppstages
  (Ur shaderStageInfos, shaderModules) <- pure $ first (Unsafe.toLinear Ur . map (\case (Ur x) -> x)) $ -- [Ur x] to Ur [x]
                                                 unzip $ map (uncurry shaderInfo) shaders :: Renderer (Ur [Vk.PipelineShaderStageCreateInfo '[]], [Vk.ShaderModule])

  let

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

  logT "Creating pipeline layout"
  Ur unsafePipelineLayout <- liftSystemIOU $ Vk.createPipelineLayout dev pipelineLayoutInfo Nothing

  let 
    pipelineInfo = Vk.GraphicsPipelineCreateInfo { next = ()
                                                 , flags = Vk.PipelineCreateFlagBits 0
                                                 , stageCount = fromIntegral $ V.length shaderStages
                                                 , stages = shaderStages
                                                 , vertexInputState = Just (VkC.SomeStruct vertexInputInfo)
                                                 , inputAssemblyState = Just assemblyStateInfo
                                                 , tessellationState = VkC.SomeStruct Prelude.<$> mbTessellationStateInfo
                                                 , viewportState = Just (VkC.SomeStruct viewportStateInfo)
                                                 , rasterizationState = Just (VkC.SomeStruct rasterizerInfo)
                                                 , multisampleState = Just (VkC.SomeStruct multisamplingInfo)
                                                 , depthStencilState = Just depthStencilInfo
                                                 , colorBlendState = Just (VkC.SomeStruct colorBlendingInfo)
                                                 , dynamicState = Just dynamicStateInfo
                                                 , layout = unsafePipelineLayout
                                                 , renderPass = undefined -- renderP._renderPass
                                                 , subpass = 0 -- the index of the subpass in the render pass where this pipeline will be used.
                                                 , basePipelineHandle = Vk.NULL_HANDLE
                                                 , basePipelineIndex = -1
                                                 }

  logT "Create actual graphics pipeline"
  Ur (_, pipelines) <- liftSystemIOU $ Vk.createGraphicsPipelines dev Vk.NULL_HANDLE [VkC.SomeStruct pipelineInfo] Nothing
  pipeline <- pure $ assert (V.length pipelines == 1) $ V.unsafeHead pipelines

  logT "Free shader modules"
  devs <- Data.Linear.traverse (liftIO . destroyShaderModule dev) shaderModules -- destroy shader modules after creating the pipeline
  Unsafe.toLinear (\_ -> pure ()) devs -- forget dev aliases

  pure (VulkanPipeline pipeline unsafePipelineLayout, dpool)

-- TODO: createComputePipeline

destroyPipeline :: RendererPipeline t ⊸ Renderer ()
destroyPipeline = Unsafe.toLinear \(VulkanPipeline pipeline pipelineLayout) -> unsafeUseDevice \dev -> do
  Vk.destroyPipeline dev pipeline Nothing
  Vk.destroyPipelineLayout dev pipelineLayout Nothing


createShaderModule :: Vk.Device ⊸ ShaderByteCode -> IO (Vk.ShaderModule, Vk.Device)
createShaderModule = Unsafe.toLinear $ \dev sbc ->
  let createInfo = Vk.ShaderModuleCreateInfo
                 { next = ()
                 , flags = zero
                 , code = BS.toStrict $ coerce sbc
                 }
   in liftSystemIO $ (,dev) Prelude.<$> Vk.createShaderModule dev createInfo Nothing

newtype ShaderByteCode = SBC LBS.ByteString

-- | Compile a shader program into SPIR-V bytecode.
--
-- === Example
--
-- @
-- vertexShaderByteCode <- compileShader SimpleShader.vertex
-- fragShaderByteCode   <- compileShader SimpleShader.fragment
-- @
compileFIRShader :: FIR.CompilableProgram prog => prog -> IO ShaderByteCode
compileFIRShader m = liftSystemIO do
  FIR.compile [] m Prelude.>>= \case
#ifdef DEBUG_WRITE_SHADERS
    Right (Just (FIR.ModuleBinary bs), _) -> Prelude.do
      writeSystemTempFile "GhenginShader.spv" (((unpack . decodeUtf8)) bs)
      Prelude.pure (SBC bs)
#else
    Right (Just (FIR.ModuleBinary bs), _) -> Prelude.pure (SBC bs)
#endif
    Left e -> error $ "Failed to compiler Shader:\n" ++ show e
    _      -> error "Couldn't generate module binary when compiling"

destroyShaderModule :: Vk.Device ⊸ Vk.ShaderModule ⊸ System.IO.Linear.IO Vk.Device
destroyShaderModule = Unsafe.toLinear2 \d sm -> d <$ liftSystemIO (Vk.destroyShaderModule d sm Nothing)

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
  %p -> Vk.ShaderModule
   ⊸ (Ur (Vk.PipelineShaderStageCreateInfo '[]), Vk.ShaderModule) -- the shader module is copied to the shader create info? either way the shader module should be freed after the pipeline shader stage create info is used
shaderInfo = Unsafe.toLinear2 \shaderStage shaderModule ->
  (Ur Vk.PipelineShaderStageCreateInfo
    { Vk.next               = ()
    , Vk.flags              = zero
    , Vk.name               = "main"
    , Vk.module'            = shaderModule
    , Vk.stage              = stageFlag shaderStage
    , Vk.specializationInfo = Nothing
    }, shaderModule)

