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
import Prelude.Linear hiding (zero, fromMaybe, IO)
import Control.Functor.Linear as Linear
import qualified Data.Functor.Linear as Data.Linear
import Control.Monad.IO.Class.Linear
import System.IO.Linear
import Data.Bifunctor.Linear

import Control.Exception (assert)

import Data.Bits ((.|.))
import Data.Coerce
import qualified Data.Functor ((<&>))
import Data.Maybe
import FIR
  ( (:->)((:->))
  , BindingStrides, VertexLocationDescriptions
  , GetVertexInputInfo
  , ImageFormat
  , ImageFormat(ImageFormat), pattern UI, pattern I, pattern F
  , Known, knownValue
  , PipelineInfo
  , PrimitiveTopology(..)
  , Shader(..)
  , Word32
  )
import FIR.Validation.Pipeline (ValidPipelineInfo)
import GHC.TypeNats ( Nat )
import Vulkan.Zero (zero)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector as V
import qualified FIR hiding (ShaderPipeline, (:>->))
import qualified Vulkan as Vk
import qualified Vulkan.CStruct.Extends as VkC

import qualified Unsafe.Linear as Unsafe

import Ghengin.Core.Shader.Pipeline
import Ghengin.Vulkan.Renderer.Kernel
import Ghengin.Vulkan.Renderer.RenderPass

data RendererPipeline (t :: PipelineType)
  = VulkanPipeline { _pipeline :: Vk.Pipeline
                   , _pipelineLayout :: Vk.PipelineLayout
                   }

-- ROMES:TODO: Type data
data PipelineType = Graphics | Compute

dynamicStates :: V.Vector Vk.DynamicState
dynamicStates = [ Vk.DYNAMIC_STATE_VIEWPORT -- TODO: Eventually only the viewport needs to be dynamic right?
                , Vk.DYNAMIC_STATE_SCISSOR ]

-- withGraphicsPipeline :: -- (KnownDefinitions vertexdefs, KnownDefinitions fragdefs)
--                      Module vertexdefs -> Module fragdefs
--                      -> Vk.RenderPass
--                      -> V.Vector Vk.DescriptorSetLayout
--                      -> V.Vector Vk.PushConstantRange
--                      -> (VulkanPipeline -> Renderer a) -> Renderer a
-- withGraphicsPipeline vert frag rp sls pcr f = Renderer $ ReaderT $ \renv ->
--                                           bracket
--                                             (runReaderT (unRenderer $ createGraphicsPipeline vert frag rp sls pcr) renv)
--                                             ((`runReaderT` renv) . unRenderer . destroyPipeline)
--                                             ((`runReaderT` renv) . unRenderer . f)

type PipelineConstraints info top descs strides =
          ( ValidPipelineInfo info
          , '(top, descs, strides) ~ GetVertexInputInfo info
          , Known (PrimitiveTopology Nat)    top
          , Known VertexLocationDescriptions descs
          , Known BindingStrides             strides
          )

-- | Create a pipeline given a vertex shader and a fragment shader (in this
-- order)
--
-- Note that the returned vulkan pipeline must be managed in a structure that
-- ensures each pipeline renders all related items in sequence instead of
-- jumping in between pipeline
createGraphicsPipeline  :: -- (KnownDefinitions vertexdefs, KnownDefinitions fragdefs)
                        -- (CompilableProgram v, CompilableProgram f)
                        ∀  ( info    :: PipelineInfo               )
                           ( top     :: PrimitiveTopology Nat      )
                           ( descs   :: VertexLocationDescriptions )
                           ( strides :: BindingStrides             )
                        .  PipelineConstraints info top descs strides
                        => ShaderPipeline info
                        -> RenderPass -- ^ Must be reference counted since the graphics pipeline keeps an alias to it
                         ⊸ V.Vector Vk.DescriptorSetLayout
                         ⊸ V.Vector Vk.PushConstantRange
                        -> Renderer (RendererPipeline Graphics, RenderPass, V.Vector Vk.DescriptorSetLayout)
createGraphicsPipeline = Unsafe.toLinearN @4 \ppstages renderP descriptorSetLayouts pushConstantRanges -> Linear.do

  Ur dev <- unsafeGetDevice

  let
    pipelineShaders :: [(FIR.Shader, Vk.ShaderModule)]
                     ⊸ ShaderPipeline info2
                    -> IO [(FIR.Shader, Vk.ShaderModule)]
    pipelineShaders acc (ShaderPipeline FIR.VertexInput) = pure $ reverse acc
    pipelineShaders acc ( info :>-> sm@(FIR.ShaderModule _ :: FIR.ShaderModule name shader defs endState) )
      = Linear.do
        (vksm, dev') <- compileFIRShader sm >>= Unsafe.toLinear2 createShaderModule dev
        Unsafe.toLinear (\_ -> pure ()) dev' -- forget dev' alias
        pipelineShaders ( (knownValue @shader, vksm) : acc) info

  shaders <- liftIO $ pipelineShaders [] ppstages
  (Ur shaderStageInfos, shaderModules) <- pure $ first (Unsafe.toLinear Ur . map (\case (Ur x) -> x)) $ -- [Ur x] to Ur [x]
                                                 unzip $ map (uncurry shaderInfo) shaders :: Renderer (Ur [Vk.PipelineShaderStageCreateInfo '[]], [Vk.ShaderModule])

  let

    shaderStages = V.fromList $ map VkC.SomeStruct shaderStageInfos :: V.Vector (VkC.SomeStruct Vk.PipelineShaderStageCreateInfo)

    (assemblyInfo, vertexInputInfo) = assemblyAndVertexInputStateInfo @info

    -- Fixed functions configuration
    dynamicStateInfo = Vk.PipelineDynamicStateCreateInfo zero dynamicStates
    
    -- vertexInputInfo = Vk.PipelineVertexInputStateCreateInfo
    --                   { next = ()
    --                   , flags = zero
    --                     -- ROMES:TODO: Hardcoded for now. Later we might have
    --                     -- graphics pipelines for different types of vertices
    --                   , vertexBindingDescriptions = [Mesh.vertexInputBindingDescription]
    --                   , vertexAttributeDescriptions = Mesh.vertexInputAttributeDescriptions
    --                   }

    -- inputAssembly = Vk.PipelineInputAssemblyStateCreateInfo
    --                 { flags = zero
    --                 , topology = Vk.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST -- _STRIP -- 3 vertices = triangle with no reuse.
    --                 , primitiveRestartEnable = False -- Whether 0xFFFF and 0xFFFFFFFF are special values to break _STRIP topology variants
    --                 }

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
                     , polygonMode = Vk.POLYGON_MODE_FILL -- Fill the area of the polygon with fragments; VK_POLYGON_MODE_LINE: polygon edges drawn as lines (WIREFRAME?); VK_POLYGON_MODE_POINT: polygon vertices are drawn as points (other modes require GPU feature)
                     , lineWidth = 1 -- Thickness of lines in terms of number of fragments (Any >1 requires wideLines feature)
                       -- Face culling: https://learnopengl.com/Advanced-OpenGL/Face-culling
                       -- cullMode = Vk.CULL_MODE_BACK_BIT    -- Cull back faces (polygons that from the viewer perspective are counterclockwise which means we are facing their back)
                     , cullMode = Vk.CULL_MODE_NONE
                       -- frontFace = Vk.FRONT_FACE_COUNTER_CLOCKWISE -- Default vertice front face to be defined clock wise
                     , frontFace = Vk.FRONT_FACE_CLOCKWISE
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
    -- We're not doing this but the parameters for it are in the tutorial

    -- Disabled color blending
    colorBlendAttachment = Vk.PipelineColorBlendAttachmentState
                           { colorWriteMask = Vk.COLOR_COMPONENT_R_BIT .|. Vk.COLOR_COMPONENT_G_BIT .|. Vk.COLOR_COMPONENT_B_BIT .|. Vk.COLOR_COMPONENT_A_BIT
                           , blendEnable = False
                           , srcColorBlendFactor = Vk.BLEND_FACTOR_ONE
                           , dstColorBlendFactor = Vk.BLEND_FACTOR_ZERO
                           , colorBlendOp = Vk.BLEND_OP_ADD
                           , srcAlphaBlendFactor = Vk.BLEND_FACTOR_ONE
                           , dstAlphaBlendFactor = Vk.BLEND_FACTOR_ZERO
                           , alphaBlendOp = Vk.BLEND_OP_ADD
                           }

    colorBlendingInfo = Vk.PipelineColorBlendStateCreateInfo
                        { next  = ()
                        , flags = zero
                        , logicOpEnable = False
                        , logicOp = Vk.LOGIC_OP_COPY
                        , attachmentCount = 1
                        , attachments = [colorBlendAttachment]
                        , blendConstants = (0,0,0,0)
                        }

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
    pipelineLayoutInfo = Vk.PipelineLayoutCreateInfo
                         { flags = zero
                         , setLayouts = descriptorSetLayouts
                         , pushConstantRanges = pushConstantRanges
                         }

  Ur unsafePipelineLayout <- liftSystemIOU $ Vk.createPipelineLayout dev pipelineLayoutInfo Nothing

  let 
    pipelineInfo = Vk.GraphicsPipelineCreateInfo { next = ()
                                                 , flags = Vk.PipelineCreateFlagBits 0
                                                 , stageCount = 2
                                                 , stages = shaderStages
                                                 , vertexInputState = Just (VkC.SomeStruct vertexInputInfo)
                                                 , inputAssemblyState = Just assemblyInfo
                                                 , tessellationState = Nothing -- still no tesselation
                                                 , viewportState = Just (VkC.SomeStruct viewportStateInfo)
                                                 , rasterizationState = Just (VkC.SomeStruct rasterizerInfo)
                                                 , multisampleState = Just (VkC.SomeStruct multisamplingInfo)
                                                 , depthStencilState = Just depthStencilInfo
                                                 , colorBlendState = Just (VkC.SomeStruct colorBlendingInfo)
                                                 , dynamicState = Just dynamicStateInfo
                                                 , layout = unsafePipelineLayout
                                                 , renderPass = renderP._renderPass
                                                 , subpass = 0 -- the index of the subpass in the render pass where this pipeline will be used.
                                                 , basePipelineHandle = Vk.NULL_HANDLE
                                                 , basePipelineIndex = -1
                                                 }

  Ur (_, pipelines) <- liftSystemIOU $ Vk.createGraphicsPipelines dev Vk.NULL_HANDLE [VkC.SomeStruct pipelineInfo] Nothing
  pipeline <- pure $ assert (V.length pipelines == 1) $ V.unsafeHead pipelines

  -- ROMES:TODO: Free shader modules
  devs <- Data.Linear.traverse (liftIO . destroyShaderModule dev) shaderModules -- destroy shader modules after creating the pipeline
  Unsafe.toLinear (\_ -> pure ()) devs -- forget dev aliases

  pure (VulkanPipeline pipeline unsafePipelineLayout, renderP, descriptorSetLayouts)



destroyPipeline :: RendererPipeline t ⊸ Renderer ()
destroyPipeline = Unsafe.toLinear \(VulkanPipeline pipeline pipelineLayout) -> unsafeUseDevice \dev -> do
  Vk.destroyPipeline dev pipeline Nothing
  Vk.destroyPipelineLayout dev pipelineLayout Nothing


-- :| Shader Modules |:

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
    Right (Just (FIR.ModuleBinary bs), _) -> Prelude.pure (SBC bs)
    Left e -> error $ show e
    _      -> error "Couldn't generate module binary when compiling"

destroyShaderModule :: Vk.Device ⊸ Vk.ShaderModule ⊸ System.IO.Linear.IO Vk.Device
destroyShaderModule = Unsafe.toLinear2 \d sm -> d <$ liftSystemIO (Vk.destroyShaderModule d sm Nothing)


-- From https://gitlab.com/sheaf/fir/-/blob/master/fir-examples/src/Vulkan/Pipeline.hs

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

simpleFormat :: ImageFormat Word32 -> Maybe Vk.Format
simpleFormat ( ImageFormat UI widths ) = case widths of
  [8]           -> Just Vk.FORMAT_R8_UINT
  [8,8]         -> Just Vk.FORMAT_R8G8_UINT
  [8,8,8]       -> Just Vk.FORMAT_R8G8B8_UINT
  [8,8,8,8]     -> Just Vk.FORMAT_R8G8B8A8_UINT
  [16]          -> Just Vk.FORMAT_R16_UINT
  [16,16]       -> Just Vk.FORMAT_R16G16_UINT
  [16,16,16]    -> Just Vk.FORMAT_R16G16B16_UINT
  [16,16,16,16] -> Just Vk.FORMAT_R16G16B16A16_UINT
  [32]          -> Just Vk.FORMAT_R32_UINT
  [32,32]       -> Just Vk.FORMAT_R32G32_UINT
  [32,32,32]    -> Just Vk.FORMAT_R32G32B32_UINT
  [32,32,32,32] -> Just Vk.FORMAT_R32G32B32A32_UINT
  [64]          -> Just Vk.FORMAT_R64_UINT
  [64,64]       -> Just Vk.FORMAT_R64G64_UINT
  [64,64,64]    -> Just Vk.FORMAT_R64G64B64_UINT
  [64,64,64,64] -> Just Vk.FORMAT_R64G64B64A64_UINT
  _             -> Nothing
simpleFormat ( ImageFormat I widths ) = case widths of
  [8]           -> Just Vk.FORMAT_R8_SINT
  [8,8]         -> Just Vk.FORMAT_R8G8_SINT
  [8,8,8]       -> Just Vk.FORMAT_R8G8B8_SINT
  [8,8,8,8]     -> Just Vk.FORMAT_R8G8B8A8_SINT
  [16]          -> Just Vk.FORMAT_R16_SINT
  [16,16]       -> Just Vk.FORMAT_R16G16_SINT
  [16,16,16]    -> Just Vk.FORMAT_R16G16B16_SINT
  [16,16,16,16] -> Just Vk.FORMAT_R16G16B16A16_SINT
  [32]          -> Just Vk.FORMAT_R32_SINT
  [32,32]       -> Just Vk.FORMAT_R32G32_SINT
  [32,32,32]    -> Just Vk.FORMAT_R32G32B32_SINT
  [32,32,32,32] -> Just Vk.FORMAT_R32G32B32A32_SINT
  [64]          -> Just Vk.FORMAT_R64_SINT
  [64,64]       -> Just Vk.FORMAT_R64G64_SINT
  [64,64,64]    -> Just Vk.FORMAT_R64G64B64_SINT
  [64,64,64,64] -> Just Vk.FORMAT_R64G64B64A64_SINT
  _             -> Nothing
simpleFormat ( ImageFormat F widths ) = case widths of
  [16]          -> Just Vk.FORMAT_R16_SFLOAT
  [16,16]       -> Just Vk.FORMAT_R16G16_SFLOAT
  [16,16,16]    -> Just Vk.FORMAT_R16G16B16_SFLOAT
  [16,16,16,16] -> Just Vk.FORMAT_R16G16B16A16_SFLOAT
  [32]          -> Just Vk.FORMAT_R32_SFLOAT
  [32,32]       -> Just Vk.FORMAT_R32G32_SFLOAT
  [32,32,32]    -> Just Vk.FORMAT_R32G32B32_SFLOAT
  [32,32,32,32] -> Just Vk.FORMAT_R32G32B32A32_SFLOAT
  [64]          -> Just Vk.FORMAT_R64_SFLOAT
  [64,64]       -> Just Vk.FORMAT_R64G64_SFLOAT
  [64,64,64]    -> Just Vk.FORMAT_R64G64B64_SFLOAT
  [64,64,64,64] -> Just Vk.FORMAT_R64G64B64A64_SFLOAT
  _             -> Nothing
simpleFormat _ = Nothing

topology :: PrimitiveTopology n -> Vk.PrimitiveTopology
topology Points                    = Vk.PRIMITIVE_TOPOLOGY_POINT_LIST
topology (Line List              ) = Vk.PRIMITIVE_TOPOLOGY_LINE_LIST
topology (Line Strip             ) = Vk.PRIMITIVE_TOPOLOGY_LINE_STRIP
topology (Line Fan               ) = error "Invalid topology: fan of lines."
topology (Triangle List          ) = Vk.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
topology (Triangle Strip         ) = Vk.PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP
topology (Triangle Fan           ) = Vk.PRIMITIVE_TOPOLOGY_TRIANGLE_FAN
topology (Line AdjacencyList     ) = Vk.PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY
topology (Line AdjacencyStrip    ) = Vk.PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY
topology (Triangle AdjacencyList ) = Vk.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY
topology (Triangle AdjacencyStrip) = Vk.PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY
topology (PatchesOfSize         _) = Vk.PRIMITIVE_TOPOLOGY_PATCH_LIST

makeAssemblyInfo
  :: FIR.PrimitiveTopology n -> Vk.PipelineInputAssemblyStateCreateInfo
makeAssemblyInfo primTop =
  Vk.PipelineInputAssemblyStateCreateInfo
    { Vk.flags                  = zero
    , Vk.topology               = topology primTop
    , Vk.primitiveRestartEnable = False
    }

assemblyAndVertexInputStateInfo
  :: ∀
      ( info    :: PipelineInfo               )
      ( top     :: PrimitiveTopology Nat      )
      ( descs   :: VertexLocationDescriptions )
      ( strides :: BindingStrides             )
  . ( '(top, descs, strides) ~ GetVertexInputInfo info
    , Known (PrimitiveTopology Nat)    top
    , Known VertexLocationDescriptions descs
    , Known BindingStrides             strides
    )
  => ( Vk.PipelineInputAssemblyStateCreateInfo, Vk.PipelineVertexInputStateCreateInfo '[] )
assemblyAndVertexInputStateInfo =
  let
    primTop :: FIR.PrimitiveTopology Word32
    primTop = knownValue @top

    bindingStrides :: [ Word32 :-> Word32 ]
    bindingStrides = knownValue @strides

    attributes :: [ Word32 :-> (Word32, Word32, ImageFormat Word32) ]
    attributes = knownValue @descs

    computeVulkanFormat :: ImageFormat Word32 -> Vk.Format
    computeVulkanFormat fmt
      = fromMaybe
          ( error $ "Unsupported format " ++ show fmt ++ " used as a vertex input attribute." )
          ( simpleFormat fmt )

    vertexBindingDescriptions :: [ Vk.VertexInputBindingDescription ]
    vertexBindingDescriptions =
      bindingStrides Data.Functor.<&> \( binding :-> stride ) ->
          Vk.VertexInputBindingDescription
            { Vk.binding   = binding
            , Vk.stride    = stride
            , Vk.inputRate = Vk.VERTEX_INPUT_RATE_VERTEX
            }

    vertexAttributeDescriptions :: [ Vk.VertexInputAttributeDescription ]
    vertexAttributeDescriptions =
      attributes Data.Functor.<&> \ ( location :-> ( binding, offset, format ) ) ->
        Vk.VertexInputAttributeDescription 
          { Vk.location = location
          , Vk.binding  = binding
          , Vk.format   = computeVulkanFormat format
          , Vk.offset   = offset
          }
 
    vertexInputStateInfo :: Vk.PipelineVertexInputStateCreateInfo '[]
    vertexInputStateInfo =
      Vk.PipelineVertexInputStateCreateInfo
        { Vk.next                        = ()
        , Vk.flags                       = zero
        , Vk.vertexBindingDescriptions   = V.fromList vertexBindingDescriptions
        , Vk.vertexAttributeDescriptions = V.fromList vertexAttributeDescriptions
        }

   in (makeAssemblyInfo primTop, vertexInputStateInfo)

----------------------
---- Vulkan Utils ----
----------------------

stageFlag :: FIR.Shader %p -> Vk.ShaderStageFlagBits
stageFlag FIR.VertexShader                 = Vk.SHADER_STAGE_VERTEX_BIT
stageFlag FIR.TessellationControlShader    = Vk.SHADER_STAGE_TESSELLATION_CONTROL_BIT
stageFlag FIR.TessellationEvaluationShader = Vk.SHADER_STAGE_TESSELLATION_EVALUATION_BIT
stageFlag FIR.GeometryShader               = Vk.SHADER_STAGE_GEOMETRY_BIT
stageFlag FIR.FragmentShader               = Vk.SHADER_STAGE_FRAGMENT_BIT
stageFlag FIR.ComputeShader                = Vk.SHADER_STAGE_COMPUTE_BIT
