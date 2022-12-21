{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
module Ghengin.Render.Pipeline where

import GHC.Records
import Data.IORef

import Data.List.NonEmpty (NonEmpty((:|)))
import Data.IntMap (IntMap)
import Data.Vector (Vector)
import Foreign.Storable

import Geomancy.Mat4
import qualified Vulkan as Vk
import Apecs

import Ghengin.Component.Mesh
import {-# SOURCE #-} Ghengin.Component.Material (SomeMaterial)
import Ghengin.Shaders
import Ghengin.Utils
import Ghengin.Vulkan.RenderPass
import Ghengin.Vulkan.Pipeline
import Ghengin.Vulkan.DescriptorSet
import Ghengin.Vulkan

-- | A render pipeline consists of the descriptor sets and a graphics pipeline
-- required to render certain 'RenderPacket's
data RenderPipeline info = RenderPipeline { _index :: {-# UNPACK #-} !Int
                                          , _graphicsPipeline  :: VulkanPipeline
                                          , _renderPass        :: VulkanRenderPass
                                          , _descriptorSetsSet :: NonEmpty (Vector DescriptorSet, Vk.DescriptorPool) -- We need descriptor sets for each frame in flight
                                          , _shaderPipeline    :: GShaderPipeline info
                                          }

data SomeRenderPipeline where
  SomeRenderPipeline :: ∀ α. RenderPipeline α
                     -> IORef [SomeMaterial] -- ^ To insert materials in this list use the function that validates them
                     -> SomeRenderPipeline

-- TODO: PushConstants must also be inferred from the shader code
newtype PushConstantData = PushConstantData { pos_offset :: Mat4 } deriving Storable

-- TODO: Ensure mesh type matches vertex input
-- TODO: Shader pipeline and buffers should only be created once and reused
-- across render packets that use the same one
-- TODO: Currently we assume all our descriptor sets are Uniform buffers and
-- our buffers too but eventually Uniform will be just a constructor of a more
-- general Buffer and we should select the correct type of buffer individually.
makeRenderPipeline :: ( PipelineConstraints info tops descs strides
                      , HasField "_renderPipelines" ext (IORef [SomeRenderPipeline]) )
                   => GShaderPipeline info
                   -> Renderer ext (RenderPipeline info)
makeRenderPipeline shaderPipeline = do

  simpleRenderPass <- createSimpleRenderPass

  -- Create the descriptor sets and graphics pipeline based on the shader
  -- pipeline
  --
  -- (1) Create the uniform buffers and the mapped memory
  -- (2) Create the descriptor sets from the descriptor set layout
  -- (3) Update the descriptor sets with the buffers information
  --
  -- 'createDescriptorSets' does (1) (2) and (3)
  --
  -- We need to do 'createDescriptorSets' as many times as there are frames in flight.
  dsetsSet@(dsets:|_) <- mapM (const (createDescriptorSets shaderPipeline)) [1..MAX_FRAMES_IN_FLIGHT]

  pipeline <- createGraphicsPipeline shaderPipeline simpleRenderPass._renderPass (fmap (._descriptorSetLayout) (fst dsets)) [Vk.PushConstantRange { offset = 0 , size   = fromIntegral $ sizeOf @PushConstantData undefined , stageFlags = Vk.SHADER_STAGE_VERTEX_BIT }] -- Model transform in push constant

  renderPipelinesRef <- asks (._extension._renderPipelines)
  renderPipelines <- liftIO $ readIORef renderPipelinesRef -- Use length as index so that when creating the materials we can access the correct pipeline ref.
  let rp = RenderPipeline (length renderPipelines) pipeline simpleRenderPass dsetsSet shaderPipeline

  -- Add this render pipeline to the registered pipelines
  matsRef <- liftIO $ newIORef []
  liftIO(modifyIORef' renderPipelinesRef (<> [SomeRenderPipeline rp matsRef]))

  pure rp

-- newRenderPacket :: RenderPipeline info
--                 -> Mesh     -- TODO: Must be compatible with input type of RenderPipeline
--                 -> Material -- TODO: Must be compatible with input type of RenderPipeline
--                 -> Renderer ext RenderPacket
-- newRenderPacket rp@(RenderPipeline pipeline renderPass descriptorSetsSet _) mesh material = do


--   pure $ RenderPacket rp mesh material


