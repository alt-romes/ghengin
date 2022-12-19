{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-} -- instance Has w m RenderPacket
module Ghengin.Render.Packet where
 
import GHC.Records

import Data.List.NonEmpty (NonEmpty((:|)))
import Data.IntMap (IntMap)
import Data.Vector (Vector)
import Foreign.Storable

import Geomancy.Mat4
import qualified Vulkan as Vk
import Apecs

import Ghengin.Component.Mesh
import Ghengin.Shaders
import Ghengin.Utils
import Ghengin.Vulkan.RenderPass
import Ghengin.Vulkan.Pipeline
import Ghengin.Vulkan.DescriptorSet
import Ghengin.Vulkan

data Material

data RenderPacket = RenderPacket { _renderPipeline :: RenderPipeline
                                 , _renderMesh     :: Mesh
                                 , _renderMaterial :: Material
                                 }

instance Component RenderPacket where
  type Storage RenderPacket = Map RenderPacket

-- TODO: Instructions on having a World record with "meshes"

instance (Monad m, HasField "renderPackets" w (Storage RenderPacket)) => Has w m RenderPacket where
  getStore = SystemT (asks (.renderPackets))



-- | A render pipeline consists of the descriptor sets and a graphics pipeline
-- required to render certain 'RenderPacket's
data RenderPipeline = RenderPipeline { _graphicsPipeline  :: VulkanPipeline
                                     , _renderPass        :: VulkanRenderPass
                                     , _descriptorSetsSet :: NonEmpty (Vector DescriptorSet, Vk.DescriptorPool) -- We need descriptor sets for each frame in flight
                                     , _shaderPipeline    :: GShaderPipeline
                                     }

-- TODO: PushConstants must also be inferred from the shader code
newtype PushConstantData = PushConstantData { pos_offset :: Mat4 } deriving Storable

-- TODO: Ensure mesh type matches vertex input
-- TODO: Shader pipeline and buffers should only be created once and reused
-- across render packets that use the same one
-- TODO: Currently we assume all our descriptor sets are Uniform buffers and
-- our buffers too but eventually Uniform will be just a constructor of a more
-- general Buffer and we should select the correct type of buffer individually.
makeRenderPipeline :: GShaderPipeline
                   -> Renderer RenderPipeline
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
  pipeline <- createGraphicsPipeline shaderPipeline simpleRenderPass._renderPass (fmap (._descriptorSetLayout) (fst dsets)) [Vk.PushConstantRange { offset = 0 , size   = fromIntegral $ sizeOf @PushConstantData undefined , stageFlags = Vk.SHADER_STAGE_VERTEX_BIT }]

  pure $ RenderPipeline pipeline simpleRenderPass dsetsSet shaderPipeline


newRenderPacket :: RenderPipeline
                 -> Mesh     -- TODO: Must be compatible with input type of RenderPipeline
                 -> Material -- TODO: Must be compatible with input type of RenderPipeline
                 -> Renderer RenderPacket
newRenderPacket rp@(RenderPipeline pipeline renderPass descriptorSetsSet _) mesh material = do


  pure $ RenderPacket rp mesh material



