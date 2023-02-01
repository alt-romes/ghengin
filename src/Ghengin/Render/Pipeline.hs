{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
module Ghengin.Render.Pipeline where

import qualified Data.IntMap as IM
import qualified Data.Vector as V
import Control.Monad
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Vector (Vector)
import Foreign.Storable

import Geomancy.Mat4
import qualified Vulkan as Vk

import Ghengin.Shaders
import Ghengin.Vulkan.Buffer
import Ghengin.Vulkan.RenderPass
import Ghengin.Vulkan.Pipeline
import Ghengin.Vulkan.DescriptorSet
import Ghengin.Vulkan

-- | A render pipeline consists of the descriptor sets and a graphics pipeline
-- required to render certain 'RenderPacket's
data RenderPipeline info = RenderPipeline { _graphicsPipeline  :: VulkanPipeline
                                          , _renderPass        :: VulkanRenderPass
                                          , _descriptorSetsSet :: NonEmpty (Vector DescriptorSet, DescriptorPool) -- We need descriptor sets for each frame in flight
                                          , _shaderPipeline    :: GShaderPipeline info
                                          }

-- TODO: PushConstants must also be inferred from the shader code
newtype PushConstantData = PushConstantData { pos_offset :: Mat4 } deriving Storable

-- TODO: Ensure mesh type matches vertex input
-- TODO: Shader pipeline and buffers should only be created once and reused
-- across render packets that use the same one
-- TODO: Currently we assume all our descriptor sets are Uniform buffers and
-- our buffers too but eventually Uniform will be just a constructor of a more
-- general Buffer and we should select the correct type of buffer individually.
makeRenderPipeline :: ( PipelineConstraints info tops descs strides )
                   => GShaderPipeline info
                   -> Renderer χ (RenderPipeline info)
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
  --
  -- TODO: The dpool per frame in flight doesn't make any sense at the moment, for now we simply allocate from the first pool.
  -- TODO: it doesn't need to be per frame in flight, we just need two to switch between, despite the number of frames in flight
  dsetsSet@(dsets:|_) <- mapM (const (createPipelineDescriptorSets shaderPipeline)) [1..MAX_FRAMES_IN_FLIGHT]

  pipeline <- createGraphicsPipeline shaderPipeline simpleRenderPass._renderPass (V.fromList $ fmap fst (IM.elems $ (snd dsets)._set_bindings)) [Vk.PushConstantRange { offset = 0 , size   = fromIntegral $ sizeOf @PushConstantData undefined , stageFlags = Vk.SHADER_STAGE_VERTEX_BIT }] -- Model transform in push constant

  pure $ RenderPipeline pipeline simpleRenderPass dsetsSet shaderPipeline

createPipelineDescriptorSets :: GShaderPipeline info -> Renderer ext (Vector DescriptorSet, DescriptorPool)
createPipelineDescriptorSets pp = do

  -- The pipeline should only allocate a descriptor set #0, it needs no else

  -- Currently we allocate unnecessary #1 sets: the used ones are allocated on a per-material basis
  let dsetmap = createDescriptorSetBindingsMap pp
  dpool <- createDescriptorPool dsetmap
  dsetf <- allocateDescriptorSet 0 dpool -- allocate descriptor set #0

  let makeResource :: BindingsMap -> Renderer ext ResourceMap
      makeResource = traverse (\case (Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER,size,_ssf) -> UniformResource <$> createMappedBuffer size Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER)

  -- Write with resources for descriptor set #0
  dset <- dsetf =<< makeResource (dsetmap IM.! 0)

  pure ([dset], dpool)



destroyRenderPipeline :: RenderPipeline α -> Renderer ext ()
destroyRenderPipeline (RenderPipeline gp rp dss _) = do
  forM_ dss $ \(dsets, dpool) -> do
    destroyDescriptorPool dpool
    -- TODO: Destroy descriptor set resources if they are not shared (for now,
    -- this is only set #0, so this is always fine since there is nothing
    -- shared here)
    mapM_ destroyDescriptorSet dsets
  destroyRenderPass rp
  destroyPipeline gp

