{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DisambiguateRecordFields #-}
module Ghengin.Core.Render.Pipeline where

import qualified Apecs
import Data.Kind
import Control.Lens (Lens', lens)
import Control.Monad
import Data.List.NonEmpty (NonEmpty((:|)))
import Foreign.Storable
import Geomancy.Mat4
import Ghengin.Core.Render.Property
-- TODO: Remove dependency on Ghengin non-core
import Ghengin.Shader
import Ghengin.Utils (GHList(..))
-- TODO: Remove dependency on Vulkan somehow?
import Ghengin.Vulkan
import Ghengin.Vulkan.DescriptorSet
import Ghengin.Vulkan.Pipeline
import Ghengin.Vulkan.RenderPass
import qualified Data.IntMap as IM
import qualified Data.Vector as V
import qualified Vulkan as Vk
import qualified FIR.Pipeline

-- | A render pipeline consists of the descriptor sets and a graphics pipeline
-- required to render certain 'RenderPacket's
type RenderPipeline :: FIR.Pipeline.PipelineInfo -> [Type] -> Type
data RenderPipeline info tys where

  RenderPipeline :: { _graphicsPipeline  :: VulkanPipeline
                    , _renderPass        :: VulkanRenderPass
                    , _descriptorSetsSet :: NonEmpty (DescriptorSet, DescriptorPool) -- A descriptor set per frame; currently we are screwing up drawing multiple frames. Descriptor Set for the render properties.
                    , _shaderPipeline    :: ShaderPipeline info
                    } -> RenderPipeline info '[] 

  RenderProperty :: ∀ α β info
                  . PropertyBinding α
                 -> RenderPipeline info β
                 -> RenderPipeline info (α : β)

data SomePipeline = ∀ α β. SomePipeline (RenderPipeline α β)
instance Apecs.Component SomePipeline where
  type Storage SomePipeline = Apecs.Map SomePipeline
{-# DEPRECATED makeRenderPipeline "Storage should be a cache" #-}

-- TODO: PushConstants must also be inferred from the shader code
-- Add them as possible alternative to descritpor set #2?
newtype PushConstantData = PushConstantData { pos_offset :: Mat4 } deriving Storable

-- TODO: Ensure mesh type matches vertex input
-- TODO: Shader pipeline and buffers should only be created once and reused
-- across render packets that use the same one
-- TODO: Currently we assume all our descriptor sets are Uniform buffers and
-- our buffers too but eventually Uniform will be just a constructor of a more
-- general Buffer and we should select the correct type of buffer individually.
makeRenderPipeline :: forall τ info tops descs strides χ
                    . ( PipelineConstraints info tops descs strides )
                   => ShaderPipeline info
                   -> (RenderPipeline info '[] -> RenderPipeline info τ)
                   -> Renderer χ (RenderPipeline info τ)
makeRenderPipeline shaderPipeline mkRP = do

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
  -- BIG:TODO: Fix multiple frames in flight
  dsetsSet@((dsetf,dpool):|_) <- mapM (const (createPipelineDescriptorSets shaderPipeline)) [1..MAX_FRAMES_IN_FLIGHT]

  -- TODO: Merge this implementation with the one in Core.Material
  dummySet <- dsetf mempty 
  let dummyRP :: RenderPipeline info τ = mkRP $ RenderPipeline undefined undefined [(dummySet, dpool)] shaderPipeline

  -- Make the resource map for this render pipeline using the dummyRP
  resources <- makeResources (renderProperties @info @τ dummyRP)

  actualSets <- mapM (\(f,p) -> (,p) <$> f resources) dsetsSet

  pipeline <- createGraphicsPipeline shaderPipeline simpleRenderPass._renderPass (V.fromList $ fmap fst (IM.elems dpool._set_bindings)) [Vk.PushConstantRange { offset = 0 , size   = fromIntegral $ sizeOf @PushConstantData undefined , stageFlags = Vk.SHADER_STAGE_VERTEX_BIT }] -- Model transform in push constant

  pure $ mkRP $ RenderPipeline pipeline simpleRenderPass actualSets shaderPipeline

createPipelineDescriptorSets :: ShaderPipeline info -> Renderer ext (ResourceMap -> Renderer ext DescriptorSet, DescriptorPool)
createPipelineDescriptorSets pp = do

  -- The pipeline should only allocate a descriptor set #0 to be used by render
  -- properties

  let dsetmap = createDescriptorSetBindingsMap pp
  dpool <- createDescriptorPool dsetmap

  -- Allocate descriptor set #0 to be used by this render pipeline's
  -- render properties The resources themselves will be written to the
  -- descriptor set by the function effectively creating the render pipeline
  -- (which has knowledge of the render properties to allocate the correct type
  -- of resource for each descriptor)
  --
  -- The allocation actually returns a closure which must be applied to a
  -- resource map to be complete
  dsetf <- allocateDescriptorSet 0 dpool 

  pure (dsetf, dpool)

instance HasProperties (RenderPipeline π) where
  properties = \case
    RenderPipeline {} -> GHNil
    RenderProperty x xs -> x :## properties xs

  descriptorSet :: Lens' (RenderPipeline π α) DescriptorSet
  descriptorSet = lens get' set' where
    get' :: RenderPipeline π α -> DescriptorSet
    get' = \case
      RenderPipeline {_descriptorSetsSet = ((dset,_) :| _)} -> dset
      RenderProperty _ xs -> get' xs

    set' :: RenderPipeline π α -> DescriptorSet -> RenderPipeline π α
    set' p d = case p of
      rp@RenderPipeline {_descriptorSetsSet = ((_,dpool) :| fms)} -> rp{_descriptorSetsSet = (d,dpool) :| fms}
      RenderProperty x m' -> RenderProperty x (set' m' d)

  puncons :: RenderPipeline π (α:β) -> (PropertyBinding α, RenderPipeline π β)
  puncons (RenderProperty p xs) = (p, xs)

  pcons :: PropertyBinding α -> RenderPipeline π β -> RenderPipeline π (α:β)
  pcons = RenderProperty

renderProperties :: RenderPipeline info τ -> PropertyBindings τ
renderProperties = \case
  RenderPipeline {} -> GHNil
  RenderProperty x xs -> x :## renderProperties xs

destroyRenderPipeline :: RenderPipeline α τ -> Renderer ext ()
destroyRenderPipeline (RenderProperty _ rp) = destroyRenderPipeline rp
destroyRenderPipeline (RenderPipeline gp rp dss _) = do
  forM_ dss $ \(dset, dpool) -> do
    destroyDescriptorPool dpool
    -- TODO: Destroy descriptor set resources if they are not shared (for now,
    -- this is only set #0, so this is always fine since there is nothing
    -- shared here)
    -- TODO: This now crashes the program
    destroyDescriptorSet dset
  destroyRenderPass rp
  destroyPipeline gp

graphicsPipeline :: Lens' (RenderPipeline α τ) VulkanPipeline
graphicsPipeline = lens get' set'
  where
    get' :: RenderPipeline α τ -> VulkanPipeline
    get' RenderPipeline{_graphicsPipeline} = _graphicsPipeline
    get' (RenderProperty _ rp) = get' rp

    set' :: RenderPipeline α τ -> VulkanPipeline -> RenderPipeline α τ
    set' rp@RenderPipeline{} newrp = rp{_graphicsPipeline = newrp}
    set' (RenderProperty x rp) newrp = RenderProperty x (set' rp newrp)

renderPass :: Lens' (RenderPipeline α τ) VulkanRenderPass
renderPass = lens get' set'
  where
    get' :: RenderPipeline τ α -> VulkanRenderPass
    get' RenderPipeline{_renderPass} = _renderPass
    get' (RenderProperty _ rp) = get' rp

    set' :: RenderPipeline τ α -> VulkanRenderPass -> RenderPipeline τ α
    set' rp@RenderPipeline{} newrp = rp{Ghengin.Core.Render.Pipeline._renderPass = newrp}
    set' (RenderProperty x rp) newrp = RenderProperty x (set' rp newrp)

descriptorPool :: RenderPipeline α τ -> DescriptorPool
descriptorPool RenderPipeline{_descriptorSetsSet=(_,dpool) :| _} = dpool
descriptorPool (RenderProperty _ rp) = descriptorPool rp

