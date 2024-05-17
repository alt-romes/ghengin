{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Ghengin.Core.Render.Pipeline where

-- These don't have to be hardcoded, but what used to be hardcoded to them must be updated
-- import qualified Apecs
-- import Geomancy.Mat4 ( Mat4 )
-- import Control.Lens (Lens', lens)

import Data.V.Linear (make)
import Ghengin.Core.Prelude as Linear

import Data.Typeable

import Ghengin.Core.Type.Compatible ( CompatiblePipeline )

-- What I really want to do with the methods below is create hsigs for each of
-- them individually.  We will not need the freeing/destruction functions
-- anymore since most things will be reference counted in the linear IO monad,
-- and the free function will be an internal implementation detail
-- import Ghengin.Vulkan.DescriptorSet
--     ( allocateDescriptorSet,
--       createDescriptorPool,
--       createDescriptorSetBindingsMap,
--       destroyDescriptorPool,
--       destroyDescriptorSet,
--       DescriptorPool(_set_bindings),
--       DescriptorSet,
--       ResourceMap )
-- import Ghengin.Vulkan.Pipeline
--     ( createGraphicsPipeline,
--       destroyPipeline,
--       PipelineConstraints,
--       VulkanPipeline )
-- import Ghengin.Vulkan.RenderPass
--     ( createSimpleRenderPass,
--       destroyRenderPass,
--       VulkanRenderPass(_renderPass) )

import qualified FIR.Pipeline

import qualified Vulkan as Vk

import qualified Data.Linear.Alias as Alias

import Ghengin.Core.Render.Property
import Ghengin.Core.Shader.Pipeline ( ShaderPipeline )

import Ghengin.Core.Renderer.Kernel
import Ghengin.Core.Renderer.Pipeline
import Ghengin.Core.Renderer.RenderPass
import Ghengin.Core.Renderer.DescriptorSet
import Ghengin.Core.Log


-- | A render pipeline consists of the descriptor sets and a graphics pipeline
-- required to render certain 'RenderPacket's
type RenderPipeline :: FIR.Pipeline.PipelineInfo -> [Type] -> Type
data RenderPipeline info tys where

  RenderPipeline :: RendererPipeline Graphics -- ^ The graphics pipeline underlying this render pipeline. Can a graphics pipeline be shared amongst Render Pipelines such that this field needs to be ref counted?
                 ⊸  Alias RenderPass -- ^ A reference counted reference to a render pass, since we might share render passes amongst pipelines
                 ⊸  (Alias DescriptorSet, Alias ResourceMap, Alias DescriptorPool) -- A descriptor set per frame; currently we are screwing up drawing multiple frames. Descriptor Set for the render properties.
                 ⊸  ShaderPipeline info
                 -> RenderPipeline info '[] 

  RenderProperty :: ∀ α β info
                  . PropertyBinding α
                  ⊸ RenderPipeline info β
                  ⊸ RenderPipeline info (α : β)

data SomePipeline = ∀ α β. Typeable β => SomePipeline (RenderPipeline α β) -- ROMES:TODO Can I not have Typeable here? I think we need it to make comparisons on the type of the SomePipelineRef

-- TODO: PushConstants must also be inferred from the shader code
-- Add them as possible alternative to descritpor set #2?
-- ROMES:TODO:PushConstants automatically used alongside dset #2
-- How can I delete this and this still is correct? :D
-- newtype PushConstantData = PushConstantData { pos_offset :: () -- Mat4
--                                             } deriving Storable

-- Shader pipeline and buffers are only be created once and reused across
-- render packets that use the same one (Note that render packets store
-- references to these things).
-- TODO: Currently we assume all our descriptor sets are Uniform buffers and
-- our buffers too but eventually Uniform will be just a constructor of a more
-- general Buffer and we should select the correct type of buffer individually.
makeRenderPipeline :: forall τ info tops descs strides
                    . ( PipelineConstraints info tops descs strides
                      , CompatiblePipeline τ info
                      )
                   => ShaderPipeline info
                   -> PropertyBindings τ
                    ⊸ Renderer (RenderPipeline info τ)
makeRenderPipeline shaderPipeline props0 = Linear.do

  logT "Making render pass"
  !simpleRenderPass <- createSimpleRenderPass

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
  -- dsetsSet@((dsetf,dpool):|_) <- mapM (const (createPipelineDescriptorSets shaderPipeline)) [1..MAX_FRAMES_IN_FLIGHT]

  -- The pipeline should only allocate a descriptor set #0 to be used by render
  -- properties

  logT "Creating descriptor pool"
  dpool0 <- createDescriptorPool shaderPipeline

  -- Allocate descriptor set #0 to be used by this render pipeline's
  -- render properties
  --
  -- The allocation returns a descriptor set to which no resources were written,
  -- a resource map must be used with 'updateDescriptorSet' to be complete.
  logT "Allocating descriptor set"
  (dset0, dpool1) <- allocateEmptyDescriptorSet 0 dpool0

  -- Make the resource map for this render pipeline using the dummyRP
  logT "Making resources"
  (resources0, props1) <- makeResources props0

  -- Bind resources to descriptor set
  logT "Updating descriptor set"
  (dset1, resources1) <- updateDescriptorSet dset0 resources0

  -- Create the graphics pipeline
  logT "Creating graphics pipeline"
  (pipeline, simpleRenderPass2, dpool2) <- createGraphicsPipeline
                                     shaderPipeline simpleRenderPass dpool1
                                     -- ROMES:TODO: Update push constants! This is not it! (It's hardcoded, and things are never actually pushed)
                                     [Vk.PushConstantRange { offset = 0 , size = 64 :: Word32, stageFlags = Vk.SHADER_STAGE_VERTEX_BIT }] -- Model transform in push constant

  logT "Creating reference counted"
  dpool3 <- Alias.newAlias destroyDescriptorPool dpool2
  (dpool4,dpool5) <- Alias.share dpool3
  dset2 <- Alias.newAlias (\s -> freeDescriptorSets dpool4 (make s)) dset1
  resources2 <- Alias.newAlias freeResourceMap resources1
  simpleRenderPass3 <- Alias.newAlias destroyRenderPass simpleRenderPass2

  pure $ mkRP (RenderPipeline pipeline simpleRenderPass3 (dset2, resources2, dpool5) shaderPipeline) props1
    where
      mkRP :: ∀ info (b :: [Type]). RenderPipeline info '[] ⊸ PropertyBindings b ⊸ RenderPipeline info b
      mkRP x GHNil = x
      mkRP x (p :## pl) = RenderProperty p (mkRP x pl)

instance HasProperties (RenderPipeline π) where

  -- Worry about performance of doing things safely later.
  -- For now, simply strive for correctness.

  properties :: RenderPipeline π τ ⊸ Renderer (PropertyBindings τ, RenderPipeline π τ)
  properties = \case
    RenderPipeline a b c d -> pure (GHNil, RenderPipeline a b c d)
    RenderProperty p0 xs -> Linear.do
      (p1,p2) <- Alias.share p0
      (xs', mat') <- properties xs
      pure (p1 :## xs', RenderProperty p2 mat')

  descriptors :: RenderPipeline π α ⊸ Renderer (Alias DescriptorSet, Alias ResourceMap, RenderPipeline π α)
  descriptors = \case
    RenderPipeline gpip rpass (dset0, rmap0, dpool) spip -> Linear.do
      ((dset1, rmap1), (dset2, rmap2)) <- Alias.share (dset0, rmap0)
      pure (dset1, rmap1, RenderPipeline gpip rpass (dset2, rmap2, dpool) spip)
    RenderProperty p xs -> Linear.do
      (dset, rmap, mat') <- descriptors xs
      pure (dset, rmap, RenderProperty p mat')

  puncons (RenderProperty p xs) = (p, xs)
  pcons = RenderProperty


destroyRenderPipeline :: RenderPipeline α τ ⊸ Renderer ()
destroyRenderPipeline (RenderProperty b rp) = enterD "Destroying render pipeline" Linear.do
  Alias.forget b
  destroyRenderPipeline rp
destroyRenderPipeline (RenderPipeline gp rp (a,b,c) _) = enterD "Destroying render pipeline" Linear.do
  Alias.forget a >> Alias.forget b >> Alias.forget c
  Alias.forget rp
  destroyPipeline gp


