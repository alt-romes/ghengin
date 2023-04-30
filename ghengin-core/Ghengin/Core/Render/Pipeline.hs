{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Ghengin.Core.Render.Pipeline where

-- These don't have to be hardcoded, but what used to be hardcoded to them must be updated
-- import qualified Apecs
-- import Geomancy.Mat4 ( Mat4 )
-- import Control.Lens (Lens', lens)

import qualified Prelude
import Prelude.Linear
import Control.Functor.Linear as Linear
import Control.Monad.IO.Class.Linear
import qualified Unsafe.Linear as Unsafe

import Data.Typeable
import Data.Kind
import Control.Monad ( forM_ )
import Data.List.NonEmpty
import Foreign.Storable ( Storable(sizeOf) )

-- TODO: Remove dependency on Vulkan somehow?
-- ROMES:TODO: What are we importing from here?
-- import Ghengin.Vulkan
--
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

import qualified Data.IntMap as IM
import qualified Data.Vector as V
import qualified FIR.Pipeline

import qualified Vulkan as Vk

import Data.Counted as Counted
import qualified Data.Counted.Unsafe as Unsafe.Counted

import Ghengin.Core.Render.Property
import Ghengin.Core.Shader.Pipeline ( ShaderPipeline )

import Ghengin.Core.Renderer.Kernel
import Ghengin.Core.Renderer.Pipeline
import Ghengin.Core.Renderer.RenderPass
import Ghengin.Core.Renderer.DescriptorSet


-- | A render pipeline consists of the descriptor sets and a graphics pipeline
-- required to render certain 'RenderPacket's
type RenderPipeline :: FIR.Pipeline.PipelineInfo -> [Type] -> Type
data RenderPipeline info tys where

  RenderPipeline :: RendererPipeline Graphics -- ^ The graphics pipeline underlying this render pipeline. Can a graphics pipeline be shared amongst Render Pipelines such that this field needs to be ref counted?
                 ⊸  RefC RenderPass -- ^ A reference counted reference to a render pass, since we might share render passes amongst pipelines
                 -- ⊸  NonEmpty (DescriptorSet, DescriptorPool) -- (TODO:REFCOUNT THEM) A descriptor set per frame; currently we are screwing up drawing multiple frames. Descriptor Set for the render properties.
                 ⊸  (RefC DescriptorSet, RefC ResourceMap, DescriptorPool) -- (TODO:REFCOUNT THEM) A descriptor set per frame; currently we are screwing up drawing multiple frames. Descriptor Set for the render properties.
                 ⊸  ShaderPipeline info
                 %p -> RenderPipeline info '[] 

  RenderProperty :: ∀ α β info
                  . PropertyBinding α
                 -> RenderPipeline info β
                 ⊸  RenderPipeline info (α : β)

data SomePipeline = ∀ α β. Typeable β => SomePipeline (RenderPipeline α β) -- ROMES:TODO Can I not have Typeable here?

-- ROMES:TODO: Apecs instances should be provided out of Core, despite being
-- for these types -- they'll exist as orphan instances in Ghengin...
-- instance Apecs.Component SomePipeline where
--   type Storage SomePipeline = Apecs.Map SomePipeline
-- {-# DEPRECATED makeRenderPipeline "Storage should be a cache" #-}

-- TODO: PushConstants must also be inferred from the shader code
-- Add them as possible alternative to descritpor set #2?
-- ROMES:TODO:PushConstants automatically used alongside dset #2
newtype PushConstantData = PushConstantData { pos_offset :: () -- Mat4
                                            } deriving Storable

-- TODO: Ensure mesh type matches vertex input
-- TODO: Shader pipeline and buffers should only be created once and reused
-- across render packets that use the same one
-- TODO: Currently we assume all our descriptor sets are Uniform buffers and
-- our buffers too but eventually Uniform will be just a constructor of a more
-- general Buffer and we should select the correct type of buffer individually.
makeRenderPipeline :: forall τ info tops descs strides
                    . ( PipelineConstraints info tops descs strides )
                   => ShaderPipeline info
                   -> (RenderPipeline info '[] ⊸ RenderPipeline info τ)
                   -> Renderer (RenderPipeline info τ)
makeRenderPipeline shaderPipeline mkRP = Linear.do

  simpleRenderPass <- createSimpleRenderPass
  (srpass1, srpass2) <- share simpleRenderPass

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

  dpool0 <- createDescriptorPool shaderPipeline

  -- Allocate descriptor set #0 to be used by this render pipeline's
  -- render properties
  --
  -- The allocation returns a descriptor set to which no resources were written,
  -- a resource map must be used with 'updateDescriptorSet' to be complete.
  (dset0, dpool1) <- allocateEmptyDescriptorSet 0 dpool0

  -- We need a dummy pipeline to analyse the structure and generate the resources to fill the actually pipeline
  let dummyRP :: RenderPipeline info τ = mkRP $ RenderPipeline undefined undefined undefined shaderPipeline -- (do i need empty dset?)

  -- Make the resource map for this render pipeline using the dummyRP
  resources0 <- makeResources (case properties @(RenderPipeline info) @τ dummyRP of (Ur pbs, _) -> pbs)

  -- Bind resources to descriptor set
  (dset1, resources1) <- updateDescriptorSet dset0 resources0

  -- Create the graphics pipeline
  pipeline <- createGraphicsPipeline shaderPipeline srpass1
                                     -- (V.fromList $ fmap fst (IM.elems dpool._set_bindings))
                                     [Vk.PushConstantRange { offset = 0 , size   = fromIntegral $ sizeOf @PushConstantData undefined , stageFlags = Vk.SHADER_STAGE_VERTEX_BIT }] -- Model transform in push constant

  dset2 <- Counted.new freeDescriptorSet dset1
  resources2 <- Counted.new freeResourceMap resources1

  pure $ mkRP $ RenderPipeline pipeline srpass2 (dset2, resources2, dpool1) shaderPipeline

instance HasProperties (RenderPipeline π) where
  properties :: RenderPipeline π τ ⊸ (Ur (PropertyBindings τ), RenderPipeline π τ)
  properties = Unsafe.toLinear $ \rp -> (unsafeGo rp, rp) where
    unsafeGo :: RenderPipeline π τ -> Ur (PropertyBindings τ)
    unsafeGo = \case
      RenderPipeline {} -> Ur GHNil
      RenderProperty x xs -> case unsafeGo xs of Ur xs' -> Ur (x :## xs')

  descriptors :: RenderPipeline π α ⊸ Renderer (RefC DescriptorSet, RefC ResourceMap, RenderPipeline π α)
  descriptors = Unsafe.toLinear (\rp -> unsafeGo rp >>= \case
                                          (dset, rmap) -> pure (dset, rmap, rp)) where
    -- Note it's not linear on the pipeline, unsafe! -- but we return the original reference
    unsafeGo :: RenderPipeline π α -> Renderer (RefC DescriptorSet, RefC ResourceMap)
    unsafeGo = \case
      RenderPipeline gpip rpass (dset, rmap, dpool) spip ->
        -- In descriptors, we're returning the whole render pipeline unchanged.
        -- To return DescriptorSet and ResourceMap we increment their reference
        -- counts because we unsafely keep one reference in the original
        -- renderpipeline we return
        (,) <$> Unsafe.Counted.inc dset <*> Unsafe.Counted.inc rmap

      -- TODO: This will possibly have to become linear
      RenderProperty _ xs -> unsafeGo xs

  puncons (RenderProperty p xs) = (Ur p, xs)
  pcons = Unsafe.toLinear RenderProperty


-- destroyRenderPipeline :: RenderPipeline α τ
--                        ⊸ Renderer ()
-- destroyRenderPipeline (RenderProperty _ rp) = destroyRenderPipeline rp
-- destroyRenderPipeline (RenderPipeline gp rp dss _) = do
--   forM_ dss $ \(dset, dpool) -> do
--     destroyDescriptorPool dpool
--     -- TODO: Destroy descriptor set resources if they are not shared (for now,
--     -- this is only set #0, so this is always fine since there is nothing
--     -- shared here)
--     -- TODO: This now crashes the program
--     destroyDescriptorSet dset
--   destroyRenderPass rp
--   destroyPipeline gp


