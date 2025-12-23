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

import Ghengin.Core.Type.Compatible ( CompatiblePipeline )

import qualified FIR.Pipeline

import qualified Data.Linear.Alias as Alias

import qualified Data.IntMap.Strict as IM

import Ghengin.Core.Render.Property
import Ghengin.Core.Shader.Pipeline ( ShaderPipeline )
import Data.Unique

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
                 ⊸  (Alias DescriptorSet, Alias ResourceMap, Ur DescriptorSetMap, Alias DescriptorPool) -- A descriptor set per frame; currently we are screwing up drawing multiple frames. Descriptor Set for the render properties.
                 ⊸  ShaderPipeline info
                 -> Unique
                 -> RenderPipeline info '[] 

  RenderProperty :: ∀ α β info
                  . PropertyBinding α
                  ⊸ RenderPipeline info β
                  ⊸ RenderPipeline info (α : β)

data SomePipeline = ∀ α β. SomePipeline (RenderPipeline α β)

makeRenderPipeline :: forall τ info tops descs strides
                    . ( PipelineConstraints info tops descs strides
                      , CompatiblePipeline τ info
                      )
                   => Alias RenderPass
                    ⊸ ShaderPipeline info
                   -> PropertyBindings τ
                    ⊸ Renderer (RenderPipeline info τ)
makeRenderPipeline = makeRenderPipelineWith defaultGraphicsPipelineSettings

-- | 'makeRenderPipeline' with explicit settings
makeRenderPipelineWith :: forall τ info tops descs strides
                    . ( PipelineConstraints info tops descs strides
                      , CompatiblePipeline τ info
                      )
                   => GraphicsPipelineSettings
                   -> Alias RenderPass
                    ⊸ ShaderPipeline info
                   -> PropertyBindings τ
                    ⊸ Renderer (RenderPipeline info τ)
makeRenderPipelineWith gps renderPass shaderPipeline props0 = Linear.do

  -- Create the descriptor sets and graphics pipeline based on the shader
  -- pipeline
  --
  -- (1) Create the uniform/storage buffers and the mapped memory
  -- (2) Create the descriptor sets from the descriptor set layout
  -- (3) Update the descriptor sets with the buffers information
  --
  -- 'createDescriptorSets' does (1) (2) and (3)
  --
  -- TODO: We need to do 'createDescriptorSets' as many times as there are frames in flight.
  --
  -- TODO: The dpool per frame in flight doesn't make any sense at the moment, for now we simply allocate from the first pool.
  -- TODO: it doesn't need to be per frame in flight, we just need two to switch between, despite the number of frames in flight
  -- BIG:TODO: Fix multiple frames in flight
  -- dsetsSet@((dsetf,dpool):|_) <- mapM (const (createPipelineDescriptorSets shaderPipeline)) [1..MAX_FRAMES_IN_FLIGHT]

  -- The pipeline should only allocate a descriptor set #0 to be used by render
  -- properties.
  -- Each Material and Mesh then allocates additional descriptor sets from this pool on creation.

  (Ur descSetMap) <- pure $ createDescriptorSetBindingsMap shaderPipeline

  logT "Creating descriptor pool"
  dpool0 <- createDescriptorPool descSetMap

  -- Allocate descriptor set #0 to be used by this render pipeline's
  -- render properties
  --
  -- The allocation returns a descriptor set to which no resources were written,
  -- a resource map must be used with 'updateDescriptorSet' to be complete.
  logT "Allocating descriptor set"
  (dset0, dpool1) <- allocateEmptyDescriptorSet 0 dpool0

  -- Make the resource map for this render pipeline using the dummyRP
  logT "Making resources"
  (resources0, props1) <- makeResources ((fromMaybe (error "DescriptorSetMap doesn't contain shader pipeline descriptors.") (IM.lookup 0 descSetMap))) props0

  -- Bind resources to descriptor set
  logT "Updating descriptor set"
  (dset1, resources1) <- updateDescriptorSet dset0 resources0

  -- Create the graphics pipeline
  logT "Creating graphics pipeline"
  (renderPass, (pipeline, dpool2))
    <- Alias.useM renderPass $
        createGraphicsPipeline gps
           shaderPipeline
           [] -- No push constants?
           dpool1

  logT "Creating reference counted"
  dpool3 <- Alias.newAlias destroyDescriptorPool dpool2
  (dpool4,dpool5) <- Alias.share dpool3
  dset2 <- Alias.newAlias (\s -> freeDescriptorSets dpool4 (make s)) dset1
  resources2 <- Alias.newAlias freeResourceMap resources1

  -- Make the unique identifier for this pipeline reference
  Ur uniq <- liftSystemIOU newUnique

  pure $ mkRP (RenderPipeline pipeline renderPass (dset2, resources2, (Ur descSetMap), dpool5) shaderPipeline uniq) props1
    where
      mkRP :: ∀ info (b :: [Type]). RenderPipeline info '[] ⊸ PropertyBindings b ⊸ RenderPipeline info b
      mkRP x GHNil = x
      mkRP x (p :## pl) = RenderProperty p (mkRP x pl)

instance HasProperties (RenderPipeline π) where

  -- Worry about performance of doing things safely later.
  -- For now, simply strive for correctness.

  properties :: RenderPipeline π τ ⊸ Renderer (PropertyBindings τ, RenderPipeline π τ)
  properties = \case
    RenderPipeline a b c d e -> pure (GHNil, RenderPipeline a b c d e)
    RenderProperty p0 xs -> Linear.do
      (p1,p2) <- Alias.share p0
      (xs', mat') <- properties xs
      pure (p1 :## xs', RenderProperty p2 mat')

  descriptors :: RenderPipeline π α ⊸ Renderer (Alias DescriptorSet, Alias ResourceMap, RenderPipeline π α)
  descriptors = \case
    RenderPipeline gpip rpass (dset0, rmap0, dmap, dpool) spip uq -> Linear.do
      ((dset1, rmap1), (dset2, rmap2)) <- Alias.share (dset0, rmap0)
      pure (dset1, rmap1, RenderPipeline gpip rpass (dset2, rmap2, dmap, dpool) spip uq)
    RenderProperty p xs -> Linear.do
      (dset, rmap, mat') <- descriptors xs
      pure (dset, rmap, RenderProperty p mat')

  puncons (RenderProperty p xs) = (p, xs)
  pcons = RenderProperty


destroyRenderPipeline :: RenderPipeline α τ ⊸ Renderer ()
destroyRenderPipeline (RenderProperty b rp) = enterD "Destroying render pipeline" Linear.do
  Alias.forget b
  destroyRenderPipeline rp
destroyRenderPipeline (RenderPipeline gp rp (a,b,(Ur _),c) _ _) = enterD "Destroying render pipeline" Linear.do
  Alias.forget a >> Alias.forget b >> Alias.forget c
  Alias.forget rp
  destroyPipeline gp

pipelineUID :: RenderPipeline α τ ⊸ (Ur Unique, RenderPipeline α τ)
pipelineUID = \case
  RenderPipeline a b c d uq -> (Ur uq, RenderPipeline a b c d uq)
  RenderProperty p xs -> case pipelineUID xs of (uq, pip) -> (uq, RenderProperty p pip)
