{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLists #-}
module Ghengin.Render where

import Ghengin.Core.Prelude hiding (insert)
import Ghengin.Core.Log
import qualified Prelude
import Control.Functor.Linear as Linear
-- import Data.Maybe

import qualified Data.Vector as V
import qualified Vulkan as Vk
import Geomancy.Mat4

import Apecs (Has, cfold)
import qualified Apecs as Apecs

import Ghengin.Component.Camera
import Ghengin.Component.Transform
import Ghengin.Vulkan.Renderer.Command
import Ghengin.Vulkan.Renderer.Buffer
import Ghengin.Vulkan.Renderer.Pipeline
import Ghengin.Vulkan.Renderer.DescriptorSet
import Ghengin.Vulkan.Renderer.RenderPass
import Ghengin.Vulkan.Renderer.Kernel
import Ghengin.Vulkan.Renderer
import Ghengin.Core.Renderer

import Ghengin.Core.Render.Packet
import Ghengin.Core.Mesh
import Ghengin.Core.Render.Property
import Ghengin.Core.Material

import Ghengin.Core.Type.Utils (nat)

import qualified Ghengin.DearImGui as IM

import Ghengin.Scene.Graph
import Ghengin.Render.Queue
import {-# SOURCE #-} Ghengin.World (World)
import {-# SOURCE #-} Ghengin (Ghengin)

import qualified Data.Linear.Alias as Alias
import qualified Data.Linear.Alias.Unsafe as Unsafe.Alias
import qualified Unsafe.Linear as Unsafe

type RenderConstraints w = ( Has (World w) Renderer Transform
                           , Has (World w) Renderer Camera
                           , Has (World w) Renderer ModelMatrix
                           , Has (World w) Renderer Parent

                           -- Core render constraints
                           -- , Apecs.Get (World w) Renderer RenderPacket
                           -- , Apecs.Get (World w) Renderer SomePipeline
                           -- , Apecs.Get (World w) Renderer SomeMaterial
                           )


-- ROMES:TODO: Move 'render' to ghengin-vulkan, and related world things like RenderPackets and Pipelines and Materials

-- TODO: Deferred rendering!

{-
Note [Renderer]
~~~~~~~~~~~~~~~

The renderer (see 'render') first traverses the scene graph and fills the
render queue, and then traverses the render queue and issues a draw call for
each renderable entity.

 -}

-- | 'render' first traverses the scene graph and fills the render queue, and
-- then traverses the render queue and issues a draw call for each renderable
-- entity.
--
-- Additionally, see notes:
--  * Note [Renderer] (TODO)
--  * Note [Render Queue]
--  * Note [Scene Graph]
--  * Note [Renderable entities]
render :: RenderConstraints w
       => Int -- frame identifier (frame count)
       -> RenderQueue ModelMatrix
       -> Renderer (Ur ())
render i renderQueue = enterD "render" $ do

  -- ROMES:TODO: This might cause flickering once every frame overflow due to ... overflows?
  -- Need to consider what happens if it overflows. For now, good enough, it's
  -- unlikely the frame count overflows, with 60 frames per second the game
  -- would have to run for years to overflow a 64 bit integer
  let frameIndex = i `mod` (nat @MAX_FRAMES_IN_FLIGHT_T)

  -- Some required variables
  Ur extent <- getRenderExtent
  let viewport = viewport' extent
      scissor  = scissor' extent

  {-
     Here's a rundown of the draw function for each frame in flight:

     ∀ pipeline ∈ registeredPipelines do
        bind global descriptor set at set #0

        ∀ material ∈ pipeline.registeredMaterials do
          bind material descriptor set at set #1

          ∀ object that uses material do
            
            bind object descriptor set at set #2
            bind object model (vertex buffer)
            draw object

      This makes the descriptor set #0 bound once per pipeline
                 the descriptor set #1 bound once per material
                 the descriptor set #2 bound once per object

      The data bound globally for the pipeline must be compatible with the descriptor set #0 layout
      All materials bound in a certain pipeline must be compatible with the descriptor set #1 layout
      All object data bound in a certain pipeline must be compatible with descriptor set #2 layout
      All object's vertex buffers bound in a certain pipeline must be compatible with the vertex input of that pipeline

      In practice, the code doesn't look exactly like this. We bind the
      descriptor sets and pipelines linearly because the ordering of the render
      queue ensures that the GPU state changes will be minimized and hence the
      iteration will actually look a bit like the described above
   -}

  withCurrentFramePresent frameIndex $ \cmdBuffer currentImage -> enterD "closure passed to withCurrentFramePresent" $ Linear.do

    cmdBuffer' <- recordCommand cmdBuffer $ Linear.do

      -- Now, render the renderable entities from the render queue in the given order.
      -- If everything works as expected, if we blindly bind the descriptor sets as
      -- they come, we should bind the pipeline once and each material once.
      traverseRenderQueue
        renderQueue
        -- Whenever we have a new pipeline, start its renderpass (lifting RenderPassCmd to Command)
        (\(SomePipelineRef (Ref pp'_ref)) m -> enterD "Lifting things" Linear.do
          Ur (SomePipeline pp') <- lift $ Apecs.get (Apecs.Entity pp'_ref)
          (rp, _pp') <- getRenderPass pp'
          (rp', ()) <- Alias.useM rp $ Unsafe.toLinear $ \rp' -> Linear.do
            renderPassCmd rp'._renderPass (rp'._framebuffers V.! currentImage) extent m -- nice and unsafe
            pure (rp', ())

          lift $ lift $ Alias.forget rp'

          Unsafe.toLinear (\_ -> pure ()) _pp' -- forget pipeline??
        )
        (\(SomePipelineRef (Ref pipeline_ref)) -> enterD "Pipeline changed" Linear.do
            Ur (SomePipeline pipeline) <- lift $ Apecs.get (Apecs.Entity pipeline_ref)

            logT "Binding pipeline"
            Ur (graphicsPipeline, _p) <- pure $ unsafeGraphicsPipeline pipeline

            -- The render pass for this pipeline has been bound already. Later on the render pass might not be necessarily coupled to the pipeline
            -- Bind the pipeline
            gppp' <- bindGraphicsPipeline (graphicsPipeline._pipeline)
            setViewport viewport
            setScissor  scissor

            lift (lift (descriptors pipeline)) >>= \case-- TODO: Fix frames in flight... here it migth be crrect actylly, descriptor sets are shared, only one frame is being drawn at the time despite the double buffering
              (dset, rmap, pipeline') -> Linear.do

                -- These render properties are necessarily compatible with this
                -- pipeline in the set #0, so the 'descriptorSetBinding' buffer
                -- will always be valid to write with the corresponding
                -- material binding
                (rmap', pipeline'') <- lift $ Alias.useM rmap (\rmap' -> writePropertiesToResources rmap' pipeline')
                
                -- Bind descriptor set #0
                (dset', pLayout) <- Alias.useM dset (Unsafe.toLinear $ \dset' -> Linear.do
                  (pLayout', vkdset) <-
                    bindGraphicsDescriptorSet graphicsPipeline._pipelineLayout 0 dset'._descriptorSet
                  pure (Unsafe.toLinear (\_ -> dset') vkdset, pLayout') --forget vulkan dset
                                              )

                -- Dangerous!! Could be forgetting values that need to be
                -- reference-counted forgotten, or otherwise references become
                -- obsolete. These values are probably shared before being
                -- returned from 'descriptors' T_T.
                -- OK, I made it less bad, now I'm only forgetting stuff I got unsafely...
                -- And the pipeline T_T
                Unsafe.toLinearN @3 (\_ _ _ -> pure ()) pipeline'' pLayout gppp' -- The pipeline is still in the Apecs store. Really, these functions should have no Unsafes and in that case all would be right (e.g. the resource passed to this function would have to be freed in this function, guaranteeing that it is reference counted or something?....

                lift $ lift $ Linear.do
                  Alias.forget dset'
                  Alias.forget rmap'

            pure $ SomePipeline pipeline
          )
        (\(SomePipeline pipeline) (SomeMaterialRef (Ref material_ref)) -> enterD "Material changed" Linear.do
            Ur (Some @Material material') <- lift $ Apecs.get (Apecs.Entity material_ref)

            logT "Binding material..."
            Ur (graphicsPipeline, _p) <- pure $ unsafeGraphicsPipeline pipeline

            lift (lift (descriptors material')) >>= \case
              (dset,rmap,material'') -> Linear.do

                -- These materials are necessarily compatible with this pipeline in
                -- the set #1, so the 'descriptorSetBinding' buffer will always be
                -- valid to write with the corresponding material binding
                (rmap', material''') <- lift $ Alias.useM rmap (\rmap' -> writePropertiesToResources rmap' material'')
                
                -- static bindings will have to choose a different dset
                -- Bind descriptor set #1
                (dset', pLayout) <- Alias.useM dset (Unsafe.toLinear $ \dset' -> Linear.do
                  (pLayout', vkdset) <-
                    bindGraphicsDescriptorSet graphicsPipeline._pipelineLayout 1 (dset'._descriptorSet)
                  pure (Unsafe.toLinear (\_ -> dset') vkdset, pLayout') --forget vulkan dset
                                              )

                Unsafe.toLinearN @2 (\_ _ -> pure ()) material''' pLayout -- The material still in the Apecs store. Really, these functions should have no Unsafes and in that case all would be right (e.g. the resource passed to this function would have to be freed in this function, guaranteeing that it is reference counted or something?....

                lift $ lift $ Linear.do
                  Alias.forget dset'
                  Alias.forget rmap'

          )
        (\(SomePipeline pipeline) (Some mesh) (ModelMatrix mm _) -> enterD "Mesh changed" Linear.do

            logT "Drawing mesh"
            Ur (graphicsPipeline, _p) <- pure $ unsafeGraphicsPipeline pipeline

            -- TODO: Bind descriptor set #2

            pLayout <- pushConstants graphicsPipeline._pipelineLayout Vk.SHADER_STAGE_VERTEX_BIT mm
            mesh' <- renderMesh mesh

            Unsafe.toLinearN @2 (\_ _ -> pure ()) pLayout mesh' -- forget mesh, it's in the Apecs store... ouch
          )
        (
          -- Draw UI (TODO: Special render pass...?)
          liftSystemIO IM.getDrawData >>= IM.renderDrawData
        )

    pure (Ur (), cmdBuffer')
    
 where
  -- The region of the framebuffer that the output will be rendered to. We
  -- render from (0,0) to (width, height) i.e. the whole framebuffer
  -- Defines a transformation from image to framebuffer
  viewport' extent = Vk.Viewport { x = 0.0
                         , y = 0.0
                         , width  = fromIntegral $ extent.width
                         , height = fromIntegral $ extent.height
                         , minDepth = 0
                         , maxDepth = 1
                         }

  -- Defines the region in which pixels will actually be stored. Any pixels
  -- outside of the scissor will be discarded. We keep it as the whole viewport
  scissor' extent = Vk.Rect2D (Vk.Offset2D 0 0) extent

-- | Write a property value to its corresponding resource.
--
-- (1) For each property binding, update the property
--    (1.1) If it's dynamic, write the mapped buffer
--    (1.2) If it's static, do nothing because the buffer is already written
--    (1.3) If it's a texture, do nothing because the texture is written only once and has already been bound
--
-- (2) The written resource must be updated in the corresponding descriptor set which must be bound (This is done in the render function)
--
-- The important logic is done by 'writeProperty', this function simply iterates over the properties to write them
--
-- The render property bindings function should be created from a compatible pipeline
writePropertiesToResources :: ∀ φ α ω. (HasProperties φ, Dupable ω) => ResourceMap ⊸ φ α ⊸ Ghengin ω (ResourceMap, φ α)
writePropertiesToResources rmap' fi
  = enterD "writePropertiesToResources"
    Linear.do (pbs, fi')     <- lift $ properties fi
              logT "Going on rmap'"
              (rmap'', pbs') <- go rmap' 0 pbs
              logT "Forgetting property bindings"
              lift $ Alias.forget pbs'
              pure (rmap'', fi')

  where
    go :: ∀ β. ResourceMap ⊸ Int -> PropertyBindings β ⊸ Ghengin ω (ResourceMap, PropertyBindings β)
    go rmap n = \case
      GHNil -> pure (rmap, GHNil)
      binding :## as -> Linear.do
        (res, rmap'') <- lift $ getDescriptorResource rmap n
        (res', binding') <- lift $ writeProperty res binding -- TODO: We don't want to fetch the binding so often. Each propety could have its ID and fetch it if required
        lift $ Alias.forget res' -- gotten from rmap, def. not the last ref
        (rmap''', bs) <- go rmap'' (n+1) as
        pure (rmap''', binding':##bs)

-- ROMES:TODO: move all this unwrapping device buffers to the Command module
-- it is hard because Mesh is not defined in ghengin-vulkan, and ghengin-vulkan can't yet depend on ghengin-core because of backpack bugs
-- (TODO: Report it)
renderMesh :: MonadIO m => Mesh a ⊸ RenderPassCmdM m (Mesh a)
renderMesh = \case
  SimpleMesh (VertexBuffer (DeviceLocalBuffer buf mem) nverts) -> Linear.do
      buffers <- pure $ Unsafe.toLinear V.singleton buf
      let offsets = [0]
      buffers' <- bindVertexBuffers 0 buffers offsets
      draw nverts
      pure (SimpleMesh (VertexBuffer (DeviceLocalBuffer (Unsafe.toLinear V.unsafeHead buffers') mem) nverts))
  IndexedMesh (VertexBuffer (DeviceLocalBuffer vbuf mem) nverts) (Index32Buffer (DeviceLocalBuffer ibuf imem) nixs) -> Linear.do
      buffers <- pure $ Unsafe.toLinear V.singleton vbuf -- while we don't have linear lets...
      let offsets = [0]
      buffers' <- bindVertexBuffers 0 buffers offsets
      ibuf'    <- bindIndex32Buffer ibuf 0
      drawIndexed nixs
      pure (IndexedMesh (VertexBuffer (DeviceLocalBuffer (Unsafe.toLinear V.unsafeHead buffers') mem) nverts)
                        (Index32Buffer (DeviceLocalBuffer ibuf' imem) nixs)
           )

-- Utils
-- These being used really go to show how broken the ghengin side of things
-- (outside of core) seem to be wrt linearity.

getRenderPass :: ∀ m α info. MonadIO m => RenderPipeline info α ⊸ m (Alias RenderPass, RenderPipeline info α)
getRenderPass = Unsafe.toLinear $ \x -> (,x) <$> get' x  -- Safe since we increment the reference count of the thing we return
  where
    get' :: ∀ b. RenderPipeline info b -> m (Alias RenderPass)
    get' (RenderPipeline _ rp _ _) = Unsafe.Alias.inc rp
    get' (RenderProperty _ rp) = get' rp

unsafeGraphicsPipeline :: ∀ α info. RenderPipeline info α ⊸ Ur (RendererPipeline Graphics, RenderPipeline info α)
unsafeGraphicsPipeline = Unsafe.toLinear $ \x -> Ur (get' x,x)  -- just unsafe...
  where
    get' :: ∀ b. RenderPipeline info b -> (RendererPipeline Graphics)
    get' (RenderPipeline rpg _ _ _) = rpg
    get' (RenderProperty _ rp) = get' rp

getResourceMap :: ∀ m α info. MonadIO m => RenderPipeline info α ⊸ m (Alias ResourceMap, RenderPipeline info α)
getResourceMap = Unsafe.toLinear $ \x -> (,x) <$> get' x  -- Safe since we increment the reference count of the thing we return
  where
    get' :: ∀ b. RenderPipeline info b -> m (Alias ResourceMap)
    get' (RenderPipeline _ _ (_, rmap, _) _) = Unsafe.Alias.inc rmap
    get' (RenderProperty _ rp) = get' rp

