{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE BlockArguments #-}
module Ghengin.Render where

import Apecs (Has, cfold)
import Data.Maybe

import Control.Monad.State

import qualified Data.Vector as V
import qualified Vulkan as Vk
import Geomancy.Mat4

import qualified Apecs
import Ghengin.Component.Camera
import Ghengin.Component.Transform
import Ghengin.Vulkan.Command
import Ghengin.Vulkan.Buffer
import Ghengin.Vulkan.Pipeline
import Ghengin.Vulkan.DescriptorSet
import Ghengin.Vulkan.RenderPass
import qualified Ghengin.DearImGui as IM
import Ghengin.Vulkan
import Ghengin.Scene.Graph
import Ghengin.Core.Render.Packet
import Ghengin.Render.Queue
import Ghengin.Core.Mesh
import Ghengin.Utils
import Ghengin.Core.Render.Property
import Ghengin.Core.Material
import {-# SOURCE #-} Ghengin.World (World)
import {-# SOURCE #-} Ghengin (Ghengin)
import Control.Lens ((^.))

type RenderConstraints w = ( Has (World w) (Renderer ()) Transform
                           , Has (World w) (Renderer ()) Camera
                           , Has (World w) (Renderer ()) ModelMatrix
                           , Has (World w) (Renderer ()) Parent

                           -- Core render constraints
                           , Apecs.Get (World w) (Renderer ()) RenderPacket
                           , Apecs.Get (World w) (Renderer ()) SomePipeline
                           , Apecs.Get (World w) (Renderer ()) SomeMaterial
                           )


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
       => Int -- frame identifier
       -> Ghengin w ()
render i = do

  -- Some required variables
  extent <- lift getRenderExtent
  let viewport = viewport' extent
      scissor  = scissor' extent

  -- Traverse all nodes in the scene graph updating the model matrices
  -- TODO: Currently called traverseSceneGraph, but the name should reflect that the model matrices are updated
  traverseSceneGraph i (const . const $ pure ())


  -- Get all the 'RenderPacket's to create the 'RenderQueue' ahead
  renderQueue <- cfold (flip $ \(p :: RenderPacket, fromMaybe (ModelMatrix identity 0) -> mm) -> insert p mm) mempty


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

  _ <- withCurrentFramePresent $ \cmdBuffer currentImage currentFrame -> do

    recordCommand cmdBuffer $ do

      -- Now, render the renderable entities from the render queue in the given order.
      -- If everything works as expected, if we blindly bind the descriptor sets as
      -- they come, we should bind the pipeline once and each material once.
      traverseRenderQueue
        renderQueue
        -- Whenever we have a new pipeline, start its renderpass (lifting RenderPassCmd to Command)
        (\(SomePipelineRef (Ref pp'_ref)) m -> do
          SomePipeline pp' <- lift $ Apecs.get pp'_ref -- TODO: Share this with the next one
          renderPassCmd (pp' ^. renderPass)._renderPass ((pp' ^. renderPass)._framebuffers V.! currentImage) extent m
        )
        (\(SomePipelineRef (Ref pipeline_ref)) -> do
            SomePipeline pipeline <- lift $ Apecs.get pipeline_ref

            logTrace "Binding pipeline"

            -- The render pass for this pipeline has been bound already. Later on the render pass might not be necessarily coupled to the pipeline
            -- Bind the pipeline
            bindGraphicsPipeline ((pipeline^.graphicsPipeline)._pipeline)
            setViewport viewport
            setScissor  scissor

            case pipeline ^. descriptorSet of -- TODO: Fix frames in flight...
              EmptyDescriptorSet -> pure () -- Bail out, we don't have to do anything on an empty descriptor set. This happens if there isn't a single binding in set #1
              ppDSet@DescriptorSet{} -> do

                -- These render properties are necessarily compatible with this
                -- pipeline in the set #0, so the 'descriptorSetBinding' buffer
                -- will always be valid to write with the corresponding
                -- material binding
                --
                -- getUniformBuffer is partially applied to matDSet so it can be used to fetch each material descriptor
                lift $ writePropertiesToResources (getUniformBuffer ppDSet) pipeline
                
                -- Bind descriptor set #0
                bindGraphicsDescriptorSet (pipeline^.graphicsPipeline)._pipelineLayout 0 ppDSet._descriptorSet

            pure $ SomePipeline pipeline
          )
        (\(SomePipeline pipeline) (SomeMaterialRef (Ref material_ref)) -> do
            SomeMaterial material <- lift $ Apecs.get material_ref

            logTrace "Binding material"

            case material ^. descriptorSet of
              EmptyDescriptorSet -> pure () -- Bail out, we don't have to do anything on an empty descriptor set. This happens if there isn't a single binding in set #1
              matDSet@DescriptorSet{} -> do

                -- These materials are necessarily compatible with this pipeline in
                -- the set #1, so the 'descriptorSetBinding' buffer will always be
                -- valid to write with the corresponding material binding
                lift $ writePropertiesToResources (getUniformBuffer matDSet) material
                
                -- static bindings will have to choose a different dset
                -- Bind descriptor set #1
                bindGraphicsDescriptorSet (pipeline^.graphicsPipeline)._pipelineLayout 1 matDSet._descriptorSet

          )
        (\(SomePipeline pipeline) (SomeMesh mesh) (ModelMatrix mm _) -> do

            logTrace "Drawing mesh"

            -- TODO: Bind descriptor set #2

            pushConstants (pipeline^.graphicsPipeline)._pipelineLayout Vk.SHADER_STAGE_VERTEX_BIT mm
            renderMesh mesh
          )
        (do
          -- Draw UI (TODO: Special render pass...?)
          IM.renderDrawData =<< IM.getDrawData
        )

  pure ()
    
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
-- The render property bindings function should be created from a compatible pipeline
writePropertiesToResources :: ∀ φ α ω. HasProperties φ => (Int -> MappedBuffer) -> φ α -> Ghengin ω ()
writePropertiesToResources propertyBinding = go 0 . properties where

  go :: ∀ β. Int -> PropertyBindings β -> Ghengin ω ()
  go n = \case
    GHNil -> pure ()
    binding :## as -> do
      lift $ writeProperty (propertyBinding n) binding -- TODO: We don't want to fetch the binding so often. Each propety could have its ID and fetch it if required
      go (n+1) as

