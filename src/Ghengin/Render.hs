{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE BlockArguments #-}
module Ghengin.Render where

import GHC.Records
import Apecs (Storage, cfold)

import Control.Monad.State

import qualified Data.Vector as V
import qualified Vulkan as Vk

import Ghengin.Vulkan.Command
import Ghengin.Vulkan.Buffer
import Ghengin.Vulkan.Command
import Ghengin.Vulkan.Pipeline
import Ghengin.Vulkan.DescriptorSet
import Ghengin.Vulkan.RenderPass
import Ghengin.Vulkan.GLFW.Window
import Ghengin.Vulkan
import Ghengin.Scene.Graph
import Ghengin.Render.Packet
import Ghengin.Render.Queue
import Ghengin.Component.Transform
import Ghengin.Component.Mesh
import Ghengin.Component.Material
import {-# SOURCE #-} Ghengin (Ghengin)


type RenderConstraints w = ( HasField "meshes" w (Storage Mesh)
                           , HasField "materials" w (Storage SharedMaterial)
                           , HasField "transforms" w (Storage Transform)
                           , HasField "renderPackets" w (Storage RenderPacket)
                           , HasField "modelMatrices" w (Storage ModelMatrix)
                           , HasField "entityParents" w (Storage Parent)
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
--  * Note [Renderable entities] (TODO)
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
  traverseSceneGraph i (const $ pure ())


  -- Get all the 'RenderPacket's to create the 'RenderQueue' ahead
  renderPackets <- cfold (\acc (p :: RenderPacket) -> p:acc) mempty


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

    -- Now, render the renderable entities from the render queue in the given order.
    -- If everything works as expected, if we blindly bind the descriptor sets as
    -- they come, we should bind the pipeline once and each material once.
    traverseRenderQueue (makeRenderQueue renderPackets) \(RenderPacket mesh material pipeline key) -> (`evalState` (-1, -1)) do

      let (pkey, mkey) = splitKey key

      (previousPKey, previousMKey) <- get


      recordCommand cmdBuffer $ do

        -- First, bind the pipeline and start its renderpass if it hasn't been bound yet
        -- when (pkey /= previousPKey) do

        renderPass pipeline._renderPass._renderPass (pipeline._renderPass._framebuffers V.! currentImage) extent $ do

          bindGraphicsPipeline (pipeline._graphicsPipeline._pipeline)
          setViewport viewport
          setScissor  scissor

      when (mkey /= previousMKey) do
        _

      pure ()

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
