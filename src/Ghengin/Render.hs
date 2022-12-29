{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE BlockArguments #-}
module Ghengin.Render where

import GHC.Records
import Apecs (Storage, cfold, cmapM)
import Data.Maybe

import Control.Monad.State
import Control.Exception

import qualified Data.IntMap as IM
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V
import qualified Vulkan as Vk
import Geomancy.Mat4
import qualified Foreign.Storable as S
import Foreign.Ptr

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
import Ghengin.Render.Packet
import Ghengin.Render.Queue
import Ghengin.Component.Mesh
import Ghengin.Component.Material
import Ghengin.Utils
import {-# SOURCE #-} Ghengin (Ghengin)


type RenderConstraints w = ( HasField "transforms" w (Storage Transform)
                           , HasField "renderPackets" w (Storage RenderPacket)
                           , HasField "cameras" w (Storage Camera)
                           , HasField "modelMatrices" w (Storage ModelMatrix)
                           , HasField "entityParents" w (Storage Parent)
                           )

data UniformBufferObject = UBO { view :: Mat4
                               , proj :: Mat4
                               } deriving Show

-- TODO: Use and export derive-storable?
instance S.Storable UniformBufferObject where
  sizeOf _ = 2 * S.sizeOf @Mat4 undefined
  alignment _ = 16
  peek (castPtr -> p) = do
    vi <- S.peek p
    pr <- S.peekElemOff p 1
    pure $ UBO vi pr
  poke (castPtr -> p) (UBO vi pr) = do
    S.poke p vi
    S.pokeElemOff p 1 pr

-- instance Poke UniformBufferObject Extended where
--   type SizeOf Extended UniformBufferObject = 128 -- 2*16
--   type Alignment Extended UniformBufferObject = 128 -- 2*16
--   poke = S.poke

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

    let
        descriptorSet :: RenderPipeline α -> Int -> DescriptorSet
        descriptorSet pipeline setIx = fst (pipeline._descriptorSetsSet NE.!! currentFrame) V.! setIx -- must guarantee that there exist this amount of sets in this pipeline

        descriptorSetBinding :: RenderPipeline α -> Int -> Int -> MappedBuffer
        descriptorSetBinding pipeline setIx bindingIx = fst $ (descriptorSet pipeline setIx)._bindings IM.! bindingIx

    recordCommand cmdBuffer $ do

      -- Now, render the renderable entities from the render queue in the given order.
      -- If everything works as expected, if we blindly bind the descriptor sets as
      -- they come, we should bind the pipeline once and each material once.
      traverseRenderQueue
        renderQueue
        -- Whenever we have a new pipeline, start its renderpass
        (\(SomePipeline pp') -> renderPass pp'._renderPass._renderPass (pp'._renderPass._framebuffers V.! currentImage) extent)
        (\(SomePipeline pipeline) -> do

            logTrace ("Binding pipeline")

            -- The render pass for this pipeline has been bound already. Later on the render pass might not be necessarily coupled to the pipeline
            -- Bind the pipeline
            bindGraphicsPipeline (pipeline._graphicsPipeline._pipeline)
            setViewport viewport
            setScissor  scissor

            -- TODO: This next bit has got to be re-done
            -- TODO: Bind pipeline-global data to descriptor set #0
            -- Get main camera, for the time being it's the only possible pipeline data for the shader
            -- The last camera will override the write buffer
            lift $ cmapM $ \(Camera proj view, fromMaybe noTransform -> camTr) -> do

              projM <- lift $ makeProjection proj
              let viewM = makeView camTr view

                  ubo   = UBO viewM projM

              -- TODO : Move out of cmapM
              -- Currently the descriptor set #0 always has just a uniform buffer object
              case descriptorSetBinding pipeline 0 0 of
                buf -> lift $ writeMappedBuffer buf ubo


            -- Bind descriptor set #0
            bindGraphicsDescriptorSet pipeline._graphicsPipeline._pipelineLayout
              0 (descriptorSet pipeline 0)._descriptorSet

          )
        (\(SomePipeline pipeline) (SomeMaterial material) -> do

            logTrace ("Binding material")

            -- These materials are necessarily compatible with this pipeline in
            -- the set #1, so the 'descriptorSetBinding' buffer will always be
            -- valid to write with the corresponding material binding
            lift $ writeMaterial (descriptorSetBinding pipeline 1) material
            
            -- static bindings will have to choose a different dset
            -- Bind descriptor set #1
            bindGraphicsDescriptorSet pipeline._graphicsPipeline._pipelineLayout
              1 (descriptorSet pipeline 1)._descriptorSet

          )
        (\(SomePipeline pipeline) (mesh :: Mesh) (ModelMatrix mm _) -> do

            logTrace ("Drawing mesh")

            -- TODO: Bind descriptor set #2

            pushConstants pipeline._graphicsPipeline._pipelineLayout Vk.SHADER_STAGE_VERTEX_BIT mm
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

-- | Bind a material.
--
-- (1) For each binding
--    (1.1) If it's dynamic, write the default (default-bound) buffer
--    (1.2) If it's static, bind the static buffer?
-- (2) Bind the descriptor set #1 with the chosen descriptors
--
-- For now we ignore (1.2) (TODO!) and simply write material data into
-- the default buffers
--
-- The material bindings function should be created from a compatible pipeline
writeMaterial :: ∀ σ ω. (Int -> MappedBuffer) -> Material σ -> Ghengin ω ()
writeMaterial materialBinding mat = go (matSizeBindings mat - 1) mat where

  go :: ∀ υ. Int -> Material υ -> Ghengin ω ()
  go n = \case
    Done -> assert (n == (-1)) (pure ()) -- (only triggered with -O0)
    DynamicBinding (a :: α) as -> do
      case materialBinding n of
        -- TODO: Ensure unsafeCoerce is safe here by only allowing
        -- the construction of dynamic materials if validated at
        -- compile time against the shader pipeline in each
        -- matching position
        buf -> lift $ writeMappedBuffer @α buf a

      go (n-1) as
    -- StaticMaterial -> undefined -- TODO: Bind the static descriptor ?
       
