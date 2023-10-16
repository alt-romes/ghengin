{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-|
The entry module for ghengin-core, which defines the surface level of Core with
which one expresses more high-level game-engine and rendering abstractions, like Cameras.
 -}
module Ghengin.Core where

import qualified Prelude
import Ghengin.Core.Prelude
import Ghengin.Core.Log
import qualified Data.V.Linear as V

import Control.Functor.Linear as Linear
import qualified Data.Functor.Linear as Data
import Control.Monad.IO.Class.Linear as Linear

import Ghengin.Core.Render.Queue
import Ghengin.Core.Renderer.Pipeline
import Ghengin.Core.Renderer.RenderPass
import Ghengin.Core.Renderer.Kernel
import Ghengin.Core.Renderer
import Ghengin.Core.Render.Pipeline
import Ghengin.Core.Render

import Ghengin.Core.Render.Packet
import Ghengin.Core.Mesh
import Ghengin.Core.Render.Property
import Ghengin.Core.Material

import Ghengin.Core.Type.Utils

import qualified Data.Linear.Alias as Alias


import qualified Vulkan as Vk

import qualified Data.Linear.Alias.Unsafe as Unsafe.Alias
import qualified Unsafe.Linear as Unsafe

data CoreState
  = CoreState { frameCounter :: {-# UNPACK #-} !Int
              }
newtype Core α = Core (StateT CoreState Renderer α)
  deriving (Functor, Data.Functor, Applicative, Data.Applicative, Monad, MonadIO, HasLogger)

runCore :: Core a ⊸ IO a
runCore (Core st)
  = runRenderer $ Linear.do
      (a, CoreState i) <- runStateT st (CoreState 0)
      () <- pure (consume i)
      return a

-- | A postfix operator to lift Renderer computations to Core (postfix version of liftCore)
--
-- Example usage: @ Ur ext <- ( getRenderExtent ↑ )@
--
-- Digraph (-!) in vim, i.e. typing <C-k>-! in insert mode, inserts ↑.
(↑), liftCore :: Renderer a ⊸ Core a
(↑)      = Core . lift
liftCore = Core . lift

render :: RenderQueue ()
          -- ^ The unit type parameter is data attached to each item in the
          -- render queue, we could eventually use it for something relevant...
        ⊸ Core (RenderQueue ())
render (RenderQueue renderQueue) = Core $ StateT $ \CoreState{frameCounter=fcounter'} -> enterD "render" $ Linear.do
  Ur fcounter    <- pure (move fcounter')

  -- ROMES:TODO: This might cause flickering once every frame overflow due to ... overflows?
  -- Need to consider what happens if it overflows. For now, good enough, it's
  -- unlikely the frame count overflows, with 60 frames per second the game
  -- would have to run for years to overflow a 64 bit integer
  let frameIndex = fcounter `mod` (nat @MAX_FRAMES_IN_FLIGHT_T)

  -- Some required variables
  -- TODO: instead, use defaults and provide functions to change viewport and scissor
  -- if so desired
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

    (renderQueue', cmdBuffer') <- recordCommand cmdBuffer $ Linear.do

      -- Now, render the renderable entities from the render queue in the given order.
      -- If everything works as expected, if we blindly bind the descriptor sets as
      -- they come, we should bind the pipeline once and each material once.

      -- I can prob make this linear simply by using mapAccumL
      renderQueue' <- Data.traverse (Unsafe.toLinear \(Some2 @RenderPipeline @π @bs pipeline, materials) -> enterD "Traverse: pipeline" Linear.do

        -- Whenever we have a new pipeline, start its renderpass (lifting RenderPassCmd to Command)

        {-
           We'll likely want to do some pre-processing of user-defined render passes,
           but let's keep it simple and working for now.
        -}

        Ur rp' <- pure $ unsafeGetRenderPass pipeline
        let rp = Unsafe.Alias.get rp' -- nice and unsafe
        renderPassCmd currentImage rp extent $ Linear.do

          -- then

          logT "Binding pipeline"
          (graphicsPipeline, rebuildPipeline) <- pure $ getGraphicsPipeline pipeline

          -- The render pass for this pipeline has been bound already. Later on the render pass might not be necessarily coupled to the pipeline
          -- Bind the pipeline
          graphicsPipeline <- bindGraphicsPipeline graphicsPipeline
          setViewport viewport
          setScissor  scissor

          lift (descriptors $ rebuildPipeline graphicsPipeline) >>= \case
            (dset, rmap, pipeline') -> Linear.do

              -- These render properties are necessarily compatible with this
              -- pipeline in the set #0, so the 'descriptorSetBinding' buffer
              -- will always be valid to write with the corresponding
              -- material binding
              (rmap', pipeline'') <- lift $ Alias.useM rmap (\rmap' -> writePropertiesToResources rmap' pipeline')

              (graphicsPipeline, rebuildPipeline) <- pure $ getGraphicsPipeline pipeline''
              
              -- Bind descriptor set #0
              (dset', graphicsPipeline) <- Alias.useM dset (bindGraphicsDescriptorSet graphicsPipeline 0)

              lift $ Linear.do
                Alias.forget dset'
                Alias.forget rmap'

              -- For every material...
              (materials', graphicsPipeline) <- runStateT (Data.traverse handleMaterial materials) graphicsPipeline

              {-
                We'll likely want to do some post-processing of user-defined
                 render passes, but let's keep it simple and working for now. We'll
                 get back to a clean dear-imgui add-on to ghengin-core later.
                 (For each pipeline)
              -}

              -- Draw UI (TODO: Special render pass...?)
              -- liftSystemIO IM.getDrawData >>= IM.renderDrawData

              return (Some2 $ rebuildPipeline graphicsPipeline, materials')

        ) renderQueue
      return renderQueue'

    -- We can return the unsafe queue after having rendered from it because
    -- rendering does not do anything to the resources in the render queue (it
    -- only draws the scene specified by it) It is rather edited by the game,
    -- in the loops before rendering
    pure ((RenderQueue renderQueue', CoreState{frameCounter=fcounter+1}), cmdBuffer')
    
 where

  handleMaterial :: Data.Traversable t
                 => (Some Material, t (Some2 Mesh, ()))
                  ⊸ StateT (RendererPipeline Graphics) (RenderPassCmdM Renderer) (Some Material, t (Some2 Mesh, ()))
  handleMaterial (Some @Material @ms material, meshes) = StateT $ \graphicsPipeline -> enterD "Material changed" Linear.do

    lift (descriptors material) >>= \case

     (dset,rmap,material') -> Linear.do

      -- These materials are necessarily compatible with this pipeline in
      -- the set #1, so the 'descriptorSetBinding' buffer will always be
      -- valid to write with the corresponding material binding
      (rmap', material'') <- lift $ Alias.useM rmap (\rmap' -> writePropertiesToResources rmap' material')
      
      -- static bindings will have to choose a different dset
      -- Bind descriptor set #1
      (dset', graphicsPipeline) <- Alias.useM dset (bindGraphicsDescriptorSet graphicsPipeline 1)

      lift $ Linear.do
        Alias.forget dset'
        Alias.forget rmap'

      -- For every mesh...
      --    (we still attach no data to the render queue, but we could, and it would be inplace of this unit)
      (meshes', graphicsPipeline) <- runStateT (Data.traverse handleMesh meshes) graphicsPipeline

      return ((Some material'', meshes'), graphicsPipeline)

  handleMesh :: (Some2 Mesh, ()) ⊸ StateT (RendererPipeline Graphics) (RenderPassCmdM Renderer) (Some2 Mesh, ())
  handleMesh (Some2 @Mesh @ts mesh0, ()) = StateT $ \graphicsPipeline -> enterD "Mesh changed" Linear.do

    logT "Drawing mesh"

    -- Bind descriptor set #2 when we have that information in meshes
    lift (descriptors mesh0) >>= \case
     (dset,rmap,mesh1) -> Linear.do

      -- These meshes are necessarily compatible with this pipeline
      -- in the set #2, so the 'descriptorSetBinding' buffer will
      -- always be valid to write with the corresponding mesh binding
      (rmap', mesh2) <- lift $ Alias.useM rmap (\rmap' -> writePropertiesToResources rmap' mesh1)
      
      -- static bindings will have to choose a different dset
      -- Bind descriptor set #2
      (dset', graphicsPipeline) <- Alias.useM dset (bindGraphicsDescriptorSet graphicsPipeline 2)

      lift $ Linear.do
        Alias.forget dset'
        Alias.forget rmap'

      -- TODO: No more push constants, for now!!!! They're being hardcoded to something but we don't codify what to push... allow push constants!
      -- pLayout <- pushConstants graphicsPipeline._pipelineLayout Vk.SHADER_STAGE_VERTEX_BIT mm

      mesh3 <- renderMesh mesh2

      return ((Some2 mesh3, ()), graphicsPipeline)




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
writePropertiesToResources :: ∀ φ α ω. HasProperties φ => ResourceMap ⊸ φ α ⊸ Renderer (ResourceMap, φ α)
writePropertiesToResources rmap' fi
  = enterD "writePropertiesToResources"
    Linear.do (pbs, fi')     <- properties fi
              logT "Going on rmap'"
              (rmap'', pbs') <- go rmap' 0 pbs
              logT "Forgetting property bindings"
              Alias.forget pbs'
              pure (rmap'', fi')

  where
    go :: ∀ β. ResourceMap ⊸ Int -> PropertyBindings β ⊸ Renderer (ResourceMap, PropertyBindings β)
    go rmap n = \case
      GHNil -> pure (rmap, GHNil)
      binding :## as -> Linear.do
        (res, rmap'') <- getDescriptorResource rmap n
        (res', binding') <- writeProperty res binding -- TODO: We don't want to fetch the binding so often. Each propety could have its ID and fetch it if required
        Alias.forget res' -- gotten from rmap, def. not the last ref
        (rmap''', bs) <- go rmap'' (n+1) as
        pure (rmap''', binding':##bs)


renderMesh :: (Functor m, MonadIO m) => Mesh vs a ⊸ RenderPassCmdM m (Mesh vs a)
renderMesh = \case
  SimpleMesh vb ds uq -> Linear.do
    vb' <- drawVertexBuffer vb
    return $ SimpleMesh vb' ds uq
  IndexedMesh vb ib ds uq -> Linear.do
    (vb', ib') <- drawVertexBufferIndexed vb ib
    return $ IndexedMesh vb' ib' ds uq
  MeshProperty p xs -> MeshProperty p <$> renderMesh xs

getGraphicsPipeline :: ∀ α info. RenderPipeline info α ⊸ (RendererPipeline Graphics, RendererPipeline Graphics ⊸ RenderPipeline info α)
getGraphicsPipeline (RenderPipeline rpg a b c) = (rpg, \rg -> RenderPipeline rg a b c)
getGraphicsPipeline (RenderProperty p rp) = case getGraphicsPipeline rp of (rg, rpf) -> (rg, \rg' -> RenderProperty p (rpf rg'))

-- completely unsafe things, todo:fix

unsafeGetRenderPass :: ∀ α info. RenderPipeline info α ⊸ Ur (Alias RenderPass)
unsafeGetRenderPass = Unsafe.toLinear $ \x -> Ur (get' x)
  where
    get' :: ∀ b. RenderPipeline info b -> Alias RenderPass
    get' (RenderPipeline _ rp _ _) = rp
    get' (RenderProperty _ rp) = get' rp

