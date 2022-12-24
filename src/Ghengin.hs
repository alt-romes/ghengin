{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLists #-}
module Ghengin
  ( module Ghengin
  , module Geomancy.Vec3
  , module Geomancy.Mat4
  ) where

import GHC.Records
import Unsafe.Coerce

import GHC.Stack

import Foreign.Storable
import Foreign.Ptr

import Data.Maybe
import Control.Monad.Reader

import Data.IORef

import Data.Time.Clock
import Data.Vector (Vector)
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V
import qualified Data.IntMap as IM
import qualified Vulkan as Vk
import Apecs
import Geomancy.Vec3
import Geomancy.Mat4

import Ghengin.Vulkan.Buffer
import Ghengin.Vulkan.Command
import Ghengin.Vulkan.Pipeline
import Ghengin.Vulkan.DescriptorSet
import Ghengin.Vulkan.RenderPass
import Ghengin.Vulkan.GLFW.Window
import Ghengin.Vulkan
import Ghengin.Render.Packet
import Ghengin.Scene.Graph

import Ghengin.Shaders

import qualified Ghengin.DearImGui as IM

import Ghengin.Component.Camera
import Ghengin.Component.Material
import Ghengin.Component.Mesh
import Ghengin.Component.Transform
import Ghengin.Component.UI


-- TODO: Somehow systems that want to delete entities should call a special
-- destructor function that gets rid of resources stuck in components such as
-- meshes


type Ghengin w = SystemT w (Renderer GEnv)

data GEnv = GEnv { _renderPipelines :: IORef [SomeRenderPipeline]
                 }

initGEnv :: MonadIO m => m GEnv
initGEnv = do
  GEnv <$> liftIO (newIORef [])

windowLoop :: Ghengin w Bool -> Ghengin w ()
windowLoop action = do
  win <- lift (asks (._vulkanWindow._window))
  loopUntilClosedOr win action

type DeltaTime = Float -- Converted from NominalDiffTime

data UniformBufferObject = UBO { view :: Mat4
                               , proj :: Mat4
                               }
-- TODO: Use and export derive-storable?
instance Storable UniformBufferObject where
  sizeOf _ = 2 * sizeOf @Mat4 undefined
  alignment _ = 16
  peek (castPtr -> p) = do
    vi <- peek p
    pr <- peekElemOff p 1
    pure $ UBO vi pr
  poke (castPtr -> p) (UBO vi pr) = do
    poke p vi
    pokeElemOff p 1 pr

type WorldConstraints w = ( HasField "meshes" w (Storage Mesh)
                          , HasField "materials" w (Storage SharedMaterial)
                          , HasField "transforms" w (Storage Transform)
                          , HasField "modelMatrices" w (Storage ModelMatrix)
                          , HasField "entityParents" w (Storage Parent)
                          , HasField "cameras" w (Storage Camera)
                          , HasField "uiwindows" w (Storage UIWindow)
                          )
ghengin :: WorldConstraints w
        => w           -- ^ World
        -> Ghengin w a -- ^ Init
        -> Ghengin w b -- ^ Run every simulation step (currently ignored)
        -> (a -> DeltaTime -> [Bool] -> Ghengin w Bool) -- ^ Run every game
                                                          -- loop? iteration. The list of list of bools indicates whether the components in
                                                          -- a UI window were changed. The returned Bool indicates whether we should exit the gameloop
        -- -> Ghengin w c -- ^ Run every draw step?
        -> Ghengin w c -- ^ Run once the game is quit (for now that is when the window closed)
        -> IO ()
ghengin world initialize _simstep loopstep finalize = (\x -> initGEnv >>= flip runVulkanRenderer x) . (`runSystem` world) $ do

  -- TODO: If the materials added to each entity are ordered, when we get all
  -- the materials will the materials be ordered? If so, we can simply render
  -- with them sequentially without being afraid of changing back and forwards
  a <- initialize

  -- BIG:TODO: Bundle descriptor sets, render passes, and pipelines into a single abstraction
  -- ^^ This is going well, they are created in Render.Packet
  --
  -- TODO: Use linear types. Can I make the monad stack over a multiplicity polymorphic monad?

  -- Init ImGui for this render pass (should eventually be tied to the UI render pass)
  -- BIG:TODO: Don't hardcode the renderpass from the first renderpacket...
  (head -> pp) <- liftIO . readIORef =<< lift (asks (._extension._renderPipelines))
  imCtx <- lift $ IM.initImGui (case pp of SomeRenderPipeline pp _ -> pp._renderPass._renderPass)

  currentTime <- liftIO (getCurrentTime >>= newIORef)
  lastFPSTime <- liftIO (getCurrentTime >>= newIORef)
  frameCounter <- liftIO (newIORef (0 :: Int))

  windowLoop $ do

    newTime <- liftIO getCurrentTime

    -- FPS Counter
    lastFPS <- liftIO (readIORef lastFPSTime)
    liftIO (modifyIORef' frameCounter (+1))
    when (diffUTCTime newTime lastFPS > 1) (liftIO $ do
      frames <- readIORef frameCounter
      putStrLn $ "FPS: " <> show frames
      writeIORef frameCounter 0
      writeIORef lastFPSTime newTime
      )

    -- Fix Your Timestep: A Very Hard Thing To Get Right. For now, the simplest approach:
    frameTime <- diffUTCTime newTime <$> liftIO(readIORef currentTime)
    liftIO(writeIORef currentTime newTime)

    -- DearImGui frame
    -- TODO: Draw UI (define all UI components in the frame)
    bs <- drawUI

    -- Game loop step
    b <- loopstep a (min MAX_FRAME_TIME $ realToFrac frameTime) bs

    -- Create a model matrix for all scene entities
    traverseSceneGraph =<< liftIO (readIORef frameCounter)

    -- Render frame
    drawFrame

    pure b

  Vk.deviceWaitIdle =<< lift getDevice

  lift $ do
    IM.destroyImCtx imCtx
    -- BIG:TODO: Destroy allocated 'RenderPipeline's
    -- destroyDescriptorPool dpool
    -- mapM_ destroyUniformBuffer objUBs
    -- mapM_ destroyDescriptorSetLayout descriptorSetLayouts
    -- destroyRenderPass simpleRenderPass
    -- destroyPipeline pipeline

  _ <- finalize

  pure ()

drawUI :: WorldConstraints w => Ghengin w [Bool]
drawUI = do
    IM.vulkanNewFrame
    IM.glfwNewFrame
    IM.newFrame

    bs <- cfoldM (\acc (uiw :: UIWindow) -> do
      bs <- lift $ IM.pushWindow uiw
      pure (bs:acc)) []

    IM.render

    pure bs

-- TODO: Eventually move drawFrame to a better contained renderer part

drawFrame :: (WorldConstraints w, HasCallStack)
          => Ghengin w ()
drawFrame = do

  extent <- lift getRenderExtent
  let
    -- The region of the framebuffer that the output will be rendered to. We
    -- render from (0,0) to (width, height) i.e. the whole framebuffer
    -- Defines a transformation from image to framebuffer
    viewport = Vk.Viewport {..} where
                 x = 0.0
                 y = 0.0
                 width  = fromIntegral $ extent.width
                 height = fromIntegral $ extent.height
                 minDepth = 0
                 maxDepth = 1

    -- Defines the region in which pixels will actually be stored. Any pixels
    -- outside of the scissor will be discarded. We keep it as the whole viewport
    scissor = Vk.Rect2D (Vk.Offset2D 0 0) extent

  {-
     Here's a rundown of the draw function:

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

   -}

  withCurrentFramePresent $ \cmdBuffer currentImage currentFrame -> do

  -- Draw frame is actually here, within 'withCurrentFramePresent'
  
    pipelines <- liftIO . readIORef =<< lift (asks (._extension._renderPipelines))
    forM pipelines $ \(SomeRenderPipeline pipeline materialsRef) -> do

      materials <- liftIO (readIORef materialsRef)
      let descriptorSet setIx = fst (pipeline._descriptorSetsSet NE.!! currentFrame) V.! setIx -- must guarantee that there exist this amount of sets in this pipeline
          descriptorSetBinding setIx bindingIx = fst $ (descriptorSet setIx)._bindings IM.! bindingIx

      -- TODO: Should be specific to each pipeline. E.g. if I have a color
      -- attribute that should be displayed that should be described in the
      -- pipeline constructor

      recordCommand cmdBuffer $ do

        renderPass pipeline._renderPass._renderPass (pipeline._renderPass._framebuffers V.! currentImage) extent $ do

          bindGraphicsPipeline (pipeline._graphicsPipeline._pipeline)
          setViewport viewport
          setScissor  scissor

          -- TODO: Bind pipeline-global data to descriptor set #0
          -- Get main camera, for the time being it's the only possible pipeline data for the shader
          -- The last camera will override the write buffer
          lift $ cmapM $ \(Camera proj view, fromMaybe noTransform -> camTr) -> do

            -- TODO: Some buffers should already be computed by the time we get to the draw phase: means we only have to bind things and that things only have a cost if changed?
            projM <- lift $ makeProjection proj
            let viewM = makeView camTr view

                ubo   = UBO viewM projM

            -- TODO : Move out of cmapM
            case descriptorSetBinding 0 0 of
              SomeMappedBuffer b -> lift $ writeMappedBuffer (unsafeCoerce b) ubo


          -- Bind descriptor set #0
          bindGraphicsDescriptorSet pipeline._graphicsPipeline._pipelineLayout
            0 (descriptorSet 0)._descriptorSet

          forM materials $ \(SomeMaterial material materialIndex) -> do

            -- These materials are compatible with this pipeline in the set #1,
            -- so the 'descriptorSetBinding' buffer will always be valid to
            -- write with the corresponding material binding
            () <- case material of
              Done -> pure ()
              -- StaticMaterial -> undefined -- TODO: Bind the static descriptor set
              DynamicBinding (a :: α) _ -> do
                  case descriptorSetBinding 1 0 of
                    -- TODO: Ensure unsafeCoerce is safe here by only allowing
                    -- the construction of dynamic materials if validated at
                    -- compile time against the shader pipeline in each
                    -- matching position
                    SomeMappedBuffer (unsafeCoerce -> buf :: MappedBuffer α) ->
                      lift . lift $ writeMappedBuffer buf a

            -- static bindings will have to choose a different dset
            -- Bind descriptor set #1
            bindGraphicsDescriptorSet pipeline._graphicsPipeline._pipelineLayout
              1 (descriptorSet 1)._descriptorSet

            embed cmapM $ \(mesh :: Mesh, SharedMaterial pipIx matIx, fromMaybe (ModelMatrix identity 0) -> ModelMatrix mm _) -> do

              -- TODO: Is it bad that we're going over *all* meshes for each
              -- material? Probably yes, we should have a good scene graph
              -- that sorts material in render order, handles hierarchy, and
              -- can additionally partition the visible space :)
              when (pipIx == pipeline._index && matIx == materialIndex) $ do

                -- TODO: Bind descriptor set #2

                pushConstants pipeline._graphicsPipeline._pipelineLayout Vk.SHADER_STAGE_VERTEX_BIT mm
                renderMesh mesh

          -- Draw UI
          IM.renderDrawData =<< IM.getDrawData

      
  pure ()


pattern MAX_FRAME_TIME :: Float
pattern MAX_FRAME_TIME = 0.5
