{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLists #-}
module Ghengin
  ( module Ghengin
  , module Apecs
  , module Geomancy.Vec3
  , module Geomancy.Mat4
  ) where

import GHC.Records
import Unsafe.Coerce

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

import Ghengin.Shaders

import qualified Ghengin.DearImGui as IM

import Ghengin.Component.Camera
import Ghengin.Component.Mesh
import Ghengin.Component.Transform
import Ghengin.Component.UI


-- TODO: Somehow systems that want to delete entities should call a special
-- destructor function that gets rid of resources stuck in components such as
-- meshes


type Ghengin w a = SystemT w (Renderer GEnv) a

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

type WorldConstraints w = (HasField "renderPackets" w (Storage RenderPacket), HasField "transforms" w (Storage Transform), HasField "cameras" w (Storage Camera), HasField "uiwindows" w (Storage UIWindow))
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
  renderPackets <- cfold (\acc (renderPacket :: RenderPacket) -> (renderPacket:acc)) []
  imCtx <- lift $ IM.initImGui $ case (head renderPackets) of RenderPacket {_renderPipeline = pp} -> pp._renderPass._renderPass

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

drawFrame :: WorldConstraints w
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


  -- Get main camera, we currently can't do anything without it
  cfold (\acc (cam :: Camera, tr :: Maybe Transform) -> (cam, fromMaybe noTransform tr):acc) [] >>= \case
    [] -> liftIO $ fail "No camera"
    (Camera proj view, camTr):_ -> do

      projM <- lift $ makeProjection proj
      let viewM = makeView camTr view

      -- BIG:TODO: Iterate over known graphics pipelines rather than switching it for every different mesh...

      -- Get each object mesh render cmd
      renderPackets <- cfold (\acc (renderPacket :: RenderPacket, tr :: Maybe Transform) -> (renderPacket, fromMaybe noTransform tr):acc) []
      -- let wrongSharedPipeline = (fst $ head renderPackets)._renderPipeline
      --     (wspp, wsrp, wsds) = (wrongSharedPipeline._graphicsPipeline, wrongSharedPipeline._renderPass, fmap fst $ wrongSharedPipeline._descriptorSetsSet)

      -- TODO: move out
      withCurrentFramePresent $ \cmdBuffer currentImage currentFrame -> do

      -- Draw frame is actually here, within 'withCurrentFramePresent'
      
        pipelines <- liftIO . readIORef =<< lift (asks (._extension._renderPipelines))
        forM pipelines $ \(SomeRenderPipeline pipeline) -> do

          -- TODO: Should be specific to each pipeline. E.g. if I have a color
          -- attribute that should be displayed that should be described in the
          -- pipeline constructor
          -- TODO: Should be done in separate stages: descriptor sets with different indexes get bound a different number of times...
          lift $ writeMappedBuffer (case fst $ ((fmap fst (pipeline._descriptorSetsSet) NE.!! currentFrame) V.! 0)._bindings IM.! 0 of SomeMappedBuffer b -> unsafeCoerce b) (UBO viewM projM)

          recordCommand cmdBuffer $ do

            renderPass pipeline._renderPass._renderPass (pipeline._renderPass._framebuffers V.! currentImage) extent $ do

              bindGraphicsPipeline (pipeline._graphicsPipeline._pipeline)
              setViewport viewport
              setScissor  scissor

              -- TODO: Render all meshes that share the same descriptor sets and graphics pipeline...
              -- forM_ meshRenderCmds $ \(meshRenderCmd, transform) -> do

              bindGraphicsDescriptorSets pipeline._graphicsPipeline._pipelineLayout (fmap (._descriptorSet) $ fmap fst (pipeline._descriptorSetsSet) NE.!! currentFrame)
              -- pushConstants wspp._pipelineLayout Vk.SHADER_STAGE_VERTEX_BIT (makeTransform transform)
              pushConstants pipeline._graphicsPipeline._pipelineLayout Vk.SHADER_STAGE_VERTEX_BIT (makeTransform (snd $ head renderPackets))
              renderMesh (fst $ head renderPackets)._renderMesh

              -- Draw UI
              IM.renderDrawData =<< IM.getDrawData
          
  pure ()


pattern MAX_FRAME_TIME :: Float
pattern MAX_FRAME_TIME = 0.5
