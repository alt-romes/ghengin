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

import Foreign.Storable
import Foreign.Ptr

import Data.Maybe
import Control.Monad.Reader

import Data.IORef

import Data.Time.Clock
import Data.Vector (Vector)
import qualified Data.Vector as V
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

import Ghengin.Shaders
import qualified Ghengin.Shaders.SimpleShader as SimpleShader

import qualified Ghengin.DearImGui as IM

import Ghengin.Component.Camera
import Ghengin.Component.Mesh
import Ghengin.Component.Transform


-- TODO: Somehow systems that want to delete entities should call a special
-- destructor function that gets rid of resources stuck in components such as
-- meshes


type Ghengin w a = SystemT w Renderer a


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

type WorldConstraints w = (HasField "meshes" w (Storage Mesh), HasField "transforms" w (Storage Transform), HasField "cameras" w (Storage Camera))
ghengin :: WorldConstraints w
        => w           -- ^ World
        -> Ghengin w a -- ^ Init
        -> Ghengin w b -- ^ Run every simulation step (currently ignored)
        -> (a -> DeltaTime -> Ghengin w Bool) -- ^ Run every game loop? iteration. Bool indicates whether we should exit the gameloop
        -- -> Ghengin w c -- ^ Run every draw step?
        -> Ghengin w c -- ^ Run once the game is quit (for now that is when the window closed)
        -> IO ()
ghengin world initialize _simstep loopstep finalize = runVulkanRenderer . (`runSystem` world) $ do

  -- TODO: If the materials added to each entity are ordered, when we get all
  -- the materials will the materials be ordered? If so, we can simply render
  -- with them sequentially without being afraid of changing back and forwards
  a <- initialize

  vert <- liftIO $ compileFIRShader SimpleShader.vertex
  frag <- liftIO $ compileFIRShader SimpleShader.fragment
  
  -- BIG:TODO: Bundle descriptor sets, render passes, and pipelines into a single abstraction
  -- TODO: Use linear types here. Can I make the monad stack over a multiplicity polymorphic monad?
  descriptorSetLayout <- lift $ createDescriptorSetLayout
  simpleRenderPass   <- lift $ createSimpleRenderPass
  pipeline           <- lift $ createGraphicsPipeline vert frag simpleRenderPass._renderPass [descriptorSetLayout]

  -- Init ImGui for this render pass (should eventually be tied to the UI render pass)
  imCtx <- lift $ IM.initImGui simpleRenderPass._renderPass

  objUBs <- lift $ mapM (const createMappedUniformBuffer) [1..MAX_FRAMES_IN_FLIGHT]
  (dsets, dpool) <- lift $ createUniformBufferDescriptorSets [descriptorSetLayout | _ <- [1..MAX_FRAMES_IN_FLIGHT]]

  lift $ V.zipWithM_ writeUniformBufferDescriptorSet (fmap (.buffer) objUBs) dsets

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

    -- Game loop step
    b <- loopstep a (min MAX_FRAME_TIME $ realToFrac frameTime)

    -- DearImGui frame
    -- TODO: Draw UI (define all UI components in the frame)
    IM.vulkanNewFrame
    IM.glfwNewFrame
    IM.newFrame

    IM.showDemoWindow

    -- Render frame
    drawFrame pipeline simpleRenderPass objUBs dsets

    pure b

  Vk.deviceWaitIdle =<< lift getDevice

  lift $ do
    IM.destroyImCtx imCtx
    destroyDescriptorPool dpool
    mapM_ destroyUniformBuffer objUBs
    destroyDescriptorSetLayout descriptorSetLayout
    destroyRenderPass simpleRenderPass
    destroyPipeline pipeline

  _ <- finalize

  pure ()


-- TODO: Eventually move drawFrame to a better contained renderer part

drawFrame :: WorldConstraints w
          => VulkanPipeline
          -> VulkanRenderPass
          -> Vector (UniformBuffer UniformBufferObject)
          -> Vector Vk.DescriptorSet
          -> Ghengin w ()
drawFrame pipeline rpass objUBs dsets = do

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

  -- TODO: Is cfold as efficient as cmap?

  -- Get main camera
  cfold (\acc (cam :: Camera, tr :: Maybe Transform) -> (cam, fromMaybe noTransform tr):acc) [] >>= \case
    [] -> liftIO $ fail "No camera"
    (Camera proj view, camTr):_ -> do

      projM <- lift $ makeProjection proj
      let viewM = makeView camTr view

      -- Get each object mesh render cmd
      meshRenderCmds <- cfold (\acc (mesh :: Mesh, tr :: Maybe Transform) -> (renderMesh mesh, fromMaybe noTransform tr):acc) []

      lift $ withCurrentFramePresent $ \cmdBuffer currentImage currentFrame -> do

        -- Draw frame is actually here, within 'withCurrentFramePresent'
        
        -- Draw UI
        IM.render

        writeUniformBuffer (objUBs V.! currentFrame) (UBO viewM projM)

        recordCommand cmdBuffer $ do

          renderPass rpass._renderPass (rpass._framebuffers V.! currentImage) extent $ do

            bindGraphicsPipeline (pipeline._pipeline)
            setViewport viewport
            setScissor  scissor

            forM_ meshRenderCmds $ \(meshRenderCmd, transform) -> do

              bindGraphicsDescriptorSets pipeline._pipelineLayout [dsets V.! currentFrame]
              pushConstants pipeline._pipelineLayout Vk.SHADER_STAGE_VERTEX_BIT (makeTransform transform)
              meshRenderCmd

            IM.renderDrawData
        
  pure ()


pattern MAX_FRAME_TIME :: Float
pattern MAX_FRAME_TIME = 0.5
