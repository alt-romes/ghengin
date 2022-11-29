{-# LANGUAGE PatternSynonyms #-}
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

import Data.Maybe
import Control.Monad.Reader

import Data.IORef

import Data.Time.Clock
import Data.Vector (Vector)
import qualified Data.Vector as V
import Vulkan.Zero (zero)
import qualified Vulkan as Vk
import Apecs
import Geomancy.Vec3
import Geomancy.Mat4

import qualified Graphics.UI.GLFW as GLFW

import Ghengin.Vulkan.Command
import Ghengin.Vulkan.Pipeline
import Ghengin.Vulkan.RenderPass
import Ghengin.Vulkan.Synchronization
import Ghengin.Vulkan.GLFW.Window
import Ghengin.Vulkan

import Ghengin.Shaders
import qualified Ghengin.Shaders.SimpleShader as SimpleShader

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

  a <- initialize

  vert <- liftIO $ compileFIRShader SimpleShader.vertex
  frag <- liftIO $ compileFIRShader SimpleShader.fragment

  -- TODO: Use linear types here. Can I make the monad stack over a multiplicity polymorphic monad?
  simpleRenderPass   <- lift $ createSimpleRenderPass
  pipeline           <- lift $ createGraphicsPipeline vert frag simpleRenderPass._renderPass
  inFlightFence1     <- lift $ createFence True
  inFlightFence2     <- lift $ createFence True
  imageAvailableSem1 <- lift $ createSemaphore
  imageAvailableSem2 <- lift $ createSemaphore
  renderFinishedSem1 <- lift $ createSemaphore
  renderFinishedSem2 <- lift $ createSemaphore

  currentTime <- liftIO (getCurrentTime >>= newIORef)
  lastFPSTime <- liftIO (getCurrentTime >>= newIORef)
  frameCounter <- liftIO (newIORef (0 :: Int))
  framesInFlightCounter <- liftIO(newIORef 0)
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

    n <- liftIO(readIORef framesInFlightCounter)

    b <- loopstep a (min MAX_FRAME_TIME $ realToFrac frameTime)

    drawFrame pipeline simpleRenderPass
              [inFlightFence1, inFlightFence2]
              [imageAvailableSem1, imageAvailableSem2]
              [renderFinishedSem1, renderFinishedSem2]
              n

    liftIO(modifyIORef' framesInFlightCounter (\x -> (x + 1) `rem` fromIntegral MAX_FRAMES_IN_FLIGHT))

    pure b

  Vk.deviceWaitIdle =<< lift getDevice

  lift $ do
    destroyRenderPass simpleRenderPass
    destroyPipeline pipeline
    destroyFence inFlightFence1
    destroyFence inFlightFence2
    destroySem imageAvailableSem1
    destroySem imageAvailableSem2
    destroySem renderFinishedSem1
    destroySem renderFinishedSem2

  _ <- finalize

  pure ()


-- TODO: Eventually move drawFrame to a better contained renderer part

drawFrame :: WorldConstraints w
          => VulkanPipeline
          -> VulkanRenderPass
          -> Vector Vk.Fence
          -> Vector Vk.Semaphore
          -> Vector Vk.Semaphore
          -> Int -- ^ Frame number
          -> Ghengin w ()
drawFrame pipeline rpass inFlightFences imageAvailableSems renderFinishedSems n = lift (asks (._commandBuffers)) >>= \cmdBuffers -> lift getDevice >>= \device -> do

  let cmdBuffer = cmdBuffers V.! n
      inFlightFence = inFlightFences V.! n
      imageAvailableSem = imageAvailableSems V.! n
      renderFinishedSem = renderFinishedSems V.! n

  -- Wait for the previous frame to finish
  -- Acquire an image from the swap chain
  -- Record a command buffer which draws the scene onto that image
  -- Submit the recorded command buffer
  -- Present the swap chain image 
  _ <- Vk.waitForFences device [inFlightFence] True maxBound
  Vk.resetFences device [inFlightFence]

  i <- lift $ acquireNextImage imageAvailableSem

  Vk.resetCommandBuffer cmdBuffer zero

  extent <- lift $ getRenderExtent

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

      -- Render each object mesh
      meshRenderCmds <- cfold (\acc (mesh :: Mesh, tr :: Maybe Transform) -> (renderMesh mesh, fromMaybe noTransform tr):acc) []

      recordCommand cmdBuffer $ do

        renderPass rpass._renderPass (rpass._framebuffers V.! i) extent $ do

          bindGraphicsPipeline (pipeline._pipeline)
          setViewport viewport
          setScissor  scissor

          forM_ meshRenderCmds $ \(meshRenderCmd, transform) -> do

            pushConstants pipeline._pipelineLayout Vk.SHADER_STAGE_VERTEX_BIT (makeTransform transform <> viewM <> projM)
            meshRenderCmd
        


  lift $ do

    submitGraphicsQueue cmdBuffer imageAvailableSem renderFinishedSem inFlightFence

    presentPresentQueue renderFinishedSem i

  pure ()


getKey :: GLFW.Key -> Ghengin w GLFW.KeyState
getKey k = lift $ do
  w <- asks (._vulkanWindow._window)
  liftIO $ GLFW.getKey w k

ifPressed :: GLFW.Key
          -> Ghengin w a -- ^ Then
          -> Ghengin w a -- ^ Else
          -> Ghengin w a -- ^ Result
ifPressed k t e = do
  getKey k >>= \case
    GLFW.KeyState'Pressed -> t
    _ -> e

pattern MAX_FRAME_TIME :: Float
pattern MAX_FRAME_TIME = 0.5
