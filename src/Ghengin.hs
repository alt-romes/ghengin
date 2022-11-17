{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLists #-}
module Ghengin where

import GHC.Records

import Control.Monad.Reader

import Data.IORef

import Data.Vector (Vector)
import qualified Data.Vector as V
import Vulkan.Zero (zero)
import qualified Vulkan as Vk
import Apecs

import Ghengin.Vulkan.Command
import Ghengin.Vulkan.Device
import Ghengin.Vulkan.Pipeline
import Ghengin.Vulkan.RenderPass
import Ghengin.Vulkan.Synchronization
import Ghengin.Vulkan.GLFW.Window
import Ghengin.Vulkan

import Ghengin.Shaders
import qualified Ghengin.Shaders.SimpleShader as SimpleShader

import Ghengin.Component.Mesh


-- TODO: Somehow systems that want to delete entities should call a special
-- destructor function that gets rid of resources stuck in components such as
-- meshes


type Ghengin w a = SystemT w Renderer a


windowLoop :: Ghengin w Bool -> Ghengin w ()
windowLoop action = do
  win <- lift (asks (._vulkanWindow._window))
  loopUntilClosedOr win action


ghengin :: HasField "meshes" w (Storage Mesh)
        => w           -- ^ World
        -> Ghengin w a -- ^ Init
        -> Ghengin w b -- ^ Run every simulation step (currently ignored)
        -> (a -> Ghengin w Bool) -- ^ Run every game loop? iteration. Bool indicates whether we should exit the gameloop
        -- -> Ghengin w c -- ^ Run every draw step?
        -> Ghengin w c -- ^ Run once the game is quit (for now that is when the window closed)
        -> IO ()
ghengin world initialize _simstep loopstep finalize = runVulkanRenderer . (`runSystem` world) $ do

  a <- initialize

  vert <- liftIO $ compileFIRShader SimpleShader.vertex
  frag <- liftIO $ compileFIRShader SimpleShader.fragment

  -- Use linear types here. Can I make the monad stack over a multiplicity polymorphic monad?
  withSimpleRenderPass $ \simpleRenderPass ->
    withGraphicsPipeline vert frag simpleRenderPass._renderPass $ \pipeline ->
      withFence True $ \inFlightFence1 ->
      withFence True $ \inFlightFence2 ->
        withSemaphore $ \imageAvailableSem1 ->
        withSemaphore $ \imageAvailableSem2 ->
          withSemaphore $ \renderFinishedSem1 ->
          withSemaphore $ \renderFinishedSem2 -> do

            renv <- ask

            counter <- liftIO(newIORef 0)
            windowLoop $ do

              n <- liftIO(readIORef counter)

              b <- loopstep a

              drawFrame pipeline simpleRenderPass
                        [inFlightFence1, inFlightFence2]
                        [imageAvailableSem1, imageAvailableSem2]
                        [renderFinishedSem1, renderFinishedSem2]
                        n

              liftIO(modifyIORef' counter (\x -> (x + 1) `rem` fromIntegral MAX_FRAMES_IN_FLIGHT))

              pure b

            Vk.deviceWaitIdle renv._vulkanDevice._device

  _ <- finalize

  pure ()


-- TODO: Eventually move to a better contained renderer part

-- drawFrame :: HasField "meshes" w (Storage Mesh) => Ghengin w ()
-- drawFrame = do

--   cmapM_ $ \(mesh :: Mesh) -> liftIO . print $ mesh


drawFrame :: VulkanPipeline
          -> VulkanRenderPass
          -> Vector Vk.Fence
          -> Vector Vk.Semaphore
          -> Vector Vk.Semaphore
          -> Int -- ^ Frame number
          -> Renderer ()
drawFrame pipeline rpass inFlightFences imageAvailableSems renderFinishedSems n = asks (._commandBuffers) >>= \cmdBuffers -> getDevice >>= \device -> do

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

  i <- acquireNextImage imageAvailableSem

  Vk.resetCommandBuffer cmdBuffer zero

  extent <- getRenderExtent

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

  liftIO $ recordCommand cmdBuffer $ do

    renderPass rpass._renderPass (rpass._framebuffers V.! i) extent $ do

      bindGraphicsPipeline (pipeline._pipeline)
      setViewport viewport
      setScissor  scissor

      draw 3

  submitGraphicsQueue cmdBuffer imageAvailableSem renderFinishedSem inFlightFence

  presentPresentQueue renderFinishedSem i

  pure ()
