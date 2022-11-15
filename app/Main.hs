{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Monad.Reader
import Control.Monad.IO.Class
import qualified Data.Vector as V
-- import Ghengin.VulkanEngine as VE
-- import Ghengin.VulkanEngine.GLFW.Window as G
-- import Ghengin.VulkanEngine.Command
-- import Ghengin.VulkanEngine.Queue
import Vulkan.Zero (zero)
import qualified Vulkan as Vk

import Ghengin.Vulkan.GLFW.Window
import Ghengin.Vulkan.Command
import Ghengin.Vulkan.Pipeline
import Ghengin.Vulkan.Synchronization
import Ghengin.Vulkan
import Ghengin

import qualified Ghengin.Shaders.SimpleShader as SimpleShader
import Ghengin.Shaders

-- drawFrame = do
--   acquireNextImage
--   recordCommandBuf buf drawCommand


main :: IO ()
main = runVulkanRenderer $ ask >>= \renv -> do

  (_, nExts) <- Vk.enumerateInstanceExtensionProperties Nothing

  liftIO $ putStr "Extensions: " >> print nExts

  vert <- compileFIRShader SimpleShader.vertex
  frag <- compileFIRShader SimpleShader.fragment

  withGraphicsPipeline vert frag _ $ \pipeline ->
    withFence                        $ \inFlightFence ->
      withSemaphore                    $ \imageAvailableSem ->
        withSemaphore                    $ \renderFinishedSem -> do

         gameLoop $ do
           drawFrame pipeline inFlightFence imageAvailableSem renderFinishedSem

  Vk.deviceWaitIdle renv._vulkanDevice._device
  liftIO $ putStrLn "Goodbye"


drawFrame :: VulkanPipeline -> Vk.Fence -> Vk.Semaphore -> Vk.Semaphore -> Renderer ()
drawFrame pipeline inFlightFence imageAvailableSem renderFinishedSem = asks (._commandBuffer) >>= \cmdBuffer -> getDevice >>= \device -> do
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

  recordCommand cmdBuffer $ do

    renderPass _ (eng.vkSwapChainFramebuffers V.! i) extent $ do

      bindGraphicsPipeline (pipeline._pipeline)
      setViewport viewport
      setScissor  scissor

      draw 3

  submitGraphicsQueue cmdBuffer imageAvailableSem renderFinishedSem inFlightFence

  presentPresentQueue renderFinishedSem i

  pure ()



