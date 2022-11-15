{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import qualified Data.Vector as V
import Ghengin.VulkanEngine as VE
import Ghengin.VulkanEngine.GLFW.Window as G
import Ghengin.VulkanEngine.Command
import Ghengin.VulkanEngine.Queue
import Vulkan.Zero (zero)
import qualified Vulkan as Vk

import Ghengin.Vulkan.Command

-- drawFrame = do
--   acquireNextImage
--   recordCommandBuf buf drawCommand


main :: IO ()
main = withVulkanEngine $ \eng -> do

  -- getRequiredInstanceExtensions
  (_, nExts) <- Vk.enumerateInstanceExtensionProperties Nothing

  putStr "Extensions: " >> print nExts

  loopUntilClosed (eng.vkWindow) $ do

    drawFrame eng

  Vk.deviceWaitIdle eng.vkDevice

  putStrLn "Goodbye"


drawFrame :: VulkanEngine -> IO ()
drawFrame eng = do
  -- Wait for the previous frame to finish
  -- Acquire an image from the swap chain
  -- Record a command buffer which draws the scene onto that image
  -- Submit the recorded command buffer
  -- Present the swap chain image 
  _ <- Vk.waitForFences eng.vkDevice [eng.vkInFlightFence] True maxBound
  Vk.resetFences eng.vkDevice [eng.vkInFlightFence]
  i <- acquireNextImage eng

  Vk.resetCommandBuffer eng.vkCommandBuffer zero

  let
    -- The region of the framebuffer that the output will be rendered to. We
    -- render from (0,0) to (width, height) i.e. the whole framebuffer
    -- Defines a transformation from image to framebuffer
    viewport = Vk.Viewport {..} where
                 x = 0.0
                 y = 0.0
                 width  = fromIntegral $ eng.vkSwapChainExtent.width
                 height = fromIntegral $ eng.vkSwapChainExtent.height
                 minDepth = 0
                 maxDepth = 1

    -- Defines the region in which pixels will actually be stored. Any pixels
    -- outside of the scissor will be discarded. We keep it as the whole viewport
    scissor = Vk.Rect2D (Vk.Offset2D 0 0) eng.vkSwapChainExtent

  recordCommand eng.vkCommandBuffer $ do

    renderPass eng.vkRenderPass (eng.vkSwapChainFramebuffers V.! i) eng.vkSwapChainExtent $ do

      bindGraphicsPipeline eng.vkPipeline
      setViewport viewport
      setScissor  scissor

      draw 3

  submitQueue eng.vkGraphicsQueue eng.vkCommandBuffer eng.vkImageAvailableSem eng.vkRenderFinishedSem eng.vkInFlightFence

  queuePresent eng.vkPresentQueue eng.vkRenderFinishedSem eng.vkSwapChain i

  pure ()



