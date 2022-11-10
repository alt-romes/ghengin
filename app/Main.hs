{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Ghengin.VulkanEngine as VE
import Ghengin.VulkanEngine.GLFW.Window as G
import Ghengin.VulkanEngine.Command
import Ghengin.VulkanEngine.Queue
import Vulkan.Zero (zero)
import qualified Vulkan as Vk

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
  recordCommandBuffer eng.vkSwapChainExtent eng.vkSwapChainFramebuffers eng.vkRenderPass eng.vkPipeline eng.vkCommandBuffer i
  submitQueue eng.vkGraphicsQueue eng.vkCommandBuffer eng.vkImageAvailableSem eng.vkRenderFinishedSem eng.vkInFlightFence
  queuePresent eng.vkPresentQueue eng.vkRenderFinishedSem eng.vkSwapChain i
  pure ()





