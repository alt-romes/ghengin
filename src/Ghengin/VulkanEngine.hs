{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Ghengin.VulkanEngine where

import Data.Vector     qualified as V
import Data.ByteString qualified as BS

import Control.Exception

import Vulkan qualified as Vk

import Ghengin.VulkanEngine.Instance
import Ghengin.VulkanEngine.PhysicalDevice
import Ghengin.VulkanEngine.QueueFamilies
import Ghengin.VulkanEngine.SwapChain
import Ghengin.VulkanEngine.Device
import Ghengin.VulkanEngine.Queue
import Ghengin.VulkanEngine.GLFW.Window
import Ghengin.VulkanEngine.ImageView
import Ghengin.VulkanEngine.RenderPass
import Ghengin.VulkanEngine.FrameBuffer
import Ghengin.VulkanEngine.Command
import Ghengin.VulkanEngine.Synchronization
import Ghengin.Pipeline
import Ghengin.Shaders
import qualified Ghengin.Shaders.SimpleShader as SimpleShader

data VulkanEngine
  = VulkanEngine
    { vkInstance       :: !Vk.Instance
    , vkPhysicalDevice :: !Vk.PhysicalDevice
    , vkDevice         :: !Vk.Device
    , vkGraphicsQueue  :: !Vk.Queue
    , vkPresentQueue   :: !Vk.Queue
    , vkWindow         :: !Window
    , vkWindowSurface  :: !Vk.SurfaceKHR
    , vkSwapChain      :: !Vk.SwapchainKHR
    , vkSwapChainExtent :: !Vk.Extent2D
    , vkSwapChainImageViews :: !(V.Vector Vk.ImageView)
    , vkPipelineLayout :: !Vk.PipelineLayout
    , vkRenderPass     :: !Vk.RenderPass
    , vkPipeline       :: !Vk.Pipeline
    , vkSwapChainFramebuffers :: !(V.Vector Vk.Framebuffer)
    , vkCommandPool    :: !Vk.CommandPool
    , vkCommandBuffer  :: !Vk.CommandBuffer
    , vkImageAvailableSem :: !Vk.Semaphore
    , vkRenderFinishedSem :: !Vk.Semaphore
    , vkInFlightFence     :: !Vk.Fence
    }

validationLayers :: V.Vector BS.ByteString
validationLayers = [ "VK_LAYER_KHRONOS_validation"
                   ]

deviceExtensions :: V.Vector BS.ByteString
deviceExtensions = [ Vk.KHR_SWAPCHAIN_EXTENSION_NAME
                   ]


initVulkanEngine :: IO VulkanEngine
initVulkanEngine = do
  win            <- createWindow 800 600 "VulkanEngine"
  inst           <- createInstance validationLayers
  surface        <- createSurface inst win
  physicalDevice <- pickPhysicalDevice deviceExtensions inst surface
  Just qfi@(QFI i1 i2)  <- findQueueFamilies physicalDevice surface
  device         <- createLogicalDevice validationLayers deviceExtensions physicalDevice qfi
  graphicsQueue  <- getDeviceQueue device i1 0
  presentQueue   <- getDeviceQueue device i2 0
  (swapChain, swapChainImages, swapChainSurfaceFormat, swapChainExtent) <- createSwapChain physicalDevice surface win qfi device
  swapChainImageViews <- V.mapM (createImageView device swapChainSurfaceFormat.format) swapChainImages

  -- Graphics Pipeline
  v' <- compileFIRShader SimpleShader.vertex
  f' <- compileFIRShader SimpleShader.fragment

  renderPass <- createRenderPass device swapChainSurfaceFormat.format
  (pipeline, pipelineLayout) <- createGraphicsPipeline device swapChainExtent v' f' renderPass

  swapChainFramebuffers <- V.mapM (createFrameBuffer device renderPass swapChainExtent) swapChainImageViews

  commandPool <- createCommandPool device qfi
  [commandBuffer] <- createCommandBuffers device commandPool

  (imageAvailableSem, renderFinishedSem, inFlightFence) <- createSyncObjects device

  pure $ VulkanEngine inst physicalDevice device graphicsQueue presentQueue win surface swapChain swapChainExtent swapChainImageViews pipelineLayout renderPass pipeline swapChainFramebuffers commandPool commandBuffer imageAvailableSem renderFinishedSem inFlightFence 


cleanup :: VulkanEngine -> IO ()
cleanup (VulkanEngine inst _ device _ _ w s swpc _ scImgsViews pply renderPass pipeline scFramebuffers cpool _ s1 s2 f1) = do
  putStrLn "[START] Clean up"
  destroySem device s1
  destroySem device s2
  destroyFence device f1
  destroyCommandPool device cpool
  mapM_ (destroyFrameBuffer device) scFramebuffers
  destroyPipeline device pipeline
  destroyPipelineLayout device pply
  destroyRenderPass device renderPass
  mapM_ (destroyImageView device) scImgsViews
  destroySwapChain device swpc
  destroyLogicalDevice device
  destroySurface inst s
  destroyInstance inst
  destroyWindow w
  putStrLn "[DONE] Clean up"
  

withVulkanEngine :: (VulkanEngine -> IO a) -> IO a
withVulkanEngine = bracket initVulkanEngine cleanup

acquireNextImage :: VulkanEngine -> IO Int
acquireNextImage eng =
  fromIntegral . snd <$> Vk.acquireNextImageKHR eng.vkDevice eng.vkSwapChain maxBound eng.vkImageAvailableSem Vk.NULL_HANDLE


