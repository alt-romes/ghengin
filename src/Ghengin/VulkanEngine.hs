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
    , vkSwapChainImageViews :: !(V.Vector Vk.ImageView)
    , vkPipelineLayout :: !Vk.PipelineLayout
    , vkRenderPass     :: !Vk.RenderPass
    , vkPipeline       :: !Vk.Pipeline
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

  pure $ VulkanEngine inst physicalDevice device graphicsQueue presentQueue win surface swapChain swapChainImageViews pipelineLayout renderPass pipeline


cleanup :: VulkanEngine -> IO ()
cleanup (VulkanEngine inst _ device _ _ w s swpc scImgsViews pply renderPass pipeline) = do
  putStrLn "[START] Clean up"
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


getWin :: VulkanEngine -> Window
getWin (VulkanEngine { vkWindow = win }) = win

