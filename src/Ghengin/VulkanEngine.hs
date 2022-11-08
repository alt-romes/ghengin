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
  pure $ VulkanEngine inst physicalDevice device graphicsQueue presentQueue win surface swapChain


cleanup :: VulkanEngine -> IO ()
cleanup (VulkanEngine i _ d _ _ w s swpc) = do
  putStrLn "[START] Clean up"
  destroySwapChain d swpc
  destroyLogicalDevice d
  destroySurface i s
  destroyInstance i
  destroyWindow w
  putStrLn "[DONE] Clean up"
  

withVulkanEngine :: (VulkanEngine -> IO a) -> IO a
withVulkanEngine = bracket initVulkanEngine cleanup


getWin :: VulkanEngine -> Window
getWin (VulkanEngine { vkWindow = win }) = win

