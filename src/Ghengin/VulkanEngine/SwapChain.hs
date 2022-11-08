{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Ghengin.VulkanEngine.SwapChain where

import Data.Word
import Data.Ord
import qualified Data.List as L
import qualified Data.Vector as V

import qualified Graphics.UI.GLFW as GLFW
import qualified Vulkan as Vk

import Ghengin.VulkanEngine.QueueFamilies
import Ghengin.VulkanEngine.GLFW.Window

data SwapChainSupportDetails = SCSD { _capabilities :: Vk.SurfaceCapabilitiesKHR
                                    , _formats      :: V.Vector Vk.SurfaceFormatKHR
                                    , _presentModes :: V.Vector Vk.PresentModeKHR
                                    }

querySwapChainSupport :: Vk.PhysicalDevice -> Vk.SurfaceKHR -> IO SwapChainSupportDetails
querySwapChainSupport p s = do
  surfaceCapabilities <- Vk.getPhysicalDeviceSurfaceCapabilitiesKHR p s
  (_, surfaceFormats) <- Vk.getPhysicalDeviceSurfaceFormatsKHR p s
  (_, surfacePresentModes) <- Vk.getPhysicalDeviceSurfacePresentModesKHR p s
  pure $ SCSD surfaceCapabilities surfaceFormats surfacePresentModes

chooseSwapSurfaceFormat :: V.Vector Vk.SurfaceFormatKHR
                        -> Vk.SurfaceFormatKHR
chooseSwapSurfaceFormat (V.toList -> availableFormats) =
  case L.sortOn (Down . rateSurfaceFormat) availableFormats of
    [] -> error "chooseSwapSurfaceFormat: no available surfaceFormat"
    x:_ -> x

  where
    rateSurfaceFormat :: Vk.SurfaceFormatKHR -> Bool
    rateSurfaceFormat availableFormat =
      availableFormat.format == Vk.FORMAT_B8G8R8A8_SRGB &&
        availableFormat.colorSpace == Vk.COLOR_SPACE_SRGB_NONLINEAR_KHR


chooseSwapPresentMode :: V.Vector Vk.PresentModeKHR
                        -> Vk.PresentModeKHR
chooseSwapPresentMode (V.toList -> availablePresentModes) =
  case filter hasMailboxMode availablePresentModes of
    [] -> Vk.PRESENT_MODE_FIFO_KHR -- Guaranteed to be available
    x:_ -> x                       -- Mailbox mode available

  where
    hasMailboxMode :: Vk.PresentModeKHR -> Bool
    hasMailboxMode = (==) Vk.PRESENT_MODE_MAILBOX_KHR

chooseSwapExtent :: Window -> Vk.SurfaceCapabilitiesKHR -> IO Vk.Extent2D 
chooseSwapExtent (W win) capabilities =
  if capabilities.currentExtent.width /= maxBound @Word32
     then pure capabilities.currentExtent
     else do
       (w,h) <- GLFW.getFramebufferSize win
       pure $ Vk.Extent2D (clamp (capabilities.minImageExtent.width,  capabilities.maxImageExtent.width) (fromIntegral w))
                          (clamp (capabilities.minImageExtent.height, capabilities.maxImageExtent.height) (fromIntegral h))

createSwapChain :: Vk.PhysicalDevice -> Vk.SurfaceKHR -> Window -> QueueFamiliesIndices -> Vk.Device -> IO (Vk.SwapchainKHR, V.Vector Vk.Image, Vk.SurfaceFormatKHR, Vk.Extent2D)
createSwapChain pd sr win qfi device = do
  scsd <- querySwapChainSupport pd sr

  let ssurfaceFormat = chooseSwapSurfaceFormat scsd._formats
      spresentMode   = chooseSwapPresentMode scsd._presentModes

  sextent <- chooseSwapExtent win scsd._capabilities

  let desiredIC  = scsd._capabilities.minImageCount + 1
      imageCount = if scsd._capabilities.maxImageCount > 0            -- 0 indicates there is no maximum
                      && desiredIC > scsd._capabilities.maxImageCount -- We can't ask for more than there are available
                        then scsd._capabilities.maxImageCount         -- Simply take the max
                        else desiredIC                                -- Min + 1 so we don't need to wait for the driver before we can acquire another image to draw to

      areDifferentFamily = qfi._graphicsFamily /= qfi._presentFamily
      indices = [qfi._graphicsFamily, qfi._presentFamily] :: V.Vector Word32
      config = Vk.SwapchainCreateInfoKHR {..} where
                 next = ()
                 flags = Vk.SwapchainCreateFlagBitsKHR 0
                 surface = sr
                 minImageCount = imageCount
                 imageFormat = ssurfaceFormat.format
                 imageColorSpace = ssurfaceFormat.colorSpace
                 imageExtent = sextent
                 imageArrayLayers = 1
                 imageUsage = Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT
                 imageSharingMode = if areDifferentFamily then Vk.SHARING_MODE_CONCURRENT else Vk.SHARING_MODE_EXCLUSIVE
                 queueFamilyIndices = if areDifferentFamily then indices else []
                 preTransform = scsd._capabilities.currentTransform
                 compositeAlpha = Vk.COMPOSITE_ALPHA_OPAQUE_BIT_KHR
                 presentMode = spresentMode
                 clipped = True
                 oldSwapchain = Vk.NULL_HANDLE


  swpc <- Vk.createSwapchainKHR device config Nothing
  (_, swpchainImages) <- Vk.getSwapchainImagesKHR device swpc
  pure (swpc,swpchainImages,ssurfaceFormat,sextent)
  
destroySwapChain :: Vk.Device -> Vk.SwapchainKHR -> IO ()
destroySwapChain d swpc = Vk.destroySwapchainKHR d swpc Nothing
