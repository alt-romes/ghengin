{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE BlockArguments #-}
module Ghengin.Vulkan.Renderer.SwapChain (VulkanSwapChain(..), createSwapChain, destroySwapChain) where

import Prelude hiding (($))
import Prelude.Linear (($))
import Data.Ord
import Data.Word
import qualified Unsafe.Linear as Unsafe
import qualified Control.Monad.IO.Class.Linear as Linear

import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.List as L

import qualified Graphics.UI.GLFW as GLFW
import qualified Vulkan as Vk

import Ghengin.Vulkan.Renderer.Device
import Ghengin.Vulkan.Renderer.GLFW.Window
import Ghengin.Vulkan.Renderer.Image

data VulkanSwapChain = VulkanSwapChain { _swapchain     :: !Vk.SwapchainKHR
                                       , _imageViews    :: !(Vector Vk.ImageView)
                                       , _surfaceFormat :: !Vk.SurfaceFormatKHR
                                       , _surfaceExtent :: !Vk.Extent2D
                                       , _depthImage    :: !VulkanImage
                                       }

createSwapChain :: Linear.MonadIO m => VulkanWindow ⊸ VulkanDevice ⊸ m (VulkanSwapChain, VulkanWindow, VulkanDevice)
createSwapChain = Unsafe.toLinear2 \win device -> Linear.liftSystemIO $ do

  let physicalDevice = device._physicalDevice
      surface        = win._surface

  -- Query Swapchain Support
  surfaceCapabilities      <- Vk.getPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice surface
  (_, surfaceFormats)      <- Vk.getPhysicalDeviceSurfaceFormatsKHR physicalDevice surface
  (_, surfacePresentModes) <- Vk.getPhysicalDeviceSurfacePresentModesKHR physicalDevice surface

  let surfaceFormat = chooseSwapSurfaceFormat surfaceFormats
      presentMode   = chooseSwapPresentMode   surfacePresentModes

  extent <- chooseSwapExtent win._window surfaceCapabilities

  let desiredIC  = surfaceCapabilities.minImageCount + 1
      imageCount = if surfaceCapabilities.maxImageCount > 0            -- 0 indicates there is no maximum
                      && desiredIC > surfaceCapabilities.maxImageCount -- We can't ask for more than there are available
                        then surfaceCapabilities.maxImageCount         -- Simply take the max
                        else desiredIC                                 -- Min + 1 so we don't need to wait for the driver before we can acquire another image to draw to

      areDifferentFamily = device._graphicsQueueFamily /= device._presentQueueFamily
      indices = [device._graphicsQueueFamily, device._presentQueueFamily] :: V.Vector Word32
      config = Vk.SwapchainCreateInfoKHR { next = ()
                                         , flags = Vk.SwapchainCreateFlagBitsKHR 0
                                         , surface = surface
                                         , minImageCount = imageCount
                                         , imageFormat = surfaceFormat.format
                                         , imageColorSpace = surfaceFormat.colorSpace
                                         , imageExtent = extent
                                         , imageArrayLayers = 1
                                         , imageUsage = Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT
                                         , imageSharingMode = if areDifferentFamily then Vk.SHARING_MODE_CONCURRENT else Vk.SHARING_MODE_EXCLUSIVE
                                         , queueFamilyIndices = if areDifferentFamily then indices else []
                                         , preTransform = surfaceCapabilities.currentTransform
                                         , compositeAlpha = Vk.COMPOSITE_ALPHA_OPAQUE_BIT_KHR
                                         , presentMode = presentMode
                                         , clipped = True
                                         , oldSwapchain = Vk.NULL_HANDLE
                                         }

  swpc <- Vk.createSwapchainKHR device._device config Nothing
  (_, swpchainImages) <- Vk.getSwapchainImagesKHR device._device swpc
  swpchainImageViews  <- V.mapM (createImageView device._device surfaceFormat.format Vk.IMAGE_ASPECT_COLOR_BIT) swpchainImages

  let
    depthFormat = Vk.FORMAT_D32_SFLOAT -- We could query for supported formats and choose the best

  depthImage <- createImage device depthFormat (Vk.Extent3D extent.width extent.height 1) Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT Vk.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT Vk.IMAGE_ASPECT_DEPTH_BIT

  pure (VulkanSwapChain swpc swpchainImageViews surfaceFormat extent depthImage, win, device)


destroySwapChain :: Linear.MonadIO m => VulkanDevice ⊸ VulkanSwapChain ⊸ m VulkanDevice
destroySwapChain = Unsafe.toLinear2 \d swpc -> Linear.liftSystemIO $ do

  Vk.destroyImageView d._device swpc._depthImage._imageView Nothing
  Vk.destroyImage     d._device swpc._depthImage._image     Nothing
  Vk.freeMemory       d._device swpc._depthImage._devMem    Nothing

  mapM_ (destroyImageView d._device) swpc._imageViews
  Vk.destroySwapchainKHR d._device swpc._swapchain Nothing
  pure d


chooseSwapSurfaceFormat :: V.Vector Vk.SurfaceFormatKHR -> Vk.SurfaceFormatKHR
chooseSwapSurfaceFormat (V.toList -> availableFormats) =
  case L.sortOn (Down . rateSurfaceFormat) availableFormats of
    [] -> error "chooseSwapSurfaceFormat: no available surfaceFormat"
    x:_ -> x
  where
    rateSurfaceFormat :: Vk.SurfaceFormatKHR -> Bool
    rateSurfaceFormat availableFormat =
      availableFormat.format == Vk.FORMAT_B8G8R8A8_SRGB &&
        availableFormat.colorSpace == Vk.COLOR_SPACE_SRGB_NONLINEAR_KHR

chooseSwapPresentMode :: V.Vector Vk.PresentModeKHR -> Vk.PresentModeKHR
chooseSwapPresentMode availablePresentModes =
  case V.uncons $ V.filter hasMailboxMode availablePresentModes of
    Nothing -> Vk.PRESENT_MODE_FIFO_KHR -- Guaranteed to be available
    Just (x, _) -> x                    -- Mailbox mode available
  where
    hasMailboxMode :: Vk.PresentModeKHR -> Bool
    hasMailboxMode = (==) Vk.PRESENT_MODE_MAILBOX_KHR

chooseSwapExtent :: GLFW.Window -> Vk.SurfaceCapabilitiesKHR -> IO Vk.Extent2D 
chooseSwapExtent win capabilities =
  if capabilities.currentExtent.width /= maxBound @Word32
     then pure capabilities.currentExtent
     else do
       (w,h) <- GLFW.getFramebufferSize win
       pure $ Vk.Extent2D (clamp (capabilities.minImageExtent.width,  capabilities.maxImageExtent.width) (fromIntegral w))
                          (clamp (capabilities.minImageExtent.height, capabilities.maxImageExtent.height) (fromIntegral h))

