{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Ghengin.VulkanEngine.SwapChain where

import Data.Word
import Data.Ord
import qualified Data.List as L
import qualified Data.Vector as V

import qualified Graphics.UI.GLFW as GLFW
import qualified Vulkan as Vk

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
