{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE BlockArguments #-}
module Ghengin.Vulkan.Renderer.Context.Swapchain
  ( SwapchainInfo(..)
  , createSwapchain, destroySwapchain
  , chooseSwapchainFormat
  )
  where

import Prelude hiding (($))
import Prelude.Linear (($), Ur(..))
import Data.Ord
import Data.Word
import qualified Unsafe.Linear as Unsafe
import qualified Control.Functor.Linear as Linear
import qualified Control.Monad.IO.Class.Linear as Linear

import qualified Data.Vector as V
import qualified Data.V.Linear as VL
import qualified Data.List as L

import qualified Vulkan as Vk
import qualified Vulkan as Vk.Surface
  ( SurfaceFormatKHR(..)
  , SurfaceCapabilitiesKHR(..) )
import qualified Vulkan.Zero as Vk

import Ghengin.Vulkan.Renderer.GLFW.Window
import Ghengin.Core.Prelude (withSized)
import Ghengin.Core.Type.Utils (With(..))

import GHC.TypeNats
--------------------------------------------------------------------------------
data SwapchainInfo (n :: Nat)
  = SwapchainInfo
      { swapchain        :: Vk.SwapchainKHR
      , swapchainImages  :: VL.V n Vk.Image
        -- ^ These images are managed by the swapchain, and should never be
        -- freed directly. See 'destroySwapchain'.
        -- Note: the N frames here do not necessarily match the number of
        -- frames-in-flight.
      , swapchainSurface :: Vk.SurfaceKHR
      , swapchainExtent  :: Ur Vk.Extent2D
      , surfaceFormat    :: Ur Vk.SurfaceFormatKHR
      }

createSwapchain
  :: Linear.MonadIO m
  => Vk.PhysicalDevice %1
  -> Vk.Device %1
  -> Vk.SurfaceKHR %1
  -> Vk.SurfaceFormatKHR
  -> Vk.ImageUsageFlags
  -> m (SwapchainInfo `With` KnownNat, Vk.PhysicalDevice, Vk.Device)
createSwapchain = Unsafe.toLinear3 \physicalDevice device surface surfaceFormat imageUsage -> Linear.liftSystemIO $ do

  surfaceCapabilities <- Vk.getPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice surface
  (_, presentModes)   <- Vk.getPhysicalDeviceSurfacePresentModesKHR physicalDevice surface

  let
    minImageCount, maxImageCount, imageCount :: Word32
    minImageCount = Vk.Surface.minImageCount surfaceCapabilities
    maxImageCount = Vk.Surface.maxImageCount surfaceCapabilities
    imageCount
      | maxImageCount == 0 = minImageCount + 1 -- no maximum
      | otherwise = min ( minImageCount + 1 ) maxImageCount

    currentExtent :: Vk.Extent2D
    currentExtent = Vk.Surface.currentExtent surfaceCapabilities

    currentTransform :: Vk.SurfaceTransformFlagBitsKHR
    currentTransform = Vk.Surface.currentTransform surfaceCapabilities

    swapchainCreateInfo :: Vk.SwapchainCreateInfoKHR '[]
    swapchainCreateInfo =
      Vk.SwapchainCreateInfoKHR
        { next                  = ()
        , flags                 = Vk.zero
        , surface               = surface
        , minImageCount         = imageCount
        , imageFormat           = Vk.Surface.format     surfaceFormat
        , imageColorSpace       = Vk.Surface.colorSpace surfaceFormat
        , imageExtent           = currentExtent
        , imageArrayLayers      = 1
        , imageUsage            = imageUsage
        , imageSharingMode      = Vk.SHARING_MODE_EXCLUSIVE
        , queueFamilyIndices    = V.empty
        , preTransform          = currentTransform -- or VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR
        , compositeAlpha        = Vk.COMPOSITE_ALPHA_OPAQUE_BIT_KHR
        , presentMode           = chooseSwapchainPresentMode presentModes
        , clipped               = True
        , oldSwapchain          = Vk.NULL_HANDLE
        }

  swapchain              <- Vk.createSwapchainKHR device swapchainCreateInfo Nothing
  (_, swapchainImageVec) <- Vk.getSwapchainImagesKHR device swapchain
  withSized swapchainImageVec \ swapchainImages -> Linear.do
    let swapchainInfo = SwapchainInfo
          { swapchain
          , swapchainImages
          , swapchainSurface = surface
          , swapchainExtent  = Ur currentExtent
          , surfaceFormat    = Ur surfaceFormat
          }
    pure (SomeWith swapchainInfo, physicalDevice, device)

destroySwapchain :: Linear.MonadIO m
                 => Vk.Instance %1
                 -> Vk.Device %1
                 -> SwapchainInfo n %1
                 -> m (Vk.Instance, Vk.Device)
destroySwapchain = Unsafe.toLinear3 \inst device
  SwapchainInfo
    { swapchain
    , swapchainImages = _ {- managed by swapchain, not us! -}
    , swapchainSurface
    , swapchainExtent = Ur _
    , surfaceFormat = Ur _
    } -> Linear.do
      Linear.liftSystemIO $ Vk.destroySwapchainKHR device swapchain Nothing
      inst <- destroySurface inst swapchainSurface
      Linear.pure (inst, device)

--------------------------------------------------------------------------------
-- Choosing Swapchain properties
--------------------------------------------------------------------------------

chooseSwapchainFormat
  :: Linear.MonadIO m
  => Vk.SurfaceFormatKHR
  -> Vk.PhysicalDevice %1
  -> Vk.SurfaceKHR %1
  -> m (Ur Vk.SurfaceFormatKHR, Vk.PhysicalDevice, Vk.SurfaceKHR)
chooseSwapchainFormat
  preferredFormat@( Vk.SurfaceFormatKHR fmt_p spc_p )
  = Unsafe.toLinear2 \physicalDevice surface -> Linear.liftSystemIO $ do
      surfaceFormats <- snd <$> Vk.getPhysicalDeviceSurfaceFormatsKHR physicalDevice surface

      case L.sortOn ( Down . score ) ( V.toList surfaceFormats ) of
        [] -> error "No formats found."
        ( best : _ )
          | Vk.FORMAT_UNDEFINED <- Vk.Surface.format best
            -> pure (Ur preferredFormat, physicalDevice, surface)
          | otherwise
            -> pure (Ur best, physicalDevice, surface)

    where
      match :: Eq a => a -> a -> Int
      match a b
        | a == b    = 1
        | otherwise = 0

      score :: Vk.SurfaceFormatKHR -> Int
      score ( Vk.SurfaceFormatKHR fmt spc )
        = match fmt fmt_p
        + match spc spc_p

chooseSwapchainPresentMode :: V.Vector Vk.PresentModeKHR -> Vk.PresentModeKHR
chooseSwapchainPresentMode availablePresentModes =
  case V.uncons $ V.filter hasMailboxMode availablePresentModes of
    Nothing -> Vk.PRESENT_MODE_FIFO_KHR -- Guaranteed to be available
    Just (x, _) -> x                    -- Mailbox mode available
  where
    hasMailboxMode :: Vk.PresentModeKHR -> Bool
    hasMailboxMode = (==) Vk.PRESENT_MODE_MAILBOX_KHR
