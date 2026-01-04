{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Inspired by FIR's fir-examples/src/Vulkan/Context.hs
module Ghengin.Vulkan.Renderer.Context where

-- base
import qualified Prelude as P
import Data.Bits
  ( (.|.) )
import GHC.TypeLits

-- bytestring
import Data.ByteString
  ( ByteString )

-- vulkan
import qualified Vulkan
import qualified Vulkan.Zero as Vulkan

-- ghengin-core
import Ghengin.Core.Prelude as Linear
import Ghengin.Core.Log
import Ghengin.Core.Type.Utils (With(..))

import Ghengin.Vulkan.Renderer.GLFW.Window as GLFW
import Ghengin.Vulkan.Renderer.Context.Swapchain
import Ghengin.Vulkan.Renderer.Context.Instance
import Ghengin.Vulkan.Renderer.Context.Device
import Ghengin.Vulkan.Renderer.Context.Device.Physical

import qualified Unsafe.Linear as Unsafe

----------------------------------------------------------------------------
-- Two different rendering contexts: with or without a swapchain.

data RenderingContext
  = Headless
  | WithSwapchain

data SRenderingContext (ctx :: RenderingContext) where
  SHeadless :: SRenderingContext Headless
  SWithSwapchain :: SRenderingContext WithSwapchain

class KnownRenderingContext ( ctx :: RenderingContext ) where
  renderingContext :: SRenderingContext ctx
instance KnownRenderingContext Headless where
  renderingContext = SHeadless
instance KnownRenderingContext WithSwapchain where
  renderingContext = SWithSwapchain

----------------------------------------------------------------------------

type family ContextSurfaceInfo ( ctx :: RenderingContext ) :: Type where
  ContextSurfaceInfo Headless      = ()
  ContextSurfaceInfo WithSwapchain = SurfaceInfo

data RenderInfo ( ctx :: RenderingContext ) where
  RenderInfo
    :: { queueType   :: Vulkan.QueueFlags
       , surfaceInfo :: ContextSurfaceInfo ctx
       }
    -> RenderInfo ctx

data SurfaceInfo
  = SurfaceInfo
  { surfaceWindow   :: GLFW.Window
  , preferredFormat :: Ur Vulkan.SurfaceFormatKHR
  , surfaceUsage    :: Ur [ Vulkan.ImageUsageFlags ]
  }

data family ContextSwapchainInfo ( ctx :: RenderingContext ) :: Type
data instance ContextSwapchainInfo Headless     = NoSwapchain
data instance ContextSwapchainInfo WithSwapchain where
  ASwapchainInfo :: KnownNat n => SwapchainInfo n %1 -> ContextSwapchainInfo WithSwapchain

data family ContextWindow ( ctx :: RenderingContext ) :: Type
data instance ContextWindow Headless     = NoWindow
data instance ContextWindow WithSwapchain where
  ContextWindow :: GLFW.Window %1 -> ContextWindow WithSwapchain

data VulkanContext ( ctx :: RenderingContext )
  = VulkanContext
  { vkInstance       :: Vulkan.Instance
  , physicalDevice   :: Vulkan.PhysicalDevice
  , device           :: Vulkan.Device
  , queueFamilyIndex :: Int
  , queue            :: Vulkan.Queue
  , aSwapchainInfo   :: ContextSwapchainInfo ctx
  , window           :: ContextWindow ctx
  }

type VulkanSwapchainContext = VulkanContext WithSwapchain
type VulkanHeadlessContext  = VulkanContext Headless

withSwapchainInfo
  :: ContextSwapchainInfo WithSwapchain
  -> ( forall n. KnownNat n => SwapchainInfo n %1 -> r )
  -> r
withSwapchainInfo ( ASwapchainInfo swapchain ) f = f swapchain

initialiseContext
  :: forall ctx m. ( KnownRenderingContext ctx, HasLogger m )
  => ByteString -> RenderInfo ctx -> m ( VulkanContext ctx )
initialiseContext appName ( RenderInfo { queueType, surfaceInfo } ) = Linear.do

  vkInstance <-
    logDebug "Creating Vulkan instance" >>
    createInstance appName

  ( physicalDevice, vkInstance ) <-
    logDebug "Creating physical device" >>
    choosePhysicalDevice vkInstance

  ( Ur queueFamilyIndex, physicalDevice ) <-
    logDebug "Finding suitable queue family" >>
    findQueueFamilyIndex physicalDevice [queueType]

  (vkInstance, device, physicalDevice, aSwapchainInfo, window) <- case renderingContext @ctx of
    SHeadless      -> Linear.do
      ( device, physicalDevice ) <-
        logDebug "Creating logical device" >>
        createDevice physicalDevice queueFamilyIndex []
      pure (vkInstance, device, physicalDevice, NoSwapchain, NoWindow)
    SWithSwapchain -> Linear.do
      ( device, physicalDevice ) <-
        logDebug "Creating logical device" >>
        createDevice physicalDevice queueFamilyIndex [ Vulkan.KHR_SWAPCHAIN_EXTENSION_NAME ]

      let SurfaceInfo
            { surfaceWindow
            , preferredFormat = Ur preferredFormat
            , surfaceUsage = Ur surfaceUsage
            } = surfaceInfo
      (surface, vkInstance, surfaceWindow) <-
        logDebug "Creating surface" >>
        createSurface vkInstance surfaceWindow

      (physicalDevice, surface) <-
        assertSurfacePresentable queueFamilyIndex physicalDevice surface

      (Ur surfaceFormat, physicalDevice, surface) <-
        logDebug "Choosing swapchain format & color space" >>
        chooseSwapchainFormat preferredFormat physicalDevice surface

      (SomeWith swapchainInfo, physicalDevice, device) <-
        logDebug "Creating swapchain" >>
        createSwapchain
          physicalDevice device
          surface surfaceFormat
          ( P.foldr (.|.) ( Vulkan.zero :: Vulkan.ImageUsageFlags ) surfaceUsage )
      pure (vkInstance, device, physicalDevice, ASwapchainInfo swapchainInfo, ContextWindow surfaceWindow)

  ( queue, device ) <- getDeviceQueue device ( fromIntegral queueFamilyIndex ) 0

  pure VulkanContext {..}
--------------------------------------------------------------------------------
assertSurfacePresentable
  :: Linear.MonadIO m
  => Int
  -> Vulkan.PhysicalDevice %1
  -> Vulkan.SurfaceKHR %1
  -> m (Vulkan.PhysicalDevice, Vulkan.SurfaceKHR)
assertSurfacePresentable queueFamilyIndex =
  Unsafe.toLinear2 \physicalDevice surface -> Linear.liftSystemIO $ do
    isPresentable <-
      Vulkan.getPhysicalDeviceSurfaceSupportKHR
        physicalDevice
        ( fromIntegral queueFamilyIndex )
        ( surface )

    if isPresentable then
      P.return (physicalDevice, surface)
    else
      error "Surface is not presentable"
