{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedRecordDot   #-}
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

class HasVulkanContext m where
  withVulkanContext
    :: ( VulkanContext WithSwapchain %1 -> IO (r, VulkanContext WithSwapchain) ) %1
    -> m r

type VulkanContextM = StateT (VulkanContext WithSwapchain) IO
instance HasVulkanContext VulkanContextM where
  withVulkanContext = StateT

withVulkanContextM :: (HasVulkanContext m, MonadIO m) => VulkanContextM a %1 -> m a
withVulkanContextM m = withVulkanContext $ \ctx -> liftIO (runStateT m ctx)

-- todo: use linear optics.
withDevice :: HasVulkanContext m => (Vulkan.Device %1 -> IO (a, Vulkan.Device)) %1 -> m a
withDevice f = withVulkanContext $ \VulkanContext{..} -> f device >>= \case
  (a, device) -> pure (a, VulkanContext{..})

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
    :: { queueType   :: Ur Vulkan.QueueFlags
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
  :: ContextSwapchainInfo WithSwapchain %1
  -> ( forall n. KnownNat n => SwapchainInfo n %1 -> r ) %1
  -> r
withSwapchainInfo ( ASwapchainInfo swapchain ) f = f swapchain

initialiseContext
  :: forall ctx m. ( KnownRenderingContext ctx, HasLogger m )
  => ByteString -> RenderInfo ctx %1 -> m ( VulkanContext ctx )
initialiseContext appName ( RenderInfo { queueType = Ur queueType, surfaceInfo } ) = Linear.do

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
    SHeadless -> case surfaceInfo of
      () -> Linear.do
        ( device, physicalDevice ) <-
          logDebug "Creating logical device" >>
          createDevice physicalDevice queueFamilyIndex []
        pure (vkInstance, device, physicalDevice, NoSwapchain, NoWindow)
    SWithSwapchain -> Linear.do
      ( device, physicalDevice ) <-
        logDebug "Creating logical device" >>
        createDevice physicalDevice queueFamilyIndex [ Vulkan.KHR_SWAPCHAIN_EXTENSION_NAME ]

      let !SurfaceInfo
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

-- | Block until the device is idle. Use this before tearing down resources
-- that might still be in use by the GPU.
waitVkContextIdle :: Linear.MonadIO m => VulkanContext ctx %1 -> m (VulkanContext ctx)
waitVkContextIdle = Unsafe.toLinear \ctx@VulkanContext{device} -> Linear.do
  liftSystemIO $ Vulkan.deviceWaitIdle device
  return ctx

-- | Fully destroy a 'VulkanContext'
destroyVulkanContext :: Linear.MonadIO m => VulkanContext WithSwapchain %1 -> m ()
destroyVulkanContext = Unsafe.toLinear \VulkanContext{..} -> Linear.do
  (vkInstance, device) <-
    case aSwapchainInfo of
      ASwapchainInfo swp_info ->
        destroySwapchain vkInstance device swp_info
  case window of
    ContextWindow win ->
      destroyWindow win
  destroyDevice device
  destroyInstance vkInstance

--------------------------------------------------------------------------------
-- | Return the Extent3D of the vulkan context with the swapchain Extent2D and @depth = 1@.
vkContextExtent :: VulkanContext WithSwapchain %1 -> (Ur Vulkan.Extent3D, VulkanContext WithSwapchain)
vkContextExtent VulkanContext
  { aSwapchainInfo = ASwapchainInfo SwapchainInfo
      { swapchainExtent = Ur ext2D
      , .. }
  , .. } =
  let extent3D :: Vulkan.Extent3D
      extent3D
        = Vulkan.Extent3D
            { Vulkan.width  = ext2D.width
            , Vulkan.height = ext2D.height
            , Vulkan.depth  = 1
            }
  in (Ur extent3D, VulkanContext
      { aSwapchainInfo = ASwapchainInfo SwapchainInfo
          { swapchainExtent = Ur ext2D
          , .. }
      ,.. })
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
