{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
module Ghengin.Vulkan.Renderer
  ( module Ghengin.Vulkan.Renderer.DescriptorSet
  , module Ghengin.Vulkan.Renderer.Buffer
  , module Ghengin.Vulkan.Renderer.Command
  , module Ghengin.Vulkan.Renderer.Kernel
  , module Ghengin.Vulkan.Renderer.Texture
  , module Ghengin.Vulkan.Renderer.Sampler
  , module Ghengin.Vulkan.Renderer
  )
  where


import Ghengin.Vulkan.Renderer.Texture
import Ghengin.Vulkan.Renderer.Sampler

import qualified Prelude
import qualified Unsafe.Linear as Unsafe

import Ghengin.Core.Prelude as Linear
import qualified Data.Functor.Linear as Data

import Type.Reflection
import Data.Finite
import Data.IORef
import Data.Bits
import Data.Word

import qualified Control.Monad

import GHC.Ptr

import Data.ByteString (ByteString)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.V.Linear.Internal as VL
import qualified Data.List as L
import qualified Data.Foldable as L

import qualified Vulkan.Extensions
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan as Vk
import Vulkan.Zero (zero)

import Ghengin.Core.Log
import Ghengin.Core.Type.Utils

import qualified Graphics.UI.GLFW as GLFW

-- reexport
import Ghengin.Vulkan.Renderer.DescriptorSet
import Ghengin.Vulkan.Renderer.Buffer

import Ghengin.Vulkan.Renderer.Context
import Ghengin.Vulkan.Renderer.Context.Instance
import Ghengin.Vulkan.Renderer.Context.Device
import Ghengin.Vulkan.Renderer.Context.Swapchain
import Ghengin.Vulkan.Renderer.Image
import Ghengin.Vulkan.Renderer.Synchronization
import Ghengin.Vulkan.Renderer.Command
import Ghengin.Vulkan.Renderer.GLFW.Window as GLFW
import Ghengin.Vulkan.Renderer.ImmediateSubmit
import Ghengin.Vulkan.Renderer.Kernel
import qualified System.IO.Linear as Linear

runRenderer :: (Int, Int)
            -- ^ Dimensions of the window to render on (width, height)
            -> Renderer a ⊸ Linear.IO a
runRenderer dimensions r = Linear.do

  (Ur logger, cleanupLogger) <-
    newLogger (LogStdout defaultBufSize) -- or (LogFileNoRotate "log.ghengin.log" defaultBufSize)

  -- Initialisation
  -----------------
  glfwtoken <- initGLFW

  let appName = "Ghengin" -- todo: receive as input

  window    <- createWindow WindowInfo
    { width      = fst dimensions
    , height     = snd dimensions
    , windowName = appName
    }

  vkContext <- runWithLogger logger $
    initialiseContext @WithSwapchain (fromString appName) RenderInfo
      { queueType = Ur Vk.QUEUE_GRAPHICS_BIT
      , surfaceInfo = SurfaceInfo
        { surfaceWindow   = window
        , preferredFormat = Ur $
            Vk.SurfaceFormatKHR
              Vk.FORMAT_B8G8R8A8_SRGB
              Vk.COLOR_SPACE_SRGB_NONLINEAR_KHR
        , surfaceUsage = Ur $
            [ -- Needed for screenshots?
              -- Vk.IMAGE_USAGE_TRANSFER_SRC_BIT
              Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT
            ]
        }
      }

  (Ur extent3D, vkContext) <- pure $
    vkContextExtent vkContext

  -- todo: check which depth attachment supported format is best,
  -- see https://www.howtovulkan.com/#depth-attachment
  let depthFmt = Vk.FORMAT_D32_SFLOAT
  (depthImage, vkContext) <-
    createImage vkContext
      (Default2DImageInfo extent3D depthFmt
        Vk.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT)
      (WithViewInfo Vk.IMAGE_VIEW_TYPE_2D Vk.IMAGE_ASPECT_DEPTH_BIT)
      Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT -- on GPU only

  ((fences, presentSemaphores), vkContext) <- withResource vkContext $ Linear.do
    fences            <- genSizedWithCtx @FramesInFlight (`createFence` True)
    presentSemaphores <- genSizedWithCtx @FramesInFlight createSemaphore
    pure (fences, presentSemaphores)

  (SomeV @_swpImgs renderSemaphores, vkContext) <- case vkContext of
    VulkanContext{..} -> withSwapchainInfo aSwapchainInfo mkRenderSemaphores
      where
        mkRenderSemaphores
          :: ∀ swpImgs. KnownNat swpImgs
          => SwapchainInfo swpImgs %1
          -> Linear.IO (SomeV Vk.Semaphore, VulkanContext WithSwapchain)
        mkRenderSemaphores swpInfo = Linear.do
          let ctx = VulkanContext{aSwapchainInfo=ASwapchainInfo swpInfo, ..}
          (semsv, ctx) <- withResource ctx $
            genSizedWithCtx @swpImgs createSemaphore
          return (SomeV semsv, ctx)

  (commandPool, vkContext) <- createCommandPool vkContext
  (commandBuffers, vkContext, commandPool) <- createCommandBuffers @FramesInFlight vkContext commandPool

  -- (imsCtx, vkContext) <- createImmediateSubmitCtx vkContext

  -- Run renderer
  ---------------
  (a, RendererEnv{..}) <- runRenderer' logger
    RendererEnv{commandBuffers = VL.map (Just . Some) commandBuffers, ..} r

  -- Terminate
  ------------

  -- device <- destroyImmediateSubmitCtx device imsCtx

  (vkContext, commandPool) <- destroyCommandBuffers vkContext commandPool (VL.map (expectJust "All command buffers should be back in the vector when exiting") commandBuffers)
  vkContext <- destroyCommandPool vkContext commandPool

  ((), vkContext) <- withResource vkContext $ Linear.do
    destroyVs fences destroyFence
    destroyVs presentSemaphores destroySemaphore
    destroyVs renderSemaphores destroySemaphore

  vkContext <- destroyImage vkContext depthImage
  destroyVulkanContext vkContext
  terminateGLFW glfwtoken

  cleanupLogger

  pure a
  where
    genSizedWithCtx
      :: ∀ s a. KnownNat s
      => (VulkanContext WithSwapchain %1 -> IO (a, VulkanContext WithSwapchain))
      -> StateT (VulkanContext WithSwapchain) IO (V s a)
    genSizedWithCtx k = genSizedM @s $ const $ StateT k

    destroyVs
      :: KnownNat n
      => V n s %1
      -> (res %1 -> s %1 -> IO res)
      -> StateT res IO ()
    destroyVs v k = consumeV <$> Data.forM v (\x -> StateT $ \c' -> ((),) <$> k c' x)


-- | Render loop: new frame
newFrame
  :: ( forall swpcImgs
      . KnownNat swpcImgs
      => Finite FramesInFlight
      -> Finite swpcImgs
      -> Renderer a ) %1
  -> Renderer a
newFrame action = Linear.do
  Ur frameIndex <- nextFrameInFlight
  (Ur (SomeWith (imageIndex :: Finite swpcImgs))) <- renderer $ \RendererEnv{..} -> Linear.do
    let !(frameFence, reconFences) = focusV frameIndex fences
    -- After waiting for this fence, it's safe to update resources for this
    -- frame index (e.g. the descriptor sets, uniform data, and other things
    -- defined by the given user action)
    (frameFence, vkContext) <- waitForFence vkContext frameFence
    (frameFence, vkContext) <- resetFence   vkContext frameFence

    let !(framePresentSem, reconPresentSems) = focusV frameIndex presentSemaphores
    (imageIndex, framePresentSem, vkContext) <- case vkContext of
      VulkanContext{..} -> withSwapchainInfo aSwapchainInfo acquireIt
        where
          acquireIt
            :: KnownNat swpImgs
            => SwapchainInfo swpImgs %1
            -> Linear.IO (Ur (Finite `With` KnownNat), Vk.Semaphore, VulkanContext WithSwapchain)
          acquireIt swpInfo = Linear.do
            (Ur (fin :: Finite swpImgs), framePresentSem, swpInfo, device) <-
              acquireNextImage device swpInfo framePresentSem
            let ctx = VulkanContext{aSwapchainInfo=ASwapchainInfo swpInfo, ..}
            return (Ur (SomeWith fin), framePresentSem, ctx)

    pure (imageIndex, RendererEnv
      { fences = reconFences frameFence
      , presentSemaphores = reconPresentSems framePresentSem
      , .. })

  -- The user action will call record command buffers (renderWith ...), submit them, and update resources.
  a <- action frameIndex imageIndex

  -- We present the image afterwards.
  renderer $ \RendererEnv{renderSemaphores=(renderSemaphores::V swpImgs' Vk.Semaphore), ..} -> case vkContext of
    VulkanContext{..} -> withSwapchainInfo aSwapchainInfo useInfo
      where
        useInfo
          :: ∀ swpImgs. KnownNat swpImgs
          => SwapchainInfo swpImgs %1
          -> Linear.IO ((), RendererEnv FramesInFlight)
        useInfo swpInfo =
          case (sameNat (Proxy @swpImgs') (Proxy @swpImgs), sameNat (Proxy @swpcImgs) (Proxy @swpImgs)) of
            -- Witness that the number of swapchain images is the same as the
            -- number of render semaphores is the same as the Finite index for
            -- imageIndex (all were created from same SwapchainInfo)
            (Just Refl, Just Refl) -> Linear.do
              (renderSemaphores, swpInfo, queue) <-
                presentPresentQueue queue swpInfo (renderSemaphores :: V swpImgs Vk.Semaphore) imageIndex
              let vkContext = VulkanContext{aSwapchainInfo=ASwapchainInfo swpInfo, ..}
              return ((), RendererEnv { vkContext, .. })

            _ -> error "impossible, but I don't know how to prove it"
              RendererEnv{vkContext = VulkanContext{aSwapchainInfo = ASwapchainInfo swpInfo, ..}, .. }

  -- TODO: Reconstruct swapchain here if it changed.

  return a

acquireNextImage
  :: ( MonadIO m, KnownNat n )
  => Vk.Device %1
  -> SwapchainInfo n %1
  -> Vk.Semaphore %1
  -> m (Ur (Finite n), Vk.Semaphore, SwapchainInfo n, Vk.Device)
acquireNextImage = Unsafe.toLinear3
  \device (info@SwapchainInfo { swapchain }) signal -> Linear.liftSystemIO $ do
    -- TODO: reconstruct swapchain on VK_ERROR_OUT_OF_DATE_KHR
    (_TODO_RECON_SWPC, fin) <- Vk.acquireNextImageKHR device swapchain maxBound signal Vk.NULL_HANDLE
    Prelude.return (Ur (fromIntegral fin), signal, info, device)

submitGraphicsQueue :: Vk.CommandBuffer ⊸ Vk.Semaphore ⊸ Vk.Semaphore ⊸ Vk.Fence ⊸ Renderer (Vk.CommandBuffer, Vk.Semaphore, Vk.Semaphore, Vk.Fence)
submitGraphicsQueue = Unsafe.toLinearN @4 \cb sem1 sem2 fence -> Linear.do
  let
    submitInfo = Vk.SubmitInfo { next = ()
                                -- We want to wait with writing colors to the image until it's available
                               , waitSemaphores = [sem1]
                               , waitDstStageMask = [Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
                                -- Semaphores to signal when we are done
                               , signalSemaphores = [sem2]
                               , commandBuffers = [ cb.commandBufferHandle ]
                               }
               -- this pattern should be used as unsafeAsk (make utils...)
  Ur gqueue <- renderer (Unsafe.toLinear $ \renv -> undefined) -- pure (Ur renv._vulkanDevice._graphicsQueue, renv))
  liftSystemIO $ Vk.queueSubmit gqueue [Vk.SomeStruct submitInfo] fence
  pure (cb, sem1, sem2, fence)

presentPresentQueue
  :: ( MonadIO m, KnownNat swpImgs )
  => Vk.Queue               %1
  -> SwapchainInfo swpImgs  %1
  -> V swpImgs Vk.Semaphore %1
  -> Finite swpImgs
  -> m (V swpImgs Vk.Semaphore, SwapchainInfo swpImgs, Vk.Queue)
presentPresentQueue = Unsafe.toLinear3 \presentQueue swpc renderSemaphores imageIndex -> Linear.do
  let !(renderSemaphore, _) = focusV imageIndex renderSemaphores
  let presentInfo = Vk.PresentInfoKHR
        { next = ()
        , waitSemaphores = [renderSemaphore]
        , swapchains = [swpc.swapchain]
        , imageIndices = [fromIntegral (getFinite imageIndex)]
        , results = nullPtr
        }
  Ur _TODO_REBUILD_SWPCHAIN_IF_OUTDATED <- liftSystemIOU $
    Vk.queuePresentKHR presentQueue presentInfo
  pure (renderSemaphores{-they weren't modified-}, swpc, presentQueue)

shouldCloseWindow :: Renderer (Ur Bool)
shouldCloseWindow = renderer $ Unsafe.toLinear $ \renv -> Linear.do
  let !(ContextWindow window) = renv.vkContext.window
  b <- liftSystemIOU (GLFW.windowShouldClose window)
  pure $ (b, renv)

pollWindowEvents :: Renderer ()
pollWindowEvents = liftSystemIO $ GLFW.pollEvents


getMousePos :: Renderer (Ur (Double, Double))
getMousePos = renderer $ Unsafe.toLinear $ \renv -> Linear.do
  let !(ContextWindow window) = renv.vkContext.window
  p <- liftSystemIOU (GLFW.getCursorPos window)
  pure (p, renv)

withWindow :: (GLFW.Window ⊸ Linear.IO GLFW.Window) -> Renderer ()
withWindow f = withWindow' (\w -> f w >>= \w -> pure ((), w))

withWindow' :: (GLFW.Window ⊸ Linear.IO (a, GLFW.Window)) -> Renderer a
withWindow' f = renderer $ Unsafe.toLinear $ \renv -> Linear.do
  let !(ContextWindow window) = renv.vkContext.window
  (x, window) <- f window
  pure (x, renv{vkContext = renv.vkContext{window = ContextWindow window}})

--------------------------------------------------------------------------------
-- * Utils
--------------------------------------------------------------------------------

(.&&.) :: Bits a => a -> a -> Bool
x .&&. y = (Prelude./= zeroBits) (x .&. y)

-- | Returns the first element in a foldable structure for that the
-- monadic predicate holds true, and @Nothing@ if no such element
-- exists.
findM :: ∀ m t a. (Prelude.Monad m, Prelude.Foldable t)
      => (a -> m Bool) -> t a -> m (Maybe a)
findM p = Prelude.foldr go (Prelude.pure Nothing)
  where
    go :: a -> m (Maybe a) -> m (Maybe a)
    go x acc = do
      b <- p x
      if b then Prelude.pure (Just x) else acc

