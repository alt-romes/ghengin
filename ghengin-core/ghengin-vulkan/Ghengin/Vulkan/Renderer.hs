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
    VulkanContext{..} -> Linear.do
      let mkRenderSemaphores
            :: forall swpImgs. KnownNat swpImgs
            => SwapchainInfo swpImgs %1
            -> Linear.IO (SomeV Vk.Semaphore, VulkanContext WithSwapchain)
          mkRenderSemaphores swpInfo = Linear.do
            let ctx = VulkanContext{aSwapchainInfo=ASwapchainInfo swpInfo, ..}
            (semsv, ctx) <- withResource ctx $
              genSizedWithCtx @swpImgs createSemaphore
            return (SomeV semsv, ctx)
      withSwapchainInfo aSwapchainInfo mkRenderSemaphores

  (commandPool, vkContext) <- createCommandPool vkContext
  (commandBuffers, vkContext, commandPool) <- createCommandBuffers @FramesInFlight vkContext commandPool

  -- (imsCtx, vkContext) <- createImmediateSubmitCtx vkContext

  -- Run renderer
  ---------------
  (a, RendererEnv{..}) <- runRenderer' logger RendererEnv{..} r

  -- Terminate
  ------------

  -- device <- destroyImmediateSubmitCtx device imsCtx

  (vkContext, commandPool) <- destroyCommandBuffers vkContext commandPool commandBuffers
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
      :: forall (s::Nat) a
       . KnownNat s
      => (VulkanContext WithSwapchain %1 -> IO (a, VulkanContext WithSwapchain))
      -> StateT (VulkanContext WithSwapchain) IO (V s a)
    genSizedWithCtx k = genSizedM @s $ const $ StateT k

    destroyVs
      :: forall (n :: Nat) s res
       . KnownNat n
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
  (Ur (SomeWith imageIndex)) <- renderer $ \RendererEnv{..} -> Linear.do
    let !(frameFence, reconFences) = focusV frameIndex fences
    -- After waiting for this fence, it's safe to update resources for this
    -- frame index (e.g. the descriptor sets, uniform data, and other things
    -- defined by the given user action)
    (frameFence, vkContext) <- waitForFence vkContext frameFence
    (frameFence, vkContext) <- resetFence   vkContext frameFence

    let !(framePresentSem, reconPresentSems) = focusV frameIndex presentSemaphores
    (imageIndex, framePresentSem, vkContext) <- case vkContext of
      VulkanContext{..} -> Linear.do
        let acquireIt
              :: forall swpImgs. KnownNat swpImgs
              => SwapchainInfo swpImgs %1
              -> Linear.IO (Ur (Finite `With` KnownNat), Vk.Semaphore, VulkanContext WithSwapchain)
            acquireIt swpInfo = Linear.do
              (Ur (fin :: Finite swpImgs), framePresentSem, swpInfo, device) <-
                acquireNextImage device swpInfo framePresentSem
              let ctx = VulkanContext{aSwapchainInfo=ASwapchainInfo swpInfo, ..}
              return (Ur (SomeWith fin), framePresentSem, ctx)
        withSwapchainInfo aSwapchainInfo acquireIt

    pure (imageIndex, RendererEnv
      { fences = reconFences frameFence
      , presentSemaphores = reconPresentSems framePresentSem
      , ..
      })

  -- The user action will call record command buffers (renderWith ...) and update resources.
  a <- action frameIndex imageIndex

  -- We submit this frame's command buffer and present the image afters

  return a

-- withCurrentFramePresent :: ( Vk.CommandBuffer
--                               ⊸ Int -- ^ Current image index
--                              -> Renderer (a, Vk.CommandBuffer)
--                            )
--                          ⊸ Renderer a
-- withCurrentFramePresent action = Linear.do
--
--   Ur frameIndexRef <- Renderer $ asks (\(Ur env) -> Ur env.frameIndexRef)
--   Ur frameIndex    <- liftSystemIOU (Data.IORef.readIORef frameIndexRef)
--   liftSystemIO $ modifyIORef' frameIndexRef (Prelude.+ 1)
--
--   Ur unsafeCurrentFrame <- renderer $ Unsafe.toLinear $ \renv -> pure (Ur (case renv._frames of (VI.V vec) -> vec V.! currentFrameIndex),renv)
--   -- These are all unsafe too
--   let
--       cmdBuffer         = unsafeCurrentFrame._commandBuffer
--       inFlightFence     = unsafeCurrentFrame._renderFence
--       imageAvailableSem = unsafeCurrentFrame._renderSemaphore
--       renderFinishedSem = unsafeCurrentFrame._presentSemaphore
--
--   -- Wait for the previous frame to finish
--   -- Acquire an image from the swap chain
--   -- Record a command buffer which draws the scene onto that image
--   -- Submit the recorded command buffer
--   -- Present the swap chain image 
--   unsafeUseDevice (\device -> do
--     _ <- Vk.waitForFences device [inFlightFence] True maxBound
--     Vk.resetFences device [inFlightFence]
--                   )
--
--   (Ur i, imageAvailableSem') <- acquireNextSwapchainImage imageAvailableSem
--
--   liftSystemIO $ Vk.resetCommandBuffer cmdBuffer zero
--
--   (a, cmdBuffer') <- action cmdBuffer i
--
--   -- Finally, submit and present
--   (cmdBuffer'',imageAvailableSem'', renderFinishedSem', inFlightFence')
--     <- submitGraphicsQueue cmdBuffer' imageAvailableSem' renderFinishedSem inFlightFence
--
--   renderFinishedSem'' <- presentPresentQueue renderFinishedSem' i
--
--   -- Forget these as they're in the renderer environment still, remember we got them unsafely in the first place...
--   Unsafe.toLinearN @4 (\_ _ _ _ -> pure ()) cmdBuffer'' imageAvailableSem'' renderFinishedSem'' inFlightFence'
--
--   pure a

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


presentPresentQueue :: Vk.Semaphore ⊸ Int -> Renderer Vk.Semaphore
presentPresentQueue = Unsafe.toLinear \sem imageIndex -> Linear.do
  (Ur swpc, Ur presentQueue) <- renderer (Unsafe.toLinear $ \renv -> undefined) -- pure ((Ur renv._vulkanSwapChain._swapchain, Ur renv._vulkanDevice._presentQueue), renv))
  let presentInfo = Vk.PresentInfoKHR { next = ()
                                      , waitSemaphores = [sem]
                                      , swapchains = [swpc]
                                      , imageIndices = [fromIntegral imageIndex]
                                      , results = nullPtr
                                      }
  Ur _ <- liftSystemIOU $ Vk.queuePresentKHR presentQueue presentInfo
  pure sem

shouldCloseWindow :: Renderer (Ur Bool)
shouldCloseWindow = renderer $ Unsafe.toLinear $ \renv@(RendererEnv{..}) -> Linear.do
  b <- liftSystemIOU (GLFW.windowShouldClose undefined) --_vulkanWindow._window)
  pure $ (b, renv)

pollWindowEvents :: Renderer ()
pollWindowEvents = liftSystemIO $ GLFW.pollEvents

withWindow :: (GLFW.Window ⊸ Linear.IO GLFW.Window) -> Renderer ()
withWindow f = renderer $ Unsafe.toLinear $ \renv@(RendererEnv{..}) -> Linear.do
  undefined
  -- w' <- f (_vulkanWindow._window)
  -- pure ((), renv{_vulkanWindow = renv._vulkanWindow{_window = w'}})

getMousePos :: Renderer (Ur (Double, Double))
getMousePos = renderer $ Unsafe.toLinear $ \renv@(RendererEnv{..}) -> Linear.do
  p <- liftSystemIOU (GLFW.getCursorPos undefined)--_vulkanWindow._window)
  pure (p, renv)

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

