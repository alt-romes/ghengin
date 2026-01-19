{-# OPTIONS_GHC -Wno-name-shadowing #-}
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

-- For re-exports
import Ghengin.Vulkan.Renderer.Texture
import Ghengin.Vulkan.Renderer.Sampler

import qualified Prelude
import Prelude.Linear hiding (zero, IO)
import qualified Unsafe.Linear as Unsafe

import Control.Functor.Linear as Linear
import qualified Data.Functor.Linear as Data.Linear
import Control.Monad.IO.Class.Linear

import Data.IORef
import Data.Bits
import Data.Word

import qualified Control.Monad

import GHC.Ptr

import Data.ByteString (ByteString)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.V.Linear.Internal as VI
import qualified Data.List as L

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
import Ghengin.Vulkan.Renderer.Command
import Ghengin.Vulkan.Renderer.Frame
import Ghengin.Vulkan.Renderer.GLFW.Window as GLFW
import Ghengin.Vulkan.Renderer.ImmediateSubmit
import Ghengin.Vulkan.Renderer.Kernel
import qualified System.IO.Linear as Linear

runRenderer :: (Int, Int)
            -- ^ Dimensions of the window to render on (width, height)
            -> Renderer a ⊸ Linear.IO a
runRenderer dimensions r = Linear.do

  -- Initialisation
  -----------------
  glfwtoken <- initGLFW

  let appName = "Ghengin" -- todo: receive as input

  window    <- createWindow WindowInfo
    { width      = fst dimensions
    , height     = snd dimensions
    , windowName = appName
    }

  vkContext <- initialiseContext @WithSwapchain appName RenderInfo
    { queueType = Vk.QUEUE_GRAPHICS_BIT
    , surfaceInfo = SurfaceInfo
      { surfaceWindow   = window
      , preferredFormat = Ur $
          Vk.SurfaceFormatKHR
            Vk.FORMAT_B8G8R8A8_SRGB
            Vk.COLOR_SPACE_SRGB_NONLINEAR_KHR
      , surfaceUsage = Ur $
          [ -- Needed for screenshots?
            -- Vk.IMAGE_USAGE_TRANSFER_SRC_BIT
          , Vk.IMAGE_USAGE_COLOR_ATTACHMENT_BIT
          ]
      }
    }

  (Ur extent3D, vkContext) <- pure $
    vkContextExtent3D vkContext

  -- todo: check which depth attachment supported format is best,
  -- see https://www.howtovulkan.com/#depth-attachment
  let depthFmt = Vk.FORMAT_D32_SFLOAT
  -- > We only need a single image, even if we do double buffering in other
  -- places. That's because the image is only ever accessed by the GPU and the
  -- GPU can only ever write to a single depth image at a time.
  (depthImage, vkContext) <-
    createImage vkContext
      (Default2DImageInfo extent3D depthFmt
        Vk.IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT)
      (WithViewInfo Vk.IMAGE_VIEW_TYPE_2D Vk.IMAGE_ASPECT_DEPTH_BIT)
      Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT -- on GPU only

  (imsCtx, device) <- createImmediateSubmitCtx device

  -- (For now) we allocate just one command pool and one command buffer
  (commandPool, device) <- createCommandPool device
  (cmdBuffers, device, commandPool) <- createCommandBuffers @MAX_FRAMES_IN_FLIGHT_T device commandPool

  (frames, device) <- runStateT (Data.Linear.mapM (StateT . initVulkanFrameData) cmdBuffers) device

  -- (Ur logger,cleanupLogger)  <- newLogger (LogFileNoRotate "log.ghengin.log" defaultBufSize)
  (Ur logger,cleanupLogger)  <- newLogger (LogStdout defaultBufSize)

  -- Run renderer
  ---------------
  (a, REnv inst device win swapchain commandPool' frames' imsCtx)
    <- runRenderer' logger (REnv inst device win swapchain commandPool frames imsCtx) r

  -- Terminate
  ------------
  liftSystemIO $ logger._log "[Start] Vulkan clean up\n"

  (vunit, device) <- runStateT (Data.Linear.mapM (\f -> StateT (fmap ((),) . destroyVulkanFrameData f)) frames') device
  pure $ consumeUnits vunit

  device <- destroyCommandPool device commandPool'
  device <- destroyImmediateSubmitCtx device imsCtx

  vkContext <- destroyImage vkContext depthImage
  destroyVulkanContext vkContext
  terminateGLFW glfwtoken

  liftSystemIO $ logger._log "[Done] Vulkan clean up\n"

  cleanupLogger

  pure a

-- | Run a 'Renderer' action that depends on a command buffer and the current
-- image index of the swapchain to typically by writing to the command buffer
-- the draw calls (to the renderpasse's framebuffer responsible for that
-- image).
--
-- Afterwards, submits that command buffer to the graphics queue and then
-- presents the current image.
-- 
-- It handles all the synchronization necessary so that the command buffer
-- being written to is not written again before it is submitted, and so that we
-- wait for the GPU after having written N frames, where N = 2 means we're
-- doing double buffering, and N = 3 triple buffering.
--
-- That is, this function will block the third time it's called if N = 2 but no
-- image has been presented yet
--
-- N is 'MAX_FRAMES_IN_FLIGHT'
--
-- TODO: Figure out mismatch between current image index and current image frame.
withCurrentFramePresent :: ( Vk.CommandBuffer
                              ⊸ Int -- ^ Current image index
                             -> Renderer (a, Vk.CommandBuffer)
                           )
                         ⊸ Renderer a
withCurrentFramePresent action = Linear.do

  Ur frameCountRef <- Renderer $ asks (\(Ur env) -> Ur (env.frameCounter))
  Ur frameCount <- liftSystemIOU (readIORef frameCountRef)
  liftSystemIO $ modifyIORef' frameCountRef (Prelude.+ 1)

  -- This could in principle overflow... For now, good enough. It's
  -- unlikely the frame count overflows with only 60 frames per second.
  -- The game would have to run for years to overflow a 64 bit integer
  let currentFrameIndex = frameCount `mod` (nat @MAX_FRAMES_IN_FLIGHT_T)


  Ur unsafeCurrentFrame <- renderer $ Unsafe.toLinear $ \renv -> pure (Ur (case renv._frames of (VI.V vec) -> vec V.! currentFrameIndex),renv)
  -- These are all unsafe too
  let
      cmdBuffer         = unsafeCurrentFrame._commandBuffer
      inFlightFence     = unsafeCurrentFrame._renderFence
      imageAvailableSem = unsafeCurrentFrame._renderSemaphore
      renderFinishedSem = unsafeCurrentFrame._presentSemaphore

  -- Wait for the previous frame to finish
  -- Acquire an image from the swap chain
  -- Record a command buffer which draws the scene onto that image
  -- Submit the recorded command buffer
  -- Present the swap chain image 
  unsafeUseDevice (\device -> do
    _ <- Vk.waitForFences device [inFlightFence] True maxBound
    Vk.resetFences device [inFlightFence]
                  )

  (Ur i, imageAvailableSem') <- acquireNextImage imageAvailableSem

  liftSystemIO $ Vk.resetCommandBuffer cmdBuffer zero

  (a, cmdBuffer') <- action cmdBuffer i

  -- Finally, submit and present
  (cmdBuffer'',imageAvailableSem'', renderFinishedSem', inFlightFence')
    <- submitGraphicsQueue cmdBuffer' imageAvailableSem' renderFinishedSem inFlightFence

  renderFinishedSem'' <- presentPresentQueue renderFinishedSem' i

  -- Forget these as they're in the renderer environment still, remember we got them unsafely in the first place...
  Unsafe.toLinearN @4 (\_ _ _ _ -> pure ()) cmdBuffer'' imageAvailableSem'' renderFinishedSem'' inFlightFence'

  pure a

acquireNextImage :: Vk.Semaphore ⊸ Renderer (Ur Int, Vk.Semaphore)
acquireNextImage = Unsafe.toLinear $ \sem -> Linear.do
  Ur renv <- renderer (Unsafe.toLinear $ \renv -> pure (Ur renv, renv))
  i <- liftSystemIOU $ Prelude.fromIntegral Prelude.. Prelude.snd Prelude.<$> Vk.acquireNextImageKHR renv._vulkanDevice._device renv._vulkanSwapChain._swapchain maxBound sem Vk.NULL_HANDLE
  pure (i, sem)

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
  Ur gqueue <- renderer (Unsafe.toLinear $ \renv -> pure (Ur renv._vulkanDevice._graphicsQueue, renv))
  liftSystemIO $ Vk.queueSubmit gqueue [Vk.SomeStruct submitInfo] fence
  pure (cb, sem1, sem2, fence)


presentPresentQueue :: Vk.Semaphore ⊸ Int -> Renderer Vk.Semaphore
presentPresentQueue = Unsafe.toLinear \sem imageIndex -> Linear.do
  (Ur swpc, Ur presentQueue) <- renderer (Unsafe.toLinear $ \renv -> pure ((Ur renv._vulkanSwapChain._swapchain, Ur renv._vulkanDevice._presentQueue), renv))
  let presentInfo = Vk.PresentInfoKHR { next = ()
                                      , waitSemaphores = [sem]
                                      , swapchains = [swpc]
                                      , imageIndices = [fromIntegral imageIndex]
                                      , results = nullPtr
                                      }
  Ur _ <- liftSystemIOU $ Vk.queuePresentKHR presentQueue presentInfo
  pure sem

shouldCloseWindow :: Renderer (Ur Bool)
shouldCloseWindow = renderer $ Unsafe.toLinear $ \renv@(REnv{..}) -> Linear.do
  b <- liftSystemIOU (GLFW.windowShouldClose _vulkanWindow._window)
  pure $ (b, renv)

pollWindowEvents :: Renderer ()
pollWindowEvents = liftSystemIO $ GLFW.pollEvents

withWindow :: (GLFW.Window ⊸ Linear.IO GLFW.Window) -> Renderer ()
withWindow f = renderer $ Unsafe.toLinear $ \renv@(REnv{..}) -> Linear.do
  w' <- f (_vulkanWindow._window)
  pure ((), renv{_vulkanWindow = renv._vulkanWindow{_window = w'}})

getMousePos :: Renderer (Ur (Double, Double))
getMousePos = renderer $ Unsafe.toLinear $ \renv@(REnv{..}) -> Linear.do
  p <- liftSystemIOU (GLFW.getCursorPos _vulkanWindow._window)
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

