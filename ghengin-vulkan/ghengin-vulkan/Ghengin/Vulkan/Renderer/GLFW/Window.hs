{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BlockArguments #-}
-- | TODO: One day, abstract over window API too
module Ghengin.Vulkan.Renderer.GLFW.Window
  (VulkanWindow(..), createVulkanWindow, destroyVulkanWindow, loopUntilClosedOr
  , initGLFW, terminateGLFW
  , GLFWToken
  ) where

import GHC.Int (Int32)
import qualified Prelude
import Prelude.Linear
import qualified Prelude.Linear as Linear (IO(..))
import Control.Functor.Linear as Linear
import Control.Monad.IO.Class.Linear as Linear
import qualified Unsafe.Linear as Unsafe

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable

import qualified Control.Exception
import qualified Control.Monad as Monad

import qualified Graphics.UI.GLFW as GLFW
import qualified Vulkan.Exception as Vk (VulkanException(..))
import qualified Vulkan as Vk

data VulkanWindow = VulkanWindow { _window  :: !GLFW.Window
                                 , _surface :: !Vk.SurfaceKHR
                                 }

createVulkanWindow :: Linear.MonadIO m
                   => Vk.Instance
                    ⊸ (Int, Int) -- ^ (width, height)
                   -> String     -- ^ window name
                   -> m (VulkanWindow, Vk.Instance)
createVulkanWindow inst dimensions label = Linear.do
  win <- createWindow dimensions label
  (surface, inst', win') <- createSurface inst win
  pure (VulkanWindow win' surface, inst')

-- TODO: Can I destroy the window before the instance?
destroyVulkanWindow :: Linear.MonadIO m => Vk.Instance ⊸ VulkanWindow ⊸ m Vk.Instance
destroyVulkanWindow inst (VulkanWindow win surface) = Linear.do
  inst' <- destroySurface inst surface
  destroyWindow win
  pure inst'

-- | Creates a window and its GLFW context. Probably doesn't work if we need
-- multiple windows
createWindow :: Linear.MonadIO m => (Int, Int) -> String -> m GLFW.Window
createWindow (w,h) label = liftSystemIO $ do
  Just win <- GLFW.createWindow w h label Nothing Nothing
  Prelude.pure win
{-# INLINE createWindow #-}

-- | Destroys the window and the GLFW context. To have multiple windows this
-- wouldn't work
destroyWindow :: Linear.MonadIO m => GLFW.Window ⊸ m ()
destroyWindow = Unsafe.toLinear \win -> liftSystemIO $ GLFW.destroyWindow win
{-# INLINE destroyWindow #-}

-- | Create a surface (it must be destroyed by Vulkan).
createSurface :: Linear.MonadIO m => Vk.Instance ⊸ GLFW.Window ⊸ m (Vk.SurfaceKHR, Vk.Instance, GLFW.Window)
createSurface = Unsafe.toLinear2 \i w -> liftSystemIO $ do
  alloca $ \surfacePtr -> do
    r <- Vk.Result Prelude.<$> GLFW.createWindowSurface @Int32 (Vk.instanceHandle i) w nullPtr surfacePtr
    Monad.when (r Prelude.< Vk.SUCCESS) (Control.Exception.throwIO (Vk.VulkanException r))
    surface <- peek surfacePtr
    Prelude.pure (surface, i, w)
{-# INLINE createSurface #-}

destroySurface :: Linear.MonadIO m => Vk.Instance ⊸ Vk.SurfaceKHR ⊸ m Vk.Instance
destroySurface = Unsafe.toLinear2 \i s -> liftSystemIO $ i Prelude.<$ Vk.destroySurfaceKHR i s Nothing
{-# INLINE destroySurface #-}

-- | Run an IO action many many times until the window is closed by a normal
-- window-closing event.
loopUntilClosedOr :: ∀ m s. Linear.MonadIO m => GLFW.Window ⊸ Ur s ⊸ (Ur s ⊸ m (Bool, Ur s)) -> m (GLFW.Window, Ur s)
loopUntilClosedOr = loopUntilClosedOr' False
  where
  loopUntilClosedOr' :: Linear.MonadIO m => Bool ⊸ GLFW.Window ⊸ Ur s ⊸ (Ur s ⊸ m (Bool, Ur s)) -> m (GLFW.Window, Ur s)
  loopUntilClosedOr' shouldClose win (Ur s) action =
    if shouldClose then pure (win,Ur s)
    else Linear.do
      windowShouldClose win >>= \case
        (True , win') -> pure (win', Ur s)
        (False, win') -> Linear.do
          liftSystemIO GLFW.pollEvents
          (shouldClose',Ur s') <- action (Ur s)
          loopUntilClosedOr' shouldClose' win' (Ur s') action
      where
        windowShouldClose :: GLFW.Window ⊸ m (Bool, GLFW.Window)
        windowShouldClose = Unsafe.toLinear \w -> (,w) <$> liftSystemIO (GLFW.windowShouldClose w)

data GLFWToken = GLFWToken

-- | Returns a linear token to guarantee GLFW is terminated
initGLFW :: Linear.MonadIO m => m GLFWToken
initGLFW = liftSystemIO $ do
  True <- GLFW.init
  True <- GLFW.vulkanSupported
  GLFW.windowHint (GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI)
  Prelude.pure GLFWToken

terminateGLFW :: Linear.MonadIO m => GLFWToken ⊸ m ()
terminateGLFW GLFWToken = liftSystemIO $ GLFW.terminate
