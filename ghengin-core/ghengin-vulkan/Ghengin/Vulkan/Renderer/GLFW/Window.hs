{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BlockArguments #-}
module Ghengin.Vulkan.Renderer.GLFW.Window
  ( 
  -- * Creating windows and surfaces
    WindowInfo(..)
  -- ** Window
  , createWindow
  , destroyWindow
  -- ** Surface
  , createSurface
  , destroySurface

  -- * Initializing GLFW
  , GLFWToken
  , initGLFW
  , terminateGLFW

  -- * GLFW re-exports
  , GLFW.Window
  , GLFW.windowShouldClose
  , GLFW.pollEvents
  ) where

import GHC.Int (Int32)
import qualified Prelude
import Prelude.Linear
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

-- | Window construction info
data WindowInfo
  = WindowInfo
  { width      :: Int
  , height     :: Int
  , windowName :: String
  }

-- | Creates a window and its GLFW context.
createWindow :: Linear.MonadIO m => WindowInfo -> m GLFW.Window
createWindow WindowInfo{..} = liftSystemIO $ do
  Just win <- GLFW.createWindow width height windowName Nothing Nothing
  Prelude.pure win
{-# INLINE createWindow #-}

-- | Destroys the window and the GLFW context.
destroyWindow :: Linear.MonadIO m => GLFW.Window ⊸ m ()
destroyWindow = Unsafe.toLinear \win -> liftSystemIO $ GLFW.destroyWindow win
{-# INLINE destroyWindow #-}

-- | Create a vulkan surface from the 'GLFW.Window'
createSurface :: Linear.MonadIO m => Vk.Instance ⊸ GLFW.Window ⊸ m (Vk.SurfaceKHR, Vk.Instance, GLFW.Window)
createSurface = Unsafe.toLinear2 \i w -> liftSystemIO $ do
  alloca $ \surfacePtr -> do
    r <- Vk.Result Prelude.<$> GLFW.createWindowSurface @Int32 (Vk.instanceHandle i) w nullPtr surfacePtr
    Monad.when (r Prelude.< Vk.SUCCESS) (Control.Exception.throwIO (Vk.VulkanException r))
    surface <- peek surfacePtr
    Prelude.pure (surface, i, w)
{-# INLINE createSurface #-}

-- | Destroy a vulkan surface
-- Note: All SwapchainKHR objects created for surface must have been destroyed prior to destroying surface
destroySurface :: Linear.MonadIO m => Vk.Instance ⊸ Vk.SurfaceKHR ⊸ m Vk.Instance
destroySurface = Unsafe.toLinear2 \i s -> liftSystemIO $ i Prelude.<$ Vk.destroySurfaceKHR i s Nothing
{-# INLINE destroySurface #-}

-- | A linear token to ensure GLFW is terminated
data GLFWToken = GLFWToken

-- | Returns a linear token to guarantee GLFW is terminated
initGLFW :: Linear.MonadIO m => m GLFWToken
initGLFW = liftSystemIO $ do
  ginit <- GLFW.init
  if ginit then do
    vsupport <- GLFW.vulkanSupported
    if vsupport then do
      GLFW.windowHint (GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI)
      Prelude.pure GLFWToken
    else do
      error "GLFW says vulkan is not supported. Are you sure vulkan is properly configured (e.g. are you in the project's nix-shell?)"
  else do
    error "GLFW failed to initialize"

-- | Consume a linear token to terminate GLFW
terminateGLFW :: Linear.MonadIO m => GLFWToken ⊸ m ()
terminateGLFW GLFWToken = liftSystemIO $ GLFW.terminate

