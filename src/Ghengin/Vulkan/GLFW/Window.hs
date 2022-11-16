{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
module Ghengin.Vulkan.GLFW.Window (VulkanWindow(..), createVulkanWindow, destroyVulkanWindow, loopUntilClosed) where

import GHC.Int

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable

import Control.Monad.IO.Class
import Control.Monad
import Control.Exception

import qualified Graphics.UI.GLFW as GLFW
import qualified Vulkan.Exception as Vk (VulkanException(..))
import qualified Vulkan as Vk

data VulkanWindow = VulkanWindow { _window :: GLFW.Window
                                 , _surface :: Vk.SurfaceKHR
                                 }

createVulkanWindow :: Vk.Instance
                   -> (Int, Int) -- ^ (width, height)
                   -> String     -- ^ window name
                   -> IO VulkanWindow
createVulkanWindow inst dimensions label = do
  !win     <- createWindow dimensions label
  surface <- createSurface inst win
  pure $ VulkanWindow win surface

-- TODO: Can I destroy the window before the instance?
destroyVulkanWindow :: Vk.Instance -> VulkanWindow -> IO ()
destroyVulkanWindow inst (VulkanWindow win surface) = do
  destroySurface inst surface
  destroyWindow win

-- | Creates a window and its GLFW context. Probably doesn't work if we need
-- multiple windows
createWindow :: (Int, Int) -> String -> IO GLFW.Window
createWindow (w,h) label = do
  True     <- GLFW.init
  True     <- GLFW.vulkanSupported
  GLFW.windowHint (GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI)
  Just win <- GLFW.createWindow w h label Nothing Nothing
  pure win
{-# INLINE createWindow #-}

-- | Destroys the window and the GLFW context. To have multiple windows this
-- wouldn't work
destroyWindow :: GLFW.Window -> IO ()
destroyWindow win = do
  GLFW.destroyWindow win
  GLFW.terminate
{-# INLINE destroyWindow #-}

-- | Create a surface (it must be destroyed by Vulkan).
createSurface :: Vk.Instance -> GLFW.Window -> IO Vk.SurfaceKHR
createSurface i w = do
  alloca $ \surfacePtr -> do
    r <- Vk.Result <$> GLFW.createWindowSurface @Int32 (Vk.instanceHandle i) w nullPtr surfacePtr
    when (r < Vk.SUCCESS) (throwIO (Vk.VulkanException r))
    surface <- peek surfacePtr
    pure surface
{-# INLINE createSurface #-}

destroySurface :: Vk.Instance -> Vk.SurfaceKHR -> IO ()
destroySurface i s = Vk.destroySurfaceKHR i s Nothing
{-# INLINE destroySurface #-}

-- -- | Run an IO action many many times until the window is closed by a normal
-- -- window-closing event.
loopUntilClosed :: MonadIO m => GLFW.Window -> m () -> m ()
loopUntilClosed win action = do
  liftIO (GLFW.windowShouldClose win) >>= \case
    True  -> pure ()
    False -> do
      action
      liftIO GLFW.pollEvents
      loopUntilClosed win action

