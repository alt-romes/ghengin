{-# LANGUAGE LambdaCase #-}
module Ghengin.Window
  ( Window
  , withWindow
  , loopUntilClosed
  ) where

import Control.Exception

import qualified Graphics.UI.GLFW as GLFW

newtype Window = W GLFW.Window

-- | Creates a window and its GLFW context. Probably doesn't work if we need
-- multiple windows
createWindow :: Int -> Int -> String -> IO Window
createWindow w h label = do
  True     <- GLFW.init
  True     <- GLFW.vulkanSupported
  GLFW.windowHint (GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI)
  Just win <- GLFW.createWindow w h label Nothing Nothing
  pure (W win)

-- | Destroys the window and the GLFW context. To have multiple windows this
-- wouldn't work
destroyWindow :: Window -> IO ()
destroyWindow (W win) = do
  GLFW.destroyWindow win
  GLFW.terminate

-- | Create a window which is automatically destroyed when the function that
-- uses it is finished.
withWindow :: Int -> Int -> String -> (Window -> IO a) -> IO a
withWindow w h label f = bracket (createWindow w h label) destroyWindow f

-- | Run an IO action many many times until the window is closed by a normal
-- window-closing event.
loopUntilClosed :: Window -> IO () -> IO ()
loopUntilClosed (W win) io = do
  GLFW.windowShouldClose win >>= \case
    False -> do
      io
      GLFW.pollEvents
      loopUntilClosed (W win) io
    True  -> pure ()



