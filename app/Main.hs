{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Graphics.UI.GLFW as GLFW
import Vulkan

import qualified Data.Vector as V

main :: IO ()
main = do

  True <- GLFW.init
  GLFW.windowHint (GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI)

  Just win <- GLFW.createWindow 800 600 "Vulkan" Nothing Nothing

  True <- GLFW.vulkanSupported
  -- getRequiredInstanceExtensions
  (_, V.length -> nExts) <- enumerateInstanceExtensionProperties Nothing

  putStr "N extensions: "
  print nExts

  mainloop win

  GLFW.destroyWindow win

  GLFW.terminate

  


mainloop :: GLFW.Window -> IO ()
mainloop win =
  GLFW.windowShouldClose win >>= \case
    False -> do
      GLFW.pollEvents
      mainloop win
    True  -> pure ()

