{-# LANGUAGE OverloadedRecordDot #-}
module Ghengin.Input
  ( GLFW.Key(..)
  , ifPressed
  , ifReleased
  , getKey
  ) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Graphics.UI.GLFW as GLFW
import Ghengin.Vulkan.GLFW.Window
import Ghengin.Vulkan
import Ghengin

ifPressed :: GLFW.Key
          -> Ghengin w a -- ^ Then
          -> Ghengin w a -- ^ Else
          -> Ghengin w a -- ^ Result
ifPressed k t e = do
  getKey k >>= \case
    GLFW.KeyState'Pressed -> t
    _ -> e

-- | If it is not being pressed currently
ifReleased :: GLFW.Key
          -> Ghengin w a -- ^ Then
          -> Ghengin w a -- ^ Else
          -> Ghengin w a -- ^ Result
ifReleased k t e = do
  getKey k >>= \case
    GLFW.KeyState'Released -> t
    _ -> e

getKey :: GLFW.Key -> Ghengin w GLFW.KeyState
getKey k = do
  w <- lift $ asks (._vulkanWindow._window)
  liftIO $ GLFW.getKey w k
