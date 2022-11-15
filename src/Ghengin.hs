{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Ghengin where

import Control.Monad.Reader

import Ghengin.Vulkan.GLFW.Window
import Ghengin.Vulkan

gameLoop :: Renderer () -> Renderer ()
gameLoop action = ask >>= \renv -> do
  loopUntilClosed (renv._vulkanWindow._window)
    action


