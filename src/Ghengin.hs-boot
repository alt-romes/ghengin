module Ghengin where

import Apecs
import Ghengin.Vulkan

type Ghengin w = SystemT w (Renderer GEnv)

data GEnv
  -- = GEnv { _renderPipelines :: IORef [SomeRenderPipeline] }
