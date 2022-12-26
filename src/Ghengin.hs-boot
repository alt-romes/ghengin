{-# LANGUAGE DataKinds #-}
module Ghengin where

import Apecs
import Ghengin.Vulkan

type Ghengin w = SystemT w (Renderer ())

