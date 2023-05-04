{-# LANGUAGE DataKinds #-}
module Ghengin where

import Apecs.Linear
import Ghengin.Core.Renderer.Kernel
import {-# SOURCE #-} Ghengin.World (World)

type Ghengin w = SystemT (World w) Renderer

