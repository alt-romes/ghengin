{-# LANGUAGE DataKinds #-}
module Ghengin where

import Apecs
import Ghengin.Vulkan
import {-# SOURCE #-} Ghengin.World (World)

type Ghengin w = SystemT (World w) (Renderer ())

