{-# LANGUAGE DataKinds #-}
module Ghengin where

import Apecs.Linear
import Data.Unrestricted.Linear (Dupable)
import Ghengin.Vulkan.Renderer.Kernel -- See Todo about making this a Core import. For now we can't since Ghengin still depends too much on vulkan specific parts of the API
import Ghengin.Core.Log
import {-# SOURCE #-} Ghengin.World (World)

type Ghengin w = SystemT (World w) Renderer

instance Dupable w => HasLogger (Ghengin w)

