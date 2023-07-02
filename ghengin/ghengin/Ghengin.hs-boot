{-# LANGUAGE DataKinds #-}
module Ghengin where

import Apecs
import Data.Unrestricted.Linear (Dupable)
import Ghengin.Vulkan.Renderer.Kernel -- See Todo about making this a Core import. For now we can't since Ghengin still depends too much on vulkan specific parts of the API
import Ghengin.Core.Log
import {-# SOURCE #-} Ghengin.World (World)
import Data.Unrestricted.Linear (UrT)

type Ghengin w = SystemT (World w) (UrT Renderer)

instance Dupable w => HasLogger (Ghengin w)

