module Ghengin.Component.Camera where

import Data.Word (Word32)

-- It would likely be good to use just this module for the component Camera
-- import Geomancy.Vulkan.Projection
import Geomancy.Mat4
import Geomancy.Vec3

-- It would also be better to have 1 type of camera for each projection x view,
-- besides a generic completely-dynamic one whose projection and view can
-- easily change... (although one might also be able to easily change the
-- Camera). So... maybe... the best would be a Camera parametrized over the
-- kind of projection and kind of view, which determines at runtime exactly
-- which one it is - so no runtime hit for having it be "dynamic" ie in a single type)
