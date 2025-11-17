{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
module Ghengin.Camera where

import GHC.TypeLits
import GHC.Generics
import Data.Word (Word32)

import Ghengin.Core.Prelude as L

-- It would likely be good to use just this module for the component Camera
-- import Geomancy.Vulkan.Projection
import Geomancy.Mat4
import Geomancy.Vec3
import Geomancy.Transform
import Ghengin.Core.Shader.Data

import Geomancy.Vulkan.View
import Geomancy.Vulkan.Projection

import qualified FIR
import qualified Math.Linear as FIR

-- | A camera component.
--
-- The two type arguments are the field names for the view and projection
-- matrix in the shader, for instance @"view_matrix"@ and @"proj_matrix"@.
data Camera (view_field :: Symbol) (proj_field :: Symbol)
  = Camera { view :: !Mat4
           , proj :: !Mat4
           }
           deriving Generic
           deriving anyclass Block

-- | A default camera looking at (0, 0, 0) using a perspective projection.
--
-- +X right, +Y down, +Z forward
defaultCamera :: Camera view_field proj_field
defaultCamera = 
  let
    up = vec3 0 (-1) 0
    forward = vec3 0 0 1
    eye = vec3 0 0 0
  in
    Camera
    { view = unTransform $ lookAtRH eye forward up 
    -- pi / 2 = 90 degrees
    -- pi / 4 = 45 degrees
    , proj = unTransform $ reverseDepthRH (pi / 2) 0.1 640 480
    }

--------------------------------------------------------------------------------

instance ShaderData (Camera view_field proj_field) where
  type FirType (Camera view_field proj_field)
            = FIR.Struct '[ view_field 'FIR.:-> FIR.M 4 4 Float
                          , proj_field 'FIR.:-> FIR.M 4 4 Float ]
