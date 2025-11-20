{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
module Ghengin.Camera where

import GHC.TypeLits
import GHC.Generics

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

-- | A perspective camera at the given position looking at the given target, for the given resolution
--
-- +X right, +Y down, +Z forward
cameraLookAt :: Vec3 {-^ camera position -} -> Vec3 {-^ camera target -} -> (Float, Float) {-^ width x height -}
             -> Camera view_field proj_field
cameraLookAt eye target (width, height) =
  Camera
    { view = unTransform $ lookAtRH_ eye target
    , proj = unTransform $ reverseDepthRH (pi / 2) 0.1 width height
    }

--------------------------------------------------------------------------------

instance ShaderData (Camera view_field proj_field) where
  type FirType (Camera view_field proj_field)
            = FIR.Struct '[ view_field 'FIR.:-> FIR.M 4 4 Float
                          , proj_field 'FIR.:-> FIR.M 4 4 Float ]
