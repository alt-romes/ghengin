{-# LANGUAGE OverloadedRecordDot #-}
module Camera where

import Ghengin.Core.Prelude as Linear
import Ghengin.Core.Render

import Geomancy.Vec3
import Geomancy.Mat4

-- A transform is required to calculate the view matrix (see 'makeView'). It's position determines the camera position and it's rotation determines the up in the case of LookAt and Direction
data View = ViewLookAt    { _target    :: {-# UNPACK #-} !Vec3 } -- ^ Ignore the camera's transform rotation (except for calculating up) and instead look at a target
          | ViewDirection { _direction :: {-# UNPACK #-} !Vec3 } -- ^ Ignore the camera's transform rotation (except for calculating up) and instead look in a direction
          | ViewTransform                                        -- ^ Use solely the transform associated to the camera for the view matrix
          deriving Show

makeView :: Mat4 -> View -> Mat4
makeView tr view =
  let pos = posFromMat4 tr
      up  = vec3 0 (-1) 0 -- TODO: Calculate up based on camera rotation
   in case view of
    ViewLookAt target -> makeView tr (ViewDirection (target - pos))
    ViewDirection dir ->
      let w@(WithVec3 wx wy wz) = normalize dir
          u@(WithVec3 ux uy uz) = normalize $ cross w up
          v@(WithVec3 vx vy vz) = cross w u
       in colMajor ux uy uz (-dot u pos)
                   vx vy vz (-dot v pos)
                   wx wy wz (-dot w pos)
                   0  0  0  1
    ViewTransform ->
      -- Compute the inverse of the transform matrix to get the view transform
      -- matrix, but do it directly because it's a homogenous rotation matrix
      withColMajor tr (\ux vx wx _px uy vy wy _py uz vz wz _pz _ _ _ _ ->
        let u = vec3 ux uy uz
            v = vec3 vx vy vz
            w = vec3 wx wy wz
         in colMajor ux uy uz (-dot u pos)
                     vx vy vz (-dot v pos)
                     wx wy wz (-dot w pos)
                     0  0  0  1)

posFromMat4 :: Mat4 -> Vec3
posFromMat4 = flip withColMajor (\_ _ _ x _ _ _ y _ _ _ z _ _ _ _ -> vec3 x y z)
