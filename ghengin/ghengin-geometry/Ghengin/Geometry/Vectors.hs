-- | Meant to be imported qualified
--
-- @
-- Vectors.up ^+^ Vectors.down
-- @
module Ghengin.Geometry.Vectors where

import Geomancy.Vec3

up, down, left, right, forward, back :: Vec3
up      = vec3 0 (-1) 0
down    = vec3 0 1 0
left    = vec3 (-1) 0 0
right   = vec3 1 0 0
forward = vec3 0 0 1
back    = vec3 0 0 (-1)
