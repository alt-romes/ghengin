-- |
--
-- TODO: Since these are procedurally generated, it would be great if we
-- encouraged users to lift them with TemplateHaskell when the parameters are
-- statically known to make sure the static procedural parts happen at compile time.
--
-- Assumptions:
--
-- * Using a cartesian coordinate space XYZ where Y points "up" and Z points "outwards":
-- ZX is parallel to the floor/ceiling, YX is parallel to the screen, YZ is
-- perpendicular to the screen...
--
-- * Using triangle list primitive topology
--
-- * Using COUNTER_CLOCKWISE order for front facing triangles
module Ghengin.Geometry.Cube where

import Prelude -- We often don't need linearity here.
import Ghengin.Core.Mesh.Vertex
import Ghengin.Geometry.Transform

-- | A unit cube centered at (0,0,0).
cube :: [Vertex '[Vec3]]
cube = Prelude.map Sin
  [ -- Front face (z = -0.5, facing -Z, viewed from outside/front)
    vec3 (-0.5) (-0.5) (-0.5)  -- bottom-left
  , vec3 (-0.5)   0.5  (-0.5)  -- top-left
  , vec3   0.5    0.5  (-0.5)  -- top-right
  , vec3   0.5    0.5  (-0.5)  -- top-right
  , vec3   0.5  (-0.5) (-0.5)  -- bottom-right
  , vec3 (-0.5) (-0.5) (-0.5)  -- bottom-left
  -- Back face (z = 0.5, facing +Z, viewed from outside/back)
  , vec3   0.5  (-0.5)   0.5   -- bottom-right
  , vec3   0.5    0.5    0.5   -- top-right
  , vec3 (-0.5)   0.5    0.5   -- top-left
  , vec3 (-0.5)   0.5    0.5   -- top-left
  , vec3 (-0.5) (-0.5)   0.5   -- bottom-left
  , vec3   0.5  (-0.5)   0.5   -- bottom-right
  -- Right face (x = 0.5, facing +X, viewed from outside/right)
  , vec3   0.5  (-0.5) (-0.5)  -- bottom-front
  , vec3   0.5    0.5  (-0.5)  -- top-front
  , vec3   0.5    0.5    0.5   -- top-back
  , vec3   0.5    0.5    0.5   -- top-back
  , vec3   0.5  (-0.5)   0.5   -- bottom-back
  , vec3   0.5  (-0.5) (-0.5)  -- bottom-front
  -- Left face (x = -0.5, facing -X, viewed from outside/left)
  , vec3 (-0.5) (-0.5)   0.5   -- bottom-back
  , vec3 (-0.5)   0.5    0.5   -- top-back
  , vec3 (-0.5)   0.5  (-0.5)  -- top-front
  , vec3 (-0.5)   0.5  (-0.5)  -- top-front
  , vec3 (-0.5) (-0.5) (-0.5)  -- bottom-front
  , vec3 (-0.5) (-0.5)   0.5   -- bottom-back
  -- Top face (y = 0.5, facing +Y, viewed from outside/top)
  , vec3 (-0.5)   0.5  (-0.5)  -- front-left
  , vec3 (-0.5)   0.5    0.5   -- back-left
  , vec3   0.5    0.5    0.5   -- back-right
  , vec3   0.5    0.5    0.5   -- back-right
  , vec3   0.5    0.5  (-0.5)  -- front-right
  , vec3 (-0.5)   0.5  (-0.5)  -- front-left
  -- Bottom face (y = -0.5, facing -Y, viewed from outside/bottom)
  , vec3 (-0.5) (-0.5)   0.5   -- back-left
  , vec3 (-0.5) (-0.5) (-0.5)  -- front-left
  , vec3   0.5  (-0.5) (-0.5)  -- front-right
  , vec3   0.5  (-0.5) (-0.5)  -- front-right
  , vec3   0.5  (-0.5)   0.5   -- back-right
  , vec3 (-0.5) (-0.5)   0.5   -- back-left
  ]

--------------------------------------------------------------------------------
-- * Colored

type Color = Vec3

-- | A colored 'cube'.
-- Note that color is in location=0, and position in location=1
coloredCube :: [Vertex '[Color, Vec3]]
coloredCube = zipWith paint cube [1..] where
  paint face (ix::Int)
    -- every six vertices we get a new face
    | ix <= 6   = blue   :& face
    | ix <= 6*2 = green  :& face
    | ix <= 6*3 = yellow :& face
    | ix <= 6*4 = white  :& face
    | ix <= 6*5 = orange :& face
    | ix <= 6*6 = red    :& face
    | otherwise = error "unexpected vertice unit cube"

  white = vec3 0.9 0.9 0.9
  yellow = vec3 0.8 0.8 0.1
  orange = vec3 0.9 0.6 0.1
  red = vec3 0.8 0.1 0.1
  blue = vec3 0.1 0.1 0.8
  green = vec3 0.1 0.8 0.1

