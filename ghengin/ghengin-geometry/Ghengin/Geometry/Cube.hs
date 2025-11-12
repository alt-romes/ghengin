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
cube = concatMap (`transform` square)
                                                             -- TODO: These labels might be wrong:
  [ translateV (vec3     0      0 (-0.5))                    -- front face
  , translateV (vec3     0      0   0.5 ) <> rotateY (-pi)   -- back face
  , translateV (vec3   0.5      0     0 ) <> rotateY (-pi/2) -- right face
  , translateV (vec3 (-0.5)     0     0 ) <> rotateY (pi/2)  -- left face
  , translateV (vec3     0    0.5     0 ) <> rotateX (pi/2)  -- top face
  , translateV (vec3     0  (-0.5)    0 ) <> rotateX (-pi/2) -- bottom face
  ]
  -- recall, matrix mult MVP order on CPU


-- | A unit square centered at (0,0,0) along the YZ plane with its front side
-- normal along the X coordinate axis (ie "facing forward").
square :: [Vertex '[Vec3]]
square = Prelude.map Sin
  [ vec3   0.5    0.5  0 -- 1,6
  , vec3   0.5  (-0.5) 0 -- 2
  , vec3 (-0.5) (-0.5) 0 -- 3,4
  , vec3 (-0.5) (-0.5) 0 -- 3,4
  , vec3 (-0.5)   0.5  0 -- 5
  , vec3   0.5    0.5  0 -- 1,6
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

