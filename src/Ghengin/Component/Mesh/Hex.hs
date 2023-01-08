module Ghengin.Component.Mesh.Hex where

import Geomancy.Vec3

-- | Hex face: the vertices and indices to form a face
data HexFace = HexFace Int Int -- ^ q and r coords
                       ![Vec3] -- ^ vertices in this face
                       ![Int]  -- ^ indices for face and thick border
                       deriving Show

-- | Make hexagon vertices and indices
makeHexFace :: Float
            -> Float
            -> (Int,Int) -- ^ Hexagon (Axial) Coordinates
            -> HexFace
makeHexFace size percent (q, r) =
  let
    WithVec3 cx _ cz = liftHexCoord size (q,r)
    (innerCorners, outerCorners)
      = unzip $
          map ( (\((cnx,cnz), (cnix,cniz)) -> ( vec3 (cnx + cx)  (-0.1) (cnz + cz)
                                              , vec3 (cnix + cx) (-0.1) (cniz + cz) ) )
                . hexCornerOffset ) [0..5]
    
  in
    HexFace q r (vec3 cx 0 cz:(innerCorners <> innerCorners <> outerCorners))
            ([1, 2, 0, -- Inner hex triangle faces
             2, 3, 0,
             3, 4, 0,
             4, 5, 0,
             5, 6, 0,
             6, 1, 0]
             <> [ 13, 8, 7 -- Quads between outer and inner triangle
                , 13, 14, 8
                , 14, 9, 8
                , 14, 15, 9
                , 15, 10, 9
                , 15, 16, 10
                , 16, 11, 10
                , 16, 17, 11
                , 17, 12, 11
                , 17, 18, 12
                , 18, 7, 12
                , 18, 13, 7
                ])
    where
      -- One for the inner hex and other for the outer one
      hexCornerOffset :: Int   -- ^ Hex corner ix ∈ [0,6[
                      -> ((Float, Float), (Float, Float)) -- ^ The outer and inner hex coords respectively
      hexCornerOffset ix =
        let angle = case ix of
                      0 -> 2*pi*0.5/6
                      1 -> 2*pi*1.5/6
                      2 -> 2*pi*2.5/6
                      3 -> 2*pi*3.5/6
                      4 -> 2*pi*4.5/6
                      5 -> 2*pi*5.5/6
                      i -> error $ "Unexpected corner index " <> show i
         in ((size * cos angle, size * sin angle), (size * cos angle * percent, size * sin angle * percent))


-- | Lift a coordinate in hexagon grid space to a y-flat 3D World space
--
-- q basis: (0,0) -> (1,0): {x=√3, y=0}
-- r basis: (0,0) -> (0,1): {x=(width/2)=(√3/2), y=3/2}
--
-- [ x ] =  size * [ √3 √3/2 ] [ q ]
-- [ y ]           [ 0   3/2 ] [ r ]
liftHexCoord :: Float -- ^ Hexagon size
             -> (Int,Int) -- ^ Hexagon (axial) coordinates
             -> Vec3 -- ^ Position in 3D World with y=0 centered in (0,0)
liftHexCoord size (fromIntegral -> q, fromIntegral -> r) = vec3 (size * (sqrt 3 * q + (sqrt 3/2) * r)) 0 (size * (3/2) * r)


