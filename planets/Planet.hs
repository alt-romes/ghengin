{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}
module Planet where

import Debug.Trace
import Control.Monad

import Ghengin
import Ghengin.Vulkan
import Ghengin.Component.Mesh

data TerrainFace = TF [Vec3] [Int]

newTerrainFace :: Int  -- ^ Resolution
               -> Vec3 -- ^ Local up
               -> TerrainFace
newTerrainFace res up =
  let
      axisA, axisB :: Vec3
      axisA = withVec3 up (\x y z -> vec3 y z x)
      axisB = cross up axisA
      
      fres = fromIntegral res
      positions = map (normalize . \(px,py) -> up + axisA^*(2*(px - 0.5)) + axisB^*(2*(py - 0.5)))
                     do x <- [0..fres-1]
                        y <- [0..fres-1]
                        pure (x/(fres-1), y/(fres-1))
      ixs  = do x <- [0..(res-1)]
                y <- [0..(res-1)]
                guard $ (x /= res - 1) && (y /= res - 1)
                let i = x + y*res
                [i, i+res+1, i+res, i, i+1, i+res+1]

   in TF positions ixs

newSphere :: Int -> Renderer Mesh
newSphere res =
  let TF v1 i1 = newTerrainFace res (vec3 0 1 0)
      TF v2 i2 = newTerrainFace res (vec3 0 (-1) 0)
      TF v3 i3 = newTerrainFace res (vec3 1 0 0)
      TF v4 i4 = newTerrainFace res (vec3 (-1) 0 0)
      TF v5 i5 = newTerrainFace res (vec3 0 0 1)
      TF v6 i6 = newTerrainFace res (vec3 0 0 (-1))
      l  = length v1 -- all faces share same length
      is = i1 <> map (+l) i2 <> map (+l*2) i3 <> map (+l*3) i4 <> map (+l*4) i5 <> map (+l*5) i6
      ps = v1 <> v2 <> v3 <> v4 <> v5 <> v6
      ns = calculateFlatNormals is ps
   in do
     createMeshWithIxs (zipWith3 Vertex ps ns (map ((^/2) . (+ vec3 1 1 1)) ns)) is


