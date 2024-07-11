{-# OPTIONS_GHC -Wno-name-shadowing -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
module Game.Geometry.Sphere where

import Prelude
import Ghengin.Core.Prelude (GHList(..))
import Control.Monad
import Geomancy.Vec3

import Ghengin.Core.Type.Compatible
import Ghengin.Core.Renderer
import Ghengin.Core.Render.Pipeline
import Ghengin.Core.Mesh

import qualified Data.Vector.Storable as SV
import qualified Data.IntMap as IM
import qualified Data.Map as M

import Data.List (sort)
import Data.List.Split (chunksOf)

data UnitFace = UF { positions :: [Vec3]
                   , indices   :: [Int]
                   }

data UnitSphere = UnitSphere { positions :: [ Vertex '[Vec3, Vec3, Vec3] ]
                             , indices   :: [Int]
                             }

newUnitFace :: Int  -- ^ Resolution
            -> Vec3 -- ^ Local up
            -> UnitFace
newUnitFace res up =
  let
      axisA, axisB :: Vec3
      axisA = withVec3 up (\x y z -> vec3 y z x)
      axisB = cross up axisA
      
      fres = fromIntegral res
      positions' = map (normalize . \(px,py) -> up + axisA^*(2*(px - 0.5)) + axisB^*(2*(py - 0.5)))
                     do x <- [0..fres-1]
                        y <- [0..fres-1]
                        pure (x/(fres-1), y/(fres-1))
      ixs  = do x <- [0..(res-1)]
                y <- [0..(res-1)]
                guard $ (x /= res - 1) && (y /= res - 1)
                let i = x + y*res
                [i, i+res+1, i+res, i, i+1, i+res+1]

   in UF positions' ixs

-- | Crashes on resolution = 1
newUnitSphere :: Int -- ^ Resolution
          -> Maybe Vec3 -- ^ Color, use the normals if Nothing
          -> UnitSphere
newUnitSphere res color =
  let UF v1 i1 = newUnitFace res (vec3 0 1 0)
      UF v2 i2 = newUnitFace res (vec3 0 (-1) 0)
      UF v3 i3 = newUnitFace res (vec3 1 0 0)
      UF v4 i4 = newUnitFace res (vec3 (-1) 0 0)
      UF v5 i5 = newUnitFace res (vec3 0 0 1)
      UF v6 i6 = newUnitFace res (vec3 0 0 (-1))
      l  = length v1 -- all faces share same length
      is = i1 <> map (+l) i2 <> map (+l*2) i3 <> map (+l*3) i4 <> map (+l*4) i5 <> map (+l*5) i6
      ps = v1 <> v2 <> v3 <> v4 <> v5 <> v6
      ns = calculateSmoothNormals is ps
      cls = maybe (map ((^/2) . (+ vec3 1 1 1)) ns) (\x -> map (const x) ns) color
   in
      UnitSphere (zipWith3 (\a b c -> a :& b :&: c) ps ns cls) is

newSphereMesh :: CompatibleVertex [Vec3, Vec3, Vec3] π
              => RenderPipeline π bs
              -> Int -- ^ Resolution
              -> Maybe Vec3 -- ^ Color, use the normals if Nothing
              -> Renderer (Mesh [Vec3, Vec3, Vec3] '[], RenderPipeline π bs)
newSphereMesh pi res color =
  let UnitSphere vs is = newUnitSphere res color
   in createMeshWithIxs pi GHNil vs is

-- TODO: These were inlined from Core.Mesh. Make them a better home and remove occurrences there.

-- | Calculate smooth normals of vertices given vertex positions and the
-- indices that describe the faces The returned list has a normal for each
-- position in the input positions, in the same order
--
-- TODO: Take into consideration the angles or provide alternative that does
--
-- MOVE TO GHENGIN Component/Mesh or something. For now, inlined in Component.Mesh.Sphere.
calculateSmoothNormals :: [Int] -> [Vec3] -> [Vec3]
calculateSmoothNormals ixs pos =

  let fns = calculateFlatNormals ixs pos

      smoothNormalsMap = foldl' (\acc (p,n) -> M.insertWith (\(na, i) (nb, j) -> (na + nb, i + j)) p (n,1) acc) mempty (zip pos fns)

   in map (\p -> case smoothNormalsMap M.! p of (n,b) -> n^/b) pos

-- | Calculate normals of vertices given vertex positions and the indices that describe the faces
-- The returned list has a normal for each position in the input positions, in the same order
calculateFlatNormals :: [Int] -> [Vec3] -> [Vec3]
calculateFlatNormals ixs (SV.fromList -> pos) =

  let m = foldl' (\acc [a,b,c] ->
            let vab = (pos SV.! b) - (pos SV.! a)
                vbc = (pos SV.! c) - (pos SV.! b)
                n = normalize $ cross vbc vab -- vbc X vab gives the normal facing up for clockwise faces
             in IM.insertWith const a n $ IM.insertWith const b n $ IM.insertWith const c n acc) mempty (chunksOf 3 ixs)

   in map snd $ sort (IM.toList m)

