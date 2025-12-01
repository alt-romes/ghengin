{-# OPTIONS_GHC -Wno-name-shadowing -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
module Ghengin.Geometry.Sphere where

import Prelude
import Ghengin.Core.Prelude (GHList(..), Int32)
import Control.Monad
import Geomancy.Vec3

import Ghengin.Core.Type.Compatible
import Ghengin.Core.Renderer
import Ghengin.Core.Render.Pipeline
import Ghengin.Core.Mesh

import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV

import Ghengin.Geometry.Normals
import qualified Ghengin.Geometry.Vectors as Vectors

data UnitFace = UF { positions :: [Vec3]
                   , indices   :: [Int]
                   }

data UnitSphere = UnitSphere { positions :: SV.Vector (Vertex '[Vec3, Vec3])
                             , indices   :: SV.Vector Int32
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
      positions' = do
        x <- [0..fres-1]
        y <- [0..fres-1]
        let pct_x = x/(fres-1)
            pct_y = y/(fres-1)
        pure $ normalize $
          up + axisA^*(2*(pct_x - 0.5)) + axisB^*(2*(pct_y - 0.5))
      ixs  = do x <- [0..(res-1)]
                y <- [0..(res-1)]
                guard $ (x /= res - 1) && (y /= res - 1)
                let i = x + y*res
                [i, i+res, i+res+1, i, i+res+1, i+1]

   in UF positions' ixs

-- | Crashes on resolution = 1
newUnitSphere :: Int -- ^ Resolution
              -> UnitSphere
newUnitSphere res =
  let UF v1 i1 = newUnitFace res Vectors.up
      UF v2 i2 = newUnitFace res Vectors.down
      UF v3 i3 = newUnitFace res Vectors.left
      UF v4 i4 = newUnitFace res Vectors.right
      UF v5 i5 = newUnitFace res Vectors.forward
      UF v6 i6 = newUnitFace res Vectors.back
      l  = length v1 -- all faces share same length
      is = SV.fromList $
           i1 <> map (+l) i2 <> map (+(l*2)) i3 <> map (+(l*3)) i4 <> map (+(l*4)) i5 <> map (+(l*5)) i6
      ps = SV.fromList $ v1 <> v2 <> v3 <> v4 <> v5 <> v6
      ns = computeNormals is ps
   in
      UnitSphere (SV.zipWith (\a b -> a :&: b) ps (V.convert ns)) (SV.map fromIntegral is)

-- | Draw a single face
newUnitSphereFace :: Bool -- ^ Top face = TRUE; Front face = FALSE;
                  -> Int -- ^ 
                  -> UnitSphere
newUnitSphereFace topf res =
  let UF v1 i1 = newUnitFace res (if topf then Vectors.up else Vectors.back)
      is = SV.fromList i1 
      ps = SV.fromList v1
      ns = computeNormals is ps
   in
      UnitSphere (SV.zipWith (\a b -> a :&: b) ps (V.convert ns)) (SV.map fromIntegral is)

newSphereMesh :: (CompatibleMesh '[] π, CompatibleVertex [Vec3, Vec3] π)
              => RenderPipeline π bs
              -> Int -- ^ Resolution
              -> Renderer (Mesh [Vec3, Vec3] '[], RenderPipeline π bs)
newSphereMesh pi res =
  let UnitSphere vs is = newUnitSphere res
   in createMeshWithIxsSV pi GHNil vs is

