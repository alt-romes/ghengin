{-# LANGUAGE NoImplicitPrelude #-}
module Ghengin.Geometry.Normals where

import Geomancy
import qualified Geomancy.Vec3 as Vec3

import Prelude.Linear
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Array.Mutable.Linear as Array

{- | Compute the normal vectors of a mesh surface.

Uses Inigo Quilez's fast and correct algorithm: https://iquilezles.org/articles/normals/

void Mesh_normalize( Mesh *myself )
{
    Vert     *vert = myself->vert;
    Triangle *face = myself->face;

    for( int i=0; i<myself->mNumVerts; i++ ) vert[i].normal = vec3(0.0f);

    for( int i=0; i<myself->mNumFaces; i++ )
    {
        const int ia = face[i].v[0];
        const int ib = face[i].v[1];
        const int ic = face[i].v[2];

        const vec3 e1 = vert[ia].pos - vert[ib].pos;
        const vec3 e2 = vert[ic].pos - vert[ib].pos;
        const vec3 no = cross( e1, e2 );

        vert[ia].normal += no;
        vert[ib].normal += no;
        vert[ic].normal += no;
    }

    for( i=0; i<myself->mNumVerts; i++ ) verts[i].normal = normalize( verts[i].normal );
}
-}
computeNormals :: Vector Int  -- ^ Every three indices into the vertices array forms a face
               -> Vector Vec3 -- ^ The position of every vertex
               -> Vector Vec3 -- ^ The normal vector for each vertex
computeNormals ixs vs =
  unur $ Array.alloc (V.length vs) 0 $ \arr0 ->
    Array.freeze $
    Array.map Vec3.normalize $
      foldl' (\arr (Ur i) -> let
           ia = ixs ! i
           ib = ixs ! i+1
           ic = ixs ! i+2
           e1 = vs ! ia ^-^ vs ! ib
           e2 = vs ! ic ^-^ vs ! ib
           no = Vec3.cross e1 e2 
        in arr & ia += no
               & ib += no
               & ic += no
        ) arr0 [Ur i | i <- [0,3..V.length ixs - 1]]
  where
    (!) = (V.!)

    (+=) :: Int -> Vec3 -> Array.Array Vec3 %1 -> Array.Array Vec3
    (+=) i new arr0 = case Array.get i arr0 of
      (Ur exists, arr1) -> Array.set i (new ^+^ exists) arr1

 -- todo: use unsafeIndex, unsafeSet, unsafeGet
