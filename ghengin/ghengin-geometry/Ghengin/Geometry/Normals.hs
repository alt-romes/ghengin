{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- | Module with utilities for computing normals of a Mesh.
--
-- There are two "simple" shading models available:
--
-- Smooth shading - where the normals of each vertex that is shared by more
-- than one face share gets a single, averaged normal
--
-- Flat shading - where each vertex is duplicated for every triangle it's a
-- part of and gets assigned the normal of that triangle
--
-- For smooth shading we use Inigo Quilez's
-- https://iquilezles.org/articles/normals/
module Ghengin.Geometry.Normals where

import Geomancy
import qualified Geomancy.Vec3 as Vec3

import Prelude.Linear
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import qualified Data.Array.Mutable.Linear as Array

{- | Compute the smooth normal vectors of a mesh surface.

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

> This is quite fast, fast enough to do lot of mesh normalizations per frame. On
top of it, if you are using vertex shaders you can be interested on skipping
the last vertex normalization and do it on the shader (last line on the code
above). Also in some cases, like 64 or even 4 kilobyte demos, it's usual to
have all allocated buffers automatically initialized to zero. In that case, if
this is the first and only normalization for a given mesh, you may skip the
first loop on the function too of course.
-}
{-# INLINEABLE computeNormals #-}
computeNormals :: (GV.Vector v Int, GV.Vector w Vec3)
               => v Int  -- ^ Every three indices into the vertices array forms a face
               -> w Vec3 -- ^ The position of every vertex
               -> V.Vector Vec3 -- ^ The normal vector for each vertex
computeNormals ixs vs =
  unur $ -- todo: freeze variant which constructs SV.Vector?
  Array.alloc (GV.length vs) (vec3 0 0 0) $ \arr0 ->
    Array.freeze $
    Array.map Vec3.normalize $
      foldl' (\arr (Ur i) -> let
           ia = ixs ! (i)
           ib = ixs ! (i+1)
           ic = ixs ! (i+2)
           e1 = ((vs ! ia) :: Vec3) ^-^ (vs ! ib)
           e2 = (vs ! ic) ^-^ (vs ! ib)
           no = Vec3.cross e1 e2 
        in arr & ia += no
               & ib += no
               & ic += no
        ) arr0 [Ur i | i <- [0,3..GV.length ixs - 4]]
  where
    (!) :: GV.Vector t x => t x -> Int -> x
#ifdef DEBUG
    (!) = (GV.!)
#else
    (!) = GV.unsafeIndex
#endif

    (+=) :: Int -> Vec3 -> Array.Array Vec3 %1 -> Array.Array Vec3
#ifdef DEBUG
    (+=) i new arr0 = case Array.get i arr0 of
      (Ur exists, arr1) -> Array.set i (new ^+^ exists) arr1
#else
    (+=) i new arr0 = case Array.unsafeGet i arr0 of
      (Ur exists, arr1) -> Array.unsafeSet i (new ^+^ exists) arr1
#endif

-- TODO: Try backpermute

