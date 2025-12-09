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

import qualified Prelude
import Prelude ((<))
import Geomancy
import qualified Geomancy.Point as P
import qualified Geomancy.Vec3 as Vec3

import Prelude.Linear hiding ((<))
import qualified Data.List as L
import qualified Data.IntMap as IM
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

-- TODO: Try backpermute in the above

{- | Deduplicate vertices that are nearby.

For the given list of vertices and the face indices, return the updated face
indices which only use the set of vertices that were welded

The "fast" version from http://www.codersnotes.com/notes/welding-vertices/

You typically want to call this before 'computeNormals' if a mesh has
overlapping vertices which don't connect, otherwise the smooth normals won't look fine.
-}
weldVertices :: (GV.Vector v Vec3, GV.Vector w Int) => v Vec3 -> w Int -> w Int
weldVertices vxs ixs =

  -- The "fast" version
  let threshold = 0.00001
      vxs_sorted = V.fromList (L.sortOn fst (GV.toList vxs `Prelude.zip` [0..]))
      vec3X (WithVec3 x _ _) = x
      go va a_ix a b
        | b < 0 = IM.empty
        | vec3X (fst (vxs_sorted V.! b)) < (vec3X va - threshold)
        = IM.empty -- shortcut
        | P.distance (P.Point va) (P.Point (fst (vxs_sorted V.! b))) < threshold
        = IM.singleton (snd (vxs_sorted V.! b)) a_ix `IM.union` go va a_ix a (b-1)
        | otherwise
        = go va a_ix a (b-1)
      mapping =
        IM.unions
          [ go va a_ix a (a-1)
          | ((va, a_ix), a) <- Prelude.zip (GV.toList vxs_sorted) [0..]
          ]
   in GV.map (\ix -> case IM.lookup ix mapping of
                Nothing -> ix
                Just other -> other
             ) ixs

  -- The "slow" version
  {-
  let threshold = 0.00001
      mapping =
        IM.unions
          [ IM.singleton a b
          | (va, a) <- Prelude.zip (GV.toList vxs) [0..]
          , (vb, b) <- Prelude.zip (GV.toList vxs) [0..]
          , P.distance (P.Point va) (P.Point vb) < threshold
          ]
   in GV.map (\ix -> case IM.lookup ix mapping of
                Nothing -> ix
                Just other -> other
             ) ixs
  -}
