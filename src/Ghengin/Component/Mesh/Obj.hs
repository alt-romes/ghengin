{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Ghengin.Component.Mesh.Obj where

import Control.Monad
import Control.Monad.IO.Class

import qualified Data.Vector as V

import Geomancy

import Codec.Wavefront

import Ghengin.Vulkan
import Ghengin.Core.Mesh
import Ghengin.Core.Mesh.Vertex

loadObjMesh :: FilePath -> Renderer (Mesh '[Vec3, Vec3, Vec3]) -- TODO: Type class from ambiguous types defines what properties should be extracted from the object file
loadObjMesh filepath = do
  fromFile filepath >>= \case
    Left err -> liftIO $ fail err
    Right wavefrontObj -> do
      let
          locs    = wavefrontObj.objLocations
          normals = wavefrontObj.objNormals
          faces   = fmap (.elValue) wavefrontObj.objFaces

          getLoc :: FaceIndex -> Vec3
          getLoc faceIx = case locs V.! (faceIx.faceLocIndex - 1) of Location x y z w -> vec3 x (-y) z

          getNormal :: FaceIndex -> Vec3
          getNormal faceIx = maybe 0 (\norIx -> case normals V.! (norIx - 1) of Normal x y z -> vec3 x y z) faceIx.faceNorIndex

          -- Map each face to 3 vertex
          meshFaces = join $ fmap (\(Face a b c _) ->
                                      [ getLoc a :& getNormal a :&: getNormal a -- (vec3 0.5 0.5 0.5)
                                      , getLoc b :& getNormal b :&: getNormal b -- (vec3 0.5 0.5 0.5)
                                      , getLoc c :& getNormal c :&: getNormal c -- (vec3 0.5 0.5 0.5)
                                      ]) faces

          -- meshVertices = fmap (\(Location x y z w) -> Vertex (vec3 x y z) ()) (V.zip locs normals)

      -- TODO: createMeshWithIxs
      createMesh (V.toList meshFaces)

-- loadObjMesh :: FilePath -> Renderer Mesh
-- loadObjMesh filepath = do
--   fromFile filepath >>= \case
--     Left err -> liftIO $ fail err
--     Right wavefrontObj -> do
--       let
--           locs    = V.map getLoc $ wavefrontObj.objLocations
--           normals = V.map getNormal $ wavefrontObj.objNormals
--           faces   = fmap (.elValue) wavefrontObj.objFaces

--           getLoc :: Location -> Vec3
--           getLoc (Location x y z _) = vec3 x (-y) z

--           getNormal :: Normal -> Vec3
--           getNormal (Normal x y z) = vec3 x y z

--           vertices = V.zipWith3 Vertex locs normals normals
--           ixs = join $ fmap (\(Face a b c _) -> [ a.faceLocIndex , b.faceLocIndex , c.faceLocIndex ]) faces

--           -- meshVertices = fmap (\(Location x y z w) -> Vertex (vec3 x y z) ()) (V.zip locs normals)

--       liftIO $ print $ (length vertices, length normals, length ixs)
--       createMeshWithIxs (V.toList vertices) (V.toList ixs)
--       -- createMesh (V.convert meshFaces)

