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
import Ghengin.Component.Mesh

loadObjMesh :: FilePath -> Renderer ext Mesh
loadObjMesh filepath = do
  fromFile filepath >>= \case
    Left err -> liftIO $ fail err
    Right wavefrontObj -> do
      let
          locs    = wavefrontObj.objLocations
          normals = wavefrontObj.objNormals
          faces   = fmap (.elValue) wavefrontObj.objFaces

          getLoc :: FaceIndex -> Vec3
          getLoc faceIx = case locs V.! (faceIx.faceLocIndex - 1) of Location x y z w -> vec3 x y z

          getNormal :: FaceIndex -> Vec3
          getNormal faceIx = maybe 0 (\norIx -> case normals V.! (norIx - 1) of Normal x y z -> vec3 x y z) faceIx.faceNorIndex

          -- Map each face to 3 vertex
          meshFaces = join $ fmap (\(Face a b c _) ->
                                      [ Vertex (getLoc a) (getNormal a) (getNormal a) -- (vec3 0.5 0.5 0.5)
                                      , Vertex (getLoc b) (getNormal b) (getNormal b) -- (vec3 0.5 0.5 0.5)
                                      , Vertex (getLoc c) (getNormal c) (getNormal c) -- (vec3 0.5 0.5 0.5)
                                      ]) faces

          -- meshVertices = fmap (\(Location x y z w) -> Vertex (vec3 x y z) ()) (V.zip locs normals)

      -- TODO: createMeshWithIxs
      createMesh (V.convert meshFaces)

