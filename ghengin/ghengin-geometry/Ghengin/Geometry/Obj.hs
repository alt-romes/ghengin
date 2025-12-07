{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Ghengin.Geometry.Obj where

import Prelude hiding (log)
import qualified Prelude.Linear as Linear

import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV

import Geomancy

import Codec.Wavefront

import Ghengin.Core.Type.Compatible
import Ghengin.Core.Render.Pipeline
import Ghengin.Core.Render.Property
import Ghengin.Core.Renderer
import Ghengin.Core.Mesh
import qualified Data.Linear.Alias as Alias

import qualified Control.Functor.Linear as Linear
import qualified Control.Monad.IO.Class.Linear as Linear

import Ghengin.Geometry.Normals

-- | Loads a 'WavefrontOBJ' from a file.
--
-- Useful if one desires to embed the model with template haskell
loadObjFile :: FilePath -> IO (Either String WavefrontOBJ)
loadObjFile = fromFile

-- | Load an .obj file and create a mesh from it.
loadObjMesh :: (CompatibleMesh ts π, CompatibleVertex '[Vec3, Vec3] π)
            => FilePath
            -> RenderPipeline π ps
             ⊸ PropertyBindings ts
            -- ^ These must NOT be added after creating the mesh.
            -- The property bindings need to set when creating it.
             ⊸ Renderer (Mesh '[Vec3, Vec3] ts, RenderPipeline π ps)
loadObjMesh filepath rp props = Linear.do
  Linear.liftSystemIOU (loadObjFile filepath) Linear.>>= \case
    Linear.Ur (Left err) -> Linear.do
      destroyRenderPipeline rp
      Alias.forget props
      Linear.liftSystemIO (fail err)
    Linear.Ur (Right wavefrontObj) ->
      createObjMesh wavefrontObj rp props

-- | Create a mesh from a 'WavefrontOBJ'
createObjMesh :: (CompatibleMesh ts π, CompatibleVertex '[Vec3, Vec3] π)
              => WavefrontOBJ
              -> RenderPipeline π ps
               ⊸ PropertyBindings ts
              -- ^ These must NOT be added after creating the mesh.
              -- The property bindings need to set when creating it.
               ⊸ Renderer (Mesh '[Vec3, Vec3] ts, RenderPipeline π ps)
createObjMesh wavefrontObj rp props =
  let
      locs    = V.map getLoc    $ wavefrontObj.objLocations
      normals = V.map getNormal $ wavefrontObj.objNormals
      faces   = V.map (.elValue) wavefrontObj.objFaces
      ixs     = V.concatMap (\(Face a b c _) -> V.fromList $
                  [ a.faceLocIndex - 1
                  , b.faceLocIndex - 1
                  , c.faceLocIndex - 1 ]
                  ) faces

      getLoc :: Location -> Vec3
      getLoc (Location x y z _) = vec3 x (-y) z

      getNormal :: Normal -> Vec3
      getNormal (Normal x y z) = vec3 x y z

      mkVert x = locs V.! (x.faceLocIndex-1) :&: maybe (vec3 0 0 0) (\nix -> normals V.! (nix-1)) x.faceNorIndex
      mkMesh
        | V.length normals == 0
        = let welded_ixs = weldVertices locs ixs
              verts = V.zipWith (:&:) locs (computeNormals welded_ixs locs)
           in createMeshWithIxsSV rp props (V.convert verts) (SV.map fromIntegral $ V.convert welded_ixs)
        | otherwise
        = let verts = V.concatMap (\(Face a b c _) -> V.fromList $
                        [ mkVert a, mkVert b, mkVert c ]
                        ) faces
           in createMeshSV rp props (V.convert verts) -- todo: also use ixs for this case
   in mkMesh

