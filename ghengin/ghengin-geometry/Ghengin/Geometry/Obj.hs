{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Ghengin.Geometry.Obj where

import Prelude
import qualified Prelude.Linear as Linear
import Control.Monad

import qualified Data.Vector as V

import Geomancy

import Codec.Wavefront

import Ghengin.Core.Type.Compatible
import Ghengin.Core.Prelude (GHList(..))
import Ghengin.Core.Render.Pipeline
import Ghengin.Core.Render.Property
import Ghengin.Core.Renderer
import Ghengin.Core.Mesh
import Ghengin.Core.Mesh.Vertex
import qualified Data.Linear.Alias as Alias

import qualified Control.Functor.Linear as Linear
import qualified Control.Monad.IO.Class.Linear as Linear

loadObjMesh :: (CompatibleMesh ts π, CompatibleVertex '[Vec3, Vec3] π)
            => FilePath
            -> RenderPipeline π ps
             ⊸ PropertyBindings ts
            -- ^ These must NOT be added after creating the mesh.
            -- The property bindings need to set when creating it.
             ⊸ Renderer (Mesh '[Vec3, Vec3] ts, RenderPipeline π ps)
loadObjMesh filepath rp props = Linear.do
  Linear.liftSystemIOU (fromFile filepath) Linear.>>= \case
    Linear.Ur (Left err) -> Linear.do
      destroyRenderPipeline rp
      Alias.forget props
      Linear.liftSystemIO (fail err)
    Linear.Ur (Right wavefrontObj) ->
      let
          locs    = V.map getLoc    $ wavefrontObj.objLocations
          normals = V.map getNormal $ wavefrontObj.objNormals
          faces   = V.map (.elValue) wavefrontObj.objFaces

          getLoc :: Location -> Vec3
          getLoc (Location x y z _) = vec3 x (-y) z

          getNormal :: Normal -> Vec3
          getNormal (Normal x y z) = vec3 x y z

          vertices = V.zipWith (:&:) locs normals
          ixs = V.concatMap (\(Face a b c d) -> V.fromList $
                  [ a.faceLocIndex , b.faceLocIndex , c.faceLocIndex ] ++ map (.faceLocIndex) d
                  ) faces
       in
          createMeshWithIxsSV rp props (V.convert vertices) (V.convert $ V.map fromIntegral ixs)

