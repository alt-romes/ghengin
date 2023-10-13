{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE LinearTypes #-}
{-|
Note [Render Queue]
~~~~~~~~~~~~~~~~~~~

The render queue is responsible for ordering the draw calls in such a way that
the amount of GPU state changes is minimized in order to optimize the rendering
engine.

TODO:
* Can we update the ECS storage order based on the render order?


Resources:
* [Order your graphics draw calls around!](http://realtimecollisiondetection.net/blog/?p=86)
* [Optimizing State Changes in Rendering Engines](http://real.mtak.hu/28740/1/szecsi_gg14_statechange.pdf)

TODO
[ ] Use representation more efficient than simple lists?

See Note [Render Packet Key] and [Material Key]

-- TODO: Each render packet is then assigned with an ID and sorted in an optimal draw order.
-}
module Ghengin.Core.Render.Queue where

import qualified Prelude
import Prelude.Linear hiding (insert)
import Data.Unrestricted.Linear as Linear
import Control.Functor.Linear as Linear
import qualified Data.Functor.Linear as DL
import qualified Unsafe.Linear as Unsafe
import Data.Unique
import Data.Typeable
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Map.Linear as ML

import Ghengin.Core.Renderer.Kernel
import Ghengin.Core.Render.Pipeline
import Ghengin.Core.Type.Compatible
import Ghengin.Core.Mesh
import Ghengin.Core.Log
import Ghengin.Core.Type.Utils (Some2(..))
import Ghengin.Core.Material hiding (material)


newtype RenderQueue a = RenderQueue (Map TypeRep (Some2 RenderPipeline, Map Unique (Some Material, [(Some2 Mesh, a)])))
  deriving (Prelude.Functor)

instance Semigroup (RenderQueue α) where
  (<>) = Unsafe.toLinear2 \(RenderQueue q) (RenderQueue q') ->
    RenderQueue $
      M.mergeWithKey
        (\_ (p1, im1) (_p2, im2) -> -- (p1 should be the same as p2)
          Just
            (p1, M.mergeWithKey
                (\_ (m1,meshes1) (_m2, meshes2) -> -- (m1 should be the same as m2)
                  Just (m1, meshes1 ++ meshes2)
                ) id id im1 im2)
        ) id id q q'

instance Monoid (RenderQueue α) where
  mempty = RenderQueue M.empty

-- fromList :: [(RenderPacket, a)] ⊸ RenderQueue a
-- fromList = foldr (uncurry insert) mempty
-- fromList = foldr ((<>) . (`insert` mempty)) mempty :)
-- {-# INLINE fromList #-}

-- | ... We assume there are no two pipelines with the exact same shaders, so
-- we can use TypeRep π to differentiate between them
newtype PipelineKey π p = UnsafePipelineKey TypeRep

newtype MaterialKey π p m = UnsafeMaterialKey (Unique, TypeRep)

newtype MeshKey π p ma me v = UnsafeMeshKey (Unique, Unique, TypeRep) -- (mesh, mat, pipeline)

-- | Inserts a pipeline in a render queue, and returns a pipeline key indexing
-- into that render queue.
insertPipeline :: forall π p a. (Typeable π, CompatiblePipeline p π)
               => RenderPipeline π p
                ⊸ RenderQueue a
                ⊸ (RenderQueue a, Ur (PipelineKey π p))
insertPipeline 
  -- Map.insert isn't linear still, so...
  = Unsafe.toLinear2 \pipeline (RenderQueue q) ->
    let pkey = typeRep (Proxy @π)
        rq'  = M.insertWith
                  (\_ _ -> error "Inserting a duplicate pipeline??")
                  pkey
                  (Some2 pipeline, M.empty)
                  q
     in (RenderQueue rq', Ur $ UnsafePipelineKey pkey)

insertMaterial :: forall π p m a. CompatibleMaterial m π
               => PipelineKey π p   -- ^ Key for pipeline in this render queue, on which this material is defined
               -> Material m
                ⊸ RenderQueue a
                ⊸ (RenderQueue a, Ur (MaterialKey π p m))
insertMaterial (UnsafePipelineKey pkey)
  = Unsafe.toLinear2 \mat0 (RenderQueue q) -> -- unsafe bc of map insert
    let (Ur muid,mat1) = materialUID mat0
        rq' =
          M.alter
            (\case
              Nothing -> error "pipeline not found!"
              Just (p, mats) -> Just $
                (p, M.insertWith (\_ _ -> error "Inserting a duplicate material??")
                      muid
                      (Some mat1, [])
                      mats)
            )
            pkey
            q
     in (RenderQueue rq', Ur $ UnsafeMaterialKey (muid, pkey))

insertMesh :: forall π p ma me vs a
            . Compatible vs me ma p π
           => MaterialKey π p ma
           -> Mesh vs me
            ⊸ RenderQueue ()
            -- there might be more than one mesh with the same key in the
            -- render queue (if multiple instances of the same meshes are added?)
            -- although we might want to figure out a better way of re-using the same mesh multiple times... (big performance opportunities? batch mesh instancing and stuff)
            ⊸ (RenderQueue (), Ur (MeshKey π p ma me vs))
insertMesh (UnsafeMaterialKey (mkey, pkey))
  = Unsafe.toLinear2 \mesh0 (RenderQueue q) -> -- unsafe bc of map insert
    let (Ur meid, mesh) = meshId mesh0
        rq = M.alter (\case
                Nothing -> error "pipeline not found!"
                Just (p, mats) -> Just $
                  (p, M.alter (\case
                          Nothing -> error "material not found??"
                          Just (mat, meshes) -> Just $
                            (mat, (Some2 mesh, ()):meshes)
                          )
                        mkey
                        mats
                  )
              )
              pkey
              q
     in (RenderQueue rq, Ur $ UnsafeMeshKey (meid, mkey, pkey))


freeRenderQueue :: RenderQueue ()
                 ⊸ Renderer ()
freeRenderQueue (RenderQueue rq) = Linear.do
  -- For every pipeline
  pipesunit <- DL.traverse (\(Some2 @RenderPipeline @π @bs pipeline, materials) -> enterD "Freeing pipeline" Linear.do

    -- For every material...
    matsunits <- DL.traverse (\(Some @Material @ms material, meshes) -> enterD "Freeing material" Linear.do

      -- For every mesh...
      meshunits <- DL.traverse (\(Some2 @Mesh @ts mesh, ()) -> enterD "Freeing mesh" $
                                                              freeMesh mesh) meshes
      freeMaterial material
      pure (consume meshunits)

      ) materials

    () <- pure (consume matsunits)
    destroyRenderPipeline pipeline
    ) rq
  pure (consume pipesunit)

