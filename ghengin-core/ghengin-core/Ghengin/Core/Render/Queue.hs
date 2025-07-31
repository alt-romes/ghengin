{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
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
module Ghengin.Core.Render.Queue
  -- TODO: Dont' export things like editAtPipelineKey
  where

import qualified Prelude
import Prelude.Linear hiding (insert)
import Control.Functor.Linear as Linear
import qualified Data.Functor.Linear as DL
import qualified Unsafe.Linear as Unsafe
import Data.Unique
import Data.Typeable
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Map.Linear as ML
import qualified FIR.Pipeline
import Data.Kind

import Ghengin.Core.Renderer.Kernel
import Ghengin.Core.Render.Pipeline
import Ghengin.Core.Type.Compatible
import Ghengin.Core.Mesh
import Ghengin.Core.Log
import Ghengin.Core.Type.Utils (Some2(..))
import Ghengin.Core.Material hiding (material)

newtype RenderQueue a = RenderQueue (PipelineMap (MaterialMap (MeshMap a)))
  deriving (Prelude.Functor)

type PipelineMap a = Map Unique (Some2 RenderPipeline, a)
type MaterialMap a = Map Unique (Some Material, a)
type MeshMap a = Map Unique [(Some2 Mesh, a)]
-- ^ We need a list of meshes for a given mesh key because we may have more
-- than one of the same mesh in the render queue.

instance Semigroup (RenderQueue α) where
  (<>) = Unsafe.toLinear2 \(RenderQueue q) (RenderQueue q') ->
    RenderQueue $
      M.mergeWithKey
        (\_ (p1, im1) (_p2, im2) -> -- (p1 should be the same as p2)
          Just
            (p1, M.mergeWithKey
                (\_ (m1,meshes1) (_m2, meshes2) -> -- (m1 should be the same as m2)
                  Just (m1, M.unionWith (\a b -> a ++ b) meshes1 meshes2)
                ) id id im1 im2)
        ) id id q q'

instance Monoid (RenderQueue α) where
  mempty = RenderQueue M.empty

-- fromList :: [(RenderPacket, a)] ⊸ RenderQueue a
-- fromList = foldr (uncurry insert) mempty
-- fromList = foldr ((<>) . (`insert` mempty)) mempty :)
-- {-# INLINE fromList #-}

type PipelineKey :: FIR.Pipeline.PipelineInfo -> [Type] -> Type
data PipelineKey π p = UnsafePipelineKey !Unique

type MaterialKey :: FIR.Pipeline.PipelineInfo -> [Type] -> [Type] -> Type
data MaterialKey π p ma = UnsafeMaterialKey !Unique !(PipelineKey π p)

type MeshKey :: FIR.Pipeline.PipelineInfo -> [Type] -> [Type] -> [Type] -> [Type] -> Type
data MeshKey π p ma vertexAttributes meshProperties = UnsafeMeshKey !Unique !(MaterialKey π p ma)

--------------------------------------------------------------------------------

-- | Inserts a pipeline in a render queue, and returns a pipeline key indexing
-- into that render queue.
--
-- :: NOTE ::
-- All the render pipelines in the render queue must share the same render pass.
-- If you need to use different render passes, read the definition of "render"
-- and define a similar custom render function yourself.
insertPipeline :: forall π p a. (Typeable π, CompatiblePipeline p π)
               => RenderPipeline π p
                ⊸ RenderQueue a
                ⊸ (RenderQueue a, Ur (PipelineKey π p))
insertPipeline 
  -- Map.insert isn't linear still, so...
  = Unsafe.toLinear2 \pipeline0 (RenderQueue q) ->
    let (Ur pkey, pipeline1) = pipelineUID pipeline0
        rq'  = M.insertWith
                  (\_ _ -> error "Inserting a duplicate pipeline??")
                  pkey
                  (Some2 pipeline1, M.empty)
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
                      (Some mat1, M.empty)
                      mats)
            )
            pkey
            q
     in (RenderQueue rq', Ur $ UnsafeMaterialKey muid (UnsafePipelineKey pkey))

insertMesh :: forall π p ma me vs
            . Compatible vs me ma p π
           => MaterialKey π p ma
           -> Mesh vs me
            ⊸ RenderQueue ()
            -- there might be more than one mesh with the same key in the
            -- render queue (if multiple instances of the same meshesnewtype are added?)
            -- although we might want to figure out a better way of re-using the same mesh multiple times... (big performance opportunities? batch mesh instancing and stuff)
            ⊸ (RenderQueue (), Ur (MeshKey π p ma vs me))
insertMesh (UnsafeMaterialKey mkey (UnsafePipelineKey pkey))
  = Unsafe.toLinear2 \mesh0 (RenderQueue q) -> -- unsafe bc of map insert
    let (Ur meid, mesh) = meshId mesh0
        rq = M.alter (\case
                Nothing -> error "pipeline not found!"
                Just (p, mats) -> Just $
                  (p, M.alter (\case
                          Nothing -> error "material not found??"
                          Just (mat, meshes) -> Just $
                            (mat, M.insertWith (\a b -> a ++ b) meid [(Some2 mesh, ())] meshes)
                          )
                        mkey
                        mats
                  )
              )
              pkey
              q
     in (RenderQueue rq, Ur $ UnsafeMeshKey meid (UnsafeMaterialKey mkey (UnsafePipelineKey pkey)))

--------------------------------------------------------------------------------

-- | Edit a pipeline in a render queue typically using the @propertyAt@ lens
editPipeline :: PipelineKey π p
             -> RenderQueue ()
              ⊸ (RenderPipeline π p ⊸ Renderer (RenderPipeline π p))
              ⊸ Renderer (RenderQueue ())
editPipeline pkey rq edit = editAtPipelineKey pkey rq (\(a,b) -> (,b) <$> edit a)

-- | Edit a material in a render queue typically using the @propertyAt@ lens
editMaterial :: MaterialKey π p ma
             -> RenderQueue ()
              ⊸ (Material ma ⊸ Renderer (Material ma))
              ⊸ Renderer (RenderQueue ())
editMaterial mkey rq edit =
  editAtMaterialKey mkey rq (\(a,b) -> (,b) <$> edit a)

-- | Edit all the meshes matching this mesh key in a render queue typically
-- using the @propertyAt@ lens.
-- Using this is simpler than "editAtMeshesKey" since we don't have to worry about additional data (which is currently kind of fixed to unit)
editMeshes :: MeshKey π p ma va me
           -> RenderQueue ()
            ⊸ ([Mesh va me] ⊸ Renderer [Mesh va me])
            ⊸ Renderer (RenderQueue ())
editMeshes key rq edit =
  editAtMeshesKey key rq (\ls -> map (,()) <$> edit (map (\(x,()) -> x) ls))

--------------------------------------------------------------------------------

-- | Edit all the meshes matching this mesh key in a render queue typically using the @propertyAt@ lens
-- ToDo: Perhaps we should have a key per mesh rather than a key for a list of meshes
editAtMeshesKey :: MeshKey π p ma va me
                -> RenderQueue ()
                 ⊸ ([(Mesh va me, ())] ⊸ Renderer [(Mesh va me, ())])
                 ⊸ Renderer (RenderQueue ())
editAtMeshesKey (UnsafeMeshKey meid mkey) rq edit =
  editAtMaterialKey mkey rq \(mat,meshmap) -> (mat,) <$>
    ML.alterF
      (\case Nothing -> Unsafe.toLinear (\_ -> error "mesh key not in rq") edit
             Just meshes ->
               -- Key guarantees unsafe coerce is safe, since this is the "right" material for that material type
               Just . map (\(x,y) -> (Some2 x, y)) <$> edit (map (\(Some2 m, a) -> (Unsafe.coerce m, a)) meshes)
      ) meid meshmap

editAtMaterialKey :: MaterialKey π p ma
                  -> RenderQueue ()
                   ⊸ ((Material ma, MeshMap ()) ⊸ Renderer (Material ma, MeshMap ()))
                   ⊸ Renderer (RenderQueue ())
editAtMaterialKey (UnsafeMaterialKey mkey pkey) rq edit = do
  editAtPipelineKey pkey rq (\(pip,mats) -> (pip,) <$> ML.alterF
    (\case Nothing -> Unsafe.toLinear (\_ -> error "material key not in rq") edit
           Just (Some mat, meshes) ->
             -- Key guarantees unsafe coerce is safe, since this is the "right" material for that material type
             (\(x, ms) -> Just (Some x, ms)) <$> edit (Unsafe.coerce mat, meshes)
    ) mkey mats)

editAtPipelineKey
  :: PipelineKey π p
  -> RenderQueue ()
   ⊸ ((RenderPipeline π p, MaterialMap (MeshMap ())) ⊸ Renderer (RenderPipeline π p, MaterialMap (MeshMap ())))
   ⊸ Renderer (RenderQueue ())
editAtPipelineKey (UnsafePipelineKey pkey) (RenderQueue q) edit = RenderQueue <$>
  ML.alterF (\case Nothing -> Unsafe.toLinear (\_ -> error "pipeline key not in rq") edit
                   Just (Some2 rp, materials) ->
                     -- Key guarantees the type of the pipeline at that key is the same,
                     -- so this is safe
                     (\(x, ms) -> Just (Some2 x, ms)) <$> edit (Unsafe.coerce rp, materials)
            ) pkey q

--------------------------------------------------------------------------------

freeRenderQueue :: RenderQueue ()
                 ⊸ Renderer ()
freeRenderQueue (RenderQueue rq) = Linear.do
  -- For every pipeline
  pipesunit <- DL.traverse (\(Some2 @RenderPipeline @π @bs pipeline, materials) -> enterD "Freeing pipeline" Linear.do

    -- For every material...
    matsunits <- DL.traverse (\(Some @Material @ms material, meshes) -> enterD "Freeing material" Linear.do

      -- For all meshes...
      meshunits <- DL.traverse (\meshes' -> Linear.do
        -- For every mesh with the same id...
        meshunits' <- DL.traverse (\(Some2 @Mesh @ts mesh, ()) -> enterD "Freeing mesh" Linear.do
          freeMesh mesh
          ) meshes' -- a list of meshes with the same id
        pure (consume meshunits')
        ) meshes

      freeMaterial material
      pure (consume meshunits)

      ) materials

    () <- pure (consume matsunits)
    destroyRenderPipeline pipeline
    ) rq
  pure (consume pipesunit)

--------------------------------------------------------------------------------
