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
import Ghengin.Core.Mesh
import Ghengin.Core.Log
import Ghengin.Core.Type.Utils (Some2(..))
import Ghengin.Core.Material hiding (material)


newtype RenderQueue a = RenderQueue (Map TypeRep (Some2 RenderPipeline, Map Unique (Some Material, [(Some Mesh, a)])))
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

-- | ... We assume there are no two pipelines with the exact same shaders
newtype PipelineKey π p = UnsafePipelineKey TypeRep

-- | Inserts a pipeline in a render queue, and returns a pipeline key indexing
-- into that render queue.
insertPipeline :: forall π p α. Typeable π
               => RenderPipeline π p
                ⊸ RenderQueue α
                ⊸ (RenderQueue α, Ur (PipelineKey π p))
insertPipeline 
  -- Map.insert isn't linear still, so...
  = Unsafe.toLinear2 \pipeline (RenderQueue q) ->
    let pkey = typeRep (Proxy @π)
        rq'  = RenderQueue $
                 M.insertWith
                    (\_ _ -> error "Inserting a duplicate pipeline??")
                    pkey
                    (Some2 pipeline, M.empty)
                    q
     in (rq', Ur $ UnsafePipelineKey pkey)

-- TODO: Rather, to create a renderpacket we need a render queue, since we
-- extract the render key from the render queue and the references into the
-- render packet
-- TODO
-- insert :: RenderPacket ⊸ α %p -> RenderQueue α ⊸ RenderQueue α
-- insert (RenderPacket @μ @π mesh material pipeline (pkey, mkey)) x (RenderQueue q) =
--   RenderQueue $
--     M.insertWith
--         (\(p1, im1) (_p2, im2) -> -- (p1 should be the same as p2)
--             (p1, M.mergeWithKey
--                 (\_ (m1,meshes1) (_m2, meshes2) -> -- (m1 should be the same as m2)
--                   Prelude.pure (m1, meshes1 Prelude.<> meshes2)
--                 ) id id im1 im2))
--         pkey
--         (Some2 pipeline, M.insert mkey (Some material, [(Some mesh, x)]) M.empty)
--         q


freeRenderQueue :: RenderQueue ()
                 ⊸ Renderer ()
freeRenderQueue (RenderQueue rq) = Linear.do
  -- For every pipeline
  pipesunit <- DL.traverse (\(Some2 @RenderPipeline @π @bs pipeline, materials) -> enterD "Freeing pipeline" Linear.do

    -- For every material...
    matsunits <- DL.traverse (\(Some @Material @ms material, meshes) -> enterD "Freeing material" Linear.do

      -- For every mesh...
      meshunits <- DL.traverse (\(Some @Mesh @ts mesh, ()) -> enterD "Freeing mesh" $
                                                              freeMesh mesh) meshes
      freeMaterial material
      pure (consume meshunits)

      ) materials

    () <- pure (consume matsunits)
    destroyRenderPipeline pipeline
    ) rq
  pure (consume pipesunit)

