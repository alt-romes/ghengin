{-# LANGUAGE ViewPatterns #-}
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
module Ghengin.Render.Queue where

import qualified Prelude
import Prelude.Linear hiding (insert)
import Data.Unrestricted.Linear as Linear
import Control.Functor.Linear as Linear
import qualified Unsafe.Linear as Unsafe
import Data.Unique
import Data.Typeable
import Data.Map (Map)
import qualified Data.Map as M

import Ghengin.Core.Render.Packet
import Ghengin.Core.Mesh
-- import Ghengin.Utils
import Ghengin.Core.Material hiding (material)

newtype RenderQueue a = RenderQueue (Map TypeRep (SomePipelineRef, Map Unique (SomeMaterialRef, [(Some Mesh, a)])))
  deriving (Prelude.Functor)

data SomePipelineRef = ∀ π α. SomePipelineRef (Ref (RenderPipeline π α))
data SomeMaterialRef = ∀ α. SomeMaterialRef (Ref (Material α))

instance Prelude.Semigroup (RenderQueue α) where
  (<>) (RenderQueue q) (RenderQueue q') =
    RenderQueue $
      M.mergeWithKey
        (\_ (p1, im1) (_p2, im2) -> -- (p1 should be the same as p2)
          Prelude.pure
            (p1, M.mergeWithKey
                (\_ (m1,meshes1) (_m2, meshes2) -> -- (m1 should be the same as m2)
                  Prelude.pure (m1, meshes1 Prelude.<> meshes2)
                ) id id im1 im2)
        ) id id q q'

instance Prelude.Monoid (RenderQueue α) where
  mempty = RenderQueue Prelude.mempty

fromList :: [(RenderPacket, a)] -> RenderQueue a
fromList = Prelude.foldr (Prelude.uncurry insert) Prelude.mempty
-- fromList = foldr ((<>) . (`insert` mempty)) mempty :)
{-# INLINE fromList #-}


insert :: RenderPacket -> α -> RenderQueue α -> RenderQueue α
insert (RenderPacket @μ @π mesh material pipeline (pkey, mkey)) x (RenderQueue q) =
  RenderQueue $
    M.insertWith
        (\(p1, im1) (_p2, im2) -> -- (p1 should be the same as p2)
            (p1, M.mergeWithKey
                (\_ (m1,meshes1) (_m2, meshes2) -> -- (m1 should be the same as m2)
                  Prelude.pure (m1, meshes1 Prelude.<> meshes2)
                ) id id im1 im2))
        pkey
        (SomePipelineRef pipeline, M.insert mkey (SomeMaterialRef material, [(Some mesh, x)]) Prelude.mempty)
        q

-- | Traverse the render queue with a function for each different occasion:
--
-- * New render pipeline (i.e. one that we haven't seen/bound previously this frame)
--    * At this stage, a render pass will be initiated
-- * New material for the previously seen pipeline
-- * New mesh for the render packet using the previously seen material
--
--- It's this generic because used for drawing and for freeing all the meshes :)
--
-- This could be possibly much simpler if we didn't need to access it from
-- Ghengin i.e. we can't have the references to things like pipelines and
-- materials in Apecs, but we should have them rather elsewhere (TODO).
traverseRenderQueue :: (Linear.Monad μ, Linear.Monad μ')
                    => RenderQueue α -- ^ The render queue
                    -> (SomePipelineRef -> μ' () -> μ ()) -- ^ The initial context lifting from m to m' for the inner functions
                    -> (SomePipelineRef -> μ' SomePipeline) -- ^ The pipeline changed (nothing should be rendered) (return the pipeline fetched)
                    -> (SomePipeline -> SomeMaterialRef -> μ' ()) -- ^ The material changed (nothing should be rendered)
                    -> (SomePipeline -> Some Mesh %1 -> α %1 -> μ' ()) -- ^ The mesh to render
                    -> μ' () -- ^ A command at the end of each render pass
                    -> μ ()
traverseRenderQueue (RenderQueue q) ini f g h finally =
  () <$ runUrT (Prelude.traverse (\(pp,mts) -> UrT $
    move <$> ini pp Linear.do -- init the context
      Ur smpp <- Unsafe.toLinear Ur <$> f pp -- ach unsafe, the ghengin side of linear things is pretty bad.
      Ur xs <- runUrT $ Prelude.traverse (\(mt,meshes) -> UrT $
        move <$> g smpp mt <* runUrT (Prelude.traverse (\(a,b) -> UrT $ move <$> h smpp a b) meshes)) mts
      finally -- run at the end of the context
        ) q)
{-# INLINE traverseRenderQueue #-}

instance Consumable (Map Unique ()) where
  consume = Unsafe.toLinear $ \_ -> ()

