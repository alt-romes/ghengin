{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE BlockArguments #-}
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

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Ghengin.Render.Packet
import Ghengin.Component.Mesh
import Ghengin.Component.Material hiding (material)

newtype RenderQueue a = RenderQueue (IntMap (SomePipeline, IntMap (SomeMaterial, [(Mesh, a)])))
  deriving (Functor)

instance Semigroup (RenderQueue α) where
  (<>) (RenderQueue q) (RenderQueue q') =
    RenderQueue $
      IM.mergeWithKey
        (\_ (p1, im1) (_p2, im2) -> -- (p1 should be the same as p2)
          pure
            (p1, IM.mergeWithKey
                (\_ (m1,meshes1) (_m2, meshes2) -> -- (m1 should be the same as m2)
                  pure (m1, meshes1 <> meshes2)
                ) id id im1 im2)
        ) id id q q'

instance Monoid (RenderQueue α) where
  mempty = RenderQueue mempty

data SomePipeline = ∀ α. SomePipeline (RenderPipeline α)
data SomeMaterial = ∀ α. SomeMaterial (Material α)

fromList :: [(RenderPacket, a)] -> RenderQueue a
fromList = foldr (uncurry insert) mempty
-- fromList = foldr ((<>) . (`insert` mempty)) mempty :)
{-# INLINE fromList #-}


insert :: RenderPacket -> α -> RenderQueue α -> RenderQueue α
insert (RenderPacket mesh material pipeline key) x (RenderQueue q) =
  let (fromIntegral -> pkey, fromIntegral -> mkey) = splitKey key
   in RenderQueue $
    IM.insertWith
        (\(p1, im1) (_p2, im2) -> -- (p1 should be the same as p2)
            (p1, IM.mergeWithKey
                (\_ (m1,meshes1) (_m2, meshes2) -> -- (m1 should be the same as m2)
                  pure (m1, meshes1 <> meshes2)
                ) id id im1 im2))
        pkey
        (SomePipeline pipeline, IM.insert mkey (SomeMaterial material, [(mesh, x)]) mempty)
        q

-- | Traverse the render queue with a function for each different occasion:
--
-- * New render pipeline (i.e. one that we haven't seen/bound previously this frame)
--    * At this stage, a render pass will be initiated
-- * New material for the previously seen pipeline
-- * New mesh for the render packet using the previously seen material
--
--- It's this generic because used for drawing and for freeing all the meshes :)
traverseRenderQueue :: (Monad μ, Monad μ')
                    => RenderQueue α -- ^ The render queue
                    -> (SomePipeline -> μ' () -> μ ()) -- ^ The initial context lifting from m to m' for the inner functions
                    -> (SomePipeline -> μ' β) -- ^ The pipeline changed (nothing should be rendered)
                    -> (SomePipeline -> SomeMaterial -> μ' ξ) -- ^ The material changed (nothing should be rendered)
                    -> (SomePipeline -> Mesh -> α -> μ' δ) -- ^ The mesh to render
                    -> μ' () -- ^ A command at the end of each render pass
                    -> μ ()
traverseRenderQueue (RenderQueue q) ini f g h finally =
  () <$ traverse (\(pp,mts) ->
    ini pp $ do -- init the context
      _ <- f pp
      _ <- traverse (\(mt,meshes) ->
        (g pp mt) <* traverse (uncurry (h pp)) meshes) mts
      finally -- run at the end of the context
        ) q
{-# INLINE traverseRenderQueue #-}

