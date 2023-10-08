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

import Ghengin.Core.Render.Packet
import Ghengin.Core.Mesh
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

-- | Traverse the render queue with a function for each different occasion:
--
-- * New render pipeline (i.e. one that we haven't seen/bound previously this frame)
--    * At this stage, a render pass will be initiated
-- * New material for the bound pipeline
-- * New mesh for the render packet using the bound material
--
--- It's this generic because used for drawing and for freeing all the meshes :)
--
-- This could be possibly much simpler if we didn't need to access it from
-- Ghengin i.e. we can't have the references to things like pipelines and
-- materials in Apecs, but we should have them rather elsewhere (TODO).
--
-- This is a monster of a function that I've now considered harmful, and which
-- is likely impossible to make linear while useable. The traversals simply
-- have to be inlined where they are needed (in Ghengin.Core, for rendering and
-- freeing), and it is much, much, simpler.
--
-- traverseRenderQueue :: (Linear.Monad μ, Linear.Monad μ')
--                     => RenderQueue α -- ^ The render queue
--                      ⊸ (∀ π bs. RenderPipeline π bs ⊸ μ' () -> μ (RenderPipeline π bs)) -- ^ The initial context lifting from m to m' for the inner functions
--                     -- ????what was this -> (Some2 Pipeline ⊸ μ' SomePipeline) -- ^ The pipeline changed (nothing should be rendered) (return the pipeline fetched)
--                     -> (∀ π bs ms. RenderPipeline π bs ⊸ Material ms ⊸ μ' (RenderPipeline π bs, Material ms)) -- ^ The material changed (nothing should be rendered)
--                     -> (∀ π bs τs. RenderPipeline π bs ⊸ Mesh τs ⊸ α ⊸ μ' (RenderPipeline π bs, Mesh τs)) -- ^ The mesh to render
--                     -> μ' () -- ^ A command at the end of each render pass
--                     -> μ (RenderQueue α)
-- {-# INLINE traverseRenderQueue #-}
-- traverseRenderQueue (RenderQueue q) ini g h finally = Linear.do
  -- Ouch:)
  -- Ur _ <- uur <$> DL.traverse (Unsafe.toLinear \(Some2 @RenderPipeline @π @bs pp,mts) -> Linear.do
  --   Ur _ <- uur <$> ini pp Linear.do -- init the context
  --     Ur _ <- uur <$> DL.traverse (Unsafe.toLinear \(Some @Material @ms mt,meshes) -> Linear.do
  --       Ur _ <- uur <$> DL.traverse (Unsafe.toLinear \(Some @Mesh @ts a,b) -> Unsafe.toLinear (\(!_) -> ()) <$> h @π @bs @ts pp a b) meshes
  --       Unsafe.toLinear (\(!_) -> ()) <$> g @π @bs @ms pp mt) mts
  --     finally -- run at the end of the context
  --   pure ()) q
  -- pure ()
 -- where
  --  uur :: a ⊸ Ur a
  --  uur = Unsafe.toLinear Ur

--     DL.traverse (\(pp,mts) ->
--       ini pp Linear.do -- init the context
--         DL.traverse (\(mt,meshes) ->
--           g pp mt <* DL.traverse (\(a,b) -> h pp a b meshes)) mts
--         finally -- run at the end of the context
--           ) q

