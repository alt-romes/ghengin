{-# LANGUAGE ViewPatterns #-}
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

-- import Data.Word
-- import Data.Bits
-- import qualified Data.Map as M
-- import Data.Ord
import Control.Monad.IO.Class
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Ghengin.Vulkan.Command
import Ghengin.Render.Packet
import Ghengin.Component.Mesh
import Ghengin.Component.Material hiding (SomeMaterial)

newtype RenderQueue a = RenderQueue (IntMap (SomePipeline, IntMap (SomeMaterial, [(Mesh, a)])))
  -- deriving Show

data SomePipeline = ∀ α. SomePipeline (RenderPipeline α)
data SomeMaterial = ∀ α. SomeMaterial (Material α)

makeRenderQueue :: [(RenderPacket, a)] -> RenderQueue a
makeRenderQueue =
  RenderQueue .
    foldr
      (\(RenderPacket mesh material pipeline key, x) ->
        let (fromIntegral -> pkey, fromIntegral -> mkey) = splitKey key
         in
          IM.insertWith
            (\(p1, im1) (_p2, im2) -> -- (p1 should be the same as p2)
              (p1, IM.mergeWithKey
                      (\_ (m1,meshes1) (_m2, meshes2) -> -- (m1 should be the same as m2)
                        pure (m1, meshes1 <> meshes2)
                      ) id id im1 im2)
            )
            pkey
            (SomePipeline pipeline, IM.insert mkey (SomeMaterial material, [(mesh, x)]) mempty)
      ) mempty
{-# INLINE makeRenderQueue #-}

-- | Traverse the render queue with a function for each different occasion:
--
-- * New render pipeline (i.e. one that we haven't seen/bound previously this frame)
-- * New material for the previously seen pipeline
-- * New mesh for the render packet using the previously seen material
traverseRenderQueue :: MonadIO m
                    => RenderQueue a
                    -> (SomePipeline -> Command m) -- ^ The pipeline changed (nothing should be rendered)
                    -> (SomePipeline -> SomeMaterial -> RenderPassCmd m) -- ^ The material changed (nothing should be rendered)
                    -> (SomePipeline -> Mesh -> a -> RenderPassCmd m) -- ^ The mesh to render
                    -> Command m
traverseRenderQueue (RenderQueue q) f g h =
  () <$ traverse (\(pp,mts) ->
    f pp *> traverse (\(mt,meshes) ->
      unsafeCmdFromRenderPassCmd (g pp mt) *> traverse (unsafeCmdFromRenderPassCmd . uncurry (h pp)) meshes) mts) q
{-# INLINE traverseRenderQueue #-}



