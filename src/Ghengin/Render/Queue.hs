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


A map is probably a better representation for the render queue, however we will
have entities with duplicate render keys, so for now we'll use a simple list
which is sorted. Eventually, inserting elements directly into a map and then
traversing it *might* be better.
-}
module Ghengin.Render.Queue where

-- import Data.Word
-- import Data.Bits
-- import qualified Data.Map as M
-- import Data.Ord
import qualified Data.List as L
import Ghengin.Render.Packet
import {-# SOURCE #-} Ghengin (Ghengin)

newtype RenderQueue = RenderQueue [RenderPacket]
  -- deriving Show

makeRenderQueue :: [RenderPacket] -> RenderQueue
makeRenderQueue = RenderQueue . L.sort -- On Down?
{-# INLINE makeRenderQueue #-}

traverseRenderQueue :: RenderQueue
                    -> (RenderPacket -> Ghengin w a)
                    -> Ghengin w [a]
traverseRenderQueue (RenderQueue q) f = traverse f q



