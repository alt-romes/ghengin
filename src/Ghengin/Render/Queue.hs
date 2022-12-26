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

The idea is to assign a key to each renderable entity and then do a simple sort
by key. This key must take into account GPU state changes amonsgt possibly other things.

Our key is 64 bits long:
* Nothing yet, eventually viewport related stuff, translucency
* 32bits for the material id
* Nothing yet, eventually depth sorting


Note [Material ID]
~~~~~~~~~~~~~~~~~~

Every material is uniquely identifiable by a 32bit key which serves to sort
material in order of binding frequency -- giving us an optimal order to render
entities with the minimal amount of state changes

The 32bit key is composed of:
* __6 bits__ for the (shader) pipeline identifier (this allows for a maximum of
  64 different shaders, which should be enough. If more are needed, you might
  need to merge two shaders into a more generic one (a generic material shader),
  which will probably be more performant than having more shaders)
* __26 bits__ to uniquely identify the material?


-- TODO: Each render packet is then assigned with an ID and sorted in an optimal draw order.


A map is probably a better representation for the render queue, however we will
have entities with duplicate render keys, so for now we'll use a simple list
which is sorted. Eventually, inserting elements directly into a map and then
traversing it *might* be better.
-}
module Ghengin.Render.Queue where

import Data.Word
import Data.Bits
-- import qualified Data.Map as M
import Ghengin.Render.Packet

type RenderQueue = [RenderPacket]



