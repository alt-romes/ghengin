module Ghengin.Scene where

{-
Note [Scene traversals are not part of Core]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can represent a scene traversal completely outside of Core, so that's what we do, here's how:

* All objects of the scene are represented in a tree-like data structure (the scene-graph), and
each node has a transform associated to them
  * If using ECS, we can represent the objects in a store and associate them
  with a parent object too, and then we construct the scene-graph on the fly.
* Every frame, traverse the scene-graph and update the transform of the object
* The transform of the object is a render-property of the mesh of the object
  * It is automatically pushed to the GPU
* The shader pipeline rendering those meshes must apply a world-to-viewport
  transformation using the pushed transform

-}
