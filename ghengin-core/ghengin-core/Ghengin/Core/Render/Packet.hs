{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Packets are registered through the RenderQueues, see that module instead
-- This only ever be needed when writing internals of the engine
module Ghengin.Core.Render.Packet () where

{-|

Note [Renderable entities]
~~~~~~~~~~~~~~~~~~~~~~~~~~

A renderable entity is any entity with a RenderPacket component which validated
the mesh, material and pipeline components. A 'RenderPacket' has a unique key
which is used to sort all renderable entities in a way which minimizes GPU
state changes.

A renderable entity is defined by
(1) Its mesh (vertex data)
(2) Its material (set of properties that define the appearance; compatible with the pipeline)
(3) Its render pipeline (the shader programs)

The vertex data (1) is bound once per draw call for each entity.

Every material (2) is bound (hopefully) just once per draw phase. Binding a
material is binding the descriptor set #1 with all the descriptors
corresponding to material properties (which are validated to be compatible with
the pipeline when the 'RenderPacket' is constructed). The same material binding
is shared across all entities with that material

Every pipeline (3) is also bound (hopefully) just once per draw phase. The
binding is shared across all entities using that pipeline, regardless of the
material.

For every renderable entity, the model matrix of the entity is pushed as a
PushConstant. Also, after binding a pipeline (can it be at the start of the
draw phase?), the camera projection and view matrix are bound to the descriptor
set #0.

We require a Typeable constraint on every Material used to create a
RenderPacket so that we can later pattern match on the render packet and figure
out which render packet it actually is. Remember that we'll have many render
packets constructed with existential materials, and we'll want to distinguish
them during runtime to be able to do things, such as updates, to the material

The missing bits:

  We should be able to (at least) bind arbitrary data at pipeline binding time to
  descriptor set #0, and bind entity dependent data right before entity draw time
  to descriptor set #2. However, the design here is not clear enough, and without
  a driving example it's harder for now.

The render packet pipeline key is actually its typeable instance because a pipeline is uniquely identified by its type.
Unfortunately, a material is not uniquely identified by its type - the same material type constructed with different values is a different material.
The material unique key is created from a global counter. Whenever a material is
created through 'material', a unique identifier is created for that material.

 -}
