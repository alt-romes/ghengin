{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-} -- instance Has w m RenderPacket
module Ghengin.Render.Packet
  ( module Ghengin.Render.Packet
  , module Ghengin.Render.Pipeline
  ) where

import Ghengin.Render.Pipeline
import Ghengin.Component.Material
import Ghengin.Component.Mesh

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

The missing bits:

We should be able to (at least) bind arbitrary data at pipeline binding time to
descriptor set #0, and bind entity dependent data right before entity draw time
to descriptor set #2. However, the design here is not clear enough, and without
a driving example it's harder for now.


 -}

data RenderPacket where
  -- TODO:
  --  * CompatibleMaterial mesh mat pipeline
  --  * Mesh parametrized over type that is also validated against pipeline
  --  * Descriptor set #2 and #0 additional data binding?
  RenderPacket :: ∀ α β. Mesh -> Material α -> RenderPipeline β -> RenderPacket

-- TODO: Each render packet is then assigned with an ID and sorted in an optimal draw order.

-- data RenderPacket = forall info.
--                     RenderPacket { _renderPipeline :: RenderPipeline info
--                                  , _renderMesh     :: Mesh
--                                  , _renderMaterial :: Material
--                                  }

-- instance Component RenderPacket where
--   type Storage RenderPacket = Map RenderPacket

-- instance (Monad m, HasField "renderPackets" w (Storage RenderPacket)) => Has w m RenderPacket where
--   getStore = SystemT (asks (.renderPackets))



