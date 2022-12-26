{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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

import GHC.TypeLits
import GHC.Records
import Data.Proxy
import Data.Kind
import Data.Word
import Data.Bits
import Apecs (Component, Storage, Map, Has, getStore, SystemT(..), asks)
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
  RenderPacket :: ∀ α β. Compatible α β => Mesh -> Material α -> RenderPipeline β -> RenderKey -> RenderPacket

-- | TODO: A better Eq instance, this instance is not very faithful, it simply compares render keys.
-- Render keys only differentiate the render context, not the render packet itself.
-- This instance fullfills the purpose of rendering all packets with the same context in a row in the render queue
instance Eq RenderPacket where
  (==) (RenderPacket _ _ _ k1) (RenderPacket _ _ _ k2) = k1 == k2

instance Ord RenderPacket where
  compare (RenderPacket _ _ _ k1) (RenderPacket _ _ _ k2) = compare k1 k2

{-|
   'Compatible' validates at the type level that the mesh and material are
   compatible with the render pipeline.
 -}
type family Compatible xs ys :: Constraint where
  Compatible _ _ = ()


instance Component RenderPacket where
  type Storage RenderPacket = Map RenderPacket

instance (Monad m, HasField "renderPackets" w (Storage RenderPacket)) => Has w m RenderPacket where
  getStore = SystemT (asks (.renderPackets))

-- TODO: Each render packet is then assigned with an ID and sorted in an optimal draw order.
-- Alternative: Meshes, Materials and RenderPipelines have an Ord instance and we make a 3-layer map

-- | Render packet wrapper that creates the key identifier.
renderPacket :: ∀ α β. Compatible α β => Mesh -> Material α -> RenderPipeline β -> RenderPacket
renderPacket mesh material pipeline = RenderPacket mesh material pipeline (makeKey mesh material pipeline)

{-
Note [Render Packet Key]
~~~~~~~~~~~~~~~~~~~~~~~~

The idea is to assign a key to each renderable entity and then do a simple sort
by key. This key must take into account GPU state changes amonsgt possibly other things.

Our key is 64 bits long:
* Nothing yet, eventually viewport related stuff, translucency
* 4bits for the pipeline id
* 32bits for the material id
* Nothing yet, eventually depth sorting

Current view of the key:

[ padding | 4 bits: Pipeline | 32 bits: Material ]


Note [Material Key]
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
-}

-- | See Note [Render Packet Key]
type RenderKey = Word64

-- | Split a render key into the pipeline and material identifier.
-- See Note [Render Packet Key] and [Material Key]
splitKey :: RenderKey -> (Word64, Word64)
splitKey k = (k .&. 0xf00000000, k .&. 0xffffffff)

makeKey :: ∀ α β. Mesh -> Material α -> RenderPipeline β -> RenderKey
makeKey _mesh _material _pipeline = fromInteger $ natVal $ Proxy @(RenderKeyNat α β ())

-- | Compute the render key at the type level based on the type level information of the mesh, materials and pipeline.
type family RenderKeyNat mesh material pipeline :: Nat where
  RenderKeyNat _ _ _ = 0

type family MeshKey     mesh     :: Nat where
  MeshKey _ = 0

type family MaterialKey material :: Nat where
  MaterialKey _ = 0

type family PipelineKey pipeline :: Nat where
  PipelineKey _ = 0

