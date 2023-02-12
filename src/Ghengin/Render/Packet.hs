{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Ghengin.Render.Packet
  ( module Ghengin.Render.Packet
  , module Ghengin.Render.Pipeline
  ) where

import Apecs (Component, Storage, Map)
import Data.Typeable
import Data.Unique
import Ghengin.Component.Material hiding (material)
import Ghengin.Component.Mesh
import Ghengin.Core.Type.Compatible
import Ghengin.Render.Pipeline
import Ghengin.Utils

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
data RenderPacket where
  -- TODO:
  --  * CompatibleMaterial mesh mat pipeline
  --  * Mesh parametrized over type that is also validated against pipeline
  --  * Descriptor set #2 and #0 additional data binding?
  RenderPacket :: ∀ π ξ β α. (Compatible α β ξ π, Typeable α, Typeable β, Typeable ξ) => Mesh α -> Material β -> RenderPipeline ξ π -> RenderKey -> RenderPacket

-- | TODO: A better Eq instance, this instance is not very faithful, it simply compares render keys.
-- Render keys only differentiate the render context, not the render packet itself.
-- This instance fullfills the purpose of rendering all packets with the same context in a row in the render queue
instance Eq RenderPacket where
  (==) (RenderPacket _ _ _ k1) (RenderPacket _ _ _ k2) = k1 == k2

instance Ord RenderPacket where
  compare (RenderPacket _ _ _ k1) (RenderPacket _ _ _ k2) = compare k1 k2

{-|

Note [Pipeline compatible materials]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A material is compatible with a pipeline if the material bindings are
compatible with the descriptor set #1 descriptors in the shaders.

For a material binding to be compatible with a descriptor, the type of the
binding and of the descriptor must have the same size through FIR.Layout.SizeOf
described in bytes.


 -}

-- BIG:TODO: Cache around this Map storage
instance Component RenderPacket where
  type Storage RenderPacket = Map RenderPacket

-- TODO: Each render packet is then assigned with an ID and sorted in an optimal draw order.
-- Alternative: Meshes, Materials and RenderPipelines have an Ord instance and we make a 3-layer map

-- | Render packet wrapper that creates the key identifier.
renderPacket :: ∀ π ξ β α μ. (Compatible α β ξ π, Typeable α, Typeable β, Typeable ξ, MonadIO μ) => Mesh α -> Material β -> RenderPipeline ξ π -> μ RenderPacket
renderPacket mesh material pipeline = do
  incRefCount mesh
  pure $ RenderPacket mesh material pipeline (typeRep (Proxy @β), getMaterialUID material)

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

[ 28 bits: Padding | 4 bits: Pipeline | 32 bits: Material ]


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

We don't need to consider the render pipeline properties because the info is already a unique identifier of the render pipeline
-}

-- | See Note [Render Packet Key]
-- TODO: Update note render packet key and material key
type RenderKey = (TypeRep, Unique)


------------------------------------------------------------
-- | If matching fails, the other constraint is passed
-- type family TryMatch (t :: Type) (t' :: Type) (c :: Constraint) :: Constraint where
--   -- Does a Material Texture2D binding match a Texture2D shader binding?
--   TryMatch
--     Texture2D
--     (FIR.Image
--       ('FIR.Properties
--         'FIR.FloatingPointCoordinates
--         Float
--         'SPIRV.TwoD
--         ('Just 'SPIRV.NotDepthImage)
--         'SPIRV.NonArrayed
--         'SPIRV.SingleSampled
--         'SPIRV.Sampled
--         ('Just
--             ('SPIRV.ImageFormat
--                 ('SPIRV.Integer 'SPIRV.Normalised 'SPIRV.ScalarTy.Unsigned)
--                 '[8,8,8,8])))) _ = ()
--   TryMatch _ _ c = c
------------------------------------------------------------

