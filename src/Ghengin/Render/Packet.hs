{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoStarIsType #-}
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
{-# LANGUAGE UndecidableInstances #-}
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
import Ghengin.Utils

-- import FIR.Layout(Layout(..))
import Data.Type.Map
  ( Values )
import FIR.Pipeline (PipelineInfo(..))
import FIR.ProgramState
import SPIRV.Decoration (Decoration(..))

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

instance (Monad m, HasField "renderPackets" w (Storage RenderPacket)) => Has w m RenderPacket where
  getStore = SystemT (asks (.renderPackets))

-- TODO: Each render packet is then assigned with an ID and sorted in an optimal draw order.
-- Alternative: Meshes, Materials and RenderPipelines have an Ord instance and we make a 3-layer map

-- | Render packet wrapper that creates the key identifier.
renderPacket :: ∀ α β. Compatible α β => Mesh -> Material α -> RenderPipeline β -> RenderPacket
renderPacket mesh material pipeline = RenderPacket mesh material pipeline (makeKey material pipeline)

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
-}

-- | See Note [Render Packet Key]
type RenderKey = Word64

-- | Split a render key into the pipeline and material identifier.
-- See Note [Render Packet Key] and [Material Key]
splitKey :: RenderKey -> (Word64, Word64)
splitKey k = (k .&. 0xf00000000, k .&. 0xffffffff)

makeKey :: ∀ α β. (KnownNat (MaterialKey α)) => Material α -> RenderPipeline β -> RenderKey
makeKey _material _pipeline = fromInteger $ natVal $ Proxy @(RenderKeyNat α β)

-- | Compute the render key at the type level based on the type level information of the mesh, materials and pipeline.
type family RenderKeyNat material pipeline :: Nat where
  RenderKeyNat m p = MaterialKey m + PipelineKey p

type family MaterialKey material :: Nat where
  MaterialKey _ = 0

type family PipelineKey pipeline :: Nat where
  PipelineKey _ = 0

-- class Compatible (as :: [Type]) (bs :: PipelineInfo)
-- instance Compatible' (Zip (NumbersFromTo 0 (Length as)) (Reverse as '[])) bs
--   => Compatible as bs

-- | 'Compatible' validates at the type level that the mesh and material are
-- compatible with the render pipeline. See Note [Pipeline compatible materials].
type family Compatible (xs :: [Type]) (ys :: PipelineInfo) :: Constraint where
  -- Compatible as bs = Assert (Matches () ()) (Compatible' as bs (NumberOfBindings as))
  -- Reverse because the material bindings are reversed (the last element is the binding #0)
  Compatible as bs = ( -- Length as ~ Length (Length (DSetBindings 1))
                     -- ,
                     Compatible' (Zip (NumbersFromTo 0 (Length as)) (Reverse as '[])) bs)

-- TODO: If I "return" a type equality constraint can I still have nice type
-- error messages? perhaps through my own type equality constraint?
type family Compatible' (xs :: [(Nat,Type)]) (ys :: PipelineInfo) :: Constraint where
  Compatible' '[] _ = ()
  Compatible' ('(n,x) ': xs) ys = ( Sized x
                                  , Sized (DSetBinding' 1 n ys)
                                  , Matches (SizeOf x) (SizeOf (DSetBinding' 1 n ys))
                                    (Text "Material binding #" :<>: ShowType n :<>: Text " with type " :<>: ShowType x :<>: Text " of size " :<>: ShowType (SizeOf x)
                                     :<>: Text " isn't compatible with (doesn't have the same size as) the descriptor binding #" :<>: ShowType n :<>: Text " of size " :<>: ShowType (SizeOf (DSetBinding' 1 n ys))
                                     :<>: Text " with type " :<>: ShowType (DSetBinding' 1 n ys))
                                  , (Compatible' xs ys))

type family Matches (t :: k) (t' :: k) (e :: ErrorMessage) :: Constraint where
  Matches x x _ = ()
  Matches x y e = TypeError e

-- To calculate all the bindings in a descriptor set we simply keep trying the next descriptor until there's no more.
-- type family DSetBindings (set :: Nat) (info :: PipelineInfo) :: [Type] where
--   DSetBindings n info = DSetBindings' n info 0

-- type family DSetBindings' (set :: Nat) (info :: PipelineInfo) (i :: Nat) :: [Type] where
--   DSetBindings' n info i = Maybe ('[]) ((':) DSetBindings' n info (i+1)) (DSetBinding n i info)

-- | Find descriptor set #set and binding #binding in any of the pipeline stages inputs
--
-- TODO: This assumes this order as the only valid one. At least say it so in the error message.
type family DSetBinding (set :: Nat) (binding :: Nat) (info :: PipelineInfo) :: Maybe Type where
  DSetBinding set binding (VertexInputInfo _ _ _) = 'Nothing
  DSetBinding set binding (infos `Into` '(_name, 'EntryPointInfo _ defs _)) = (FindDSetInput set binding (Values (Concat (Values defs)))) :<|>: (DSetBinding set binding infos)

type family DSetBinding' (set :: Nat) (binding :: Nat) (info :: PipelineInfo) :: Type where
  DSetBinding' set binding info = FromMaybe (TypeError (Text "Uniform [Descriptor Set #" :<>: ShowType set :<>: Text ", Binding #" :<>: ShowType binding :<>: Text "] not found in " :<>: ShowType info)) (DSetBinding set binding info)

type family FindDSetInput (set :: Nat) (binding :: Nat) (inputs :: [TLInterfaceVariable]) :: Maybe Type where
  FindDSetInput set binding '[] = 'Nothing
  FindDSetInput set binding ('( '[DescriptorSet set, Binding binding], ty) ': inputs) = 'Just ty
  FindDSetInput set binding (_ ': inputs) = FindDSetInput set binding inputs

type family Assert (b :: Bool) (e :: ErrorMessage) (t :: k) :: k where
  Assert 'False e _ = TypeError e
  Assert 'True _ t = t

type family Reverse xs acc where
  Reverse '[] acc = acc
  Reverse (x ': xs) acc = Reverse xs (x ': acc)

type Zip :: [a] -> [b] -> [(a,b)]
type family Zip xs ys where
  Zip '[] _ = '[]
  Zip _ '[] = '[]
  Zip (a ': as) (b ': bs) = '(a, b) ': Zip as bs

type NumbersFromTo :: Nat -> Nat -> [Nat]
type family NumbersFromTo from to where
  NumbersFromTo to to = '[]
  NumbersFromTo from to = from ': NumbersFromTo (from+1) to

type family Length α :: Nat where
  Length '[] = 0
  Length (_ ': as) = Length as + 1

type (:<|>:) :: Maybe a -> Maybe a -> Maybe a
type family (:<|>:) mx my where
  (:<|>:) ('Just x) _ = 'Just x
  (:<|>:) 'Nothing ('Just y) = 'Just y
  (:<|>:) 'Nothing 'Nothing  = 'Nothing

type MaybeF :: b -> (a -> Maybe b) -> Maybe a -> Maybe b
type family MaybeF d f mx where
  MaybeF d f 'Nothing = 'Just d
  MaybeF d f ('Just x) = f x

