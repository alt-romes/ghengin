{-# LANGUAGE QuantifiedConstraints, DeriveAnyClass, UndecidableInstances #-}
-- | This module defines the main class which powers all of the serialization
-- and compatibility logic between CPU and GPU data types.
module Ghengin.Core.Shader.Data
  ( ShaderData(..)
  , InStruct(..)
    -- ** Re-exports
  , Poke(..), Layout(..)
  ) where

import Prelude
import GHC.TypeLits
import GHC.Generics
import Data.Kind
import Graphics.Gl.Block
import qualified FIR as FIR
import FIR.Layout as L

-- | The class which powers all of the serialization and compatibility logic
-- between CPU and GPU data types.
--
-- If a type instances 'ShaderData', it means it can be serialized according to
-- the [Shader Memory Layout](https://docs.vulkan.org/guide/latest/shader_memory_layout.html) into
-- a shader datatype such as @V 3 Float@, which has a matching
-- [SPIRV type](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#_types).
class ShaderData ty where

  -- | The primitive FIR shader type whose memory representation matches the result
  -- of serializing this datatype using 'Poke'. This is the promise that if
  -- your shader expects @FirType ty@ in a uniform/storage location, writing @ty@
  -- into the buffer will be sound, and the shader will find @ty@'s laid out 
  -- in memory according to @FirType ty@'s expected memory layout.
  --
  -- === _Example_
  --
  -- @
  -- instance FirType Vec3 where
  --   type FirType Vec3 = V 3 Float
  --
  -- instance FirType Mat4 where
  --   type FirType Mat4 = M 4 4 Float
  --
  -- instance FirType ... where
  --   type FirType ... = Struct ...
  -- @
  type family FirType (ty :: Type) :: Type

  -- ROMES:TODO: Perhaps we could instead have a family whose return kind is
  -- lifted 'SPIRV.PrimTy', and comparing that is easy (as long as we implement
  -- ShaderData for the common shader datatypes such as V 3 Float).
  -- Though that is quite considerably more burdensome (e.g. images, decorations...)
  -- Not sure if would be better.
  -- type family SpirType ty :: 'SPIRV.PrimTy

  -- | A proof that that the @Poke/SizeOf@ of the "primitive" FIR shader type
  -- matches the @Block/PackedSize@ of the CPU-level datatype. Even though this
  -- isn't a full blown proof that the types are compatible with respect to the
  -- expected memory layout by the graphics pipeline/shader, it nudges use of
  -- 'ShaderData' in the right direction.
  --
  -- Possibly, the greatest benefit for now is guaranteeing the type result of
  -- 'FirType' is actually a FIR primitive shader type, since we only have Poke
  -- instances for those.
  --
  -- ROMES:TODO: I'm not sure how it will work with images... they don't instance 'Poke'.
  -- proofSameSize :: PackedSize ty :~: PackedSize (FirType ty)

--------------------------------------------------------------------------------
-- * 'InStruct'
--------------------------------------------------------------------------------

-- | A utility to wrap any type @a@ in a Struct with a single @field@ mapping to @FirType a@
newtype InStruct (field :: Symbol) a = InStruct a
  deriving stock Generic

deriving anyclass instance (KnownNat (PackedSize a), Block a) => Block (InStruct field a)

instance (KnownNat (PackedSize a), Block a) => ShaderData (InStruct field a) where
  type FirType (InStruct field a) = FIR.Struct '[ field 'FIR.:-> FirType a ]

--------------------------------------------------------------------------------
-- * Basic instances
--------------------------------------------------------------------------------

instance ShaderData Bool where
  type FirType Bool = Bool

