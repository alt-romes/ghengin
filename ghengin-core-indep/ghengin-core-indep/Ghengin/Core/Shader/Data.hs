{-# LANGUAGE QuantifiedConstraints #-}
-- | This module defines the main class which powers all of the serialization
-- and compatibility logic between CPU and GPU data types.
module Ghengin.Core.Shader.Data
  ( ShaderData(..)
    -- ** Re-exports
  , Poke(..), Layout(..)
  ) where

import Data.Kind
import Foreign.Storable
import FIR.Prim.Types
import FIR.Layout

-- ROMES:TODO: For now, we still define 'Compatible' by means of 'InternalType', but that should change! See 'ShaderData'.

-- | The class which powers all of the serialization and compatibility logic
-- between CPU and GPU data types.
--
-- If a type instances 'ShaderData', it means it can be serialized according to
-- the [Shader Memory Layout](https://docs.vulkan.org/guide/latest/shader_memory_layout.html) (using 'Poke') into
-- a primitive datatype ('PrimTy') such as @V 3 Float@, which has a matching
-- [SPIRV type](https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#_types).
--
-- You can derive @'Poke'@ instances automatically (provide this using
-- @gl-block@ (TODO: May be hard bc of top-level info)). Another simple way to
-- get a 'Poke' instance is to convert the type to its primitive representation
-- and then leverage that representation's 'Poke' instance.
class -- (∀ lay. Poke ty (lay :: Layout)) => -- ROMES:TODO: We can't migrate to 'Poke' yet, drop 'Syntactic' first.
    Storable ty =>
    ShaderData ty where

  -- | The primitive shader type whose memory representation matches the result
  -- of serializing this datatype using 'Poke'. This is the promise that if
  -- your shader expects @firTy@ in a uniform location, writing @ty@ into the
  -- buffer will be sound, and the shader will find @ty@'s laid out in memory
  -- according to @firTy@'s expected memory layout.
  --
  -- romes:todo: I don't think we will be able to compare primitive types for
  -- 'Compatible' at runtime, so we'll likely have to resort to something else,
  -- like getting the 'FieldsOfType' of the 'PrimTy' instance of the type
  -- resulting from applying this type family.
  type family FirType (ty :: Type) :: Type

  -- ROMES:TODO: Perhaps we could instead have a family whose return kind is
  -- lifted 'SPIRV.PrimTy', and comparing that is easy (as long as we implement
  -- ShaderData for the common shader datatypes such as V 3 Float).
  -- Though that is quite considerably more burdensome (e.g. images, decorations...)
  -- Not sure if would be better.
  -- type family SpirType ty :: 'SPIRV.PrimTy

