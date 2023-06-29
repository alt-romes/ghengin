{-# LANGUAGE UndecidableInstances #-}
module Ghengin.Core.Shader.Canonical where

import Prelude
import Data.Kind
import qualified FIR
import qualified FIR.Definition as FIR
import qualified Math.Linear as FIR

-- | CanonicalizeDefs applies canonicalization to every definition
type CanonicalizeDefs :: [ k FIR.:-> FIR.Definition ] -> [ k FIR.:-> FIR.Definition ]
type family CanonicalizeDefs m where
  CanonicalizeDefs '[] = '[]
  CanonicalizeDefs ((k 'FIR.:-> def) ': xs)
    = (k 'FIR.:-> CanonicalizeDef def) ': CanonicalizeDefs xs

-- | Canonicalize tries to transform all non-primitive types into primitive
-- types using the 'Syntactic' instance.
type CanonicalizeDef :: FIR.Definition -> FIR.Definition
type family CanonicalizeDef a where
  CanonicalizeDef ( 'FIR.Global storageClass decorations ty ) = 'FIR.Global storageClass decorations (CanonicalType ty)
  -- TODO: The following two should also probably be canonicalized
  CanonicalizeDef ( 'FIR.Function a b c ) = 'FIR.Function a b c
  CanonicalizeDef ( 'FIR.EntryPoint a b ) = 'FIR.EntryPoint a b

-- ^ Note: InternalType gets the underlying value of an internal representation of
-- a data type that instances `Syntactic`

-- | Canonical type is a primitive type unchanged or, if the type isn't
-- primitive, is its FIR.InternalType
type family CanonicalType a where
  -- We can't handle all primtys through their PrimTy, because of incoherency in instances,
  -- so, instead, we manually implement HasCanonicalType for each PrimTy instance
  -- The types that instance PrimTy:
  CanonicalType FIR.Int16 = FIR.Int16
  CanonicalType FIR.Int32 = FIR.Int32
  CanonicalType FIR.Int64 = FIR.Int64
  CanonicalType FIR.Int8 = FIR.Int8
  CanonicalType FIR.Word16 = FIR.Word16
  CanonicalType FIR.Word32 = FIR.Word32
  CanonicalType FIR.Word64 = FIR.Word64
  CanonicalType FIR.Word8 = FIR.Word8
  CanonicalType FIR.AccelerationStructure = FIR.AccelerationStructure
  CanonicalType FIR.Half = FIR.Half
  CanonicalType () = ()
  CanonicalType Bool = Bool
  CanonicalType Double = Double
  CanonicalType Float = Float
  -- The following are only PrimTy if a is a PrimTy.
  CanonicalType (FIR.RuntimeArray a) = FIR.RuntimeArray (CanonicalType a)
  CanonicalType (FIR.Array l a) = FIR.Array l (CanonicalType a)
  CanonicalType (FIR.Struct l) = FIR.Struct (CanonicalTypesMap l)
  CanonicalType (FIR.V n a) = FIR.V n (CanonicalType a)
  CanonicalType (FIR.M m n a) = FIR.M m n (CanonicalType a)

  -- Now, non-primTys which somehow are also accepted in the shader defs,
  -- because they have a known definition instance. Anyway, we don't want to get
  -- their underlying type, but just pass the image on.
  CanonicalType (FIR.Image a) = FIR.Image a
  -- CanonicalType (FIR.ImageFormat a) = FIR.ImageFormat a

  -- For all other types, which don't instance PrimTy, we get the InternalType
  CanonicalType a = FIR.InternalType a

-- | Map 'CanonicalType' over a Map
type CanonicalTypesMap :: [ fld FIR.:-> Type ] -> [ fld FIR.:-> Type ]
type family CanonicalTypesMap l where
  CanonicalTypesMap '[] = '[]
  CanonicalTypesMap ((fld 'FIR.:-> ty) ': xs) = ((fld 'FIR.:-> CanonicalType ty) ': xs)


