{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ghengin.Core.Shader
  ( module Ghengin.Core.Shader
  , module Ghengin.Core.Shader.Canonical
  , module Ghengin.Core.Shader.Pipeline
  , type (FIR.:->), M.Value
  )
  where

import Data.Kind
import GHC.TypeLits
import Ghengin.Core.Shader.Canonical
import Ghengin.Core.Shader.Pipeline
import Geomancy.Vec2
import Geomancy.Vec3
import Geomancy.Mat4

import qualified FIR.Prim.Image as FIR
import qualified SPIRV.Image as SPIRV
import qualified SPIRV.ScalarTy

import Ghengin.Core.Prelude (GHList(..), Float, undefined)

import Math.Linear
import qualified FIR
import qualified FIR.AST as FIR
import qualified Data.Type.Map as M

type VertexShaderModule defs
  = FIR.ShaderModule "main" FIR.VertexShader
                     (("main" 'FIR.:-> FIR.EntryPoint '[] FIR.Vertex) ': CanonicalizeDefs defs)

type FragmentShaderModule defs
  = FIR.ShaderModule "main" FIR.FragmentShader
                     (("out_colour" 'FIR.:-> FIR.Output '[ FIR.Location 0 ] (V 4 FIR.Float)) ': ("main" 'FIR.:-> FIR.EntryPoint '[ FIR.OriginUpperLeft ] FIR.Fragment) ': CanonicalizeDefs defs)


-- * Instances for Syntactic
--
-- | The following instances are supposed to be used with deriving via:
--
-- Example
--
-- @
-- -- Internal type will be Struct [ "v" :-> V 3 Float ]
-- newtype CameraPos = CP Vec3 deriving Syntactic via (Vec3Struct @"v")
-- @
--
-- There is also an instance of Syntactic for n-ary products of syntactic things like Mat and Vec,
-- so we can easily create instances for compound structs! (We need to use
-- generic here I think, since deriving via won't coerce between SOP and datatypes)
--
-- Example
-- 
-- @
--
-- @

type StructFloat :: Symbol -> Type
newtype StructFloat name = StructFloat Float

type StructVec2 :: Symbol -> Type
newtype StructVec2 name = StructVec2 Vec2

type StructVec3 :: Symbol -> Type
newtype StructVec3 name = StructVec3 Vec3

type StructMat4 :: Symbol -> Type
newtype StructMat4 name = StructMat4 Mat4

-- Temporary? See ticket in fir
-- NO! Plain wrong, we need to completely move away from Syntactic for our use case
instance FIR.Syntactic FIR.Float where
  type Internal FIR.Float = FIR.Val FIR.Float
  toAST = FIR.Lit
  fromAST (FIR.Lit x) = x

instance FIR.Syntactic Vec2 where
  type Internal Vec2 = FIR.Val (V 2 FIR.Float)
  toAST (WithVec2 x y) = FIR.toAST (V2 x y)
  fromAST (FIR.fromAST -> V2 x y) = vec2 x y

instance KnownSymbol name => FIR.Syntactic (StructVec2 name) where
  type Internal (StructVec2 name) = FIR.Val (FIR.Struct '[ name 'FIR.:-> V 2 FIR.Float ])
  toAST (StructVec2 v2) = FIR.Struct (FIR.toAST v2 FIR.:& FIR.End)
  fromAST (FIR.fromAST FIR.. FIR.view @(FIR.Name name) -> v3) = StructVec2 v3

instance FIR.Syntactic Vec3 where
  type Internal Vec3 = FIR.Val (V 3 FIR.Float)
  toAST (WithVec3 x y z) = FIR.toAST (V3 x y z)
  fromAST (FIR.fromAST -> V3 x y z) = vec3 x y z

instance KnownSymbol name => FIR.Syntactic (StructVec3 name) where
  type Internal (StructVec3 name) = FIR.Val (FIR.Struct '[ name 'FIR.:-> V 3 FIR.Float ])
  toAST (StructVec3 v3) = FIR.Struct (FIR.toAST v3 FIR.:& FIR.End)
  fromAST (FIR.fromAST FIR.. FIR.view @(FIR.Name name) -> v3) = StructVec3 v3

instance FIR.Syntactic Mat4 where
  type Internal Mat4 = FIR.Val (M 4 4 FIR.Float)

  toAST mat
    = withColMajor mat
         \ m00 m10 m20 m30
           m01 m11 m21 m31
           m02 m12 m22 m32
           m03 m13 m23 m33 ->
             FIR.toAST ( M FIR.$
                            V4 (V4 m00 m10 m20 m30)
                               (V4 m01 m11 m21 m31)
                               (V4 m02 m12 m22 m32)
                               (V4 m03 m13 m23 m33)
                       )

  fromAST (FIR.fromAST
            -> M (V4 (V4 m00 m10 m20 m30)
                     (V4 m01 m11 m21 m31)
                     (V4 m02 m12 m22 m32)
                     (V4 m03 m13 m23 m33))
          ) = colMajor m00 m10 m20 m30
                       m01 m11 m21 m31
                       m02 m12 m22 m32
                       m03 m13 m23 m33


instance KnownSymbol name => FIR.Syntactic (StructMat4 name) where
  type Internal (StructMat4 name) = FIR.Val (FIR.Struct '[ name 'FIR.:-> M 4 4 FIR.Float ])

  toAST (StructMat4 mat)
    = withColMajor mat
         \ m00 m10 m20 m30
           m01 m11 m21 m31
           m02 m12 m22 m32
           m03 m13 m23 m33 ->
             FIR.Struct ( FIR.toAST ( M FIR.$
                            V4 (V4 m00 m10 m20 m30)
                               (V4 m01 m11 m21 m31)
                               (V4 m02 m12 m22 m32)
                               (V4 m03 m13 m23 m33)
                                    )
                          FIR.:& FIR.End )
  fromAST (FIR.fromAST FIR.. FIR.view @(FIR.Name name)
            -> M (V4 (V4 m00 m10 m20 m30)
                     (V4 m01 m11 m21 m31)
                     (V4 m02 m12 m22 m32)
                     (V4 m03 m13 m23 m33))
              ) = StructMat4 (colMajor m00 m10 m20 m30
                                       m01 m11 m21 m31
                                       m02 m12 m22 m32
                                       m03 m13 m23 m33
                             )

instance KnownSymbol name => FIR.Syntactic (StructFloat name) where
  type Internal (StructFloat name) = FIR.Val (FIR.Struct '[ name 'FIR.:-> FIR.Float ])
  -- we don't call these methods, they are needed by FIR only; our
  -- serialization is currently done by Storable (though it should really be
  -- through gl-block)
  toAST (StructFloat f) = FIR.Struct (FIR.Lit f FIR.:& FIR.End)
  fromAST = undefined -- bad!, we don't want syntactic really, this cannot be implemented correctly

