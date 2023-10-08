{-# LANGUAGE UndecidableInstances #-}
module Ghengin.Core.Shader
  ( module Ghengin.Core.Shader
  , module Ghengin.Core.Shader.Canonical
  , module Ghengin.Core.Shader.Pipeline
  )
  where

import Data.Kind
import GHC.TypeLits
import Ghengin.Core.Shader.Canonical
import Ghengin.Core.Shader.Pipeline
import Geomancy.Vec3
import Geomancy.Mat4

import Math.Linear
import qualified FIR

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
-- @
-- -- Internal type will be Struct [ "v" :-> V 3 Float ]
-- newtype CameraPos = CP Vec3 deriving Syntactic via (Vec3Struct @"v")
-- @
--
-- TODO: Make an instance of Syntactic for n-ary products of syntactic things like Mat and Vec,
-- so we can easily create instances for compound structs

type StructVec3 :: Symbol -> Type
newtype StructVec3 name = StructVec3 Vec3

instance KnownSymbol name => FIR.Syntactic (StructVec3 name) where
  type Internal (StructVec3 name) = FIR.Val (FIR.Struct '[ name 'FIR.:-> V 3 FIR.Float ])

  toAST (StructVec3 (WithVec3 x y z)) = FIR.Struct (FIR.Vec3 (FIR.Lit x) (FIR.Lit y) (FIR.Lit z) FIR.:& FIR.End)

  fromAST (FIR.view @(FIR.Name name) -> (FIR.Vec3 (FIR.Lit x) (FIR.Lit y) (FIR.Lit z))) = StructVec3 (vec3 x y z)

  fromAST (FIR.view @(FIR.Name name) -> FIR.Vec3{}) = FIR.error "impossible"

type StructMat4 :: Symbol -> Type
newtype StructMat4 name = StructMat4 Mat4

instance KnownSymbol name => FIR.Syntactic (StructMat4 name) where
  type Internal (StructMat4 name) = FIR.Val (FIR.Struct '[ name 'FIR.:-> M 4 4 FIR.Float ])

  toAST = FIR.undefined
  fromAST = FIR.undefined


