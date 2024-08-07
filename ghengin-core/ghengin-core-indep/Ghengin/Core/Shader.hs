{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ghengin.Core.Shader
  ( module Ghengin.Core.Shader
  , module Ghengin.Core.Shader.Canonical
  , module Ghengin.Core.Shader.Pipeline
  , type (FIR.:->), M.Value
  )
  where

import Graphics.Gl.Block
import Data.Kind
import GHC.TypeLits
import Ghengin.Core.Shader.Canonical
import Ghengin.Core.Shader.Pipeline
import Geomancy.Vec2
import Geomancy.Vec3
import Geomancy.Vec4
import Geomancy.Mat4

import Ghengin.Core.Prelude (Float, Generic)

import Math.Linear
import qualified FIR
import qualified Data.Type.Map as M

import Ghengin.Core.Shader.Data

type VertexShaderModule defs
  = FIR.ShaderModule "main" FIR.VertexShader
                     (("main" 'FIR.:-> FIR.EntryPoint '[] FIR.Vertex) ': CanonicalizeDefs defs)

type FragmentShaderModule defs
  = FIR.ShaderModule "main" FIR.FragmentShader
                     (("out_colour" 'FIR.:-> FIR.Output '[ FIR.Location 0 ] (V 4 Float)) ': ("main" 'FIR.:-> FIR.EntryPoint '[ FIR.OriginUpperLeft ] FIR.Fragment) ': CanonicalizeDefs defs)


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
  deriving Generic
  deriving anyclass Block

type StructVec2 :: Symbol -> Type
newtype StructVec2 name = StructVec2 Vec2
  deriving Generic
  deriving anyclass Block

type StructVec3 :: Symbol -> Type
newtype StructVec3 name = StructVec3 Vec3
  deriving Generic
  deriving anyclass Block

type StructVec4 :: Symbol -> Type
newtype StructVec4 name = StructVec4 Vec4
  deriving Generic
  deriving anyclass Block

type StructMat4 :: Symbol -> Type
newtype StructMat4 name = StructMat4 Mat4
  deriving Generic
  deriving anyclass Block

instance ShaderData Float where
  type FirType Float = Float

instance ShaderData Vec2 where
  type FirType Vec2 = V 2 Float

instance KnownSymbol name => ShaderData (StructVec2 name) where
  type FirType (StructVec2 name) = FIR.Struct '[ name 'FIR.:-> V 2 Float ]

instance ShaderData Vec3 where
  type FirType Vec3 = V 3 Float

instance ShaderData Vec4 where
  type FirType Vec4 = V 4 Float

instance KnownSymbol name => ShaderData (StructVec3 name) where
  type FirType (StructVec3 name) = FIR.Struct '[ name 'FIR.:-> V 3 Float ]

instance KnownSymbol name => ShaderData (StructVec4 name) where
  type FirType (StructVec4 name) = FIR.Struct '[ name 'FIR.:-> V 4 Float ]

instance ShaderData Mat4 where
  type FirType Mat4 = M 4 4 Float

instance KnownSymbol name => ShaderData (StructMat4 name) where
  type FirType (StructMat4 name) = FIR.Struct '[ name 'FIR.:-> M 4 4 Float ]

instance KnownSymbol name => ShaderData (StructFloat name) where
  type FirType (StructFloat name) = FIR.Struct '[ name 'FIR.:-> Float ]
