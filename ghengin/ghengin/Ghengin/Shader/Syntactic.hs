{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Allow engine-defined types at the shader level.
-- This module exports orphan definitions for engine types instancing Syntactic.
module Ghengin.Shader.Syntactic where

import qualified Ghengin.Vulkan.Renderer.Texture as Tex
import Geomancy.Vec3
import FIR
import qualified FIR.Prim.Image as FIR
import qualified SPIRV.Image as SPIRV
import qualified SPIRV.ScalarTy
import Math.Linear

instance Syntactic Vec3 where
  type Internal Vec3 = Val (V 3 Float)
  toAST (WithVec3 x y z) = FIR.Vec3 (Lit x) (Lit y) (Lit z)
  fromAST (Vec3 (Lit x) (Lit y) (Lit z)) = vec3 x y z
  fromAST Vec3{} = error "impossible"

instance Syntactic Tex.Texture2D where
  type Internal Tex.Texture2D = Val ((FIR.Image ('FIR.Properties 'FIR.FloatingPointCoordinates Float 'SPIRV.TwoD ('Just 'SPIRV.NotDepthImage) 'SPIRV.NonArrayed 'SPIRV.SingleSampled 'SPIRV.Sampled ('Just ('SPIRV.ImageFormat ('SPIRV.Integer 'SPIRV.Normalised 'SPIRV.ScalarTy.Unsigned) '[8,8,8,8])))))
  toAST = FIR.undefined
  fromAST = FIR.undefined

