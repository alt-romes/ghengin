{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Allow engine-defined types at the shader level.
-- This module exports orphan definitions for engine types instancing Syntactic.
module Ghengin.Shader.Syntactic where

import Geomancy.Vec3
import FIR
import Math.Linear

instance Syntactic Vec3 where
  type Internal Vec3 = Val (V 3 Float)
  toAST (WithVec3 x y z) = FIR.Vec3 (Lit x) (Lit y) (Lit z)
  fromAST (Vec3 (Lit x) (Lit y) (Lit z)) = vec3 x y z
  fromAST Vec3{} = error "impossible"

