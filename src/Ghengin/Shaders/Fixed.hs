{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-|

This module defines the fixed shader parameters that for now all shaders should include.

Descriptor set #0 is fixed and bound once per pipeline with the global pipeline data:
  * Camera position
  * Projection matrix
  * View matrix

The push constant is always the
  * Model matrix

For now, the vertices are fixed to three V 3s but soon they will be fully customizable as well.

Descriptor set #1 is bound once per material and is fully customizable

 -}
module Ghengin.Shaders.Fixed where

import FIR
import Math.Linear

-- Descriptor Set #0 for things bound once per pipeline (global pipeline data)
-- Descriptor Set #1 for things bound once per material
-- Descriptor Set #2 for things bound once per object

type FixedVertices
  = '[ "in_position" ':-> Input '[ Location 0 ] (V 3 Float)
     , "in_normal"   ':-> Input '[ Location 1 ] (V 3 Float)
     , "in_color"    ':-> Input '[ Location 2 ] (V 3 Float)
     ]

-- | Descriptor set #0 is fixed (for now?), and the engine always passes these parameters in the descriptor set #0
type FixedDescriptorSetZero
  = '[ "ubo" ':-> Uniform '[ DescriptorSet 0, Binding 0 ]
                           ( Struct '[ "view" ':-> M 4 4 Float
                                     , "proj" ':-> M 4 4 Float ] )
     , "camera_pos" ':-> Uniform '[ DescriptorSet 0, Binding 1 ]
                                  ( Struct '[ "val" ':-> V 3 Float ] )
     ]

-- | Push constant is always passed per-model with the model transform matrix according to the scene graph
type FixedPushConstant = '[ "push" ':-> PushConstant '[] (Struct '[ "model" ':-> M 4 4 Float ]) ]

-- type FixedDefinitions = FixedPushConstant ': FixedDescriptorSetZero :++: FixedVertices

applyMVP :: ∀ π
     . ( CanGet "push" π
       , CanGet "ubo"  π
       , _ -- extra constraints
       )
    => (Code (V 4 Float)) -> Program π π (Code (V 4 Float))
applyMVP vec = do

  modelM <- use @(Name "push" :.: Name "model" :: Optic '[] π (M 4 4 Float))
  viewM  <- use @(Name "ubo" :.: Name "view"   :: Optic '[] π (M 4 4 Float))
  projM  <- use @(Name "ubo" :.: Name "proj"   :: Optic '[] π (M 4 4 Float))

  pure $ (projM !*! viewM !*! modelM) !*^ vec

