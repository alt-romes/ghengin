{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE NamedWildCards        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module Shaders where

import GHC.TypeNats

-- fir
import FIR
import FIR.Syntax.Labels
import Math.Linear

-- ghengin
import qualified Ghengin.Core.Shader as G

pattern DO_APPLY, DONT_APPLY, DO_APPLY_TO_VERT, DO_APPLY_TO_FUN :: Float
pattern DONT_APPLY = 0
pattern DO_APPLY = 1
pattern DO_APPLY_TO_VERT = 2
pattern DO_APPLY_TO_FUN = 3

--------------------------------------------------------------------------------
-- * Simple Plotting Shader
--------------------------------------------------------------------------------

type VertexInput
  = '[ Slot 0 0 ':-> V 3 Float ]
 
shaderPipelineSimple :: forall top.
                        Known (PrimitiveTopology Nat) top
                     => (Code Float -> Code Float)
                     -> G.ShaderPipeline _
shaderPipelineSimple f
  = G.ShaderPipeline (StructInput @VertexInput @top)
  G.:>-> vertexSimple f
  G.:>-> fragmentSimple

type VertexDefs =
  '[ "in_position"  ':-> Input '[ Location 0 ] (V 3 Float)
   , "in_apply"     ':-> Uniform '[DescriptorSet 2, Binding 1] (Struct '[ "b" ':-> Float ])
   , "in_apply_offset" ':-> Uniform '[DescriptorSet 2, Binding 2] (Struct '[ "bo" ':-> Float ])
   , "projection"   ':-> Uniform '[DescriptorSet 0, Binding 0] (Struct '[ "proj" ':-> M 4 4 Float ])
   , "offset"       ':-> Uniform '[DescriptorSet 0, Binding 1] (Struct '[ "x" ':-> Float ])
   ]

vertexSimple :: (Code Float -> Code Float) -> G.VertexShaderModule VertexDefs _
vertexSimple f = shader do
    ~(Vec3 orig_x orig_y z) <- get @"in_position"

    apply <- use @(Name "in_apply" :.: Name "b")
    apply_offset <- use @(Name "in_apply_offset" :.: Name "bo")
    projection <- use @(Name "projection" :.: Name "proj")
    offset <- use @(Name "offset" :.: Name "x")

    let x = if apply_offset == Lit DO_APPLY_TO_VERT then
              orig_x - offset
            else
              orig_x
    let y = if apply == Lit DO_APPLY then
              if apply_offset == Lit DO_APPLY_TO_FUN then
                f (x + offset)
              else
                f x
            else
              orig_y

    let pos_canonical =
          projection
            !*^ (Vec4 x (-y{-vulkan y is down, we want our graph to have Y up.-}) z 1)

    put @"gl_Position" pos_canonical

fragmentSimple :: G.FragmentShaderModule '[
     "in_color"     ':-> Uniform '[DescriptorSet 2, Binding 0] (Struct '[ "c" ':-> V 3 Float ])
  ] _
fragmentSimple = shader do
  ~(Vec3 r g b) <- use @(Name "in_color" :.: Name "c")
  put @"out_colour" (Vec4 r g b 1)

