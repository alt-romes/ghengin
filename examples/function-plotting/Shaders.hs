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

pattern DO_APPLY, DONT_APPLY :: Float
pattern DO_APPLY = 1
pattern DONT_APPLY = 0

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
   --- ^ whether to apply function. FALSE for grid lines.
   , "projection"   ':-> Uniform '[DescriptorSet 0, Binding 0] (Struct '[ "proj" ':-> M 4 4 Float ])
   , "scale"        ':-> Uniform '[DescriptorSet 0, Binding 1] (Struct '[ "sproj" ':-> M 4 4 Float ])
   --- ^ projection matrix
   ]

vertexSimple :: (Code Float -> Code Float) -> G.VertexShaderModule VertexDefs _
vertexSimple f = shader do
    ~(Vec3 x orig_y z) <- get @"in_position"

    apply <- use @(Name "in_apply" :.: Name "b")
    projection <- use @(Name "projection" :.: Name "proj")
    scale_proj <- use @(Name "scale" :.: Name "sproj")

    let y = if apply == Lit DO_APPLY then
              f x
            else
              orig_y

    let pos_canonical =
          (scale_proj !*! projection)
            !*^ (Vec4 x (-y{-vulkan y is down, we want our graph to have Y up.-}) z 1)

    put @"gl_Position" pos_canonical

fragmentSimple :: G.FragmentShaderModule '[
     "in_color"     ':-> Uniform '[DescriptorSet 2, Binding 0] (Struct '[ "c" ':-> V 3 Float ])
  ] _
fragmentSimple = shader do
  ~(Vec3 r g b) <- use @(Name "in_color" :.: Name "c")
  put @"out_colour" (Vec4 r g b 1)

