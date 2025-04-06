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
                     => Float {-^ x bound -}
                     -> Float {-^ y bound -}
                     -> (Code Float -> Code Float)
                     -> G.ShaderPipeline _
shaderPipelineSimple xb yb f
  = G.ShaderPipeline (StructInput @VertexInput @top)
  G.:>-> vertexSimple xb yb f
  G.:>-> fragmentSimple

type VertexDefs =
  '[ "in_position"  ':-> Input '[ Location 0 ] (V 3 Float)
   , "in_apply"     ':-> Uniform '[DescriptorSet 2, Binding 1] (Struct '[ "b" ':-> Float ])
   --- ^ whether to apply function. FALSE for grid lines.
   ]

vertexSimple :: Float -> Float -> (Code Float -> Code Float) -> G.VertexShaderModule VertexDefs _
vertexSimple xb yb f = shader do
    ~(Vec3 x orig_y _) <- get @"in_position"

    apply <- use @(Name "in_apply" :.: Name "b")

    let y = if apply == Lit DO_APPLY then
              f x
            else
              orig_y

    let pos = graphProjection xb yb (Vec4 x y 0 1)

    put @"gl_Position" pos

fragmentSimple :: G.FragmentShaderModule '[
     "in_color"     ':-> Uniform '[DescriptorSet 2, Binding 0] (Struct '[ "c" ':-> V 3 Float ])
  ] _
fragmentSimple = shader do
  ~(Vec3 r g b) <- use @(Name "in_color" :.: Name "c")
  #out_colour .= Vec4 r g b 1

graphProjection :: Float -> Float -> Code (V 4 Float) -> Code (V 4 Float)
graphProjection xbound ybound (Vec4 x y z a) =
  -- negate y because y points down in vulkan coords
  Vec4 (x/Lit xbound) (-y/Lit ybound) z a

