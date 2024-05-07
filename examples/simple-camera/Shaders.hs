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

-- fir
import FIR
import FIR.Syntax.Labels
import Math.Linear

-- ghengin
import qualified Ghengin.Core.Shader as G

--------------------------------------------------------------------------------
-- * Colored Triangle Shader
--------------------------------------------------------------------------------

-- See FIR.Validation.Layout about locations/component layouts...
type VertexInput
  = '[ Slot 0 0 ':-> V 3 Float
     , Slot 1 0 ':-> V 3 Float ]

shaderPipeline :: G.ShaderPipeline _
shaderPipeline
  = G.ShaderPipeline (StructInput @VertexInput @(Triangle List))
  G.:>-> vertex
  G.:>-> fragment

type VertexDefs =
  '[ "in_position"  ':-> Input      '[ Location 0 ] (V 3 Float)
   , "in_color"     ':-> Input      '[ Location 1 ] (V 3 Float)
   , "frag_color"   ':-> Output     '[ Location 0 ] (V 3 Float)
   , "model"    ':-> Uniform    '[ DescriptorSet 2, Binding 0 ] (Struct '[ "m" ':-> M 4 4 Float ])
   , "camera"       ':-> Uniform    '[ DescriptorSet 0, Binding 0 ]
                          (Struct [ "view_matrix" ':-> M 4 4 Float
                                  , "proj_matrix" ':-> M 4 4 Float
                                  ])
   ]

-- Descriptor Set 0 : Pipeline properties (bound once per pipeline)
-- Descriptor Set 1 : Material properties (bound once per material)
-- Descriptor Set 2 : Mesh properties (bound once per mesh)

vertex :: G.VertexShaderModule VertexDefs _
vertex = shader do
    ~(Vec3 x y z) <- #in_position
    color         <- #in_color
    proj_matrix   <- use @(Name "camera" :.: Name "proj_matrix")
    view_matrix   <- use @(Name "camera" :.: Name "view_matrix")
    model_mat     <- use @(Name "model" :.: Name "m")
    #gl_Position .= (proj_matrix !*! view_matrix !*! model_mat) !*^ (Vec4 x y z 1)
    #frag_color  .= color

fragment :: G.FragmentShaderModule '["in_color" ':-> Input '[Location 0] (V 3 Float)] _
fragment = shader do
  ~(Vec3 r g b) <- #in_color
  #out_colour .= Vec4 r g b 1

