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

-- See FIR.Validation.Layout about locations/component layouts...
type VertexInput
  = '[ Slot 0 0 ':-> V 3 Float
     ]

shaderPipeline :: G.ShaderPipeline _
shaderPipeline
  = G.ShaderPipeline (StructInput @VertexInput @(Line Strip))
  G.:>-> vertex
  G.:>-> fragment

type VertexDefs =
  '[ "in_position"  ':-> Input      '[ Location 0 ] (V 3 Float)
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
    proj_matrix   <- use @(Name "camera" :.: Name "proj_matrix")
    view_matrix   <- use @(Name "camera" :.: Name "view_matrix")
    #gl_Position .= (proj_matrix !*! view_matrix) !*^ (Vec4 x y z 1)

fragment :: G.FragmentShaderModule '[] _
fragment = shader do
  #out_colour .= Vec4 1 0 0 1

