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

type VertexInputColors
  = '[ Slot 0 0 ':-> V 3 Float
     , Slot 1 0 ':-> V 3 Float ]

shaderPipelineColors :: G.ShaderPipeline _
shaderPipelineColors
  = G.ShaderPipeline (StructInput @VertexInputColors @(Triangle List))
  G.:>-> vertexColor
  G.:>-> fragmentColor

type VertexDefsColors =
  '[ "in_position"  ':-> Input      '[ Location 0 ] (V 3 Float)
   , "in_color"     ':-> Input      '[ Location 1 ] (V 3 Float)
   , "frag_color"   ':-> Output     '[ Location 0 ] (V 3 Float)
   ]

vertexColor :: G.VertexShaderModule VertexDefsColors _
vertexColor = shader do
    ~(Vec3 x y _) <- #in_position
    color         <- #in_color
    #gl_Position .= (Vec4 x y 0.1 1)
    #frag_color  .= color

fragmentColor :: G.FragmentShaderModule '["in_color" ':-> Input '[Location 0] (V 3 Float)] _
fragmentColor = shader do
  ~(Vec3 r g b) <- #in_color
  #out_colour .= Vec4 r g b 1

