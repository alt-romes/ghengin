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
-- * Simple Triangle Shader
--------------------------------------------------------------------------------

type VertexInput
  = '[ Slot 0 0 ':-> V 3 Float ]
 
shaderPipelineSimple :: G.ShaderPipeline _
shaderPipelineSimple
  = G.ShaderPipeline (StructInput @VertexInput @(Triangle List))
  G.:>-> vertexSimple
  G.:>-> fragmentSimple

type VertexDefs =
  '[ "in_position" ':-> Input '[ Location 0 ] (V 3 Float)
   ]

vertexSimple :: G.VertexShaderModule VertexDefs _
vertexSimple = shader do
    ~(Vec3 x y z) <- get @"in_position"
    put @"gl_Position" (Vec4 x y z 1)

fragmentSimple :: G.FragmentShaderModule '[] _
fragmentSimple = shader do
  #out_colour .= Vec4 1 0 0 1

