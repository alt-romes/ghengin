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

-- base
import Data.Foldable
  ( sequence_ )
import Data.Maybe
  ( fromJust )
import GHC.TypeNats
  ( KnownNat )

-- filepath
import System.FilePath
  ( (</>) )

-- text-short
import Data.Text.Short
  ( ShortText )

-- vector-sized
import qualified Data.Vector.Sized as Vector
  ( fromList )

-- fir
import FIR
import FIR.Syntax.Labels
import Math.Linear

-- ghengin
import qualified Ghengin.Core.Shader as G

-- ghengin-games
import Common.Shader

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

-- ROMES:TODO: If this were V 3 Float, why don't we get an error? Shouldn't 'Input' 'Location' match 'Slot's?
type VertexDefs =
  '[ "in_position"  ':-> Input      '[ Location 0 ] (V 3 Float)
   ]

vertexSimple :: G.VertexShaderModule VertexDefs _
vertexSimple = shader do
    ~(Vec3 x y _) <- get @"in_position"
    put @"gl_Position" (Vec4 x y 0 1)

fragmentSimple :: G.FragmentShaderModule '[] _
fragmentSimple = shader do
  #out_colour .= Vec4 1 0 0 1

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
    ~(Vec3 x y _) <- get @"in_position"
    color <- get @"in_color"
    put @"gl_Position" (Vec4 x y 0 1)
    put @"frag_color" color

fragmentColor :: G.FragmentShaderModule '["in_color" ':-> Input '[Location 0] (V 3 Float)] _
fragmentColor = shader do
  ~(Vec3 r g b) <- #in_color
  #out_colour .= Vec4 r g b 1

