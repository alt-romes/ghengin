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

--------------------------------------------------------------------------------
-- * Simple Plotting Shader
--------------------------------------------------------------------------------

type VertexInput
  = '[ Slot 0 0 ':-> V 3 Float ]
 
shaderPipelineSimple :: forall top.
                        Known (PrimitiveTopology Nat) top
                     => V 3 Float {-^ Color -}
                     -> Float {-^ x bound -}
                     -> Float {-^ y bound -}
                     -> (Code Float -> Code Float)
                     -> G.ShaderPipeline _
shaderPipelineSimple col xb yb f
  = G.ShaderPipeline (StructInput @VertexInput @top)
  G.:>-> vertexSimple xb yb f
  G.:>-> fragmentSimple col

type VertexDefs =
  '[ "in_position"  ':-> Input '[ Location 0 ] (V 3 Float)
   ]

vertexSimple :: Float -> Float -> (Code Float -> Code Float) -> G.VertexShaderModule VertexDefs _
vertexSimple xb yb f = shader do
    ~(Vec3 x _ _) <- get @"in_position"

    let y  = f x

    let pos = graphProjection xb yb (Vec4 x y 0 1)

    put @"gl_Position" pos

fragmentSimple :: (V 3 Float) -> G.FragmentShaderModule '[] _
fragmentSimple (V3 x y z) = shader do
  #out_colour .= Vec4 (Lit x) (Lit y) (Lit z) 1

graphProjection :: Float -> Float -> Code (V 4 Float) -> Code (V 4 Float)
graphProjection xbound ybound (Vec4 x y z a) =
  -- negate y because y points down in vulkan coords
  Vec4 (x/Lit xbound) (-y/Lit ybound) z a

gridShaderPipeline :: forall top.
                        Known (PrimitiveTopology Nat) top
                     => V 3 Float {-^ Color -}
                     -> Float {-^ x bound -}
                     -> Float {-^ y bound -}
                     -> G.ShaderPipeline _
gridShaderPipeline col xb yb
  = G.ShaderPipeline (StructInput @VertexInput @top)
  G.:>-> gridVertex xb yb
  G.:>-> fragmentSimple col

gridVertex :: Float -> Float -> G.VertexShaderModule VertexDefs _
gridVertex xb yb = shader do
    ~(Vec3 x y _) <- get @"in_position"

    let pos = graphProjection xb yb (Vec4 x y 0 1)

    put @"gl_Position" pos
