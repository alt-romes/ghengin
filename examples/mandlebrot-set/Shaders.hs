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

------------------------------------------------
-- pipeline input

type VertexInput
  = '[ Slot 0 0 ':-> V 3 Float ]

-------------------------------------
-- vertex shader

type VertexDefs =
  '[ "in_position"  ':-> Input      '[ Location 0 ] (V 3 Float)
   ]

vertex :: G.VertexShaderModule VertexDefs _
vertex = shader do
    ~(Vec3 x y z) <- get @"in_position"
    put @"gl_Position" (Vec4 x y z 1)

------------------------------------------------
-- fragment shader

type FragmentDefs =
  '[ "ubo"         ':-> Uniform '[ DescriptorSet 0, Binding 0 ]
                        ( Struct '[ "mousePos" ':-> V 2 Float ] )
   ]

#define N 64
#define B  4

fragment :: (Float, Float) -> G.FragmentShaderModule FragmentDefs _
fragment (width,height) = shader do

  ~( Vec4 x y _ _ ) <- #gl_FragCoord

  let uv = (2 *^ (Vec2 x y) ^-^ (Vec2 (Lit width) (Lit height)) ^-^ Vec2 1 1) ^/ (Lit height)

  i <- iterate (CodeComplex uv)

  let r = if i == N then 1 else i / N

  #out_colour .= Vec4 r r r 1

iterate :: _ => CodeComplex Float -> Program _s _s (Code Float)
iterate c = locally do
  #z     #= (Vec2 0 0 :: Code (V 2 Float))
  #depth #= (0 :: Code Float) -- float incremented as an integer

  loop do
    zv@(CodeComplex -> z) <- #z
    depth <- #depth
    if dot zv zv > B*B || depth >= N
    then break @1
    else do
      #z     .= codeComplex (z * z + c)
      #depth .= depth + 1

  depth <- #depth
  return depth

-- iterate' :: Code Float -> CodeComplex Float -> CodeComplex Float -> Code Float
-- iterate' !i z c = do
--   let z' = z * z + c
--   if dot (codeComplex z') (codeComplex z') > B*B
--     then i
--     else iterate' (i+1) z' c

------------------------------------------------
-- pipeline

shaderPipeline :: (Word32, Word32) -> G.ShaderPipeline _
shaderPipeline (fromIntegral -> x, fromIntegral -> y)
  = G.ShaderPipeline (StructInput @VertexInput @(Triangle List))
  G.:>-> vertex
  G.:>-> fragment (x,y)
