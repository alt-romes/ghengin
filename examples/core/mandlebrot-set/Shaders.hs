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

mandel :: CodeComplex Float -> CodeComplex Float -> CodeComplex Float
mandel z c = z * z + c

burning_ship :: CodeComplex Float -> CodeComplex Float -> CodeComplex Float
burning_ship (a :+: b) c = (a :+: abs b) + c

fragment :: (Float, Float) -> G.FragmentShaderModule FragmentDefs _
fragment (width,height) = shader do

  ~( Vec4 x y _ _ ) <- #gl_FragCoord

  let uv = (1.5 *^ (2 *^ (Vec2 x y) ^-^ (Vec2 (Lit width) (Lit height)) ^-^ Vec2 1 1)) ^/ (Lit height) ^-^ Vec2 0.4 0

  i <- iterate mandel (CodeComplex uv)

  let (Vec3 r g b) = if i == N then Vec3 0 0 0 else color (i / N)

  #out_colour .= Vec4 r g b 1

iterate :: _
        => (CodeComplex Float -> CodeComplex Float -> CodeComplex Float)
        -- ^ Fractal series function, taking complex numbers @z@ and @c@ as input
        -> CodeComplex Float
        -> Program _s _s (Code Float)
iterate fractal_s c = locally do
  #z     #= (Vec2 0 0 :: Code (V 2 Float))
  #depth #= (0 :: Code Float) -- float incremented as an integer

  loop do
    zv@(CodeComplex -> z) <- #z
    depth <- #depth
    if dot zv zv > B*B || depth >= N
    then break @1
    else do
      #z     .= codeComplex (fractal_s z c)
      #depth .= depth + 1

  zv <- #z
  depth <- #depth
  -- return (depth - log (log (dot zv zv) / log B / log 2.0))
  return depth

------------------------------------------------
-- pipeline

shaderPipeline :: (Word32, Word32) -> G.ShaderPipeline _
shaderPipeline (fromIntegral -> x, fromIntegral -> y)
  = G.ShaderPipeline (StructInput @VertexInput @(Triangle List))
  G.:>-> vertex
  G.:>-> fragment (x,y)
