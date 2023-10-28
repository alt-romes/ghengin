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

pattern2, pattern2', pattern2'' :: Code (V 2 Float) -> Program _s _s (Code Float)
pattern2 p = do
  return $ fbm p
pattern2' p = do
  q <- let' $ fbm p
  return $ fbm (p ^+^ Vec2 q q)
pattern2'' p = do
  q <- let' $ fbm p
  r <- let' $ fbm (p ^+^ Vec2 q q)
  return $ fbm (p ^+^ Vec2 r r)

fbm = fbm2 16 (1/2)

fragment :: (Float, Float) -> G.FragmentShaderModule FragmentDefs _
fragment (width,height) = shader do

  ~( Vec4 x y _ _ ) <- #gl_FragCoord
  ~(Vec2 mx my) <- use @(Name "ubo" :.: Name "mousePos")

  let uv = Vec2 (x-Lit width) (y-Lit height) ^/ Lit height
      mp = Vec2 (mx-Lit width) (my-Lit height) ^/ Lit height

  p  <- pattern2'' (uv ^+^ mp)

  let Vec3 r g b = color (p*0.5 + 0.5)

  #out_colour .= Vec4 r g b 1

------------------------------------------------
-- pipeline

shaderPipeline :: (Word32, Word32) -> G.ShaderPipeline _
shaderPipeline (fromIntegral -> x, fromIntegral -> y)
  = G.ShaderPipeline (StructInput @VertexInput @(Triangle List))
  G.:>-> vertex
  G.:>-> fragment (x,y)
