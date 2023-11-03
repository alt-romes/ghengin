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
   , "time"        ':-> Uniform '[ DescriptorSet 0, Binding 1 ] (Struct '[ "val" ':-> Float ])
   ]

pattern2, pattern2', pattern2'' :: Code (V 2 Float) -> Program _s _s (Code Float)
pattern2 p = do
  fbm p
pattern2' p = do
  q <- fbm p
  fbm (p ^+^ Vec2 q q)
pattern2'' p = do
  q <- fbm p
  r <- fbm (p ^+^ Vec2 q q)
  fbm (p ^+^ Vec2 r r)
pattern2''' p = do
  q <- fbm p
  r <- fbm (p ^+^ Vec2 q q)
  n <- fbm (p^+^Vec2 r r)
  u <- fbm (p ^+^ Vec2 q q ^+^ Vec2 n n)
  fbm (p^+^Vec2 q q ^+^ Vec2 r r ^+^ Vec2 u u)

fbm :: Code (V 2 Float) -> Program _s _s (Code Float)
fbm p = let' $ fbm2 4 (1/2) (p^+^Vec2 100 100)
{-# NOINLINE fbm #-}

fragment :: (Float, Float) -> G.FragmentShaderModule FragmentDefs _
fragment (width,height) = shader do

  ~( Vec4 x y _ _ ) <- #gl_FragCoord
  ~(Vec2 mx my) <- use @(Name "ubo" :.: Name "mousePos")
  t             <- use @(Name "time" :.: Name "val")

  let uv = Vec2 (x-Lit width) (y-Lit height) ^/ Lit height
      mp = (25 *^ Vec2 (mx-Lit width) (my-Lit height)) ^/ Lit height

  p  <- pattern2''' (uv ^+^ mp ^+^ Vec2 t t)

  let Vec3 r g b = color3 p

  #out_colour .= Vec4 p p p 1

------------------------------------------------
-- pipeline

shaderPipeline :: (Word32, Word32) -> G.ShaderPipeline _
shaderPipeline (fromIntegral -> x, fromIntegral -> y)
  = G.ShaderPipeline (StructInput @VertexInput @(Triangle List))
  G.:>-> vertex
  G.:>-> fragment (x,y)
