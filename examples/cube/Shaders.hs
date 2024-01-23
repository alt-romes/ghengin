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
-- * Vertex Shader
--------------------------------------------------------------------------------
-- The input to our vertex shader are vertices along the XY plane, which we will
-- displace according to the ocean model. We receive the vertices in `Location 0`
-- and they are `V 2 Float`s (points in the XY plane).

type VertexDefs =
  '[ "in_position"  ':-> Input      '[ Location 0 ] (V 3 Float)
   ]

type VertexInput
  = '[ Slot 0 0 ':-> V 3 Float ]
 
vertex :: G.VertexShaderModule VertexDefs _
vertex = shader do
    ~(Vec3 x y z) <- get @"in_position"
    put @"gl_Position" (Vec4 x y z 1)

--------------------------------------------------------------------------------
-- * Fragment Shader
--------------------------------------------------------------------------------

type FragmentDefs =
  '[ "ubo"         ':-> Uniform '[ DescriptorSet 0, Binding 0 ]
                        ( Struct '[ "mousePos" ':-> V 2 Float ] )
   , "time"        ':-> Uniform '[ DescriptorSet 0, Binding 1 ] (Struct '[ "val" ':-> Float ])
   ]

fragment :: (Float, Float) -> G.FragmentShaderModule FragmentDefs _
fragment (width,height) = shader do

  ~(Vec4 ix iy _ _) <- #gl_FragCoord
  ~(Vec2 mx my) <- use @(Name "ubo" :.: Name "mousePos")
  t             <- use @(Name "time" :.: Name "val")

  let uv@(Vec2 x y) = Vec2 (ix-Lit width) (iy-Lit height) ^/ Lit height

  let
    omega = pi
    k = 1
    r = 1
    alpha = -k*x-omega*t
    x' = (-alpha/k) - r*sin(alpha)
    z  = -r*cos(alpha)

  #out_colour .= Vec4 z z z 1

--------------------------------------------------------------------------------
-- * Pipeline
--------------------------------------------------------------------------------

shaderPipeline :: (Word32, Word32) -> G.ShaderPipeline _
shaderPipeline (fromIntegral -> x, fromIntegral -> y)
  = G.ShaderPipeline (StructInput @VertexInput @(Triangle List))
  G.:>-> vertex
  G.:>-> fragment (x,y)

