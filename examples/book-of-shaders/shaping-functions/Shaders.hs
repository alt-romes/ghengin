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

import qualified Prelude

-- fir
import FIR
import FIR.Syntax.Labels
import Math.Linear

-- ghengin
import qualified Ghengin.Core.Shader as G

width, height :: Prelude.Int
(width, height) = (640, 640)

--------------------------------------------------------------------------------
-- Book of Shader: 
--
-- Bufferless rendering as seen in:
--  https://www.saschawillems.de/blog/2016/08/13/vulkan-tutorial-on-rendering-a-fullscreen-quad-without-buffers/
--------------------------------------------------------------------------------

shaderPipelineSimple :: G.ShaderPipeline _
shaderPipelineSimple
  = G.ShaderPipeline (StructInput @'[] @(Triangle List))
  G.:>-> vertexSimple
  G.:>-> fragmentSimple

vertexSimple :: G.VertexShaderModule '[
    "out_uv" ':-> Output '[ Location 0 ] (V 2 Float)
  ] _
vertexSimple = shader do
  vix <- #gl_VertexIndex
  let ux = convert ((vix `shiftL` (1 :: Code Word32)) .&. 2)
      uy = convert (vix .&. 2)
  #out_uv .= Vec2 ux uy
  #gl_Position .= Vec4 (ux*2 - 1) (- (uy*2 - 1) {- in Vulkan, y points down -}) 0.1 1

plot :: Code Float -> Code Float -> Code Float
plot y pct = smoothstep (pct-0.005) pct y - smoothstep pct (pct+0.005) y

smoothstep edge0 edge1 x0 = do
  let x = clamp ((x0-edge0) / (edge1 - edge0))
      {-# NOINLINE x #-}
   in x * x * (3 - 2*x)
clamp x = min 1 $ max 0 x

fragmentSimple :: G.FragmentShaderModule '[
    "in_uv" ':-> Input '[ Location 0 ] (V 2 Float)
  ] _
fragmentSimple = shader do
  ~(Vec2 ix iy) <- #in_uv

  let
    y1   = f1 ix
    y2   = f2 ix
    y3   = f3 ix
    pct1 = plot iy y1
    pct2 = plot iy y2
    pct3 = plot iy y3

    Vec3 r g b
      = pct1*^(Vec3 0 1 0) ^+^ pct2*^(Vec3 1 0 0) ^+^ pct3*^(Vec3 0 0 1)

  #out_colour .= Vec4 r g b 1

  where
    f1 x = x ** 5
    f2 = smoothstep 0.1 0.9
    f3 x = sin (2*pi*x) / 2 + 0.5
