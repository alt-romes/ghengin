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
-- Book of Shader: Shaping functions
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
-- Produces an interpolated out_uv for the screen coords from [0, 1]
vertexSimple = shader do
  vix <- #gl_VertexIndex
  let ux = convert ((vix `shiftL` (1 :: Code Word32)) .&. 2)
      uy = convert (vix .&. 2)
  #out_uv .= Vec2 ux uy
  #gl_Position .= Vec4 (ux*2 - 1) (- (uy*2 - 1) {- in Vulkan, y points down -}) 0 1

--------------------------------------------------------------------------------
-- Fragment
--------------------------------------------------------------------------------

plot :: Code Float -> Code Float -> Code Float
plot iy y = smoothstep (y-0.005) y iy - smoothstep y (y+0.005) iy

smoothstep edge0 edge1 x0 = do
  let x = clamp ((x0-edge0) / (edge1 - edge0))
      {-# NOINLINE x #-}
   in x * x * (3 - 2*x)
clamp x = min 1 $ max 0 x

fragmentSimple :: G.FragmentShaderModule '[
    "in_uv" ':-> Input '[ Location 0 ] (V 2 Float)
  , "sides" ':-> Uniform '[ DescriptorSet 0, Binding 0 ]
                  (Struct '[ "x" ':-> Float, "y" ':-> Float
                           , "off_x" ':-> Float, "off_y" ':-> Float ])
  ] _
fragmentSimple = shader do
  ~(Vec2 ix iy) <- #in_uv
  sx <- use @(Name "sides" :.: Name "x") -- size of x and
  sy <- use @(Name "sides" :.: Name "y") -- size of y sides
  ox <- use @(Name "sides" :.: Name "off_x") -- offset x and
  oy <- use @(Name "sides" :.: Name "off_y") -- offset y

  -- * ix, iy ∈ [0,1]
  -- * sx, sy arbitrary length and height
  -- * function input is scaled by sx (ie. ix*sx)
  -- * get the resulting y coord in [0, 1] by dividing by sy
  -- * plot y in [0,1] with `plot iy y`.

  let
    x    = ix * sx + ox
    y f  = (f x - oy) / sy
    calc f = plot iy (y f)
    pct1 = calc f1
    pct2 = calc f2
    pct3 = calc f3

    Vec3 r g b
      = pct1*^(Vec3 0 1 0) ^+^ pct2*^(Vec3 1 0 0) ^+^ pct3*^(Vec3 0 0 1)

  #out_colour .= Vec4 r g b 1

  where
    f1 x = x
    f2 = smoothstep 0.1 0.9
    f3 x = sin (2*pi*x) / 2 + 0.5
