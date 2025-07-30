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
-- Utils

-- mix() performs a linear interpolation between x and y using a to weight between them
mix :: (ValidDim v d n, Semimodule d v,  CancellativeAdditiveMonoid (Scalar v))
    => OfDim v d n -> OfDim v d n -> Scalar v -> OfDim v d n
mix x y a = x ^* (1-a) ^+^ y^*a

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

plot :: Code Float -- ^ Side
     -> Code Float -- ^ Y value normalized (wrt y-side) to [0, 1]
     -> Code Float -- ^ The Y position in the quad (iy)
     -> Code Float -- ^ Near 1 when iy is near Y position, 0 otherwise
plot ss ynorm iy =
  -- if abs (ynorm - iy) < 0.003 then 1 else 0
  smoothstep (ynorm - 0.001*ss) ynorm iy - smoothstep ynorm (ynorm + 0.001*ss) iy

smoothstep :: (Ord a, DivisionRing a) => a -> a -> a -> a
smoothstep edge0 edge1 x0 = do
  let x = clamp ((x0-edge0) / (edge1 - edge0))
      {-# NOINLINE x #-}
   in x * x * (3 - 2*x)
clamp :: (Ord a, AdditiveMonoid a) => a -> a
clamp x = min 1 $ max 0 x

fragmentSimple :: G.FragmentShaderModule '[
    "in_uv" ':-> Input '[ Location 0 ] (V 2 Float)
  , "sides" ':-> Uniform '[ DescriptorSet 0, Binding 0 ]
                  (Struct '[ "s" ':-> Float
                           , "off_x" ':-> Float
                           , "off_y" ':-> Float ])
  , "time" ':-> Uniform '[ DescriptorSet 0, Binding 1 ]
                  (Struct '[ "t" ':-> Float ])
  ] _
fragmentSimple = shader do
  ~(Vec2 ix iy) <- #in_uv
  ss <- use @(Name "sides" :.: Name "s") -- size of side and
  ox <- use @(Name "sides" :.: Name "off_x") -- offset x and
  oy <- use @(Name "sides" :.: Name "off_y") -- offset y
  t  <- use @(Name "time" :.: Name "t") -- offset y

  -- * ix, iy ∈ [0,1]
  -- * ss arbitrary length and height
  -- * function input is scaled by ss (ie. ix*ss)
  -- * get the resulting y coord in [0, 1] by dividing by ss
  -- * plot y in [0,1] with `plot y iy`.

  let
    f1 x = x*log (2 ** sin (x + t))
    -- f2 = smoothstep 0.1 0.9
    f2 x = sin(t) * x
    f3 x = sin (2*pi*(x+t)) / 2 + 0.5

    x    = ix * ss + ox
    gy   = iy*ss + oy
    y f  = (f x - oy) / ss
    calc f = plot ss (y f) iy
    pct1 = calc f1
    pct2 = calc f2
    pct3 = calc f3
    fract a = a - floor a

    Vec3 r g b
      = pct1*^(Vec3 0 1 0) ^+^ pct2*^(Vec3 1 0 0) ^+^ pct3*^(Vec3 0 0 1)

    bg = mix (Vec4 1 1 1 1) (Vec4 0.5 0.5 0.5 1)
             ((norm ((Vec2 0.5 0.5) ^-^ (Vec2 ix iy)) * 1.2) ** 3.5) -- vignett
    grid = if (fract x) < (0.001*ss) || (fract gy) < (0.001*ss)
             then Vec4 0 0 0 0.3
             else bg
  
  #out_colour .= mix grid (Vec4 r g b 1) (r + g + b)
