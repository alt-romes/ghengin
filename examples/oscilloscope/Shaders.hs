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

-- erf from https://madebyevan.com/shaders/fast-rounded-rectangle-shadows/
-- vec2 erf(vec2 x) {
--   vec2 s = sign(x), a = abs(x);
--   x = 1.0 + (0.278393 + (0.230389 + 0.078108 * (a * a)) * a) * a;
--   x *= x;
--   return s - s / (x * x);
-- }
-- erf :: V 2 Float -> V 2 Float
-- erf x = do
--   let'
  

--------------------------------------------------------------------------------
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

fragmentSimple :: G.FragmentShaderModule '[
    "in_uv" ':-> Input '[ Location 0 ] (V 2 Float)
  , "time"  ':-> Uniform '[ DescriptorSet 0, Binding 0 ]
                 (Struct '[ "t" ':-> Float ])
  ] _
fragmentSimple = shader do
  ~(Vec2 ix iy) <- #in_uv
  t <- use @(Name "time" :.: Name "t")

  let side = 2
  let sigma = 0.005

  let f a = sin (t+a)

  let plot g = (g (ix*side - side/2) + (side/2)) / side

  -- pos in [0,1]x[0,1]
  let pos = Vec2 (plot f) (plot (\x -> sin(t+pi/2+x)))

  let dist = distance pos (Vec2 ix iy)

  let intensity = (1/(sigma*sqrt(2*pi)))*exp(-((dist**2)/(2*sigma**2)))
  -- let cumulative = _

  let col = Vec4 0 intensity 0 1

  #out_colour .= col

-- TODO: THIS IS NOT DONE YET!

-- erf :: Code (V 4 Float) -> _
-- erf x =
--   let s = sign(x)
--       a = abs(x)
--   in _

sign :: Code (V 4 Float) -> Code (V 4 Float)
sign (Vec4 a b c d) = Vec4 (f a) (f b) (f c) (f d)
  where
    f :: Code Float -> Code Float
    f x =
      if      x > 0 then  1
      else if x < 0 then -1
      else                0

abs4 :: Code (V 4 Float) -> Code (V 4 Float)
abs4 (Vec4 a b c d) = Vec4 (abs a) (abs b) (abs c) (abs d)
