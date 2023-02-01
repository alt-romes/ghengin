{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Ocean.Shader where

import Ghengin.Shader.FIR
import Ghengin.Shader

oceanShaderPipeline :: ShaderPipeline _
oceanShaderPipeline
  = ShaderPipeline (StructInput @(ToStructInput '[V 3 Float]) @(Triangle List))
    :>-> vertex
    :>-> fragment

vertex :: VertexShaderModule '[ "in_position" ':-> Input '[ Location 0 ] (V 3 Float)
                              ] _
vertex = shader do
  ~(Vec3 x y z) <- use @(Name "in_position")
  put @"gl_Position" (Vec4 x y z 1)

fragment :: FragmentShaderModule '[
                                  ] _
fragment = shader do
  put @"out_colour" (Vec4 1 1 1 1)

