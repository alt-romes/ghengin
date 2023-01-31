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
  pure (Lit ())

fragment :: FragmentShaderModule '[
                                  ] _
fragment = shader do
  pure (Lit ())

