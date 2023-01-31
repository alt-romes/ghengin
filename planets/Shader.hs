{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE QualifiedDo      #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
module Shader where

import Ghengin.Shader.Utils
import Ghengin.Shaders.FIR
import Ghengin.Shaders
import Ghengin.Shaders.Lighting
import Ghengin.Shaders.Fixed

-- Descriptor Set #0 for things bound once per pipeline (global pipeline data)
-- Descriptor Set #1 for things bound once per material
-- Descriptor Set #2 for things bound once per object

---- Vertex -----

type VertexDefs
  = '[ "out_position"  ':-> Output '[ Location 0 ] (V 4 Float)
     , "out_normal"    ':-> Output '[ Location 1 ] (V 4 Float)

     , "main"          ':-> EntryPoint '[] Vertex
     ]
     :++: FixedDescriptorSetZero
     :++: FixedPushConstant
     :++: FixedVertices


vertex :: ShaderModule "main" VertexShader VertexDefs _
vertex = shader do

    ~(Vec3 x y z)    <- get @"in_position"
    ~(Vec3 nx ny nz) <- get @"in_normal"

    modelM <- use @(Name "push" :.: Name "model")

    -- Output position and normal in world coordinates
    put @"out_position" (modelM !*^ (Vec4 x y z 1))
    put @"out_normal"   (modelM !*^ (Vec4 nx ny nz 0)) -- Normal is not a position so shouldn't be affected by translation (hence the 0 in the 4th component)

    put @"gl_Position" =<< applyMVP (Vec4 x y z 1)


---- Fragment -----


type FragmentDefs
  =  '[ "in_position" ':-> Input '[ Location 0 ] (V 4 Float)
      , "in_normal"   ':-> Input '[ Location 1 ] (V 4 Float)
      -- , "light_pos"  ':-> Uniform '[ DescriptorSet 0, Binding 1 ]
      --                               ( Struct '[ "val" ':-> V 3 Float ] )
      , "minmax"     ':-> Uniform '[ DescriptorSet 1, Binding 0 ]
                                  ( Struct '[ "min" ':-> Float
                                            , "max" ':-> Float ] ) -- Careful with alignment...
      , "gradient" ':-> Texture2D '[ DescriptorSet 1, Binding 1 ] (RGBA8 UNorm)

      , "out_col" ':-> Output '[ Location 0 ] (V 4 Float)
      , "main"    ':-> EntryPoint '[ OriginUpperLeft ] Fragment
      ]
      :++: FixedDescriptorSetZero
      :++: FixedPushConstant


fragment :: ShaderModule "main" FragmentShader FragmentDefs _
fragment = shader do

    ~(Vec4 px py pz _)  <- get @"in_position"

    -- Color
    min' <- use @(Name "minmax" :.: Name "min")
    max' <- use @(Name "minmax" :.: Name "max")

    let col_frac   = invLerp (norm (Vec3 px py pz)) min' max'

    ~(Vec4 cx' cy' cz' _) <- use @(ImageTexel "gradient") NilOps (Vec2 col_frac col_frac)

    ~(Vec3 colx coly colz) <- blinnPhong 16 $ Vec3 cx' cy' cz'

    put @"out_col" (Vec4 colx coly colz 1)

--- Pipeline ----

type VertexData =
  '[ Slot 0 0 ':-> V 3 Float -- in pos
   , Slot 1 0 ':-> V 3 Float -- in normal
   , Slot 2 0 ':-> V 3 Float -- in color
   ]

-- BIG:TODO: Why is the vertex data not being validated? must check again validation to make sure I don't have bugs like that

shaderPipeline :: GShaderPipeline _
shaderPipeline
  = StructInput @VertexData @(Triangle List)
    :>-> (vertex, ())
    :>-> (fragment, ())

