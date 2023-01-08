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

import Ghengin.Shaders.FIR
import Ghengin.Shaders
import Ghengin.Shaders.Lighting

-- Descriptor Set #0 for things bound once per pipeline (global pipeline data)
-- Descriptor Set #1 for things bound once per material
-- Descriptor Set #2 for things bound once per object

---- Vertex -----

type VertexDefs
  = '[ "in_position" ':-> Input '[ Location 0 ] (V 3 Float)
     , "in_normal"   ':-> Input '[ Location 1 ] (V 3 Float)
     , "out_position" ':-> Output '[ Location 0 ] (V 4 Float)
     , "out_normal"   ':-> Output '[ Location 1 ] (V 4 Float)
     , "ignored_color"    ':-> Input '[ Location 2 ] (V 3 Float)
     , "push"        ':-> PushConstant '[] (Struct '[ "model" ':-> M 4 4 Float ])
     , "ubo"         ':-> Uniform '[ DescriptorSet 0, Binding 0 ]
                                  ( Struct '[ "view" ':-> M 4 4 Float
                                            , "proj" ':-> M 4 4 Float ] )
     , "main"        ':-> EntryPoint '[] Vertex
     ]


vertex :: ShaderModule "main" VertexShader VertexDefs _
vertex = shader do
    ~(Vec3 x y z)    <- get @"in_position"
    ~(Vec3 nx ny nz) <- get @"in_normal"
    modelM <- use @(Name "push" :.: Name "model")
    viewM  <- use @(Name "ubo" :.: Name "view")
    projM  <- use @(Name "ubo" :.: Name "proj")

    put @"out_position" (modelM !*^ (Vec4 x y z 1))
    put @"out_normal"   (modelM !*^ (Vec4 nx ny nz 0)) -- Normal is not a position so shouldn't be affected by translation (hence the 0 in the 4th component)
    put @"gl_Position" ((projM !*! viewM !*! modelM) !*^ (Vec4 x y z 1))


---- Fragment -----


type FragmentDefs
  =  '[ "out_col" ':-> Output '[ Location 0 ] (V 4 Float)
      , "in_position" ':-> Input '[ Location 0 ] (V 4 Float)
      , "in_normal"   ':-> Input '[ Location 1 ] (V 4 Float)
      -- , "light_pos"  ':-> Uniform '[ DescriptorSet 0, Binding 1 ]
      --                               ( Struct '[ "val" ':-> V 3 Float ] )
      , "camera_pos" ':-> Uniform '[ DescriptorSet 0, Binding 1 ]
                                    ( Struct '[ "val" ':-> V 3 Float ] )
      -- TODO: MinMax material should be a static material because it only needs to be bound, not written every frame, because we statically know it and only change it when the mesh changes
      , "minmax"     ':-> Uniform '[ DescriptorSet 1, Binding 0 ]
                                  ( Struct '[ "min" ':-> Float
                                            , "max" ':-> Float ] ) -- Careful with alighnemt
      , "gradient" ':-> Texture2D '[ DescriptorSet 1, Binding 1 ] (RGBA8 UNorm)
      , "main"    ':-> EntryPoint '[ OriginLowerLeft ] Fragment
      ]


fragment :: ShaderModule "main" FragmentShader FragmentDefs _
fragment = shader do

    ~(Vec4 px py pz _)  <- get @"in_position"
    ~(Vec4 nx ny nz _)  <- get @"in_normal"
    ~(Vec3 cx cy cz)    <- use @(Name "camera_pos" :.: Name "val")

    -- Color
    min' <- use @(Name "minmax" :.: Name "min")
    max' <- use @(Name "minmax" :.: Name "max")

    let col_frac   = invLerp (norm (Vec3 px py pz)) min' max'

    ~(Vec4 cx' cy' cz' _) <- use @(ImageTexel "gradient") NilOps (Vec2 col_frac col_frac)

    ~(Vec3 colx coly colz) <- blinnPhong $ Vec3 cx' cy' cz'

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

