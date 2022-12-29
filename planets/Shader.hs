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

import qualified Data.IntMap as IM
import qualified Prelude
import Ghengin.Shaders.FIR
import Ghengin.Shaders
import Ghengin.Component.Mesh (VertexN)
import Ghengin (Mat4)

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
      -- , "camera_pos" ':-> Uniform '[ DescriptorSet 0, Binding 1 ]
      --                               ( Struct '[ "val" ':-> V 3 Float ] )
      -- TODO: MinMax material should be a static material because it only needs to be bound, not written every frame, because we statically know it and only change it when the mesh changes
      , "minmax"     ':-> Uniform '[ DescriptorSet 1, Binding 0 ]
                                  ( Struct '[ "min" ':-> Float
                                            , "max" ':-> Float ] ) -- Careful with alighnemt
      , "uniform_col" ':-> Uniform '[ DescriptorSet 1, Binding 1 ]
                                    ( Struct '[ "val" ':-> V 3 Float ] )
      , "main"    ':-> EntryPoint '[ OriginLowerLeft ] Fragment
      ]


fragment :: ShaderModule "main" FragmentShader FragmentDefs _
fragment = shader do

    ~(Vec4 px py pz _)  <- get @"in_position"
    ~(Vec4 nx ny nz _)  <- get @"in_normal"
    -- ~(Vec3 cx cy cz)    <- get @"camera_pos"
    ~(Vec3 bcx bcy bcz) <- use @(Name "uniform_col" :.: Name "val")

    -- Color
    min' <- use @(Name "minmax" :.: Name "min")
    max' <- use @(Name "minmax" :.: Name "max")

    let col_frac = invLerp (norm (Vec3 px py pz)) min' max'
    let col = Vec4 (lerp (bcx * 0.1) bcx col_frac) (lerp (bcy*0.1) bcy col_frac) (lerp (bcz*0.1) bcz col_frac) 1

    -- Light
    let dirToLight         = normalise (Vec4 1 (-3) (-1) 1) :: Code (V 4 Float)
        ambient            = 0.2 :: Code Float
        normalInWorldSpace = normalise (Vec4 nx ny nz 0) :: Code (V 4 Float)
        -- light intensity given by cosine of direction to light and the normal in world space
        lightItensity      = max (dot dirToLight normalInWorldSpace :: Code Float) (0 :: Code Float)


    put @"out_col" (lightItensity *^ col)

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

invLerp :: DivisionRing a => a -> a -> a -> a
invLerp value from to = (value - from) / (to - from)

lerp :: Ring a => a -> a -> a -> a
lerp from to value = from + (to - from) * value

