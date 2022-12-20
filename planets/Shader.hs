{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE QualifiedDo      #-}
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
import Ghengin.Utils (SomeStorable(..))
import Ghengin (Mat4)

-- Descriptor Set #0 for things bound once per pipeline (global pipeline data)
-- Descriptor Set #1 for things bound once per material
-- Descriptor Set #2 for things bound once per object

---- Vertex -----

type VertexDefs
  = '[ "in_position" ':-> Input '[ Location 0 ] (V 3 Float)
     , "in_normal"   ':-> Input '[ Location 1 ] (V 3 Float)
     , "out_position" ':-> Output '[ Location 0 ] (V 3 Float) 
     , "push"        ':-> PushConstant '[] (Struct '[ "model" ':-> M 4 4 Float ])
     , "ubo"         ':-> Uniform '[ DescriptorSet 1, Binding 0 ]
                                  ( Struct '[ "view" ':-> M 4 4 Float
                                            , "proj" ':-> M 4 4 Float ] )
     , "main"        ':-> EntryPoint '[] Vertex
     ]


vertex :: ShaderModule "main" VertexShader VertexDefs _
vertex = shader do
    let dirToLight = normalise (Vec4 1 (-3) (-1) 1) :: Code (V 4 Float)
        ambient    = 0.2
    ~(Vec3 x y z)    <- get @"in_position"
    ~(Vec3 nx ny nz) <- get @"in_normal"
    modelM <- use @(Name "push" :.: Name "model")
    viewM  <- use @(Name "ubo" :.: Name "view")
    projM  <- use @(Name "ubo" :.: Name "proj")

    let normalInWorldSpace = normalise (modelM !*^ (Vec4 nx ny nz 0)) -- Normal is not a position so shouldn't be affected by translation (hence the 0 in the 4th component)
        lightItensity      = ambient + max (dot dirToLight normalInWorldSpace) 0 -- light intensity given by cosine of direction to light and the normal in world space

    put @"out_position" (Vec3 x y z)
    put @"gl_Position" ((projM !*! viewM !*! modelM) !*^ (Vec4 x y z 1))


---- Fragment -----


type FragmentDefs
  =  '[ "out_col" ':-> Output  '[ Location 0                 ] (V 4 Float)
      , "in_position" ':-> Input '[ Location 0 ] (V 3 Float)

      -- TODO: How to (automatically) take into consideration that min max has to be bound (almost?) only once (the meshes are known statically)?
      , "minmax"     ':-> Uniform '[ DescriptorSet 0, Binding 0 ]
                                  ( Struct '[ "min" ':-> Float, "max" ':-> Float ] ) -- Careful with alighnemt
      , "main"    ':-> EntryPoint '[ OriginLowerLeft ] Fragment
      ]


fragment :: ShaderModule "main" FragmentShader FragmentDefs _
fragment = shader do

    pos <- get @"in_position"

    min <- use @(Name "minmax" :.: Name "min")
    max <- use @(Name "minmax" :.: Name "max")

    let col = invLerp (norm pos) min max 

    put @"out_col" (Vec4 col col col 1)

--- Pipeline ----

type VertexData =
  '[ Slot 0 0 ':-> V 3 Float -- in pos
   , Slot 1 0 ':-> V 3 Float -- in normal
   ]

shaderPipeline :: GShaderPipeline _
shaderPipeline
  = StructInput @VertexData @(Triangle List)
    :>-> (vertex, IM.singleton 1 (IM.singleton 0 (SomeStorable @(VertexN Mat4 2))))
    :>-> (fragment, IM.singleton 0 (IM.singleton 0 (SomeStorable @(VertexN Float 2))))



invLerp :: DivisionRing a => a -> a -> a -> a
invLerp value from to = (value - from) / (to - from)

