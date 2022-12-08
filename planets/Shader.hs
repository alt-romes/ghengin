{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
module Shader where

import Ghengin.Shaders.FIR

---- Vertex -----

type VertexDefs
  = '[ "in_position" ':-> Input '[ Location 0 ] (V 3 Float)
     , "in_normal"   ':-> Input '[ Location 1 ] (V 3 Float)
     , "out_position" ':-> Output '[ Location 0 ] (V 3 Float) 
     , "push"        ':-> PushConstant '[] (Struct '[ "model" ':-> M 4 4 Float ])
     , "ubo"         ':-> Uniform '[ Binding 0, DescriptorSet 0 ]
                                  ( Struct '[ "view" ':-> M 4 4 Float
                                            , "proj" ':-> M 4 4 Float ] )
     , "main"        ':-> EntryPoint '[] Vertex
     ]


vertex :: Module VertexDefs
vertex = Module $ entryPoint @"main" @Vertex do
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

      -- TODO: How to (automatically) take into consideration that min max has to be bound (almost?) only once?
      , "minmax"     ':-> Uniform '[ Binding 1, DescriptorSet 0 ]
                                  ( Struct '[ "min" ':-> Float, "max" ':-> Float ] ) -- Careful with alighnemt
      , "main"    ':-> EntryPoint '[ OriginLowerLeft ] Fragment
      ]


fragment :: Module FragmentDefs
fragment = Module $

  entryPoint @"main" @Fragment do

    pos <- get @"in_position"

    min <- use @(Name "minmax" :.: Name "min")
    max <- use @(Name "minmax" :.: Name "max")

    let col = invLerp (norm pos) min max 

    put @"out_col" (Vec4 col col col 1)


invLerp :: DivisionRing a => a -> a -> a -> a
invLerp value from to = (value - from) / (to - from)

