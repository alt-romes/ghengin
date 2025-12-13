{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Shaders where

import Ghengin.Core.Shader
import FIR hiding ((:>->), ShaderPipeline) -- TODO: Give different names
import Math.Linear

import Ghengin.Camera.Shader.Lighting

-- Descriptor Set #0 for things bound once per pipeline (global pipeline data)
-- Descriptor Set #1 for things bound once per material
-- Descriptor Set #2 for things bound once per object

---- Vertex -----

type VertexDefs
  = '[ "out_position" ':-> Output '[ Location 0 ] (V 4 Float)
     , "out_normal"   ':-> Output '[ Location 1 ] (V 4 Float)

     , "out_position_object" ':-> Output '[ Location 2 ] (V 4 Float)

     , "camera"        ':-> Uniform '[ DescriptorSet 0, Binding 0 ]
                             (Struct [ "view_matrix" ':-> M 4 4 Float
                                     , "proj_matrix" ':-> M 4 4 Float
                                     ])
     , "model"         ':-> Uniform '[ DescriptorSet 2, Binding 0 ]
                             (Struct '[ "m" ':-> M 4 4 Float ])

     , "in_position"   ':-> Input '[ Location 0, Component 0 ] (V 3 Float)
     , "in_normal"     ':-> Input '[ Location 1, Component 0 ] (V 3 Float)
     , "in_uv_y"       ':-> Input '[ Location 0, Component 3 ] (Float)
     ]


vertex :: VertexShaderModule VertexDefs _
vertex = shader do

    ~(Vec3 x y z)    <- get @"in_position"
    ~(Vec3 nx ny nz) <- get @"in_normal"

    projM  <- use @(Name "camera" :.: Name "proj_matrix")
    viewM  <- use @(Name "camera" :.: Name "view_matrix")
    modelM <- use @(Name "model" :.: Name "m")

    -- Output position and normal in view coordinates
    put @"out_position" ((viewM !*! modelM) !*^ Vec4 x y z 1)
    -- Normal is not a position so shouldn't be affected by translation (hence the 0 in the 4th component)
    -- (For non-uniform transformations we could also pre-compute a "Normal Matrix" rather than using this.)
    put @"out_normal"   ((transpose . inverse{-fix for non linear what-}) (viewM !*! modelM) !*^ Vec4 nx ny nz 0)

    -- Output position in object space
    put @"out_position_object" (Vec4 x y z 1)

    put @"gl_Position" ((projM !*! viewM !*! modelM) !*^ Vec4 x y z 1)

---- Fragment -----


type FragmentDefs
  =  '[ "in_position" ':-> Input '[ Location 0 ] (V 4 Float)
      , "in_normal"   ':-> Input '[ Location 1 ] (V 4 Float)

      -- The position in object-space, to get distance from center of planet
      , "in_position_object" ':-> Input '[ Location 2 ] (V 4 Float)

      , "minmax"      ':-> Uniform '[ DescriptorSet 1, Binding 0 ]
                                    ( Struct '[ "min" ':-> Float
                                              , "max" ':-> Float ] )

      , "gradient"    ':-> Texture2D '[ DescriptorSet 1, Binding 1 ] (RGBA8 UNorm)
      ]


fragment :: FragmentShaderModule FragmentDefs _
fragment = shader do

    ~(Vec4 pox poy poz _)  <- get @"in_position_object"

    -- ~(Vec4 nx ny nz _)  <- get @"in_normal"
    -- ~(Vec3 cx cy cz)    <- get @"in_color"

    -- Color
    min' <- use @(Name "minmax" :.: Name "min")
    max' <- use @(Name "minmax" :.: Name "max")

    let col_frac = invLerp (norm (Vec3 pox poy poz)) min' max'
    let (Vec3 _ n_poy _) = normalise (Vec3 pox poy poz)

    ~(Vec4 cx cy cz _) <- use @(ImageTexel "gradient") NilOps (Vec2 col_frac (n_poy*0.5 + 0.5))

    let lightDir  = Vec3 (-0.5) 1 0.5
    let lightCol  = Vec3 1 1 1
    let objectCol = Vec3 cx cy cz
    let specularStrength = 2
    let shininess = 8

    lightValue <- blinnPhong 0.02 shininess specularStrength lightDir lightCol

    let Vec3 colx coly colz
          = gammaCorrection defaultGamma (lightValue `pointwiseMult` objectCol)

    put @"out_colour" (Vec4 colx coly colz 1)

--- Pipeline ----

-- | Data for each vertex in this shader pipeline
type VertexData =
  '[ Slot 0 0 ':-> V 3 Float -- in pos
   , Slot 1 0 ':-> V 3 Float -- in normal
   , Slot 0 3 ':-> Float -- uv y position
   ]

shaders :: ShaderPipeline _
shaders
  = ShaderPipeline (StructInput @VertexData @(Triangle List))
    :>-> vertex
    :>-> fragment

----- Utils --------------------------------------------------------------------

invLerp :: FIR.DivisionRing a => a -> a -> a -> a
invLerp value from to = (value FIR.- from) FIR./ (to FIR.- from)

