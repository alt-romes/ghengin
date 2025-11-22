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
  = '[ "out_position"  ':-> Output '[ Location 0 ] (V 4 Float)
     , "out_normal"    ':-> Output '[ Location 1 ] (V 4 Float)


     , "camera"        ':-> Uniform '[ DescriptorSet 0, Binding 0 ]
                             (Struct [ "view_matrix" ':-> M 4 4 Float
                                     , "proj_matrix" ':-> M 4 4 Float
                                     ])
     , "model"         ':-> Uniform '[ DescriptorSet 2, Binding 0 ]
                             (Struct '[ "m" ':-> M 4 4 Float ])

     , "in_position"   ':-> Input '[ Location 0 ] (V 3 Float)
     , "in_normal"     ':-> Input '[ Location 1 ] (V 3 Float)
     ]


vertex :: VertexShaderModule VertexDefs _
vertex = shader do

    ~(Vec3 x y z)    <- get @"in_position"
    ~(Vec3 nx ny nz) <- get @"in_normal"

    projM  <- use @(Name "camera" :.: Name "proj_matrix")
    viewM  <- use @(Name "camera" :.: Name "view_matrix")
    modelM <- use @(Name "model" :.: Name "m")

    -- Output position and normal in world coordinates
    put @"out_position" ((viewM !*! modelM) !*^ Vec4 x y z 1)
    -- Normal is not a position so shouldn't be affected by translation (hence the 0 in the 4th component)
    -- (For non-uniform transformations we could also pre-compute a "Normal Matrix" rather than using the model one.)
    put @"out_normal"   ({-(transpose . inverse)-} (viewM !*! modelM) !*^ Vec4 nx ny nz 0)

    put @"gl_Position" ((projM !*! viewM !*! modelM) !*^ Vec4 x y z 1)

---- Fragment -----


type FragmentDefs
  =  '[ "in_position" ':-> Input '[ Location 0 ] (V 4 Float)
      , "in_normal"   ':-> Input '[ Location 1 ] (V 4 Float)
      -- , "in_color"    ':-> Uniform '[ Location 2 ] (V 3 Float)

      -- , "camera_pos" ':-> Uniform '[ DescriptorSet 0, Binding 2 ] (Struct '[ "v" ':-> V 3 Float ])

      -- , "minmax"      ':-> Uniform '[ DescriptorSet 1, Binding 0 ]
      --                               ( Struct '[ "min" ':-> Float
      --                                         , "max" ':-> Float ] ) -- Careful with alignment...
      -- , "gradient"    ':-> Texture2D '[ DescriptorSet 1, Binding 1 ] (RGBA8 UNorm)

      ]


fragment :: FragmentShaderModule FragmentDefs _
fragment = shader do

    -- ~(Vec4 px py pz _)  <- get @"in_position"
    -- ~(Vec4 nx ny nz _)  <- get @"in_normal"
    -- ~(Vec3 cx cy cz)    <- get @"in_color"

    -- Color
    -- min' <- use @(Name "minmax" :.: Name "min")
    -- max' <- use @(Name "minmax" :.: Name "max")
    --
    -- let col_frac   = invLerp (norm (Vec3 px py pz)) min' max'
    --
    -- ~(Vec4 cx' cy' cz' _) <- use @(ImageTexel "gradient") NilOps (Vec2 col_frac col_frac)
    --
    -- ~(Vec3 colx coly colz) <- blinnPhong 16 $ Vec3 cx' cy' cz'
    --
    -- def @"camera_pos" @R (Vec3 0 0 0)
    -- ~(Vec3 colx coly colz) <- blinnPhong 16 $ Vec3 0 1 1

    let lightDir  = Vec3 (-0.5) 1 0.5
    let lightCol  = Vec3 0.5 0.5 0.5
    let objectCol = Vec3 0.9 0.1 0.2
    let shininess = 32
    -- allow settings this? let specularStrength = 0.3

    lightValue <- blinnPhong 0.02 shininess lightDir lightCol

    let Vec3 colx coly colz
          = gammaCorrection defaultGamma (lightValue `pointwiseMult` objectCol)

    put @"out_colour" (Vec4 colx coly colz 1)

--- Pipeline ----

-- | Data for each vertex in this shader pipeline
type VertexData =
  '[ Slot 0 0 ':-> V 3 Float -- in pos
   , Slot 1 0 ':-> V 3 Float -- in normal
   ]

shaders :: ShaderPipeline _
shaders
  = ShaderPipeline (StructInput @VertexData @(Triangle List))
    :>-> vertex
    :>-> fragment

----- Utils --------------------------------------------------------------------

invLerp :: FIR.DivisionRing a => a -> a -> a -> a
invLerp value from to = (value FIR.- from) FIR./ (to FIR.- from)

