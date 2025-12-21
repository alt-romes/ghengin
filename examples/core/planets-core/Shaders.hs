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

     , "out_biome_col" ':-> Output '[ Location 2, Component 0 ] (Float)
     , "out_elevation" ':-> Output '[ Location 2, Component 1 ] (Float)

     , "camera"        ':-> Uniform '[ DescriptorSet 0, Binding 0 ]
                             (Struct [ "view_matrix" ':-> M 4 4 Float
                                     , "proj_matrix" ':-> M 4 4 Float
                                     ])
     , "model"         ':-> Uniform '[ DescriptorSet 2, Binding 0 ]
                             (Struct '[ "m" ':-> M 4 4 Float ])

     , "in_position"   ':-> Input '[ Location 0, Component 0 ] (V 3 Float)
     , "in_normal"     ':-> Input '[ Location 1, Component 0 ] (V 3 Float)
     , "in_biome_col"  ':-> Input '[ Location 0, Component 3 ] (Float)
     , "in_elevation"  ':-> Input '[ Location 1, Component 3 ] (Float)
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

    put @"out_biome_col" =<< get @"in_biome_col"
    put @"out_elevation" =<< get @"in_elevation"

    put @"gl_Position" ((projM !*! viewM !*! modelM) !*^ Vec4 x y z 1)

---- Fragment -----


type FragmentDefs
  =  '[ "in_position"  ':-> Input '[ Location 0 ] (V 4 Float)
      , "in_normal"    ':-> Input '[ Location 1 ] (V 4 Float)

     ,  "in_biome_col" ':-> Input '[ Location 2, Component 0 ] (Float)
     ,  "in_elevation" ':-> Input '[ Location 2, Component 1 ] (Float)

      , "minmax"       ':-> Uniform '[ DescriptorSet 1, Binding 0 ]
                                    ( Struct '[ "min" ':-> Float
                                              , "max" ':-> Float ] )

      , "gradient"     ':-> Texture2D '[ DescriptorSet 1, Binding 1 ] (RGBA8 UNorm)
      ]


fragment :: FragmentShaderModule FragmentDefs _
fragment = shader do

    -- Color
    min' <- use @(Name "minmax" :.: Name "min")
    max' <- use @(Name "minmax" :.: Name "max")
    biome_col_v <- get @"in_biome_col"
    elevation_v <- get @"in_elevation"

    let ocean_col    = invLerp elevation_v min' 0
    let terrain_col  = invLerp elevation_v 0 max'
    let ocean_uv_x   = lerp 0 0.5 ocean_col   -- t for first half of texture
    let terrain_uv_x = lerp 0.5 1 terrain_col -- t for second half of texture

    -- Weighted add to construct final uv_x into texture
    let mask = floor ocean_col -- 1 for terrain (elevation_v>=0), 0 for ocean (elevation_v<0)
    let uv_x = (ocean_uv_x*(1-mask)) + (terrain_uv_x*mask)

    ~(Vec4 cx cy cz _) <- use @(ImageTexel "gradient") NilOps (Vec2 uv_x biome_col_v)

    let lightDir  = Vec3 (-0.5) 1 0.5
    let lightCol  = Vec3 1 1 1
    let objectCol = Vec3 cx cy cz
    let specularStrength = 2*(1-mask)
    let shininess = 1+127*(1-mask) -- only shining on the ocean

    lightValue <- blinnPhong 0.02 shininess specularStrength lightDir lightCol

    let Vec3 colx coly colz
          = gammaCorrection defaultGamma (lightValue `pointwiseMult` objectCol)

    put @"out_colour" (Vec4 colx coly colz 1)

--- Pipeline ----

-- | Data for each vertex in this shader pipeline
type VertexData =
  '[ Slot 0 0 ':-> V 3 Float -- in pos
   , Slot 0 3 ':-> Float -- in biome frac
   , Slot 1 0 ':-> V 3 Float -- in normal
   , Slot 1 3 ':-> Float -- in elevation
   ]

shaders :: ShaderPipeline _
shaders
  = ShaderPipeline (StructInput @VertexData @(Triangle List))
    :>-> vertex
    :>-> fragment

----- Utils --------------------------------------------------------------------

invLerp :: (Ord a, FIR.DivisionRing a) => a -> a -> a -> a
invLerp value from to = max 0 (min 1 ((value - from) / (to - from)))

lerp :: (Semiring a, CancellativeAdditiveMonoid a) => a -> a -> a -> a
lerp v0 v1 t = v0 + t * (v1 - v0);
-- lerp v0 v1 t = (1 - t) * v0 + t * v1;
