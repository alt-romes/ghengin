{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Shaders where

import Ghengin.Core.Shader
import Geomancy.Vec3 hiding (dot)
import Geomancy.Mat4
import FIR hiding ((:>->), ShaderPipeline) -- TODO: Give different names
import Math.Linear

-- Descriptor Set #0 for things bound once per pipeline (global pipeline data)
-- Descriptor Set #1 for things bound once per material
-- Descriptor Set #2 for things bound once per object

---- Vertex -----

type VertexDefs
  = '[ "out_position"  ':-> Output '[ Location 0 ] (V 4 Float)
     , "out_normal"    ':-> Output '[ Location 1 ] (V 4 Float)

     , "proj"         ':-> Uniform '[ DescriptorSet 0, Binding 0 ] (Struct '[ "m" ':-> M 4 4 Float ])
     , "view"         ':-> Uniform '[ DescriptorSet 0, Binding 1 ] (Struct '[ "m" ':-> M 4 4 Float ])

     , "in_position"   ':-> Input '[ Location 0 ] (V 3 Float)
     , "in_normal"     ':-> Input '[ Location 1 ] (V 3 Float)
     , "in_color"      ':-> Input '[ Location 2 ] (V 3 Float)
     ]


vertex :: VertexShaderModule VertexDefs _
vertex = shader do

    ~(Vec3 x y z)    <- get @"in_position"
    ~(Vec3 nx ny nz) <- get @"in_normal"

    -- modelM <- use @(Name "push" :.: Name "model")

    -- Output position and normal in world coordinates
    -- put @"out_position" (modelM !*^ (Vec4 x y z 1))
    -- put @"out_normal"   (modelM !*^ (Vec4 nx ny nz 0))
    -- Normal is not a position so shouldn't be affected by translation (hence the 0 in the 4th component)

    -- Temporarily...
    put @"out_position" (Vec4 x y z 1)
    put @"out_normal"   (Vec4 nx ny nz 0)

    put @"gl_Position" =<< applyMVP (Vec4 x y z 1)

---- Fragment -----


type FragmentDefs
  =  '[ "in_position" ':-> Input '[ Location 0 ] (V 4 Float)
      , "in_normal"   ':-> Input '[ Location 1 ] (V 4 Float)

      , "camera_pos" ':-> Uniform '[ DescriptorSet 0, Binding 2 ] (Struct '[ "v" ':-> V 3 Float ]) -- try changing to CameraPos, maybe Canonicalization will catch it.

      , "minmax"      ':-> Uniform '[ DescriptorSet 1, Binding 0 ]
                                    ( Struct '[ "min" ':-> Float
                                              , "max" ':-> Float ] ) -- Careful with alignment...
      , "gradient"    ':-> Texture2D '[ DescriptorSet 1, Binding 1 ] (RGBA8 UNorm)

      ]


fragment :: FragmentShaderModule FragmentDefs _
fragment = shader do

    ~(Vec4 px py pz _)  <- get @"in_position"

    -- Color
    min' <- use @(Name "minmax" :.: Name "min")
    max' <- use @(Name "minmax" :.: Name "max")

    let col_frac   = invLerp (norm (Vec3 px py pz)) min' max'

    ~(Vec4 cx' cy' cz' _) <- use @(ImageTexel "gradient") NilOps (Vec2 col_frac col_frac)

    ~(Vec3 colx coly colz) <- blinnPhong 16 $ Vec3 cx' cy' cz'

    put @"out_colour" (Vec4 colx coly colz 1)

--- Pipeline ----

-- | Data for each vertex in this shader pipeline
type VertexData =
  '[ Slot 0 0 ':-> V 3 Float -- in pos
   , Slot 1 0 ':-> V 3 Float -- in normal
   , Slot 2 0 ':-> V 3 Float -- in color
   ]

shaders :: ShaderPipeline _
shaders
  = ShaderPipeline (StructInput @VertexData @(Triangle List))
    :>-> vertex
    :>-> fragment

----- Utils --------------------------------------------------------------------

blinnPhong :: ∀ π
            . ( V 4 Float ~ Has "in_position" π
              , V 4 Float ~ Has "in_normal" π

              , CanGet "in_position" π
              , CanGet "in_normal" π

              , CanGet "camera_pos" π

              , _ -- extra constraints (wildcard at the end)
              )
           => Code Float -> Code (V 3 Float) -> Program π π (Code (V 3 Float))
blinnPhong specularity col = do

    ~(Vec4 px py pz _) <- get @"in_position" @(V 4 Float) @π
    ~(Vec4 nx ny nz _) <- get @"in_normal"   @(V 4 Float) @π
    ~(Vec3 cx cy cz)   <- use @(Name "camera_pos" :.: Name "v")

    let

        -- Light
        viewDir    = normalise (Vec3 cx cy cz ^-^ Vec3 px py pz)
        dirToLight = normalise (Vec3 1 (-3) (-1))
        ambient    = 0.05 *^ col
        normal     = normalise (Vec3 nx ny nz)
        -- light intensity given by cosine of direction to light and the normal in world space
        diffuse    = (max (dot dirToLight normal) 0) *^ col
        halfwayDir = normalise (dirToLight ^+^ viewDir)
        specular   = ((max (dot halfwayDir normal) 0) ** specularity) *^ (Vec3 0.3 0.3 0.3 {- bright light -})

        Vec3 colx coly colz = ambient ^+^ diffuse ^+^ specular

     in
        pure $ Vec3 colx coly colz

applyMVP :: ∀ π. ( CanGet "proj" π
                 , CanGet "view"  π
                 , _ -- extra constraints
                 )
              => (Code (V 4 Float)) -> Program π π (Code (V 4 Float))
applyMVP vec = do

  -- modelM <- use @(Name "push" :.: Name "model"  :: Optic '[] π (M 4 4 Float))
  projM <- use @(Name "proj" :.: Name "m"  :: Optic '[] π (M 4 4 Float))
  viewM <- use @(Name "view" :.: Name "m"  :: Optic '[] π (M 4 4 Float))

  pure $ (projM !*! viewM) !*^ vec

invLerp :: FIR.DivisionRing a => a -> a -> a -> a
invLerp value from to = (value FIR.- from) FIR./ (to FIR.- from)

