{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE BlockArguments   #-} -- cleaner syntax for 'do' blocks (optional)
{-# LANGUAGE DataKinds        #-} -- for datatype promotion and type-level literals
{-# LANGUAGE RebindableSyntax #-} -- 'do' notation to create ASTs with indexed monads
{-# LANGUAGE TypeApplications #-} -- to specify type-level arguments
{-# LANGUAGE TypeOperators    #-} -- for type operators such as ':->, which stands for key/value assignment
{-# LANGUAGE ViewPatterns     #-}
module Ghengin.Shaders.SimpleShader (vertex, fragment, shaderPipeline) where

import qualified Prelude
import FIR
import FIR.Syntax.Labels
import Math.Linear -- for vectors
import Ghengin.Shaders

-------------------
-- Vertex shader --
-------------------

type VertexDefs
  = '[ "in_position" ':-> Input '[ Location 0 ] (V 3 Float)
     , "in_normal"  ':-> Input '[ Location 1 ] (V 3 Float)
     , "in_colour"  ':-> Input '[ Location 2 ] (V 3 Float)
     , "out_colour" ':-> Output '[ Location 0 ] (V 4 Float)
     , "push"       ':-> PushConstant '[] (Struct '[ "mat" ':-> M 4 4 Float ])
     , "ubo"        ':-> Uniform '[ Binding 0, DescriptorSet 0 ]
                                  ( Struct '[ "view" ':-> M 4 4 Float
                                            , "proj" ':-> M 4 4 Float ] ) -- column-major matrix (Vulkan convention)

     , "main"       ':-> EntryPoint '[] Vertex
     ]

vertex :: ShaderModule "main" VertexShader VertexDefs _
vertex = shader do
  let dirToLight = normalise (Vec4 1 (-3) (-1) 1) :: Code (V 4 Float)
      ambient    = 0.2
  ~(Vec3 x y z) <- get @"in_position"
  ~(Vec3 nx ny nz) <- get @"in_normal"
  ~(Vec3 r g b) <- get @"in_colour"
  modelM <- use @(Name "push" :.: Name "mat")
  viewM  <- use @(Name "ubo" :.: Name "view")
  projM  <- use @(Name "ubo" :.: Name "proj")

  let normalInWorldSpace = normalise (modelM !*^ (Vec4 nx ny nz 0)) -- Normal is not a position so shouldn't be affected by translation (hence the 0 in the 4th component)
      lightItensity      = ambient + max (dot dirToLight normalInWorldSpace) 0 -- light intensity given by cosine of direction to light and the normal in world space

  _ <- put @"out_colour" (lightItensity *^ Vec4 r g b 1)
  put @"gl_Position" ((projM !*! viewM !*! modelM) !*^ (Vec4 x y z 1))


-------------------
-- Frag shader   --
-------------------

-- Specify the input/output of the shader, with memory locations (and other interface parameters).
-- This consists of a type-level list of top-level definitions.
type FragmentDefs
  =  '[ "in_col"  ':-> Input      '[ Location 0 ] (V 4 Float)
      , "out_col" ':-> Output     '[ Location 0                 ] (V 4 Float)   -- output (varying) of type V 4 Float and memory location 0
      , "main"    ':-> EntryPoint '[ OriginLowerLeft           ] Fragment      -- fragment shader stage (using standard Cartesian coordinates)
      ]
      --  "in_pos"  ':-> Input      '[ Location 0                 ] (V 2 Float)   -- input  (varying) of type V 2 Float and memory location 0
      -- , "image"   ':-> Texture2D  '[ DescriptorSet 0, Binding 0 ] (RGBA8 UNorm) -- input sampled image (provided as binding 0 of descriptor set 0)

fragment :: ShaderModule "main" FragmentShader FragmentDefs _
fragment = shader do
  ~(Vec4 r g b a) <- get @"in_col"
  put @"out_col" (Vec4 r g b a)


type VertexData =
  '[ Slot 0 0 ':-> V 3 Float -- in pos
   , Slot 1 0 ':-> V 3 Float -- in normal
   , Slot 2 0 ':-> V 3 Float -- in color
   ]

shaderPipeline :: ShaderPipeline ()
shaderPipeline = ShaderPipeline
         $    StructInput @VertexData @(Triangle List)
         :>-> (vertex, ())
         :>-> (fragment, ())
