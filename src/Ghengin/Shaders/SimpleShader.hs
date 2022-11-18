{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE BlockArguments   #-} -- cleaner syntax for 'do' blocks (optional)
{-# LANGUAGE DataKinds        #-} -- for datatype promotion and type-level literals
{-# LANGUAGE RebindableSyntax #-} -- 'do' notation to create ASTs with indexed monads
{-# LANGUAGE TypeApplications #-} -- to specify type-level arguments
{-# LANGUAGE TypeOperators    #-} -- for type operators such as ':->, which stands for key/value assignment
{-# LANGUAGE ViewPatterns     #-}
module Ghengin.Shaders.SimpleShader (vertex, fragment) where

import Data.Maybe
import qualified Data.Vector.Sized as Vector
  ( fromList )

import FIR
import FIR.Syntax.Labels
import Math.Linear -- for vectors

-------------------
-- Vertex shader --
-------------------

type VertexDefs
  = '[ "in_position" ':-> Input '[ Location 0 ] (V 3 Float)
     , "in_normal" ':-> Input '[ Location 1 ] (V 3 Float)
     , "in_colour" ':-> Input '[ Location 2 ] (V 3 Float)
     , "out_colour" ':-> Output '[ Location 0 ] (V 4 Float)
     , "main"   ':-> EntryPoint '[] Vertex
     ]

vertex :: Module VertexDefs
vertex = Module $ entryPoint @"main" @Vertex do
  ~(Vec3 x y z) <- get @"in_position"
  ~(Vec3 r g b) <- get @"in_colour"
  put @"out_colour"  (Vec4 r g b 1)
  put @"gl_Position" (Vec4 x y z 1)

-------------------
-- Frag shader   --
-------------------

-- Specify the input/output of the shader, with memory locations (and other interface parameters).
-- This consists of a type-level list of top-level definitions.
type FragmentDefs
  =  '[ "in_col"  ':-> Input      '[ Location 0 ] (V 4 Float)
      , "out_col" ':-> Output     '[ Location 0                 ] (V 4 Float)   -- output (varying) of type V 4 Float and memory location 0
      , "main"    ':-> EntryPoint '[ OriginLowerLeft            ] Fragment      -- fragment shader stage (using standard Cartesian coordinates)
      ]
      --  "in_pos"  ':-> Input      '[ Location 0                 ] (V 2 Float)   -- input  (varying) of type V 2 Float and memory location 0
      -- , "image"   ':-> Texture2D  '[ DescriptorSet 0, Binding 0 ] (RGBA8 UNorm) -- input sampled image (provided as binding 0 of descriptor set 0)

fragment :: Module FragmentDefs
fragment = Module $
  entryPoint @"main" @Fragment do
    ~(Vec4 r g b a) <- get @"in_col"
    put @"out_col" (Vec4 r g b a)

