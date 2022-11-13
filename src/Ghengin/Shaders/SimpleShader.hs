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
  = '[ "main"   ':-> EntryPoint '[] Vertex
     ]

vertex :: Module VertexDefs
vertex = Module $ entryPoint @"main" @Vertex do
  i <- #gl_VertexIndex
  let (Vec2 x y) = view @(AnIndex (Code Word32)) i (Lit vecs :: Code (Array 3 (V 2 Float)))
  put @"gl_Position" (Vec4 x y 0 1)
    where
      vecs :: Array 3 (V 2 Float)
      vecs = MkArray . fromJust . Vector.fromList $
        [ V2 (-0.5) (-0.5)
        , V2 0.5 0.5
        , V2 (-0.5) 0.5
        ]

-------------------
-- Frag shader   --
-------------------

-- Specify the input/output of the shader, with memory locations (and other interface parameters).
-- This consists of a type-level list of top-level definitions.
type FragmentDefs
  =  '[ "out_col" ':-> Output     '[ Location 0                 ] (V 4 Float)   -- output (varying) of type V 4 Float and memory location 0
      , "main"    ':-> EntryPoint '[ OriginLowerLeft            ] Fragment      -- fragment shader stage (using standard Cartesian coordinates)
      ]
      --  "in_pos"  ':-> Input      '[ Location 0                 ] (V 2 Float)   -- input  (varying) of type V 2 Float and memory location 0
      -- , "image"   ':-> Texture2D  '[ DescriptorSet 0, Binding 0 ] (RGBA8 UNorm) -- input sampled image (provided as binding 0 of descriptor set 0)

fragment :: Module FragmentDefs
fragment = Module $
  entryPoint @"main" @Fragment do
    put @"out_col" (Vec4 1 0 0 1)


    -- pos <- get @"in_pos"
    -- col <- use @(ImageTexel "image") NilOps pos

