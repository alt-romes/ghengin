{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE NamedWildCards        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module Shaders where

-- base
import Data.Foldable
  ( sequence_ )
import Data.Maybe
  ( fromJust )
import GHC.TypeNats
  ( KnownNat )

-- filepath
import System.FilePath
  ( (</>) )

-- text-short
import Data.Text.Short
  ( ShortText )

-- vector-sized
import qualified Data.Vector.Sized as Vector
  ( fromList )

-- fir
import FIR
import FIR.Syntax.Labels
import Math.Linear

-- ghengin
import qualified Ghengin.Core.Shader as G

-- ghengin-games
import Common.Shader

--------------------------------------------------------------------------------
-- * Vertex Shader
--------------------------------------------------------------------------------
-- The input to our vertex shader are vertices along the XY plane, which we will
-- displace according to the ocean model. We receive the vertices in `Location 0`
-- and they are `V 2 Float`s (points in the XY plane).

-- ROMES:TODO: If this were V 3 Float, why don't we get an error? Shouldn't in-position match VertexInput?
type VertexDefs =
  '[ "in_position"  ':-> Input      '[ Location 0 ] (V 3 Float)
   ]

type VertexInput
  = '[ Slot 0 0 ':-> V 3 Float ]
 
vertex :: G.VertexShaderModule VertexDefs _
vertex = shader do
    ~(Vec3 x y _) <- get @"in_position"
    put @"gl_Position" (Vec4 x y 0 1)

--------------------------------------------------------------------------------
-- * Fragment Shader
--------------------------------------------------------------------------------

fragment :: G.FragmentShaderModule '[] _
fragment = shader do
  #out_colour .= Vec4 1 0 0 1

--------------------------------------------------------------------------------
-- * Pipeline
--------------------------------------------------------------------------------

shaderPipeline :: G.ShaderPipeline _
shaderPipeline
  = G.ShaderPipeline (StructInput @VertexInput @(Triangle List))
  G.:>-> vertex
  G.:>-> fragment

