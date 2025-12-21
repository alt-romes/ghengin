{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

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

-- fir-examples
-- import FIR.Examples.Paths
--   ( shaderDir )

-- ghengin
import qualified Ghengin.Core.Shader as G

------------------------------------------------
-- pipeline input

type VertexInput
  = '[ Slot 0 0 ':-> V 3 Float ]

-------------------------------------
-- vertex shader

type VertexDefs =
  '[ "in_position"  ':-> Input      '[ Location 0 ] (V 3 Float)
   ]

vertex :: G.VertexShaderModule VertexDefs _
vertex = shader do
    ~(Vec3 x y z) <- get @"in_position"
    put @"gl_Position" (Vec4 x y z 1)

------------------------------------------------
-- fragment shader

type FragmentDefs =
  '[ "ubo"         ':-> Uniform '[ DescriptorSet 0, Binding 0 ]
                        ( Struct '[ "mousePos" ':-> V 2 Float ] )
   ]

maxDepth :: Code Word32
maxDepth = 256

xSamples, ySamples :: Code Word32
xSamples = 4
ySamples = 4

xWidth, yWidth :: Code Float
xWidth = recip . fromIntegral $ xSamples
yWidth = recip . fromIntegral $ ySamples

fragment :: (Float, Float) -> G.FragmentShaderModule FragmentDefs _
fragment wsize = shader do

    ~( Vec4 x y _ _ ) <- #gl_FragCoord
    ( mkRescaledComplex wsize -> c ) <- use @(Name "ubo" :.: Name "mousePos")

    #total #= ( 0 :: Code Word32 )

    supersamplingLoop \ xNo yNo -> locally do 

      let
        dx, dy :: Code Float
        dx = ( fromIntegral xNo + 0.5 ) * xWidth - 0.5
        dy = ( fromIntegral yNo + 0.5 ) * xWidth - 0.5

      #z #= codeComplex ( mkRescaledComplex wsize ( Vec2 (x + dx) (y + dy) ) )
      #depth #= ( 0 :: Code Word32 )

      loop do
        ( CodeComplex -> z ) <- #z
        depth <- #depth
        if magnitudeIsGT z 4 || depth > maxDepth
        then break @1
        else do
          #z     .= codeComplex ( z * z + c )
          #depth .= depth + 1

      depth <- #depth
      #total %= (+depth)

      pure ()

    total <- #total
    let t = log ( fromIntegral total * xWidth * yWidth )
          / log ( fromIntegral maxDepth )

    let col = gradient t (Lit sunset)

    #out_colour .= col

magnitudeIsGT :: CodeComplex Float -> Code Float -> Code Bool
magnitudeIsGT (CodeComplex z) b = dot z z > b*b

mkRescaledComplex :: (Float, Float) -> Code (V 2 Float) -> CodeComplex Float
mkRescaledComplex (wwidth, wheight) (Vec2 x y) =
  ( (x - (Lit wwidth / 2)) / 250 ) :+: ( (y - (Lit wheight / 2)) / 250 )

supersamplingLoop
  :: _ => ( Code Word32 -> Code Word32 -> Program _s _s () )
  -> Program _s _s ()
supersamplingLoop prog = locally do
  #ssX #= (0 :: Code Word32)
  #ssY #= (0 :: Code Word32)
  while ( ( xSamples > ) <<$>> #ssX ) do
    ssX <- #ssX
    #ssY .= 0
    while ( ( ySamples > ) <<$>> #ssY ) do
      ssY <- #ssY
      embed $ prog ssX ssY
      #ssY %= (+1)
    #ssX %= (+1)
  pure ()

gradient :: forall n. KnownNat n
         => Code Float
         -> Code (Array n (V 4 Float))
         -> Code (V 4 Float)
gradient t colors
  =   ( (1-s) *^ ( view @(AnIndex _)  i    colors ) )
  ^+^ (    s  *^ ( view @(AnIndex _) (i+1) colors ) )
  where n :: Code Float
        n = Lit . fromIntegral $ knownValue @n
        i :: Code Word32
        i = floor ( (n-1) * t )
        s :: Code Float
        s = (n-1) * t - fromIntegral i


sunset :: Array 9 (V 4 Float)
sunset = MkArray . fromJust . Vector.fromList $
       [ V4 0    0    0    0
       , V4 0.28 0.1  0.38 1
       , V4 0.58 0.2  0.38 1
       , V4 0.83 0.3  0.22 1
       , V4 0.98 0.45 0.05 1
       , V4 0.99 0.62 0.2  1
       , V4 1    0.78 0.31 1
       , V4 1    0.91 0.6  1
       , V4 1    1    1    1
       ]
------------------------------------------------
-- compiling

shaderPipeline :: (Word32, Word32) -> G.ShaderPipeline _
shaderPipeline (fromIntegral -> x, fromIntegral -> y)
  = G.ShaderPipeline (StructInput @VertexInput @(Triangle List))
  G.:>-> vertex
  G.:>-> fragment (x,y)
