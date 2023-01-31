{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Ghengin.Shader.Pipeline
  ( ToStructInput
  , ShaderPipeline(ShaderPipeline, (:>->))

  -- | For now, export StructInput and construct ShaderPipeline. Eventually,
  -- export wrapper that also calls ToStructInput
  , pattern FIR.StructInput

  -- * FIR
  , FIR.PrimitiveTopology(..), FIR.PrimitiveConnectedness(..)
  ) where

import Data.Coerce
import Data.Kind
import GHC.TypeLits
import Ghengin.Component.Mesh.Vertex
import qualified FIR
import FIR.ProgramState (EntryPointInfo(..), Definedness(..))
import FIR.Validation.Pipeline (GetExecutionInfo)
import qualified FIR.Definition

newtype ShaderPipeline info = ShaderPipeline (FIR.PipelineStages info ())

-- pattern StructInput :: ∀ {top :: FIR.PrimitiveTopology Nat} {as :: [FIR.LocationSlot Nat Data.Type.Map.:-> Type]}
--                      . (FIR.Known (FIR.PrimitiveTopology Nat) top, FIR.Known FIR.VertexLocationDescriptions (FIR.Syntax.Synonyms.AnnotateLocationsWithBindingAndOffset 0 (FIR.Validation.Formats.ComputeFormats (Data.Type.Map.InsertionSort as))), KnownNat (FIR.Layout.SumSizeOfLocations as))
--                     => ShaderPipeline ('FIR.VertexInputInfo top (FIR.Syntax.Synonyms.AnnotateLocationsWithBindingAndOffset 0 (FIR.Validation.Formats.ComputeFormats (Data.Type.Map.InsertionSort as))) '[ 0 'Data.Type.Map.:-> FIR.Layout.SumSizeOfLocations as])
-- pattern StructInput = ShaderPipeline FIR.StructInput

pattern (:>->) :: ∀ {info :: FIR.PipelineInfo}
                . ()
               => ∀ {shader :: FIR.Shader} {info1 :: FIR.PipelineInfo} {name :: Symbol} {defs :: [Symbol FIR.:-> FIR.Definition]} {endState :: FIR.ProgramState}
                . (info ~ 'FIR.Into info1 '(name, 'EntryPointInfo (GetExecutionInfo shader name endState) (FIR.Definition.Variables defs) 'Defined), FIR.Known FIR.Shader shader)
               => ShaderPipeline info1
               -> FIR.ShaderModule name shader defs endState
               -> ShaderPipeline info
pattern (:>->) x y <- ShaderPipeline ((FIR.:>->) @_ @(_ :: FIR.PipelineInfo) @() (coerce -> x) (y, ()))
  where (:>->) x y  = ShaderPipeline ((FIR.:>->) @_ @(_ :: FIR.PipelineInfo) @() (coerce x)    (y, ()))

type ToStructInput :: Vertex '[Type] -> [FIR.LocationSlot Nat FIR.:-> Type]
type family ToStructInput vx where
  ToStructInput vx = ToStructInput' vx 0

type ToStructInput' :: Vertex '[Type] -> Nat -> [FIR.LocationSlot Nat FIR.:-> Type]
type family ToStructInput' vx n where
  ToStructInput' (Sin x) n = '[ 'FIR.LocationSlot n 0 'FIR.:-> x ]

