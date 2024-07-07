{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Ghengin.Core.Shader.Pipeline
  ( ToStructInput
  , ShaderPipeline(ShaderPipeline, (:>->))

  -- * FIR re-exports

  -- | For now, export StructInput and construct ShaderPipeline. Eventually,
  -- export wrapper that also calls ToStructInput.
  --
  -- ToStructInput is used to construct a type argument to StructInput
  , pattern FIR.StructInput

  -- Other types needed to construct the shader pipeline description
  , FIR.PrimitiveTopology(..), FIR.PrimitiveConnectedness(..)
  ) where

import Prelude (type (~))
import Data.Coerce
import Data.Kind
import FIR.ProgramState (EntryPointInfo(..), Definedness(..))
import FIR.Validation.Pipeline (GetExecutionInfo)
import GHC.TypeLits
import qualified FIR
import qualified FIR.Definition
import Ghengin.Core.Shader.Canonical

newtype ShaderPipeline info = ShaderPipeline (FIR.PipelineStages info ())

pattern (:>->) :: ∀ {info :: FIR.PipelineInfo}
                . ()
               => ∀ {shader :: FIR.Shader} {info1 :: FIR.PipelineInfo} {name :: Symbol} {defs :: [Symbol FIR.:-> FIR.Definition]} {endState :: FIR.ProgramState}
                . (info ~ 'FIR.Into info1 '(name, 'EntryPointInfo (GetExecutionInfo shader name endState) (FIR.Definition.Variables defs) 'Defined), FIR.Known FIR.Shader shader)
               => ShaderPipeline info1
               -> FIR.ShaderModule name shader defs endState
               -> ShaderPipeline info
pattern (:>->) x y <- ShaderPipeline ((FIR.:>->) @_ @(_ :: FIR.PipelineInfo) @() (coerce -> x) (y, ()))
  where (:>->) x y  = ShaderPipeline ((FIR.:>->) @_ @(_ :: FIR.PipelineInfo) @() (coerce x)    (y, ()))

-- TODO: Alternatively explain FIR.Layout.Validation as the non-wrapped way of
-- defining the inputs in slots
type ToStructInput :: [Type] -> [FIR.LocationSlot Nat FIR.:-> Type]
type family ToStructInput vx where
  ToStructInput vs = ToStructInput' vs 0

type ToStructInput' :: [Type] -> Nat -> [FIR.LocationSlot Nat FIR.:-> Type]
type family ToStructInput' vx n where
  ToStructInput' '[] n = '[ ]
  ToStructInput' (x ': xs) n = ('FIR.LocationSlot n 0 'FIR.:-> CanonicalType x) ': ToStructInput' xs (n+1)
  -- ToStructInput' (x ': xs) n = TypeError (Text "Ghengin: Unknown type for vertex data " :<>: ShowType x)


-- pattern StructInput :: ∀ {as :: [FIR.LocationSlot Nat Data.Type.Map.:-> Type]}
--                      . (FIR.Known (FIR.PrimitiveTopology Nat) (FIR.Triangle FIR.List), FIR.Known FIR.VertexLocationDescriptions (AnnotateLocationsWithBindingAndOffset 0 (FIR.Validation.Formats.ComputeFormats (Data.Type.Map.InsertionSort as))), KnownNat (SumSizeOfLocations as))
--                     => ShaderPipeline ('FIR.VertexInputInfo (FIR.Triangle FIR.List) (AnnotateLocationsWithBindingAndOffset 0 (FIR.Validation.Formats.ComputeFormats (Data.Type.Map.InsertionSort as))) '[ 0 'Data.Type.Map.:-> SumSizeOfLocations as])
-- pattern StructInput <- ShaderPipeline FIR.StructInput

-- | Inline 'AnnotateLocationsWithBindingAndOffset' definition from FIR to use
-- internally
-- type family AnnotateLocationsWithBindingAndOffset
--               ( bdNo :: Nat )
--               ( locationFormats :: [ Nat FIR.:-> SPIRV.ImageFormat Nat ] )
--            :: [ Nat FIR.:-> ( Nat, Nat, SPIRV.ImageFormat Nat ) ]
--            where
--   AnnotateLocationsWithBindingAndOffset _ '[] = '[]
--   AnnotateLocationsWithBindingAndOffset bdNo
--       = ( loc 'FIR.:-> '( bdNo, 16 * loc, fmt ) )
--       ': AnnotateLocationsWithBindingAndOffset bdNo locs

-- type family SumSizeOfLocations (as :: [FIR.LocationSlot Nat FIR.:-> Type]) :: Nat where
--   SumSizeOfLocations '[] = 0
--   SumSizeOfLocations ( ( _ 'FIR.:-> a ) ': as )
--     = ( FIR.SizeOf FIR.Locations a `Data.Type.Nat.RoundUp` 16 ) + SumSizeOfLocations as

