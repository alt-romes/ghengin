{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Ghengin.Core.Shader.Pipeline
  ( ShaderPipeline(ShaderPipeline, (:>->))

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
import FIR.ProgramState (EntryPointInfo(..), Definedness(..))
import FIR.Validation.Pipeline (GetExecutionInfo)
import GHC.TypeLits
import qualified FIR
import qualified FIR.Definition

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

