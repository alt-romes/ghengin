{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Ghengin.Shaders
  ( FIR.Module
  , FIR.ShaderPipeline
  , FIR.pipelineStages
  , FIR.CompilableProgram
  ) where

import qualified FIR


