{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Ghengin.Shaders
  ( module Ghengin.Shaders
  , FIR.Module
  , FIR.ShaderPipeline
  , FIR.pipelineStages
  , FIR.CompilableProgram
  ) where

import qualified Data.ByteString.Lazy as BS

import qualified FIR
import qualified Vulkan as Vk
-- import qualified FIR.Definition as FIR

newtype ShaderByteCode = SBC BS.ByteString

-- | Compile a shader program into SPIR-V bytecode.
--
-- === Example
--
-- @
-- vertexShaderByteCode <- compileShader SimpleShader.vertex
-- fragShaderByteCode   <- compileShader SimpleShader.fragment
-- @
compileFIRShader :: FIR.CompilableProgram prog => prog -> IO ShaderByteCode
compileFIRShader m = do
  FIR.compile [] m >>= \case
    Right (Just (FIR.ModuleBinary bs), _) -> pure (SBC bs)
    Left e -> fail $ show e
    _      -> fail "Couldn't generate module binary when compiling"


-- | Read a compiled shader (a SPIR-V bytecode file) into a ShaderByteCode
-- datatype which can then be used, e.g., by 'createPipeline'
readShaderFile :: FilePath -> IO ShaderByteCode
readShaderFile = fmap SBC . BS.readFile

