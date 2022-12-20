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

import Data.IntMap (IntMap)
import qualified FIR
import qualified Vulkan as Vk
import Ghengin.Utils
-- import qualified FIR.Definition as FIR


type GShaderPipeline info = FIR.PipelineStages info StorableMap
type StorableMap = IntMap (IntMap SomeStorable)


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

-- where to move this...
stageFlag :: FIR.Shader -> Vk.ShaderStageFlagBits
stageFlag FIR.VertexShader                 = Vk.SHADER_STAGE_VERTEX_BIT
stageFlag FIR.TessellationControlShader    = Vk.SHADER_STAGE_TESSELLATION_CONTROL_BIT
stageFlag FIR.TessellationEvaluationShader = Vk.SHADER_STAGE_TESSELLATION_EVALUATION_BIT
stageFlag FIR.GeometryShader               = Vk.SHADER_STAGE_GEOMETRY_BIT
stageFlag FIR.FragmentShader               = Vk.SHADER_STAGE_FRAGMENT_BIT
stageFlag FIR.ComputeShader                = Vk.SHADER_STAGE_COMPUTE_BIT

