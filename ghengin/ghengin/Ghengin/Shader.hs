module Ghengin.Shader
  ( VertexShaderModule, FragmentShaderModule
  , module Ghengin.Core.Shader.Pipeline

  -- * Shader Byte Code for internal use
  , ShaderByteCode(..), compileFIRShader
  ) where

import Prelude
import Ghengin.Core.Shader.Canonical
import Ghengin.Core.Shader.Pipeline
import Ghengin.Shader.Syntactic ()
import qualified Data.ByteString.Lazy as BS
import qualified FIR
import qualified Math.Linear as FIR

type VertexShaderModule defs
  = FIR.ShaderModule "main" FIR.VertexShader
                     (("main" 'FIR.:-> FIR.EntryPoint '[] FIR.Vertex) ': CanonicalizeDefs defs)

type FragmentShaderModule defs
  = FIR.ShaderModule "main" FIR.FragmentShader
                     (("out_colour" 'FIR.:-> FIR.Output '[ FIR.Location 0 ] (FIR.V 4 Float)) ': ("main" 'FIR.:-> FIR.EntryPoint '[ FIR.OriginUpperLeft ] FIR.Fragment) ': CanonicalizeDefs defs)


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

