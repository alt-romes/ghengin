module Ghengin.Shader
  ( VertexShaderModule, FragmentShaderModule
  , module Ghengin.Shader.Pipeline
  ) where

import Ghengin.Shader.Canonical
import Ghengin.Shader.Pipeline
import Ghengin.Shader.Syntactic ()
import qualified FIR

type VertexShaderModule defs
  = FIR.ShaderModule "main" FIR.VertexShader
                     (("main" 'FIR.:-> FIR.EntryPoint '[] FIR.Vertex) ': CanonicalizeDefs defs)

type FragmentShaderModule defs
  = FIR.ShaderModule "main" FIR.FragmentShader
                     (("main" 'FIR.:-> FIR.EntryPoint '[ FIR.OriginUpperLeft ] FIR.Fragment) ': CanonicalizeDefs defs)


