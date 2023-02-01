module Ghengin.Vulkan.Utils where

import qualified FIR
import qualified Vulkan as Vk

stageFlag :: FIR.Shader -> Vk.ShaderStageFlagBits
stageFlag FIR.VertexShader                 = Vk.SHADER_STAGE_VERTEX_BIT
stageFlag FIR.TessellationControlShader    = Vk.SHADER_STAGE_TESSELLATION_CONTROL_BIT
stageFlag FIR.TessellationEvaluationShader = Vk.SHADER_STAGE_TESSELLATION_EVALUATION_BIT
stageFlag FIR.GeometryShader               = Vk.SHADER_STAGE_GEOMETRY_BIT
stageFlag FIR.FragmentShader               = Vk.SHADER_STAGE_FRAGMENT_BIT
stageFlag FIR.ComputeShader                = Vk.SHADER_STAGE_COMPUTE_BIT

