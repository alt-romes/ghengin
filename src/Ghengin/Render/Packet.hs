module Ghengin.Render.Packet where

import Ghengin.Shaders
import Ghengin.Shaders.FIR

-- class Vertex
-- class Mesh
-- class Material

import Ghengin.Vulkan

data RenderPacket
data Mesh

makeRenderPacket :: Mesh -> Module vertexdefs -> Module fragdefs -> Renderer RenderPacket
makeRenderPacket mesh vertexShader fragmentShader = Prelude.undefined


