module Ghengin.Component.Orphans where

import Ghengin.Core.Renderer.Kernel (Renderer)
import Ghengin.Core.Render.Packet (RenderPacket)
import Ghengin.Core.Render.Pipeline (SomePipeline)
import Ghengin.Core.Material (SomeMaterial)

import Apecs.Linear

-- For now we define the Storages of the Core elements here. What's the best
-- place to do it? Perhaps here isn't so bad.

-- Core data types Storage instances

-- | BIG:TODO: Cache around this Map storage
instance Component RenderPacket where
  type Storage RenderPacket = Map RenderPacket

