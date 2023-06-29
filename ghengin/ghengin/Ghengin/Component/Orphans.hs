{-# OPTIONS_GHC -Wno-orphans #-}
module Ghengin.Component.Orphans where

import Ghengin.Vulkan.Renderer.Kernel (Renderer)
import Ghengin.Core.Render.Packet (RenderPacket)
import Ghengin.Core.Render.Pipeline (SomePipeline, RenderPipeline)
import Ghengin.Core.Material (Material)
import Ghengin.Core.Type.Utils (Some(..))

import Apecs.Linear

-- For now we define the Storages of the Core elements here. What's the best
-- place to do it? Perhaps here isn't so bad.

-- Core data types Storage instances
-- BIG:TODO: Caches around these storages in generall...

-- | BIG:TODO: Cache around this Map storage
instance Component RenderPacket where
  type Storage RenderPacket = Map RenderPacket

instance Component (SomePipeline) where
  type Storage (SomePipeline) = Map (SomePipeline)
  -- {-# DEPRECATED makeRenderPipeline "Storage should be a cache" #-}

instance Component (Some Material) where
  type Storage (Some Material) = Map (Some Material)
  -- {-# DEPRECATED material "TODO: Material storage should be a cache" #-}
