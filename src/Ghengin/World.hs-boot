{-# LANGUAGE RoleAnnotations #-}
module Ghengin.World where

import Apecs.Linear
import Apecs.Core
import Data.Unrestricted.Linear (Dupable, Consumable)
import Data.Kind

import Ghengin.Core.Renderer.Kernel
import Ghengin.Core.Render.Packet (RenderPacket)
import Ghengin.Core.Render.Pipeline (SomePipeline)
import Ghengin.Core.Material (SomeMaterial)

data World :: Type -> Type
type role World nominal

instance Consumable w => Consumable (World w)
instance Dupable w => Dupable (World w)

instance Has (World w) Renderer RenderPacket
instance Has (World w) Renderer SomePipeline
instance Has (World w) Renderer SomeMaterial

-- instance ExplGet Renderer (Storage RenderPacket)
-- instance ExplGet Renderer (Storage SomePipeline)
-- instance ExplGet Renderer (Storage SomeMaterial)

-- instance ExplSet Renderer (Storage RenderPacket)
-- instance ExplSet Renderer (Storage SomePipeline)
-- instance ExplSet Renderer (Storage SomeMaterial)

-- instance ExplMembers Renderer (Storage RenderPacket)
-- instance ExplMembers Renderer (Storage SomePipeline)
-- instance ExplMembers Renderer (Storage SomeMaterial)

