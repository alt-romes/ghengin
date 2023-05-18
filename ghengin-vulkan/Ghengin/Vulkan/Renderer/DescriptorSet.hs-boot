module Ghengin.Vulkan.Renderer.DescriptorSet where

import Data.Word (Word)
import Data.IntMap (IntMap)
import qualified Vulkan as Vk

type Size = Word
type BindingsMap = IntMap (Vk.DescriptorType, Size, Vk.ShaderStageFlags)

data DescriptorPool =
  DescriptorPool { _pool :: Vk.DescriptorPool
                 , _set_bindings :: IntMap (Vk.DescriptorSetLayout, BindingsMap)
                 }
