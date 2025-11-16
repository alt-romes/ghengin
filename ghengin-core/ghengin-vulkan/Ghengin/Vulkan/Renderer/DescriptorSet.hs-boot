module Ghengin.Vulkan.Renderer.DescriptorSet where

import Data.Int
import Data.IntMap (IntMap)
import qualified Vulkan as Vk

type BindingsMap = IntMap (Vk.DescriptorType, Vk.ShaderStageFlags)

data DescriptorPool =
  DescriptorPool { dpool :: Vk.DescriptorPool
                 , set_bindings :: IntMap Vk.DescriptorSetLayout
                 }

data DescriptorSet
  = DescriptorSet { _ix :: Int
                  , _descriptorSet :: Vk.DescriptorSet
                  }
