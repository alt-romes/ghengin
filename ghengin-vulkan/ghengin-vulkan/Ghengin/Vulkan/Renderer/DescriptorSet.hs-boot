module Ghengin.Vulkan.Renderer.DescriptorSet where

import Data.Int
import Data.Maybe (Maybe)
import Data.Word (Word)
import Data.IntMap (IntMap)
import qualified Vulkan as Vk

type BindingsMap = IntMap (Vk.DescriptorType, Maybe Word, Vk.ShaderStageFlags)

data DescriptorPool =
  DescriptorPool { dpool :: Vk.DescriptorPool
                 , set_bindings :: IntMap (Vk.DescriptorSetLayout, BindingsMap)
                 }

data DescriptorSet
  = DescriptorSet { _ix :: Int
                  , _descriptorSet :: Vk.DescriptorSet
                  }
