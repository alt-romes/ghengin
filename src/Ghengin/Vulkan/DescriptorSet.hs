{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
module Ghengin.Vulkan.DescriptorSet where

import qualified Vulkan.Zero as Vk
import qualified Vulkan as Vk

import Ghengin.Vulkan

createDescriptorSetLayout :: Renderer Vk.DescriptorSetLayout
createDescriptorSetLayout = getDevice >>= \device -> do
  let uboBindingLayout = Vk.DescriptorSetLayoutBinding { binding = 0
                                                       , descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
                                                       , descriptorCount = 1
                                                       , stageFlags = Vk.SHADER_STAGE_VERTEX_BIT
                                                       , immutableSamplers = []
                                                       }
      layoutInfo = Vk.DescriptorSetLayoutCreateInfo { bindings = [uboBindingLayout]
                                                    , next = ()
                                                    , flags = Vk.zero
                                                    }
  Vk.createDescriptorSetLayout device layoutInfo Nothing


destroyDescriptorSetLayout :: Vk.DescriptorSetLayout -> Renderer ()
destroyDescriptorSetLayout layout = getDevice >>= \dev -> Vk.destroyDescriptorSetLayout dev layout Nothing

