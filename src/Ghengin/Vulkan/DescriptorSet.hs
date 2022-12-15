{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
module Ghengin.Vulkan.DescriptorSet where

import qualified Data.Vector as V
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Zero as Vk
import qualified Vulkan as Vk

import qualified Ghengin.Shaders.FIR as FIR
import Ghengin.Vulkan

createDescriptorSetLayout :: Renderer Vk.DescriptorSetLayout
createDescriptorSetLayout = getDevice >>= \device -> do
  let uboBindingLayout = Vk.DescriptorSetLayoutBinding { binding = 0
                                                       , descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
                                                       , descriptorCount = 1 -- if this binding was an array of multiple items this number would be larger
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


-- | Create N descriptor sets from a uniform buffer descriptor pool using each
-- respective descriptor set layout
--
-- Only the pool needs to be destroyed
--
-- TODO: Linear types...
createUniformBufferDescriptorSets :: V.Vector Vk.DescriptorSetLayout -- ^ The descriptor set layouts to use. This is used to calculate the amount of descriptor sets to allocate
                                  -> Renderer (V.Vector Vk.DescriptorSet, Vk.DescriptorPool)
createUniformBufferDescriptorSets layouts = do
  let
      amount = fromIntegral $ length layouts
      poolSize = Vk.DescriptorPoolSize { descriptorCount = amount
                                       , type' = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
                                       }

      poolInfo = Vk.DescriptorPoolCreateInfo { poolSizes = [poolSize]
                                             , maxSets = amount
                                             , flags = Vk.zero
                                             , next = ()
                                             }

  device <- getDevice
  descriptorPool <- Vk.createDescriptorPool device poolInfo Nothing

  let
      allocInfo = Vk.DescriptorSetAllocateInfo { descriptorPool = descriptorPool
                                               , setLayouts = layouts
                                               , next = ()
                                               }

  descriptorSets <- Vk.allocateDescriptorSets device allocInfo
  pure (descriptorSets, descriptorPool)
  

destroyDescriptorPool :: Vk.DescriptorPool -> Renderer ()
destroyDescriptorPool p = getDevice >>= \dev -> Vk.destroyDescriptorPool dev p Nothing

-- | Write a descriptor set with a single uniform buffer bound on the 0 binding.
writeUniformBufferDescriptorSet :: Vk.Buffer -- ^ The uniform buffer
                                -> Vk.DescriptorSet -- ^ The descriptor set to write
                                -> Renderer ()
writeUniformBufferDescriptorSet buf dset = do

  let
      bufferInfo = Vk.DescriptorBufferInfo { buffer = buf
                                           , offset = 0
                                           , range  = Vk.WHOLE_SIZE -- the whole buffer
                                           }

      descriptorWrite = Vk.WriteDescriptorSet { next = ()
                                              , dstSet = dset
                                              , dstBinding = 0
                                              , dstArrayElement = 0 -- Descriptors could be arrays. We just use 0
                                              , descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
                                              , descriptorCount = 1 -- Only one descriptor in this descriptor set (the buffer)
                                              , bufferInfo = [bufferInfo]
                                              , imageInfo = []
                                              , texelBufferView = []
                                              }

  dev <- getDevice
  Vk.updateDescriptorSets dev [Vk.SomeStruct descriptorWrite] []



-- :| From Shaders |:


createDescriptorSetLayouts :: FIR.ShaderPipeline a -> Renderer (V.Vector Vk.DescriptorSetLayout)
createDescriptorSetLayouts (FIR.ShaderPipeline ppstages) = getDevice >>= \device -> do
  undefined

  -- error $ show $ go [] ppstages
  
    where
      -- go acc FIR.VertexInput = reverse acc
      -- go acc (pipe FIR.:>-> (FIR.ShaderModule _ :: FIR.ShaderModule name stage defs endState,_)) = go (FIR.annotations @defs:acc) pipe




  -- Vk.createDescriptorSetLayout device layoutInfo Nothing

