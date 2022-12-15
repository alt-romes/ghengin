{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
module Ghengin.Vulkan.DescriptorSet where

import Control.Monad
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Vector as V
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Zero as Vk
import qualified Vulkan as Vk
import Data.Bits ((.|.))

import qualified Ghengin.Shaders.FIR as FIR
import qualified FIR.Internals as FIR
import qualified SPIRV
import qualified SPIRV.Storage
import Ghengin.Vulkan
import Ghengin.Shaders

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


createDescriptorSetLayouts :: FIR.ShaderPipeline a
                           -> Renderer (V.Vector Vk.DescriptorSetLayout)
createDescriptorSetLayouts (FIR.ShaderPipeline ppstages) =

  V.fromList <$>
    mapM createDescriptorSetLayout (IM.toList $ makeDescriptorSetMap (go [] ppstages))
  
    where
      go :: [(FIR.Shader, (SPIRV.PointerTy,SPIRV.Decorations))] -> FIR.PipelineStages info a -> [(FIR.Shader, (SPIRV.PointerTy,SPIRV.Decorations))]
      go acc FIR.VertexInput = acc
      go acc (pipe FIR.:>-> (FIR.ShaderModule _ :: FIR.ShaderModule name stage defs endState,_)) = go (((map (FIR.knownValue @stage,) $ M.elems $ FIR.globalAnnotations $ FIR.annotations @defs)) <> acc) pipe


      makeDescriptorSetMap :: [(FIR.Shader, (SPIRV.PointerTy,SPIRV.Decorations))]
                           -> IM.IntMap (IM.IntMap (Vk.DescriptorType, Vk.ShaderStageFlags)) -- ^ Mapping from descriptor set indexes to a list of their bindings (corresponding binding type, shader stage)
      makeDescriptorSetMap =
        foldr (\(s,(pt,S.toList -> decs)) acc ->
          -- case L.sort (S.toList decs) of
          --   [Binding bindingIx, DescriptorSet descriptorSetIx] ->
          --   _ -> error $ "Invalid decorations for descriptor set: " <> show decs
          case L.find (\case SPIRV.DescriptorSet x -> True; _ -> False) decs of
            Just (SPIRV.DescriptorSet (fromIntegral -> descriptorIx)) -> case L.find (\case SPIRV.Binding x -> True; _ -> False) decs of
              Just (SPIRV.Binding (fromIntegral -> bindingIx)) -> IM.insertWith mergeSameDS descriptorIx (IM.singleton bindingIx (descriptorType pt, stageFlag s)) acc
              Nothing -> error "Can't have descriptor set without binding ix"
            Nothing -> acc
          ) mempty
        where
          mergeSameDS :: IM.IntMap (Vk.DescriptorType, Vk.ShaderStageFlags) -> IM.IntMap (Vk.DescriptorType, Vk.ShaderStageFlags) -> IM.IntMap (Vk.DescriptorType, Vk.ShaderStageFlags)
          mergeSameDS = IM.mergeWithKey (\_ (dt,sf) (dt',sf') -> if dt == dt' then Just (dt, sf .|. sf') else error $ "Incompatible descriptor type: " <> show dt <> " and " <> show dt') id id


  -- Vk.createDescriptorSetLayout device layoutInfo Nothing

descriptorType :: SPIRV.PointerTy -> Vk.DescriptorType
descriptorType = \case
  SPIRV.PointerTy sc _ -> case sc of
    SPIRV.Storage.Uniform -> Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
    _ -> error "Unexpected/unsupported storage class for descriptor"


createDescriptorSetLayout :: (Int,IM.IntMap (Vk.DescriptorType, Vk.ShaderStageFlags)) -> Renderer Vk.DescriptorSetLayout
createDescriptorSetLayout (_setNumber, (IM.toList -> bindingsList)) = getDevice >>= \device -> do
  let makeBinding (bindingIx,(descriptorType,sflags)) =
        Vk.DescriptorSetLayoutBinding { binding = 0
                                      , descriptorType = descriptorType
                                      , descriptorCount = 1 -- if this binding was an array of multiple items this number would be larger
                                      , stageFlags = sflags
                                      , immutableSamplers = []
                                      }
      layoutInfo = Vk.DescriptorSetLayoutCreateInfo { bindings = V.fromList $ map makeBinding bindingsList
                                                    , next = ()
                                                    , flags = Vk.zero
                                                    }
  Vk.createDescriptorSetLayout device layoutInfo Nothing


destroyDescriptorSetLayout :: Vk.DescriptorSetLayout -> Renderer ()
destroyDescriptorSetLayout layout = getDevice >>= \dev -> Vk.destroyDescriptorSetLayout dev layout Nothing





