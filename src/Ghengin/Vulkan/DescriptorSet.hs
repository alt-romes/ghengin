{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
module Ghengin.Vulkan.DescriptorSet where

import Data.Maybe
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Control.Monad
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Zero as Vk
import qualified Vulkan as Vk
import Data.Bits ((.|.))

import qualified Ghengin.Shaders.FIR as FIR
import qualified FIR.Internals as FIR
import qualified SPIRV
import qualified SPIRV.Storage
import Ghengin.Vulkan.Buffer
import Ghengin.Vulkan
import Ghengin.Shaders

data DescriptorSet = DescriptorSet { _ix :: Int
                                   , _descriptorSet :: Vk.DescriptorSet
                                   , _descriptorSetLayout :: Vk.DescriptorSetLayout
                                   , _bindings :: [(Int, SomeMappedBuffer, Vk.DescriptorType)]
                                   }

-- :| From Shaders |:


-- | Create all required descriptor sets for a shader pipeline.
--
-- This will create the descriptor set layouts, the pool from where the
-- descriptor sets will be allocated, the actual descriptor sets, the buffers
-- and mapped memory, and will update the descriptor sets with the buffers
-- information
createDescriptorSets :: FIR.ShaderPipeline a
                     -> Renderer (Vector DescriptorSet, Vk.DescriptorPool)
createDescriptorSets (FIR.ShaderPipeline ppstages) = do

  let descriptorSetMap = makeDescriptorSetMap (go [] ppstages)

  layoutsAndBuffers :: [(Int, [(Int,(Vk.DescriptorType,Vk.ShaderStageFlags))], Vk.DescriptorSetLayout, [SomeMappedBuffer])]
      <- forM (IM.toAscList descriptorSetMap) $ \(setNumber, (IM.toAscList -> bindingsList)) -> do
          layout <- createDescriptorSetLayout bindingsList
          -- TODO; Currently this only creates uniform buffers but later it should also create storage buffers, etc
          buffers <- mapM (fmap SomeMappedBuffer . createMappedBuffer . fst . snd) bindingsList
          pure (setNumber,bindingsList,layout,buffers)

  (dsets, dpool) <- allocateDescriptorSets (map (\(_,bs,l,_) -> (l,map (fst . snd) bs)) layoutsAndBuffers)

  -- Depends on the order of the layouts and the dsets matching, which they do
  configuredDescriptorSets :: Vector DescriptorSet
    <- V.zipWithM (\(dsetIx, bindingsList, dslayout, buffers) dset -> do
                       let finalBindings = zipWith (\(ix,(dt,_ssf)) b -> (ix,b,dt)) bindingsList buffers
                       updateBufferDescriptorSet dset finalBindings
                       pure (DescriptorSet dsetIx dset dslayout finalBindings)
                  ) (V.fromList layoutsAndBuffers) dsets

  pure (configuredDescriptorSets, dpool)
  
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

      mergeSameDS :: IM.IntMap (Vk.DescriptorType, Vk.ShaderStageFlags) -> IM.IntMap (Vk.DescriptorType, Vk.ShaderStageFlags) -> IM.IntMap (Vk.DescriptorType, Vk.ShaderStageFlags)
      mergeSameDS = IM.mergeWithKey (\_ (dt,sf) (dt',sf') -> if dt == dt' then Just (dt, sf .|. sf') else error $ "Incompatible descriptor type: " <> show dt <> " and " <> show dt') id id


descriptorType :: SPIRV.PointerTy -> Vk.DescriptorType
descriptorType = \case
  SPIRV.PointerTy sc _ -> case sc of
    SPIRV.Storage.Uniform -> Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
    _ -> error "Unexpected/unsupported storage class for descriptor"


createDescriptorSetLayout :: [(Int, (Vk.DescriptorType, Vk.ShaderStageFlags))] -- ^ Binding, type and stage flags for each descriptor in the set to create
                          -> Renderer Vk.DescriptorSetLayout
createDescriptorSetLayout bindingsList = getDevice >>= \device -> do
  let makeBinding (bindingIx,(descriptorType,sflags)) =
        Vk.DescriptorSetLayoutBinding { binding = fromIntegral bindingIx
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


-- | Create N descriptor sets from a uniform buffer descriptor pool using each
-- respective descriptor set layout
--
-- Only the pool needs to be destroyed
--
-- TODO: Linear types...
allocateDescriptorSets :: [(Vk.DescriptorSetLayout,[Vk.DescriptorType])] -- ^ The descriptors we need for each set (indexed by set). This is used to calculate the amount of descriptor sets to allocate and the amoumt of descriptors of each type
                       -> Renderer (Vector Vk.DescriptorSet, Vk.DescriptorPool)
allocateDescriptorSets sets = do

  let 
      descriptorsAmounts = map (\(t :| ls) -> (t, length ls + 1)) . NonEmpty.group . L.sort $ concatMap snd sets
      poolsSizes = map (\(t,fromIntegral -> a) -> Vk.DescriptorPoolSize {descriptorCount = a, type' = t}) descriptorsAmounts

      setsAmount = fromIntegral $ length sets
      poolInfo = Vk.DescriptorPoolCreateInfo { poolSizes = V.fromList poolsSizes
                                             , maxSets = setsAmount
                                             , flags = Vk.zero
                                             , next = ()
                                             }

  device <- getDevice
  descriptorPool <- Vk.createDescriptorPool device poolInfo Nothing

  let
      allocInfo = Vk.DescriptorSetAllocateInfo { descriptorPool = descriptorPool
                                               , setLayouts = V.fromList $ map fst sets
                                               , next = ()
                                               }

  descriptorSets <- Vk.allocateDescriptorSets device allocInfo
  pure (descriptorSets, descriptorPool)
  

destroyDescriptorPool :: Vk.DescriptorPool -> Renderer ()
destroyDescriptorPool p = getDevice >>= \dev -> Vk.destroyDescriptorPool dev p Nothing


-- | Update the configuration of a descriptor set with multiple buffers
updateBufferDescriptorSet :: Vk.DescriptorSet   -- ^ The descriptor set we're writing with these buffers
                          -> [(Int, SomeMappedBuffer, Vk.DescriptorType)] -- ^ The buffers, their bindings, and the type of buffer as a DescriptorType
                          -> Renderer ()
updateBufferDescriptorSet dset bufs = do

  let amount = fromIntegral $ length bufs
      makeBufferWrite i buf dty =
        -- Each descriptor only has one buffer. If we had an array of buffers in a descriptor we would need multiple descriptor buffer infos
        let bufferInfo = Vk.DescriptorBufferInfo
                                             { buffer = case buf of SomeMappedBuffer b -> b.buffer
                                             , offset = 0
                                             , range  = Vk.WHOLE_SIZE -- the whole buffer
                                             }

         in Vk.SomeStruct Vk.WriteDescriptorSet
                                  { next = ()
                                  , dstSet = dset -- the descriptor set to update with this write
                                  , dstBinding = fromIntegral i
                                  , dstArrayElement = 0 -- Descriptors could be arrays. We just use 0
                                  , descriptorType = dty -- The type of buffer
                                  , descriptorCount = 1 -- Only one buffer in the array of buffers to update
                                  , bufferInfo = [bufferInfo] -- The one buffer info
                                  , imageInfo = []
                                  , texelBufferView = []
                                  }

  dev <- getDevice
  Vk.updateDescriptorSets dev (V.fromList $ map (\(a,b,c) -> makeBufferWrite a b c) bufs) []




