{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QualifiedDo #-}
{-# OPTIONS_GHC -Wno-orphans #-} -- BindingsMap/IntMap
module Ghengin.Vulkan.Renderer.Descriptor.Set
  ( module Ghengin.Vulkan.Renderer.Descriptor.Set
  , module Ghengin.Vulkan.Renderer.Descriptor.Pool
  , module Ghengin.Vulkan.Renderer.Descriptor
  ) where

import Ghengin.Core.Prelude as Linear
import Ghengin.Core.Log

import Data.Linear.Alias.Unsafe as Unsafe.Alias
import qualified Unsafe.Linear as Unsafe

import qualified Data.IntMap.Linear as IML
import qualified Data.V.Linear as VL

import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Linear as Vk

import Ghengin.Vulkan.Renderer.Buffer
import Ghengin.Vulkan.Renderer.Image
import Ghengin.Vulkan.Renderer.Sampler
import Ghengin.Vulkan.Renderer.Pipeline
import Ghengin.Vulkan.Renderer.Kernel
import Ghengin.Vulkan.Renderer.Context
import Ghengin.Vulkan.Renderer.Command as Command

import Ghengin.Vulkan.Renderer.Texture
import Ghengin.Vulkan.Renderer.Descriptor
import Ghengin.Vulkan.Renderer.Descriptor.Pool

import Ghengin.Core.Shader.Pipeline

import FIR.Vulkan.Pipeline

import qualified Data.Linear.Alias as Alias

--------------------------------------------------------------------------------
-- * Commands
--------------------------------------------------------------------------------

bindGraphicsDescriptorSet :: Linear.MonadIO m
                          => RendererPipeline Graphics
                          ⊸ Word32 -- ^ Set index at which to bind the descriptor set
                          -> DescriptorSet ⊸ RenderCmdM m (DescriptorSet, RendererPipeline Graphics)
bindGraphicsDescriptorSet (VulkanPipeline pipelay layout) ix (DescriptorSet dix dset) = Linear.do
  (layout', dset') <- Command.bindGraphicsDescriptorSet' layout ix dset
  return (DescriptorSet dix dset', VulkanPipeline pipelay layout')
{-# INLINE bindGraphicsDescriptorSet #-}

--------------------------------------------------------------------------------
--- : | Descriptor Sets | : ---
--------------------------------------------------------------------------------

data DescriptorSet
  = DescriptorSet { _ix :: Int
                  , _descriptorSet :: Vk.DescriptorSet
                  }

-- | Allocate a descriptor set from a descriptor pool. This descriptor pool has
-- the information required to allocate a descriptor set based on its index in
-- the shader.
--
-- For example, if a shader has a descriptor set #1 with 5 different bindings,
-- @allocateDescriptorSet 1@ will allocate a descriptor set with those 5
-- bindings types (and buffers for each of them).
--
-- Each descriptor set must eventually be freed (because of the associated buffers): todo: linear types
--
-- This function allocates the descriptor sets but does not write to them. A
-- descriptor set must be written with the information of each binding (uniform
-- bindings require the underlying buffer, textures require the underlying
-- texture). This allows for the caller to allocate the required underlying
-- buffers and images as required.
--
-- To write and obtain the descriptor set, apply the returned function to a
-- resource map. If the function is applied to an empty resource map, it'll
-- simply create a descriptor set and write nothing to it.
allocateEmptyDescriptorSet :: Int -- ^ The set to allocate by index (we could enforce this number is inside of the descritpor pool if we had a type level map of the bindings in the descriptor set : TODO)
                      -> DescriptorPool -- ^ The descriptor pool associated with a shader pipeline in which the descriptor sets will be used
                       ⊸ Renderer (DescriptorSet, DescriptorPool)
allocateEmptyDescriptorSet ix = extract <=< allocateEmptyDescriptorSets (VL.make ix)
  where
    extract :: (V 1 DescriptorSet, DescriptorPool) ⊸ Renderer (DescriptorSet, DescriptorPool)
    extract (ds,p) = pure (VL.elim id ds,p)

-- | Like 'allocateEmptyDescriptorSet' but allocate multiple sets at once
-- INVARIANT: The Int vector does not have duplicate Ints
allocateEmptyDescriptorSets :: ∀ n. KnownNat n
                            => V n Int   -- ^ The sets to allocate by Ix
                            -> DescriptorPool -- ^ The descriptor pool associated with a shader pipeline in which the descriptor sets will be used
                             ⊸ Renderer (V n DescriptorSet, DescriptorPool)
allocateEmptyDescriptorSets ixs DescriptorPool{..} = enterD "allocateEmptyDescriptorSets" $ Linear.do
  -- This is one of those for which I really should have just used Unsafeness, right...
  -- What I think I really need for these cases is a sorts of *framing rule* as
  -- that in separation logic

  -- Extract the layouts info needed for allocation out of the dpool map
  (to_alloc, the_rest)  <- pure $ IML.partitionByKeys ixs set_bindings

  (Ur to_alloc_size, to_alloc) <- pure $ IML.size to_alloc
  assertM "allocateEmptyDescriptorSets" (to_alloc_size == VL.theLength @n)
  
  -- Extract the infos from the sets by ix to allocate
  (keys, layouts) <- pure $ unzip $ IML.toList $ to_alloc

  -- Allocate the descriptor sets
  (dsets, V layouts, dpool) <- enterD "Allocate the descriptor sets" $
    withDevice (Vk.allocateDescriptorSets dpool (V @n (l2vec layouts))) -- @n since the partition takes @n@ integers (well, only if the integer list is disjoint...)

  -- Reconstruct things
  case zip' keys (vec2l layouts) of
    (to_alloc, Nothing) -> Linear.do
      set_bindings  <- pure $ IML.unionWith (Unsafe.toLinear2 \_ _ -> error "impossible") (IML.fromList to_alloc) the_rest

      pure (vzipWith DescriptorSet ixs dsets, DescriptorPool dpool set_bindings)
  
-- | Update the configuration of a descriptor set with multiple resources (e.g. buffers + images)
updateDescriptorSet :: DescriptorSet -- ^ The descriptor set we're updating with these resources
                     ⊸ IntMap DescriptorResource -- ^ things to update
                     ⊸ Renderer (DescriptorSet, IntMap DescriptorResource)
updateDescriptorSet = Unsafe.toLinear2 \(DescriptorSet uix dset) resources -> enterD "updateDescriptorSet" Linear.do

  -- Ach... the resource map must only be freed when the descriptor set is no longer in use... right? Perhaps not...
  -- This could be done, e.g., by storing a reference counted alias of the
  -- things the it depends on, and free them when we free this.

  {- To update a descriptor set, we must write a Vk.WriteDescriptorSet info
      structure for each resource we want to update a descriptor set with

      This is bothersome to do linearly, we could, e.g., share an alias for
      every resource to be stored in the write-info, then pass the write infos
      to the or .. .o r... hard


      Basically, makeDescriptorWriteInfo would need to give ownwership of the
      resources to the writeInfos, give it to Vk.updateDescriptorSet, which
      would return ownership of the writeInfos, and we'd construct ownership of
      the resources again.
      -}

  let makeDescriptorWriteInfo :: Int -> DescriptorResource -> Renderer (Vk.SomeStruct Vk.WriteDescriptorSet)
      makeDescriptorWriteInfo i = \case
        UniformResource bufA ->
          -- Each descriptor only has one buffer. If we had an array of buffers in a descriptor we would need multiple descriptor buffer infos
          let bufferInfo = Vk.DescriptorBufferInfo
                                               { buffer = (Unsafe.Alias.get bufA).buffer
                                               , offset = 0
                                               , range  = Vk.WHOLE_SIZE -- the whole buffer
                                               }

           in pure $ Vk.SomeStruct Vk.WriteDescriptorSet
               { next = ()
               , dstSet = dset -- the descriptor set to update with this write
               , dstBinding = fromIntegral i
               , dstArrayElement = 0 -- Descriptors could be arrays. We just use 0
               , descriptorType = Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER -- The type of buffer
               , descriptorCount = 1 -- Only one buffer in the array of buffers to update
               , bufferInfo = [bufferInfo] -- The one buffer info
               , imageInfo = []
               , texelBufferView = []
               }

        StorageResource bufA ->
          let bufferInfo = Vk.DescriptorBufferInfo
                                               { buffer = (Unsafe.Alias.get bufA).buffer
                                               , offset = 0
                                               , range  = Vk.WHOLE_SIZE -- the whole buffer
                                               }

           in pure $ Vk.SomeStruct Vk.WriteDescriptorSet
               { next = ()
               , dstSet = dset -- the descriptor set to update with this write
               , dstBinding = fromIntegral i
               , dstArrayElement = 0 -- Descriptors could be arrays. We just use 0
               , descriptorType = Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER -- The type of buffer
               , descriptorCount = 1 -- Only one buffer in the array of buffers to update
               , bufferInfo = [bufferInfo] -- The one buffer info
               , imageInfo = []
               , texelBufferView = []
               }

        Texture2DResource talias -> undefined
          -- case Unsafe.Alias.get talias of
          --   (Texture2D vkimage sampler) ->
          --     let imageInfo = Vk.DescriptorImageInfo { imageLayout = Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
          --                                            , imageView = vkimage._imageView
          --                                            , sampler   = (Unsafe.Alias.get sampler).sampler
          --                                            }
          --      in pure $ Vk.SomeStruct Vk.WriteDescriptorSet
          --           { next = ()
          --           , dstSet = dset -- the descriptor set to update with this write
          --           , dstBinding = fromIntegral i
          --           , dstArrayElement = 0 -- Descriptors could be arrays. We just use 0
          --           , descriptorType = Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER -- The type of buffer
          --           , descriptorCount = 1 -- Only one buffer in the array of buffers to update
          --           , bufferInfo = [] -- The one buffer info
          --           , imageInfo = [imageInfo]
          --           , texelBufferView = []
          --           }


  -- The difficulty in making this linear is that we can't traverse and update
  -- one thing at a time, we must write them all first, then call update on the
  -- whole batch
  writeInfos <- IML.traverseWithKey (Unsafe.toLinear2 makeDescriptorWriteInfo) resources
  -- How can I do this well? It's really not immediatly clear

  withDevice (Unsafe.toLinear Vk.updateDescriptorSets (l2vec $ IML.elems writeInfos) [])
  pure (DescriptorSet uix dset, resources)

-- | Destroy a descriptor set 
--
-- I think this comment is outdated:
-- We must be careful here not to free resources shared across materials
--
-- (1) Mapped buffers are allocated per-material, so we always free them
--
-- (2) Texture resources are allocated outside of the material and might be
-- shared, so we never free them for now. Eventually they might be
-- automatically managed through medit and reference counting.
--
-- The texture resource is used to update the descriptor set to point to that
-- texture
--
freeDescriptorSets :: Alias DescriptorPool ⊸ V n DescriptorSet ⊸ Renderer ()
freeDescriptorSets dpoolA dsets = enterD "Freeing descriptor sets!" Linear.do
  (DescriptorPool{..}, f) <- Alias.get dpoolA
  dpool1 <- withDevice (Vk.freeDescriptorSets dpool (case VL.map (\(DescriptorSet ix dset) -> ix `lseq` dset) dsets of V v -> v))
  f (DescriptorPool{dpool=dpool1,..})
  return ()

freeResourceMap :: ResourceMap ⊸ Renderer ()
freeResourceMap = enterD "Freeing resource map!" . Alias.forget

-- | Forget a descriptor resource. This might free the resource if it's the last reference to it
-- (A DescriptorResource wraps a Reference Counted value)
freeDescriptorResource :: DescriptorResource ⊸ Renderer ()
freeDescriptorResource = enterD "Freeing descriptor resource!" . Alias.forget
