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
--------------------------------------------------------------------------------
-- !!! TODO !!!
-- Consolidate with FIR.Vulkan.Resource. That is much better than this.
-- !!! TODO !!!
--------------------------------------------------------------------------------
module Ghengin.Vulkan.Renderer.Descriptor where

import Ghengin.Core.Prelude as Linear
import Ghengin.Core.Log
import qualified Data.Functor.Linear as Data.Linear
import qualified Prelude

import Data.Linear.Alias.Unsafe as Unsafe.Alias
import qualified Unsafe.Linear as Unsafe

import Data.Bits

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.IntMap.Linear as IML
import qualified Data.Vector as V
import qualified Data.V.Linear as VL

import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Zero as Vk
import qualified Vulkan.Linear as Vk

import qualified FIR hiding (ShaderPipeline, (:>->))
import qualified FIR.Definition as FIR
import qualified SPIRV.Decoration as SPIRV
import qualified SPIRV.PrimTy as SPIRV
import qualified SPIRV.Storage

import Ghengin.Vulkan.Renderer.Buffer
import Ghengin.Vulkan.Renderer.Image
import Ghengin.Vulkan.Renderer.Sampler
import Ghengin.Vulkan.Renderer.Kernel
import Ghengin.Vulkan.Renderer.Command as Command

import Ghengin.Vulkan.Renderer.Texture

import Ghengin.Core.Shader.Pipeline

import FIR.Vulkan.Pipeline

import qualified Data.Linear.Alias as Alias

-- The descriptor set number 0 will be used for engine-global resources, and bound
-- once per frame. The descriptor set number 1 will be used for per-pass
-- resources, and bound once per pass. The descriptor set number 2 will be used
-- for material resources, and the number 3 will be used for per-object resources.
--   This way, the inner render loops will only be binding descriptor sets 2 and
--   3, and performance will be high.

-------- Resources ----------------
-- (INLINED from hsig file)
-- Resources are a part of the descriptor set module since these resources are
-- used to manipulate the descriptors of the descriptor set.
--
-- e.g. a mapped buffer resource can be bound by a descriptor such that using
-- that descriptor in the shader will read the buffer resource

type DescriptorBindingInfo = (Vk.DescriptorType, Vk.ShaderStageFlags)
type ResourceMap = IntMap DescriptorResource

data DescriptorResource where
  UniformResource   :: Alias MappedBuffer ⊸ DescriptorResource
  StorageResource   :: Alias MappedBuffer ⊸ DescriptorResource
  Texture2DResource :: Alias (Texture2D fmt) ⊸ DescriptorResource

instance Forgettable Renderer DescriptorResource where
  forget = \case
    UniformResource u -> Alias.forget u
    StorageResource u -> Alias.forget u
    Texture2DResource t -> Alias.forget t

instance Shareable m DescriptorResource where
  share = \case
    UniformResource u -> bimap UniformResource UniformResource <$> Alias.share u
    StorageResource u -> bimap StorageResource StorageResource <$> Alias.share u
    Texture2DResource t -> bimap Texture2DResource Texture2DResource <$> Alias.share t

-- | Mapping from each binding to corresponding binding type, shader stage
-- We have a maybe word because not every binding has a layout in memory (images don't)
type BindingsMap = IntMap DescriptorBindingInfo

instance Consumable BindingsMap where
  consume = Unsafe.toLinear \_bm -> () -- rnf bm
instance Dupable BindingsMap where
  dup2 = Unsafe.toLinear \bm -> (bm,bm)
instance Movable BindingsMap where
  move = Unsafe.toLinear \bm -> Ur bm

-- | Mapping from each descriptor set ix to its bindings map
type DescriptorSetMap = IntMap BindingsMap

-- | Convert descriptor binding info to buffer kind when applicable.
-- Texture descriptors have no mapped buffer and return Nothing.
bindingBufferType :: DescriptorBindingInfo -> Maybe BufferType
bindingBufferType (Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER, _) = Just Uniform
bindingBufferType (Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER, _) = Just Storage
bindingBufferType _ = Nothing

-- :| From Shaders |:

-- | Creates a mapping from descriptor set indexes to a list of their bindings
-- (corresponding binding type, size, shader stage flags) solely from the shader
-- pipeline definition.
createDescriptorSetBindingsMap :: ShaderPipeline info -> Ur DescriptorSetMap
createDescriptorSetBindingsMap ppstages = Ur $ makeDescriptorSetMap (go Prelude.mempty ppstages)
                                            -- If any of the descriptor sets is
                                            -- unused, we default to an empty bindings map
                                            <> IM.fromList [(0, mempty), (1, mempty), (2, mempty)]
  where
    go :: Map FIR.Shader [(SPIRV.PointerTy,SPIRV.Decorations)]
       -> ShaderPipeline info
       -> Map FIR.Shader [(SPIRV.PointerTy,SPIRV.Decorations)]
       -- ^ For each shader, the sets, corresponding decorations, and corresponding storable data types
    go acc (ShaderPipeline FIR.VertexInput) = acc
    go acc (pipe :>-> (FIR.ShaderModule _ :: FIR.ShaderModule name stage defs endState)) =
      go (M.insertWith (Prelude.<>) (FIR.knownValue @stage) (M.elems $ FIR.globalAnnotations $ FIR.annotations @defs) acc) pipe

    makeDescriptorSetMap :: Map FIR.Shader [(SPIRV.PointerTy, SPIRV.Decorations)]
                         -> DescriptorSetMap -- ^ Mapping from descriptor set indexes to a list of their bindings (corresponding binding type, shader stage)
    makeDescriptorSetMap =
      M.foldrWithKey (\shader ls acc' -> 
        Prelude.foldr (\(pt,S.toList -> decs) acc ->
          case decs of
            [SPIRV.Binding (fromIntegral -> bindingIx), SPIRV.DescriptorSet (fromIntegral -> descriptorSetIx)] ->
               IM.insertWith mergeSameDS descriptorSetIx
                            (IM.singleton bindingIx (descriptorType pt, stageFlag shader))
                            acc
            _ -> acc -- we keep folding. currently we don't validate anything futher
          ) acc' ls
        ) Prelude.mempty

    mergeSameDS :: BindingsMap
                -> BindingsMap
                -> BindingsMap
    mergeSameDS = IM.mergeWithKey (\_ (dt,sf) (dt',sf') ->
      if dt Prelude.== dt'
        then Just (dt, sf .|. sf')
      else error $ "Incompatible descriptor type: " <> show dt <> " and " <> show dt') id id

descriptorType :: SPIRV.PointerTy -> Vk.DescriptorType
descriptorType = \case
  SPIRV.PointerTy SPIRV.Storage.Uniform _ -> Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
  SPIRV.PointerTy SPIRV.Storage.StorageBuffer _ -> Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER
  -- SPIRV.PointerTy SPIRV.Storage.UniformConstant SPIRV.Sampler -> Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  SPIRV.PointerTy SPIRV.Storage.UniformConstant (SPIRV.SampledImage _) -> Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  x -> error $ "Unexpected/unsupported descriptor set #1 descriptor type: " <> show x

