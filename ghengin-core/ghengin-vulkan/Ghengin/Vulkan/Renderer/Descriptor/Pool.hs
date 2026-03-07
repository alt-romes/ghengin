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
module Ghengin.Vulkan.Renderer.Descriptor.Pool where

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
import Ghengin.Vulkan.Renderer.Context
import Ghengin.Vulkan.Renderer.Command as Command

import Ghengin.Vulkan.Renderer.Texture
import Ghengin.Vulkan.Renderer.Descriptor

import Ghengin.Core.Shader.Pipeline

import FIR.Vulkan.Pipeline

import qualified Data.Linear.Alias as Alias

{-
Note [Pools]
~~~~~~~~~~~~
A pool has a limited amount of resources that can be allocated from it. We
have at least a pool for each shader pipeline, and the number of resources
are in proportion to the amount of required by each descriptor set in the shaders.

We have 1 of each resource required by the descriptor set #0, 1000 of each
descriptor required by the descriptor set #1, and a 10000 of each descriptor
required by the set #2 -- allowing at most one descriptor set #0 per pipeline,
1000 materials per pipeline, and 10000 entities per pipeline.

The descriptor pool stores this pipeline-specific information to make it
possible to allocate entire descriptor sets by index (as specified in the
shader) from the pool, rather than specifying each descriptor.
-}


-- | See Note [Pools]
data DescriptorPool =
  DescriptorPool { dpool :: Vk.DescriptorPool
                 , set_bindings :: IntMap Vk.DescriptorSetLayout
                 }

-- Creates a pool as described in Note [Pools].
--
-- TODO: Right amount of descriptors. For now we simply multiply 1000 by the
-- number of all total descriptors across sets
createDescriptorPool :: DescriptorSetMap -> Renderer DescriptorPool
createDescriptorPool dsetmap = enterD "createDescriptorPool" $ Linear.do
  layouts <- Data.Linear.traverse (\bm -> case move bm of Ur bm1 -> createDescriptorSetLayout bm1) dsetmap

  let 
    descriptorsAmounts :: [(Vk.DescriptorType, Int)] -- ^ For each type, its amount
    descriptorsAmounts = Prelude.map (\(t :| ls) -> (t, 1000 * (Prelude.length ls + 1))) Prelude.. NonEmpty.group Prelude.. L.sort $ Prelude.foldMap (Prelude.foldr (\(ty,_) -> (ty:)) Prelude.mempty) dsetmap
    poolsSizes = Prelude.map (\(t,fromIntegral -> a) -> Vk.DescriptorPoolSize {descriptorCount = a, type' = t}) descriptorsAmounts

    setsAmount = fromIntegral $ Prelude.length dsetmap
    poolInfo = Vk.DescriptorPoolCreateInfo { poolSizes = V.fromList poolsSizes
                                            , maxSets = 1000 Prelude.* setsAmount
                                            , flags = Vk.zero
                                            , next = ()
                                            }

  descriptorPool <- withDevice (Vk.createDescriptorPool poolInfo Nothing)
  pure (DescriptorPool descriptorPool layouts)

destroyDescriptorPool :: DescriptorPool ⊸ Renderer ()
destroyDescriptorPool DescriptorPool{..} = enterD "destroyDescriptorPool" $ Linear.do
  withDevice (Vk.destroyDescriptorPool Nothing dpool)
  consume <$> Data.Linear.traverse (withDevice . Vk.destroyDescriptorSetLayout Nothing) set_bindings

-- | Create a DescriptorSetLayout for a group of bindings (that represent a set) and their properties.
--
-- DescriptorSetLayouts are created and stored by 'DescriptorPool's.
createDescriptorSetLayout :: BindingsMap -- ^ Binding, type and stage flags for each descriptor in the set to create
                          -> Renderer Vk.DescriptorSetLayout
createDescriptorSetLayout bindingsMap = enterD "createDescriptorSetLayout" $
  let
      makeBinding bindingIx (descriptorType',sflags) =
        Vk.DescriptorSetLayoutBinding { binding = fromIntegral bindingIx
                                      , descriptorType = descriptorType'
                                      , descriptorCount = 1 -- if this binding was an array of multiple items this number would be larger
                                      , stageFlags = sflags
                                      , immutableSamplers = []
                                      }

      layoutInfo = Vk.DescriptorSetLayoutCreateInfo { bindings = V.fromList $ IM.elems $ IM.mapWithKey makeBinding bindingsMap
                                                    , next = ()
                                                    , flags = Vk.zero
                                                    }

   in withDevice (Vk.createDescriptorSetLayout Nothing layoutInfo)
