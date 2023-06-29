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
module Ghengin.Vulkan.Renderer.DescriptorSet where

import Debug.Trace
import Ghengin.Core.Prelude as Linear
import Ghengin.Core.Log
import qualified Data.Functor.Linear as Data.Linear
import qualified Prelude

import Data.Linear.Alias.Unsafe as Unsafe.Alias
import qualified Unsafe.Linear as Unsafe

import Data.Bifunctor.Linear
import Data.Bits

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.IntMap.Linear as IML
import qualified Data.IntMap.Strict.Internal as IMI
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

import Ghengin.Vulkan.Renderer.Texture
import Ghengin.Vulkan.Renderer.Pipeline (stageFlag)

import Ghengin.Core.Shader.Pipeline

import qualified Data.Linear.Alias as Alias

import qualified FIR.Layout

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

type ResourceMap = IntMap DescriptorResource

data DescriptorResource where
  UniformResource   :: Alias MappedBuffer ⊸ DescriptorResource
  Texture2DResource :: Alias Texture2D ⊸ DescriptorResource
  deriving Generic

instance Aliasable DescriptorResource where
  -- Reliable automatic instance through generic still not working (TODO)
  countedFields (UniformResource x) = [SomeAlias x]
  countedFields (Texture2DResource x) = [SomeAlias x]

instance Forgettable Renderer DescriptorResource where
  forget = \case
    UniformResource u -> Alias.forget u
    Texture2DResource t -> Alias.forget t

instance Shareable m DescriptorResource where
  share = \case
    UniformResource u -> bimap UniformResource UniformResource <$> Alias.share u
    Texture2DResource t -> bimap Texture2DResource Texture2DResource <$> Alias.share t

-- | Mapping from each binding to corresponding binding type, size, shader stage
-- We have a maybe word because not every binding has a layout in memory (images don't)
type BindingsMap = IntMap (Vk.DescriptorType, Maybe Word, Vk.ShaderStageFlags)

instance Consumable BindingsMap where
  consume = Unsafe.toLinear \_bm -> ()
instance Dupable BindingsMap where
  dup2 = Unsafe.toLinear \bm -> (bm,bm)
instance Movable BindingsMap where
  move = Unsafe.toLinear \bm -> Ur bm

-- | Mapping from each descriptor set ix to its bindings map
type DescriptorSetMap = IntMap BindingsMap

data SomeDefs = ∀ defs. FIR.KnownDefinitions defs => SomeDefs
instance Show SomeDefs where
  show _ = "SomeDefs"

-- :| From Shaders |:


-- | Creates a mapping from descriptor set indexes to a list of their bindings
-- (corresponding binding type, size, shader stage flags) solely from the shader
-- pipeline definition.
--
-- This might be a slow
-- function so one should be careful calling it too often.  A more performant
-- less simple alternative should be added...
createDescriptorSetBindingsMap :: ShaderPipeline info -> DescriptorSetMap
createDescriptorSetBindingsMap ppstages = makeDescriptorSetMap (go Prelude.mempty ppstages)
  where
    go :: Map FIR.Shader [(SPIRV.PointerTy,SomeDefs,SPIRV.Decorations)]
       -> ShaderPipeline info
       -> Map FIR.Shader [(SPIRV.PointerTy,SomeDefs,SPIRV.Decorations)] -- ^ For each shader, the sets, corresponding decorations, and corresponding storable data types
    go acc (ShaderPipeline FIR.VertexInput) = acc
    go acc (pipe :>-> (FIR.ShaderModule _ :: FIR.ShaderModule name stage defs endState)) =
      go (M.insertWith (Prelude.<>) (FIR.knownValue @stage) (map (\(pt,dc) -> (pt,SomeDefs @defs,dc)) $ M.elems $ FIR.globalAnnotations $ FIR.annotations @defs) acc) pipe


    makeDescriptorSetMap :: Map FIR.Shader [(SPIRV.PointerTy, SomeDefs, SPIRV.Decorations)]
                         -> DescriptorSetMap -- ^ Mapping from descriptor set indexes to a list of their bindings (corresponding binding type, shader stage)
    makeDescriptorSetMap =
      M.foldrWithKey (\shader ls acc' -> 
        Prelude.foldr (\(pt@(SPIRV.PointerTy _ primTy),somedefs,S.toList -> decs) acc ->
          case decs of
            [SPIRV.Binding (fromIntegral -> bindingIx), SPIRV.DescriptorSet (fromIntegral -> descriptorSetIx)] ->
              case somedefs of
                SomeDefs @defs ->
                  -- For each descriptor we compute the buffer size from  the known definitionn
                  -- let tsize = getDescriptorBindingSize @defs descriptorSetIx bindingIx
                  let tsize = sizeOfPrimTy primTy

                   in IM.insertWith mergeSameDS descriptorSetIx
                                (IM.singleton bindingIx (descriptorType pt, tsize, stageFlag shader))
                                acc
            _ -> acc -- we keep folding. currently we don't validate anything futher
          ) acc' ls
        ) Prelude.mempty

    mergeSameDS :: BindingsMap
                -> BindingsMap
                -> BindingsMap
    mergeSameDS = IM.mergeWithKey (\_ (dt,ss,sf) (dt',_ss',sf') -> if dt Prelude.== dt' then Just (dt, ss, sf .|. sf')
                                                                                else error $ "Incompatible descriptor type: " <> show dt <> " and " <> show dt') id id -- TODO: Could pattern match on type equality too?


-- unused
-- type family FindDescriptorType (set :: Nat) (binding :: Nat) (defs :: [FIR.Definition]) :: Nat where
--   FindDescriptorType set binding (FIR.Global _ '[SPIRV.DescriptorSet set, SPIRV.Binding binding] t:ds) = Ghengin.Utils.SizeOf t
--   FindDescriptorType set binding (_:ds) = FindDescriptorType set binding ds
--   FindDescriptorType set binding '[] = TypeError (Text "Error computing descriptor type")

-- What I couldn't do: Prove that the type family application results in a type that always instances a certain class.

-- ROMES:TODO: Drop this completely, the size type is unused and even wrong since not all things have a prim ty size like this?
-- Delete the corresponding entry from "BindingsMap" too
sizeOfPrimTy :: SPIRV.PrimTy -> Maybe Word
sizeOfPrimTy x = case FIR.Layout.primTySizeAndAli FIR.Layout.Extended x of
                   Left e -> Nothing
                   Right (size,_alignment) -> Just (fromIntegral size)


descriptorType :: SPIRV.PointerTy -> Vk.DescriptorType
descriptorType = \case
  SPIRV.PointerTy SPIRV.Storage.Uniform _ -> Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
  -- SPIRV.PointerTy SPIRV.Storage.UniformConstant SPIRV.Sampler -> Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  SPIRV.PointerTy SPIRV.Storage.UniformConstant (SPIRV.SampledImage _) -> Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  x -> error $ "Unexpected/unsupported descriptor set #1 descriptor type: " <> show x


---------------------
--- : | Pools | : ---
---------------------

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
                 , set_bindings :: IntMap (Vk.DescriptorSetLayout, BindingsMap)
                 }

instance Aliasable DescriptorPool where
  countedFields _ = []
  {-# INLINE countedFields #-}

-- | Todo: Linear Types. The created pool must be freed.
--
-- Creates a pool as described in Note [Pools].
--
-- TODO: Right amount of descriptors. For now we simply multiply 1000 by the
-- number of all total descriptors across sets
createDescriptorPool :: ShaderPipeline info -> Renderer DescriptorPool
createDescriptorPool sp = enterD "createDescriptorPool" $
  case createDescriptorSetBindingsMap sp of
    dsetmap -> Linear.do

      layouts <- Data.Linear.traverse (\bm -> case move bm of Ur bm1 -> (,bm1) <$> createDescriptorSetLayout bm1) dsetmap

      let 
        descriptorsAmounts :: [(Vk.DescriptorType, Int)] -- ^ For each type, its amount
        descriptorsAmounts = Prelude.map (\(t :| ls) -> (t, 1000 * (Prelude.length ls + 1))) Prelude.. NonEmpty.group Prelude.. L.sort $ Prelude.foldMap (Prelude.foldr (\(ty,_,_) -> (ty:)) Prelude.mempty) dsetmap
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
  consume <$> Data.Linear.traverse (withDevice . Vk.destroyDescriptorSetLayout Nothing . fst) set_bindings

-- | Create a DescriptorSetLayout for a group of bindings (that represent a set) and their properties.
--
-- DescriptorSetLayouts are created and stored by 'DescriptorPool's.
createDescriptorSetLayout :: BindingsMap -- ^ Binding, type and stage flags for each descriptor in the set to create
                          -> Renderer Vk.DescriptorSetLayout
createDescriptorSetLayout bindingsMap = enterD "createDescriptorSetLayout" $
  let
      makeBinding bindingIx (descriptorType',_ss,sflags) =
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

-------------------------------
--- : | Descriptor Sets | : ---
-------------------------------


data DescriptorSet
  = DescriptorSet { _ix :: Int
                  , _descriptorSet :: Vk.DescriptorSet
                  }

instance Aliasable DescriptorSet where
  countedFields _ = []
  {-# INLINE countedFields #-}

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
    extract (ds,p)   = Linear.do
      p' <- Alias.newAlias destroyDescriptorPool p
      freeDescriptorSets p' ds -- This will destroy certainly destroy the pool, since we've just created the reference and haven't shared it.
      error $ "Internal error: Failed to allocate a single descriptor set #" <> show ix

-- | Like 'allocateEmptyDescriptorSet' but allocate multiple sets at once
-- INVARIANT: The Int vector does not have duplicate Ints
allocateEmptyDescriptorSets :: ∀ n. V n Int   -- ^ The sets to allocate by Ix
                            -> DescriptorPool -- ^ The descriptor pool associated with a shader pipeline in which the descriptor sets will be used
                             ⊸ Renderer (V n DescriptorSet, DescriptorPool)
allocateEmptyDescriptorSets ixs DescriptorPool{..} = enterD "allocateEmptyDescriptorSets" $ Linear.do
  -- This is one of those for which I really should have just used Unsafeness, right...
  -- What I think I really need for these cases is a sorts of *framing rule* as
  -- that in separation logic

  -- Extract the layouts info needed for allocation out of the dpool map
  (to_alloc, the_rest)  <- pure $ IML.partitionByKeys ixs set_bindings
  (keys, to_alloc_tups) <- pure $ unzip $ IML.toList $ to_alloc
  (layouts, bindingsxs) <- pure $ unzip $ to_alloc_tups

  -- Allocate the descriptor sets
  (dsets, V layouts, dpool) <- enterD "Allocate the descriptor sets" $
    withDevice (Vk.allocateDescriptorSets dpool (V @n (l2vec layouts))) -- @n since the partition takes @n@ integers (well, only if the integer list is disjoint...)

  -- Reconstruct things
  (to_alloc_tups, Nothing) <- pure $ zip' (vec2l layouts) bindingsxs
  (to_alloc,Nothing)       <- pure $ zip' keys to_alloc_tups
  set_bindings  <- pure $ IML.unionWith (Unsafe.toLinear2 \_ _ -> error "impossible") (IML.fromList to_alloc) the_rest

  pure (vzipWith DescriptorSet ixs dsets, DescriptorPool dpool set_bindings)
  
-- | Update the configuration of a descriptor set with multiple resources (e.g. buffers + images)
updateDescriptorSet :: DescriptorSet -- ^ The descriptor set we're updating with these resources
                     ⊸ ResourceMap
                     ⊸ Renderer (DescriptorSet, ResourceMap)
updateDescriptorSet = Unsafe.toLinear2 \(DescriptorSet uix dset) resources -> Linear.do

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

        Texture2DResource talias ->
          case Unsafe.Alias.get talias of
            (Texture2D vkimage sampler) ->
              let imageInfo = Vk.DescriptorImageInfo { imageLayout = Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
                                                     , imageView = vkimage._imageView
                                                     , sampler   = (Unsafe.Alias.get sampler).sampler
                                                     }
               in pure $ Vk.SomeStruct Vk.WriteDescriptorSet
                    { next = ()
                    , dstSet = dset -- the descriptor set to update with this write
                    , dstBinding = fromIntegral i
                    , dstArrayElement = 0 -- Descriptors could be arrays. We just use 0
                    , descriptorType = Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER -- The type of buffer
                    , descriptorCount = 1 -- Only one buffer in the array of buffers to update
                    , bufferInfo = [] -- The one buffer info
                    , imageInfo = [imageInfo]
                    , texelBufferView = []
                    }


  -- The difficulty in making this linear is that we can't traverse and update
  -- one thing at a time, we must write them all first, then call update on the
  -- whole batch
  writeInfos <- IML.traverseWithKey (Unsafe.toLinear2 makeDescriptorWriteInfo) resources
  -- How can I do this well? It's really not immediatly clear

  useDevice (Unsafe.toLinear Vk.updateDescriptorSets (l2vec $ IML.elems writeInfos) [])
  pure (DescriptorSet uix dset, resources)

-- | Destroy a descriptor set 
--
-- I think this comment is outdated:
-- We must be careful here not to free resources shared across materials
--
-- (1) Uniform buffers are allocated per-material, so we always free them
--
-- (2) Texture resources are allocated outside of the material and might be
-- shared, so we never free them for now. Eventually they might be
-- automatically managed through medit and reference counting.
--
-- The texture resource is used to update the descriptor set to point to that
-- texture
--
freeDescriptorSets :: Alias DescriptorPool ⊸ V n DescriptorSet ⊸ Renderer ()
freeDescriptorSets dpoolA dsets = Linear.do
  (DescriptorPool{..}, f) <- Alias.get dpoolA
  dpool1 <- withDevice (Vk.freeDescriptorSets dpool (case VL.map (\(DescriptorSet ix dset) -> ix `lseq` dset) dsets of V v -> v))
  f (DescriptorPool{dpool=dpool1,..})
  return ()

freeResourceMap :: ResourceMap ⊸ Renderer ()
freeResourceMap = Alias.forget

-- | Forget a descriptor resource. This might free the resource if it's the last reference to it
-- (A DescriptorResource wraps a Reference Counted value)
freeDescriptorResource :: DescriptorResource ⊸ Renderer ()
-- ROMES:TODO: Should I be ensuring this is the last reference here?
freeDescriptorResource = Alias.forget

