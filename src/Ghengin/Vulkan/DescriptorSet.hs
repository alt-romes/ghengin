{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
module Ghengin.Vulkan.DescriptorSet where

import Control.Monad
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List as L
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Zero as Vk
import qualified Vulkan as Vk

import qualified Ghengin.Shaders.FIR as FIR
import qualified FIR.Definition as FIR
import qualified SPIRV.Decoration as SPIRV
import qualified SPIRV.PrimTy as SPIRV
import qualified SPIRV.Storage
import Ghengin.Utils
import Ghengin.Vulkan.Buffer
import Ghengin.Vulkan.Image
import Ghengin.Vulkan.Sampler
import Ghengin.Vulkan
import Ghengin.Vulkan.Utils
import qualified Ghengin.Asset.Texture as T

import qualified FIR.Layout

-- | Mapping from each binding to corresponding binding type, size, shader stage
type BindingsMap = IntMap (Vk.DescriptorType, Size, Vk.ShaderStageFlags)

-- | Mapping from each descriptor set ix to its bindings map
type DescriptorSetMap = IntMap BindingsMap

data SomeDefs = ∀ defs. FIR.KnownDefinitions defs => SomeDefs

-- :| From Shaders |:


-- | Creates a mapping from descriptor set indexes to a list of their bindings
-- (corresponding binding type, size, shader stage flags) solely from the shader
-- pipeline definition.
--
-- This might be a slow
-- function so one should be careful calling it too often.  A more performant
-- less simple alternative should be added...
createDescriptorSetBindingsMap :: FIR.PipelineStages info () -> DescriptorSetMap
createDescriptorSetBindingsMap ppstages = makeDescriptorSetMap (go mempty ppstages)
  where
    go :: Map FIR.Shader [(SPIRV.PointerTy,SomeDefs,SPIRV.Decorations)]
       -> FIR.PipelineStages info ()
       -> Map FIR.Shader [(SPIRV.PointerTy,SomeDefs,SPIRV.Decorations)] -- ^ For each shader, the sets, corresponding decorations, and corresponding storable data types
    go acc FIR.VertexInput = acc
    go acc (pipe FIR.:>-> (FIR.ShaderModule _ :: FIR.ShaderModule name stage defs endState,())) =
      go (M.insertWith (<>) (FIR.knownValue @stage) (map (\(pt,dc) -> (pt,SomeDefs @defs,dc)) $ M.elems $ FIR.globalAnnotations $ FIR.annotations @defs) acc) pipe


    makeDescriptorSetMap :: Map FIR.Shader [(SPIRV.PointerTy, SomeDefs, SPIRV.Decorations)]
                         -> DescriptorSetMap -- ^ Mapping from descriptor set indexes to a list of their bindings (corresponding binding type, shader stage)
    makeDescriptorSetMap =
      M.foldrWithKey (\shader ls acc' -> 
        foldr (\(pt@(SPIRV.PointerTy _ primTy),somedefs,S.toList -> decs) acc ->
          case decs of
            [SPIRV.Binding (fromIntegral -> bindingIx), SPIRV.DescriptorSet (fromIntegral -> descriptorSetIx)] ->
              case somedefs of
                SomeDefs @defs ->
                  -- For each descriptor we compute the buffer size from  the known definitionn
                  -- let tsize = getDescriptorBindingSize @defs descriptorSetIx bindingIx
                  let tsize = fromIntegral (sizeOfPrimTy primTy)

                   in IM.insertWith mergeSameDS descriptorSetIx
                                (IM.singleton bindingIx (descriptorType pt, tsize, stageFlag shader))
                                acc
            _ -> acc -- we keep folding. currently we don't validate anything futher
          ) acc' ls
        ) mempty

    mergeSameDS :: BindingsMap
                -> BindingsMap
                -> BindingsMap
    mergeSameDS = IM.mergeWithKey (\_ (dt,ss,sf) (dt',_ss',sf') -> if dt == dt' then Just (dt, ss, sf .|. sf')
                                                                                else error $ "Incompatible descriptor type: " <> show dt <> " and " <> show dt') id id -- TODO: Could pattern match on type equality too?


-- unused
-- type family FindDescriptorType (set :: Nat) (binding :: Nat) (defs :: [FIR.Definition]) :: Nat where
--   FindDescriptorType set binding (FIR.Global _ '[SPIRV.DescriptorSet set, SPIRV.Binding binding] t:ds) = Ghengin.Utils.SizeOf t
--   FindDescriptorType set binding (_:ds) = FindDescriptorType set binding ds
--   FindDescriptorType set binding '[] = TypeError (Text "Error computing descriptor type")

-- What I couldn't do: Prove that the type family application results in a type that always instances a certain class.

sizeOfPrimTy :: SPIRV.PrimTy -> Size
sizeOfPrimTy x = case FIR.Layout.primTySizeAndAli FIR.Layout.Extended x of
                   Left e -> error (show e)
                   Right (size,_alignment) -> fromIntegral size


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
  DescriptorPool { _pool :: Vk.DescriptorPool
                 , _set_bindings :: IntMap (Vk.DescriptorSetLayout, BindingsMap)
                 }

-- | Todo: Linear Types. The created pool must be freed.
--
-- Creates a pool as described in Note [Pools].
--
-- TODO: Right amount of descriptors. For now we simply multiply 1000 by the
-- number of all total descriptors across sets
createDescriptorPool :: DescriptorSetMap -> Renderer χ DescriptorPool
createDescriptorPool dsetmap = do

  layouts <- traverse (\bm -> (,bm) <$> createDescriptorSetLayout bm) dsetmap

  let 
    descriptorsAmounts :: [(Vk.DescriptorType, Int)] -- ^ For each type, its amount
    descriptorsAmounts = map (\(t :| ls) -> (t, 1000 * (length ls + 1))) . NonEmpty.group . L.sort $ foldMap (foldr (\(ty,_,_) -> (ty:)) mempty) dsetmap
    poolsSizes = map (\(t,fromIntegral -> a) -> Vk.DescriptorPoolSize {descriptorCount = a, type' = t}) descriptorsAmounts

    setsAmount = fromIntegral $ length dsetmap
    poolInfo = Vk.DescriptorPoolCreateInfo { poolSizes = V.fromList poolsSizes
                                           , maxSets = 1000 * setsAmount
                                           , flags = Vk.zero
                                           , next = ()
                                           }

  device <- getDevice
  descriptorPool <- Vk.createDescriptorPool device poolInfo Nothing
  pure (DescriptorPool descriptorPool layouts)

destroyDescriptorPool :: DescriptorPool -> Renderer χ ()
destroyDescriptorPool p = getDevice >>= \dev -> do
  Vk.destroyDescriptorPool dev p._pool Nothing
  _ <- traverse (destroyDescriptorSetLayout . fst) p._set_bindings
  pure ()
    where
      destroyDescriptorSetLayout :: Vk.DescriptorSetLayout -> Renderer ext ()
      destroyDescriptorSetLayout layout = getDevice >>= \dev -> Vk.destroyDescriptorSetLayout dev layout Nothing


-- | Create a DescriptorSetLayout for a group of bindings (that represent a set) and their properties.
--
-- DescriptorSetLayouts are created and stored by 'DescriptorPool's.
createDescriptorSetLayout :: BindingsMap -- ^ Binding, type and stage flags for each descriptor in the set to create
                          -> Renderer ext Vk.DescriptorSetLayout
createDescriptorSetLayout bindingsMap = getDevice >>= \device -> do

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

  Vk.createDescriptorSetLayout device layoutInfo Nothing




-------------------------------
--- : | Descriptor Sets | : ---
-------------------------------


data DescriptorSet
  = DescriptorSet { _ix :: Int
                  , _descriptorSet :: Vk.DescriptorSet
                  , _bindings      ::  ResourceMap -- ^ TODO: Rename to _resources instead of _bindings?
                  }
  | EmptyDescriptorSet -- ^ TODO: We don't export this constructor?

type ResourceMap = IntMap DescriptorResource

data DescriptorResource = UniformResource   MappedBuffer
                        | Texture2DResource T.Texture2D

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
allocateDescriptorSet :: Int -- ^ The set to allocate by index
                      -> DescriptorPool -- ^ The descriptor pool associated with a shader pipeline in which the descriptor sets will be used
                      -> Renderer χ (ResourceMap -> Renderer χ DescriptorSet)
allocateDescriptorSet ix = fmap (\case [ds] -> ds; _ -> error $ "Internal error: Failed to allocate a single descriptor set #" <> show ix
                                ) . allocateDescriptorSets [ix]

-- | Like 'allocateDescriptorSet' but allocate multiple sets at once
allocateDescriptorSets :: Vector Int -- ^ The sets to allocate by Ix
                      -> DescriptorPool -- ^ The descriptor pool associated with a shader pipeline in which the descriptor sets will be used
                      -> Renderer χ (Vector (ResourceMap -> Renderer χ DescriptorSet))
allocateDescriptorSets ixs dpool = getDevice >>= \device -> do
  let
      sets :: Vector (Vk.DescriptorSetLayout, BindingsMap)
      sets = fmap (\i -> case IM.lookup i dpool._set_bindings of
                           Just x -> x
                           Nothing -> error $ "Internal error: We're trying to allocate a descriptor set #" <> show i <> " with no bindings."
                  ) ixs
                  -- The lookup will be nothing if we are trying to allocate a
                  -- descriptor set type #i, but there exist no bindings on set #1.
                  -- In that case, we don't allocate that descriptor set (which
                  -- might result in an empty returned vector)

      allocInfo = Vk.DescriptorSetAllocateInfo { descriptorPool = dpool._pool
                                               , setLayouts = fmap fst sets
                                               , next = ()
                                               }

  -- Allocate the descriptor sets
  descriptorSets <- Vk.allocateDescriptorSets device allocInfo

  -- Return a closure that when applied to a resource map will write the
  -- descriptor set with the resources information and then return it
  pure $ V.zipWith (\ix dset resources -> do
                        -- Write the descriptor set with the allocated resources
                        updateDescriptorSet dset resources
                        pure $ DescriptorSet ix dset resources) ixs descriptorSets
  
-- | Update the configuration of a descriptor set with multiple resources (e.g. buffers + images)
updateDescriptorSet :: Vk.DescriptorSet -- ^ The descriptor set we're writing with these resources
                    -> ResourceMap
                    -> Renderer ext ()
updateDescriptorSet dset resources = do

  let makeDescriptorWrite i = \case
        UniformResource buf -> do
          -- Each descriptor only has one buffer. If we had an array of buffers in a descriptor we would need multiple descriptor buffer infos
          let bufferInfo = Vk.DescriptorBufferInfo
                                               { buffer = buf.buffer
                                               , offset = 0
                                               , range  = Vk.WHOLE_SIZE -- the whole buffer
                                               }

           in Vk.SomeStruct Vk.WriteDescriptorSet
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
        Texture2DResource (T.Texture2D vkimage sampler _) -> do
          let imageInfo = Vk.DescriptorImageInfo { imageLayout = Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
                                                 , imageView = vkimage._imageView
                                                 , sampler   = sampler.sampler
                                                 }

           in Vk.SomeStruct Vk.WriteDescriptorSet
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

  dev <- getDevice

  -- Only issue the call if we're actually updating something.
  -- This allows us to use the dummy resources trick in 'material'
  -- TODO: Note Dummy Resource or a good inline comment...
  when (IM.size resources > 0) $
    Vk.updateDescriptorSets dev (V.fromList $ IM.elems $ IM.mapWithKey makeDescriptorWrite resources) []

-- | Destroy a descriptor set 
--
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
--
-- TODO: Write this to the note
destroyDescriptorSet :: DescriptorSet -> Renderer ext ()
destroyDescriptorSet EmptyDescriptorSet = pure ()
destroyDescriptorSet (DescriptorSet _ix _dset dresources) = do
  _ <- traverse freeDescriptorResource dresources
  pure ()

freeDescriptorResource :: DescriptorResource -> Renderer ext ()
freeDescriptorResource = \case
  UniformResource u -> destroyMappedBuffer u
  u@(Texture2DResource _) -> pure () -- The resources are being freed on the freeMaterial function, but this should probably be rethought

getUniformBuffer :: DescriptorSet -> Int -> MappedBuffer
getUniformBuffer dset i = case IM.lookup i dset._bindings of
                            Just (UniformResource b) -> b
                            Nothing -> error $ "Expecting a uniform descriptor resource at binding " <> show i <> " but found nothing!"
                            _ -> error $ "Expecting the descriptor resource at binding " <> show i <> " to be a uniform!"

