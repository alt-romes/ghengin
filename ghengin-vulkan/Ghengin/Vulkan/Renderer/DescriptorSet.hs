{-# LANGUAGE ScopedTypeVariables #-}
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

import qualified Prelude
import Prelude.Linear
import Control.Functor.Linear as Linear
import Control.Monad.IO.Class.Linear
import qualified Data.Functor.Linear as Data.Linear

import Data.Bits
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List as L
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import qualified Data.IntMap.Internal as IMI
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Zero as Vk
import qualified Vulkan as Vk

import qualified FIR hiding (ShaderPipeline, (:>->))
import qualified FIR.Definition as FIR
import qualified SPIRV.Decoration as SPIRV
import qualified SPIRV.PrimTy as SPIRV
import qualified SPIRV.Storage

import Ghengin.Vulkan.Renderer.Buffer
import Ghengin.Vulkan.Renderer.Image
import Ghengin.Vulkan.Renderer.Sampler
import Ghengin.Vulkan.Renderer.Kernel
-- import Ghengin.Vulkan.Renderer.Utils
-- import qualified Ghengin.Asset.Texture as T

import Ghengin.Core.Shader.Pipeline

import Ghengin.Vulkan.Renderer.Texture
import Ghengin.Vulkan.Renderer.Pipeline (stageFlag)

import Data.Counted as Counted
import qualified Data.Counted.Unsafe as Unsafe

import qualified FIR.Layout
import qualified Unsafe.Linear as Unsafe

-------- Resources ----------------
-- (INLINED from hsig file)
-- Resources are a part of the descriptor set module since these resources are
-- used to manipulate the descriptors of the descriptor set.
--
-- e.g. a mapped buffer resource can be bound by a descriptor such that using
-- that descriptor in the shader will read the buffer resource

type ResourceMap = IntMap DescriptorResource

data DescriptorResource where
  UniformResource   :: RefC MappedBuffer ⊸ DescriptorResource
  Texture2DResource :: RefC Texture2D ⊸ DescriptorResource

type Size = Word

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
                  let tsize = fromIntegral (sizeOfPrimTy primTy)

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
createDescriptorPool :: ShaderPipeline info -> Renderer DescriptorPool
createDescriptorPool sp =
  case createDescriptorSetBindingsMap sp of
    dsetmap -> Linear.do

      layouts <- Data.Linear.traverse (Unsafe.toLinear \bm -> (,bm) <$> createDescriptorSetLayout bm) dsetmap

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

      descriptorPool <- unsafeUseDevice (\device -> Vk.createDescriptorPool device poolInfo Nothing)
      pure (DescriptorPool descriptorPool layouts)

destroyDescriptorPool :: DescriptorPool ⊸ Renderer ()
destroyDescriptorPool = Unsafe.toLinear \p -> Linear.do
  unsafeUseDevice (\dev -> Vk.destroyDescriptorPool dev p._pool Nothing)
  consume_empty_IntMap <$> Data.Linear.traverse (destroyDescriptorSetLayout . Unsafe.toLinear fst) p._set_bindings
    where
      destroyDescriptorSetLayout :: Vk.DescriptorSetLayout ⊸ Renderer ()
      destroyDescriptorSetLayout = Unsafe.toLinear \layout -> unsafeUseDevice (\dev -> Vk.destroyDescriptorSetLayout dev layout Nothing)


-- | Create a DescriptorSetLayout for a group of bindings (that represent a set) and their properties.
--
-- DescriptorSetLayouts are created and stored by 'DescriptorPool's.
createDescriptorSetLayout :: BindingsMap -- ^ Binding, type and stage flags for each descriptor in the set to create
                          -> Renderer Vk.DescriptorSetLayout
createDescriptorSetLayout bindingsMap =
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

   in unsafeUseDevice (\device -> Vk.createDescriptorSetLayout device layoutInfo Nothing)




-------------------------------
--- : | Descriptor Sets | : ---
-------------------------------


data DescriptorSet
  = DescriptorSet { _ix :: Ur Int
                  , _descriptorSet :: Vk.DescriptorSet
                  }

instance Counted DescriptorSet where
  countedFields _ = []

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
allocateEmptyDescriptorSet :: Int -- ^ The set to allocate by index
                      -> DescriptorPool -- ^ The descriptor pool associated with a shader pipeline in which the descriptor sets will be used
                       ⊸ Renderer (DescriptorSet, DescriptorPool)
allocateEmptyDescriptorSet ix = fmap (Unsafe.toLinear \case ([ds], x) -> (ds, x); _ -> error $ "Internal error: Failed to allocate a single descriptor set #" <> show ix
                                ) . allocateEmptyDescriptorSets [ix]

-- | Like 'allocateEmptyDescriptorSet' but allocate multiple sets at once
allocateEmptyDescriptorSets :: [Int]    -- ^ The sets to allocate by Ix
                      -> DescriptorPool -- ^ The descriptor pool associated with a shader pipeline in which the descriptor sets will be used
                       ⊸ Renderer (Vector DescriptorSet, DescriptorPool)
allocateEmptyDescriptorSets ixs = Unsafe.toLinear \dpool -> Linear.do
  let
      sets :: [(Vk.DescriptorSetLayout, BindingsMap)]
      sets = map (Unsafe.toLinear \i -> case IM.lookup i dpool._set_bindings of
                           Just x -> x
                           Nothing -> error $ "Internal error: We're trying to allocate a descriptor set #" <> show i <> " with no bindings."
                  ) ixs
                  -- The lookup will be nothing if we are trying to allocate a
                  -- descriptor set type #i, but there exist no bindings on set #1.
                  -- In that case, we don't allocate that descriptor set (which
                  -- might result in an empty returned vector)

      allocInfo = Vk.DescriptorSetAllocateInfo { descriptorPool = dpool._pool
                                               , setLayouts = V.fromList $ map (Unsafe.toLinear fst) sets
                                               , next = ()
                                               }
  let vixs = V.map Ur $ V.fromList ixs
  -- Allocate the descriptor sets
  descriptorSets <- unsafeUseDevice (\dev -> V.zipWith DescriptorSet Prelude.<$> Prelude.pure vixs Prelude.<*> Vk.allocateDescriptorSets dev allocInfo)
  pure (descriptorSets, dpool)

  -- ROMES:TODO: make update/writing explicit and allocate only allocates the empty set
  -- This also would allow us to change the special hacks in updateDescriptorSet to an assertion
  -- Rename to allocate*Empty*descriptorSet
  -- DONE! Just delete the comments when this all works...

  -- Return a closure that when applied to a resource map will write the
  -- descriptor set with the resources information and then return it
  -- V.zipWithM (\ix dset resources -> do
  --                     -- Write the descriptor set with the allocated resources
  --                     updateDescriptorSet dset resources
  --                     pure $ DescriptorSet ix dset resources) ixs descriptorSets
  
-- | Update the configuration of a descriptor set with multiple resources (e.g. buffers + images)
updateDescriptorSet :: DescriptorSet -- ^ The descriptor set we're updating with these resources
                     ⊸ ResourceMap
                     ⊸ Renderer (DescriptorSet, ResourceMap)
updateDescriptorSet = Unsafe.toLinear2 \(DescriptorSet uix dset) resources -> Linear.do
  -- Ach... the resource map must only be freed when the descriptor set is no longer in use... right?

  let makeDescriptorWrite i = \case
        UniformResource buf ->
          -- Each descriptor only has one buffer. If we had an array of buffers in a descriptor we would need multiple descriptor buffer infos
          let bufferInfo = Vk.DescriptorBufferInfo
                                               { buffer = (Unsafe.get buf).buffer
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

                                    -- snd is safe bc despite ignoring talias here, we return it unchanged in the resources list
        Texture2DResource talias -> Unsafe.toLinear snd $ use talias $ Unsafe.toLinear $ \(Texture2D vkimage sampler) ->
          let imageInfo = Vk.DescriptorImageInfo { imageLayout = Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
                                                 , imageView = vkimage._imageView
                                                 , sampler   = Unsafe.toLinear snd $ use sampler $ Unsafe.toLinear $ \s -> (s, s.sampler) -- same unsafe as above
                                                 }
           in ( Texture2D vkimage sampler
              , Vk.SomeStruct Vk.WriteDescriptorSet
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
              )

  unsafeUseDevice (\dev -> Vk.updateDescriptorSets dev (V.fromList $ IM.elems $ IM.mapWithKey makeDescriptorWrite resources) [])
  pure (DescriptorSet uix dset, resources)

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
--
-- TODO: The descriptor sets don't need to be freed! perhaps make them unrestricted elsewhere and get rid of this function
freeDescriptorSet :: DescriptorSet ⊸ Renderer ()
freeDescriptorSet (DescriptorSet (Ur _ix) _dset) = Unsafe.toLinear (\_ -> pure ()) _dset

freeResourceMap :: ResourceMap ⊸ Renderer ()
freeResourceMap resmap = consume_empty_IntMap <$> Data.Linear.traverse freeDescriptorResource resmap

freeDescriptorResource :: DescriptorResource ⊸ Renderer ()
freeDescriptorResource = \case
  UniformResource u -> assertLast u >>= Counted.forget
  -- ROMES:TODO: It really should be refcounted, right?... should I ensure this is the last reference?
  --  -> destroyMappedBuffer u
  -- ROMES:TODO: What will be of this? I guess Texture2D will be RefCounted field of DescriptorResource!
  -- u@(Texture2DResource _) -> pure () -- The resources are being freed on the freeMaterial function, but this should probably be rethought

-- Util
consume_empty_IntMap :: IntMap () ⊸ ()
consume_empty_IntMap = Unsafe.toLinear \_ -> ()


-- Aren't these unsafe ? :)

instance Data.Linear.Functor IntMap where
  fmap f x = Unsafe.toLinear2 Prelude.fmap (Unsafe.toLinear f) x
  {-# INLINE fmap #-}

instance Data.Linear.Traversable IntMap where
  traverse :: ∀ t a b. Data.Linear.Applicative t => (a %1 -> t b) -> IntMap a %1 -> t (IntMap b)
  traverse f = go
    where
      go :: IntMap a %1 -> t (IntMap b)
      go IMI.Nil = Data.Linear.pure IMI.Nil
      go (IMI.Tip k v) = Unsafe.toLinear (\k' -> IMI.Tip k' Data.Linear.<$> f v) k
      go bin = Unsafe.toLinear (\(IMI.Bin p m l r) ->
          if m < 0
             then Data.Linear.liftA2 (flip (IMI.Bin p m)) (go r) (go l)
             else Data.Linear.liftA2 (IMI.Bin p m) (go l) (go r)
        ) bin
  {-# INLINE traverse #-}

