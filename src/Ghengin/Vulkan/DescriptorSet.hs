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
import Ghengin.Vulkan
import Ghengin.Shaders

import qualified FIR.Layout

-- | Mapping from each binding to corresponding binding type, size, shader stage
type BindingsMap = IntMap (Vk.DescriptorType, Size, Vk.ShaderStageFlags)

-- | Mapping from each descriptor set ix to its bindings map
type DescriptorSetMap = IntMap BindingsMap

data SomeDefs = ∀ defs. FIR.KnownDefinitions defs => SomeDefs

-- :| From Shaders |:


-- | Create all required descriptor sets for a shader pipeline.
--
-- This will create the descriptor set layouts, the pool from where the
-- descriptor sets will be allocated, the actual descriptor sets, the buffers
-- and mapped memory, and will update the descriptor sets with the buffers
-- information
--
-- The descriptor pool will have exact resources for a descriptor set #0, 1000
-- descriptor set #1 (allowing, for now, for up to 1000 materials), 100000
-- descriptor set #2 (allowing, fow now, for up to 1000000 entities).
-- createDescriptorSets :: FIR.PipelineStages info () -- ^ Each shader stage must be associated with a list of the storables in the order of each corresponding descriptor set binding
--                      -> Renderer ext (Vector DescriptorSet, Vk.DescriptorPool)
-- createDescriptorSets ppstages = do

--   let descriptorSetMap :: DescriptorSetMap
--       descriptorSetMap = createDescriptorSetBindingsMap ppstages

--   layoutsAndBuffers :: [(Int,BindingsMap,Vk.DescriptorSetLayout,IntMap MappedBuffer)]
--       <- forM (IM.toAscList descriptorSetMap) $ \(setNumber, bindingsMap) -> do
--           layout <- createDescriptorSetLayout bindingsMap
--           -- TODO; Currently this only creates uniform buffers but later it should also create storage buffers, etc
--           buffers <- traverse (\case (dt,size,_ssf) -> createMappedBuffer size dt) bindingsMap
--           pure (setNumber,bindingsMap,layout,buffers)

--   -- TODO: We currently allocate one descriptor set #1 and never use it because
--   -- we're now allocating a set for each material. We don't need to allocate it
--   -- here, I left it because it's more tiresome to remove it than to leave it.
--   (dsets, dpool) <- allocateDescriptorSets (map (\(_,bs,l,_) -> (l,IM.elems $ fmap (\(dt,_,_) -> dt) bs)) layoutsAndBuffers)

--   -- Depends on the order of the layouts and the dsets matching, which they do
--   configuredDescriptorSets :: Vector DescriptorSet
--     <- V.zipWithM (\(dsetIx, bindingsMap, dslayout, buffers) dset -> do
--                        let finalBindings = IM.intersectionWith (\(dt,_ss,_ssf) b -> (b,dt)) bindingsMap buffers
--                        updateBufferDescriptorSet dset finalBindings
--                        pure (DescriptorSet dsetIx dset dslayout finalBindings)
--                   ) (V.fromList layoutsAndBuffers) dsets

--   pure (configuredDescriptorSets, dpool)


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

-- getDescriptorBindingSize :: ∀ defs. FIR.KnownDefinitions defs
--                          => Int -- ^ descriptor set #
--                          -> Int -- ^ binding #
--                          -> Size
-- getDescriptorBindingSize dsix bix = case
--   foldr
--     (\x acc ->
--       case x of
--         ( SPIRV.PointerTy _ primTy
--           , [SPIRV.Binding (fromIntegral -> bindingIx), SPIRV.DescriptorSet (fromIntegral -> descriptorSetIx)]
--           )
--           | bindingIx == bix && descriptorSetIx == dsix
--           -> fromIntegral (sizeOfPrimTy primTy):acc
--         _ -> acc
--     )
--     mempty
--     (M.elems $ FIR.globalAnnotations $ FIR.annotations @defs) of
--       [size] -> size
--       _ -> error "Err calculating PrimTy size"

sizeOfPrimTy :: SPIRV.PrimTy -> Size
sizeOfPrimTy x = case FIR.Layout.primTySizeAndAli FIR.Layout.Extended x of
                   Left e -> error (show e)
                   Right (size,_alignment) -> fromIntegral size


descriptorType :: SPIRV.PointerTy -> Vk.DescriptorType
descriptorType = \case
  SPIRV.PointerTy sc _ -> case sc of
    SPIRV.Storage.Uniform -> Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
    _ -> error "Unexpected/unsupported storage class for descriptor"



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
    descriptorsAmounts :: [(Vk.DescriptorType, Int)]
    descriptorsAmounts = map (\(t :| ls) -> (t, 1000 * (length ls + 1))) . NonEmpty.group . L.sort $ foldMap (foldr (\(ty,_,_) -> (ty:)) mempty) dsetmap
    poolsSizes = map (\(t,fromIntegral -> a) -> Vk.DescriptorPoolSize {descriptorCount = a, type' = t}) descriptorsAmounts

    setsAmount = fromIntegral $ length dsetmap
    poolInfo = Vk.DescriptorPoolCreateInfo { poolSizes = V.fromList poolsSizes
                                           , maxSets = setsAmount
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


data DescriptorSet = DescriptorSet { _ix :: Int
                                   , _descriptorSet :: Vk.DescriptorSet
                                   , _bindings :: IntMap (MappedBuffer, Vk.DescriptorType) -- ^ Bindings map
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
allocateDescriptorSet :: Int -- ^ The set to allocate by index
                      -> DescriptorPool -- ^ The descriptor pool associated with a shader pipeline in which the descriptor sets will be used
                      -> Renderer χ DescriptorSet
allocateDescriptorSet ix = fmap (\case [ds] -> ds; _ -> error "impossible") . allocateDescriptorSets [ix]

-- | Like 'allocateDescriptorSet' but allocate multiple sets at once
allocateDescriptorSets :: Vector Int -- ^ The sets to allocate by Ix
                      -> DescriptorPool -- ^ The descriptor pool associated with a shader pipeline in which the descriptor sets will be used
                      -> Renderer χ (Vector DescriptorSet)
allocateDescriptorSets ixs dpool = getDevice >>= \device -> do
  let
      sets :: Vector (Vk.DescriptorSetLayout, BindingsMap)
      sets = fmap (dpool._set_bindings IM.!) ixs

      allocInfo = Vk.DescriptorSetAllocateInfo { descriptorPool = dpool._pool
                                               , setLayouts = fmap fst sets
                                               , next = ()
                                               }

  -- Allocate the descriptor sets
  descriptorSets <- Vk.allocateDescriptorSets device allocInfo

  -- Allocate the associated buffers
  -- TODO; Currently this only creates uniform buffers but later it should also create storage buffers, etc
  buffers <- traverse (traverse (\case (dt,size,_ssf) -> (,dt) <$> createMappedBuffer size dt) . snd) sets

  -- Write the descriptor set with the allocated buffers
  V.zipWithM_ updateBufferDescriptorSet descriptorSets buffers

  pure (V.zipWith3 DescriptorSet ixs descriptorSets buffers)
  
-- | Update the configuration of a descriptor set with multiple buffers
updateBufferDescriptorSet :: Vk.DescriptorSet   -- ^ The descriptor set we're writing with these buffers
                          -> IntMap (MappedBuffer, Vk.DescriptorType) -- ^ The buffers, their bindings, and the type of buffer as a DescriptorType
                          -> Renderer ext ()
updateBufferDescriptorSet dset bufs = do

  let makeBufferWrite i (buf, dty) =
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
                                  , descriptorType = dty -- The type of buffer
                                  , descriptorCount = 1 -- Only one buffer in the array of buffers to update
                                  , bufferInfo = [bufferInfo] -- The one buffer info
                                  , imageInfo = []
                                  , texelBufferView = []
                                  }

  dev <- getDevice
  Vk.updateDescriptorSets dev (V.fromList $ IM.elems $ IM.mapWithKey makeBufferWrite bufs) []


destroyDescriptorSet :: DescriptorSet -> Renderer ext ()
destroyDescriptorSet (DescriptorSet _ix _dset dbindings) = do
  _ <- traverse (destroyMappedBuffer . fst) dbindings
  pure ()



