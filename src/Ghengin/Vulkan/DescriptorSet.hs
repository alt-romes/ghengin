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

import GHC.TypeLits
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Control.Monad
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

data DescriptorSet = DescriptorSet { _ix :: Int
                                   , _descriptorSet :: Vk.DescriptorSet
                                   , _descriptorSetLayout :: Vk.DescriptorSetLayout
                                   , _bindings :: IntMap (MappedBuffer, Vk.DescriptorType) -- ^ Bindings map
                                   }

type BindingsMap = IntMap (Vk.DescriptorType, Size, Vk.ShaderStageFlags)

data SomeDefs = ∀ defs. FIR.KnownDefinitions defs => SomeDefs

-- :| From Shaders |:


-- | Create all required descriptor sets for a shader pipeline.
--
-- This will create the descriptor set layouts, the pool from where the
-- descriptor sets will be allocated, the actual descriptor sets, the buffers
-- and mapped memory, and will update the descriptor sets with the buffers
-- information
createDescriptorSets :: FIR.PipelineStages info () -- ^ Each shader stage must be associated with a list of the storables in the order of each corresponding descriptor set binding
                     -> Renderer ext (Vector DescriptorSet, Vk.DescriptorPool)
createDescriptorSets ppstages = do

  let descriptorSetMap :: IntMap BindingsMap -- ^ Map each set ix to a bindings map
      descriptorSetMap = makeDescriptorSetMap (go [] ppstages)

  layoutsAndBuffers :: [(Int,BindingsMap,Vk.DescriptorSetLayout,IntMap MappedBuffer)]
      <- forM (IM.toAscList descriptorSetMap) $ \(setNumber, bindingsMap) -> do
          layout <- createDescriptorSetLayout bindingsMap
          -- TODO; Currently this only creates uniform buffers but later it should also create storage buffers, etc
          buffers <- traverse (\case (dt,size,_ssf) -> createMappedBuffer size dt) bindingsMap
          pure (setNumber,bindingsMap,layout,buffers)

  (dsets, dpool) <- allocateDescriptorSets (map (\(_,bs,l,_) -> (l,IM.elems $ fmap (\(dt,_,_) -> dt) bs)) layoutsAndBuffers)

  -- Depends on the order of the layouts and the dsets matching, which they do
  configuredDescriptorSets :: Vector DescriptorSet
    <- V.zipWithM (\(dsetIx, bindingsMap, dslayout, buffers) dset -> do
                       let finalBindings = IM.intersectionWith (\(dt,_ss,_ssf) b -> (b,dt)) bindingsMap buffers
                       updateBufferDescriptorSet dset finalBindings
                       pure (DescriptorSet dsetIx dset dslayout finalBindings)
                  ) (V.fromList layoutsAndBuffers) dsets

  pure (configuredDescriptorSets, dpool)
  
    where
      go :: Map FIR.Shader [(SPIRV.PointerTy,SomeDefs,SPIRV.Decorations)]
         -> FIR.PipelineStages info ()
         -> Map FIR.Shader [(SPIRV.PointerTy,SomeDefs,SPIRV.Decorations)] -- ^ For each shader, the sets, corresponding decorations, and corresponding storable data types
      go acc FIR.VertexInput = acc
      go acc (pipe FIR.:>-> (FIR.ShaderModule _ :: FIR.ShaderModule name stage defs endState,())) =
        go (M.insertWith (<>) (FIR.knownValue @stage) (map (\(pt,dc) -> (pt,SomeDefs @defs,dc)) $ M.elems $ FIR.globalAnnotations $ FIR.annotations @defs) acc) pipe


      makeDescriptorSetMap :: Map FIR.Shader [(SPIRV.PointerTy, SomeDefs, SPIRV.Decorations)]
                           -> IntMap BindingsMap -- ^ Mapping from descriptor set indexes to a list of their bindings (corresponding binding type, shader stage)
      makeDescriptorSetMap =
        M.foldrWithKey (\shader ls acc' -> 
          foldr (\(pt,somedefs,S.toList -> decs) acc ->
            case decs of
              [SPIRV.Binding (fromIntegral -> bindingIx), SPIRV.DescriptorSet (fromIntegral -> descriptorSetIx)] ->
                case somedefs of
                  SomeDefs @defs ->
                    -- For each descriptor we compute the buffer size from  the known definitionn
                    let tsize = getDescriptorBindingSize @defs descriptorSetIx bindingIx
                         -- case someNatVal (fromIntegral descriptorSetIx) of
                         --   Just (SomeNat (Proxy @pSetIx)) ->
                         --     case someNatVal (fromIntegral bindingIx) of
                         --       Just (SomeNat (Proxy @pBindingIx)) ->
                         --         fromIntegral $ natVal @(FindDescriptorType pSetIx pBindingIx (Values defs)) Proxy
                         --       _ -> error ".."
                         --   _ -> error "."

                     in IM.insertWith mergeSameDS descriptorSetIx
                                  (IM.singleton bindingIx (descriptorType pt, tsize, stageFlag shader))
                                  acc
              _ -> acc -- we keep folding. currently we don't validate anything futher
            ) acc' ls
          ) mempty

      mergeSameDS :: BindingsMap
                  -> BindingsMap
                  -> BindingsMap
      mergeSameDS = IM.mergeWithKey (\_ (dt,ss,sf) (dt',_ss',sf') -> if dt == dt' then Just (dt, ss, sf .|. sf') else error $ "Incompatible descriptor type: " <> show dt <> " and " <> show dt') id id -- TODO: Could pattern match on type equality too?

type family FindDescriptorType (set :: Nat) (binding :: Nat) (defs :: [FIR.Definition]) :: Nat where
  FindDescriptorType set binding (FIR.Global _ '[SPIRV.DescriptorSet set, SPIRV.Binding binding] t:ds) = Ghengin.Utils.SizeOf t
  FindDescriptorType set binding (_:ds) = FindDescriptorType set binding ds
  FindDescriptorType set binding '[] = TypeError (Text "Error computing descriptor type")

type family SizeOf t :: Nat

-- What I couldn't do: Prove that the type family application results in a type that always instances a certain class.

getDescriptorBindingSize :: ∀ defs. FIR.KnownDefinitions defs
                         => Int -- ^ descriptor set #
                         -> Int -- ^ binding #
                         -> Size
getDescriptorBindingSize dsix bix = case
  foldr
    (\x acc ->
      case x of
        ( SPIRV.PointerTy _ primTy
          , [SPIRV.Binding (fromIntegral -> bindingIx), SPIRV.DescriptorSet (fromIntegral -> descriptorSetIx)]
          )
          | bindingIx == bix && descriptorSetIx == dsix
          -> fromIntegral (sizeOfPrimTy primTy):acc
        _ -> acc
    )
    mempty
    (M.elems $ FIR.globalAnnotations $ FIR.annotations @defs) of
      [size] -> size
      _ -> error "Err calculating PrimTy size"

sizeOfPrimTy :: SPIRV.PrimTy -> Size
sizeOfPrimTy x = case FIR.Layout.primTySizeAndAli FIR.Layout.Extended x of
                   Left e -> error (show e)
                   Right (size,_alignment) -> fromIntegral size


descriptorType :: SPIRV.PointerTy -> Vk.DescriptorType
descriptorType = \case
  SPIRV.PointerTy sc _ -> case sc of
    SPIRV.Storage.Uniform -> Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
    _ -> error "Unexpected/unsupported storage class for descriptor"


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


destroyDescriptorSetLayout :: Vk.DescriptorSetLayout -> Renderer ext ()
destroyDescriptorSetLayout layout = getDevice >>= \dev -> Vk.destroyDescriptorSetLayout dev layout Nothing


-- | Create N descriptor sets from a uniform buffer descriptor pool using each
-- respective descriptor set layout
--
-- Only the pool needs to be destroyed
--
-- TODO: Linear types...
allocateDescriptorSets :: [(Vk.DescriptorSetLayout,[Vk.DescriptorType])] -- ^ The descriptors we need for each set (indexed by set). This is used to calculate the amount of descriptor sets to allocate and the amoumt of descriptors of each type
                       -> Renderer ext (Vector Vk.DescriptorSet, Vk.DescriptorPool)
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
  

destroyDescriptorPool :: Vk.DescriptorPool -> Renderer ext ()
destroyDescriptorPool p = getDevice >>= \dev -> Vk.destroyDescriptorPool dev p Nothing

destroyDescriptorSet :: DescriptorSet -> Renderer ext ()
destroyDescriptorSet (DescriptorSet _ _dset dsetlayout dbindings) = do
  destroyDescriptorSetLayout dsetlayout
  _ <- traverse (destroyMappedBuffer . fst) dbindings
  pure ()

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


-- fromList [(VertexShader,
-- [(PtrTy {pointerTy = Pointer Input (Vector {size = 3, eltTy = Scalar (Floating W32)})},fromList [(0,fromList [(0,SomeStorable)])],fromList [Location 2]),
-- (PtrTy {pointerTy = Pointer Input (Vector {size = 3, eltTy = Scalar (Floating W32)})},fromList [(0,fromList [(0,SomeStorable)])],fromList [Location 1]),
-- (PtrTy {pointerTy = Pointer Input (Vector {size = 3, eltTy = Scalar (Floating W32)})},fromList [(0,fromList [(0,SomeStorable)])],fromList [Location 0]),
-- (PtrTy {pointerTy = Pointer Output (Vector {size = 3, eltTy = Scalar (Floating W32)})},fromList [(0,fromList [(0,SomeStorable)])],fromList [Location 0]),
-- (PtrTy {pointerTy = Pointer PushConstant (Struct {eltTys = [(Just "model",Matrix {rows = 4, cols = 4, entryTy = Floating W32},fromList [])],
-- decs = fromList [], usage = NotForBuiltins})},fromList [(0,fromList [(0,SomeStorable)])],fromList [])
--
--   ,(PtrTy {pointerTy = Pointer Uniform (Struct {eltTys = [(Just
--   "view",Matrix {rows = 4, cols = 4, entryTy = Floating W32},fromList []),(Just
--   "proj",Matrix {rows = 4, cols = 4, entryTy = Floating W32},fromList [])],
--   decs = fromList [], usage = NotForBuiltins})},fromList [(0,fromList
--   [(0,SomeStorable)])],fromList [Binding 0,DescriptorSet
--   0])]),
--
--
--   (FragmentShader,[
--
--   (PtrTy {pointerTy = Pointer Input (Vector {size = 3, eltTy = Scalar (Floating W32)})},fromList [(1,fromList [(0,SomeStorable)])],fromList [Location 0]),
--   (PtrTy {pointerTy = Pointer Uniform (Struct {eltTys = [(Just "min",Scalar (Floating W32),fromList []),(Just "max",Scalar (Floating W32),fromList [])], decs = fromList [], usage = NotForBuiltins})},fromList [(1,fromList [(0,SomeStorable)])],fromList [Binding 0,DescriptorSet 1]),
--   (PtrTy {pointerTy = Pointer Output (Vector {size = 4, eltTy = Scalar (Floating W32)})},fromList [(1,fromList [(0,SomeStorable)])],fromList [Location 0])
--     ])]
