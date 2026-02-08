{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Vulkan.Resource
  ( ResourceStatus(..)
  , ResourceInfo(..), Pre, Post
  , ResourceUsage(..)
  , ResourceType(..)
  , Resource(..)
  , PostInitialisationResult(..)
  , Descriptor, Descriptors
  , IndexBuffer, VertexBuffer
  , UniformBuffer, UniformBuffers
  , StorageBuffer, StorageBuffers
  , StorageImage, StorageImages
  , SampledImage, SampledImages
  , TopLevelAS, TopLevelASes
  , initialiseResources

  , createDescriptorPool
  ) where

-- base
import Data.Bits
  ( (.|.) )
import Data.Coerce
  ( coerce )
import Data.Function
  ( (&) )
import Data.Functor
  ( (<&>) )
import Data.Functor.Const
  ( Const(Const) )
import Data.Kind
  ( Type )
import Data.Maybe
  ( fromJust, listToMaybe, maybeToList )
import Data.Monoid
  ( Endo(Endo) )
import Data.Word
  ( Word8, Word16, Word32 )
import GHC.Exts
  ( proxy# )
import GHC.TypeNats
  ( Nat, KnownNat, natVal' )

-- containers
import qualified Data.Map.Strict as Map
  ( empty, insertWith, toList )

-- finite-typelits
import Data.Finite
  ( Finite )

-- generic-lens
import Data.Generics.Product.Constraints
  ( HasConstraints(constraints)
  , HasConstraints'(constraints')
  )

-- logging-effect
import Control.Monad.Log
  ( logDebug )

-- resourcet
import Control.Monad.Trans.Resource
  ( ReleaseKey, allocate )

-- text-short
import Data.Text.Short
  ( ShortText )
import qualified Data.Text.Short as ShortText
  ( pack )

-- transformers
import Control.Monad.IO.Class
  ( liftIO )
import Control.Monad.Trans.Class
  ( lift )
import Control.Monad.Trans.State.Strict
  ( evalStateT, get, put )

-- vector
import qualified Data.Vector as Boxed
  ( Vector )
import qualified Data.Vector as Boxed.Vector
  ( empty, fromList, imap, replicate, singleton, toList )

-- vector-sized
import qualified Data.Vector.Sized as V
  ( Vector, fromList, index, ifoldl )

-- vulkan
import qualified Vulkan
import qualified Vulkan as Vulkan.DescriptorSet
  ( WriteDescriptorSet(..) )
import qualified Vulkan.CStruct.Extends as Vulkan
  ( SomeStruct(SomeStruct) )
import qualified Vulkan.Zero as Vulkan

-- fir
import FIR
  ( Array, Layout(..), PrimTy, Poke )

-- fir-examples
-- import Vulkan.Buffer
-- import Vulkan.Monad

----------------------------------------------------------------------------
-- Higher-kinded data approach for generic resource sets.

data ResourceStatus
  = PreInitialised
  | Initialised

data ResourceInfo
  = Named
  | Specified ResourceStatus

type Pre  = Specified PreInitialised
type Post = Specified Initialised

data ResourceCount
  = Single
  | Multiple Nat

data ResourceUsage
  = DescriptorUse
  | GeneralUse

data BufferUsage
  = Vertex
  | Index
  | Uniform
  | Storage

data Arrayness
  = Simple
  | Arrayed

data ImageType
  = Store
  | Sample

data ResourceType where
  Buffer :: BufferUsage -> Arrayness -> Layout -> Type -> ResourceType
  Image  :: ImageType -> ResourceType
  TLAS   :: ResourceType

-- Resources.
data family Resource ( res :: ResourceType ) ( use :: ResourceUsage ) ( ct :: ResourceCount ) ( st :: ResourceInfo )

data instance Resource res DescriptorUse ct Named where
  StageFlags :: KnownDescriptorType res => Vulkan.ShaderStageFlags -> Resource res DescriptorUse ct Named
data instance Resource res GeneralUse ct Named = GeneralResource

type family BufferData (arr :: Arrayness) (a :: Type) :: Type where
  BufferData Simple  a = a
  BufferData Arrayed a = [a]
data instance Resource (Buffer bufTy arr lay a) use Single (Specified PreInitialised)
  = BufferData { bufferData :: BufferData arr a }

data instance Resource (Buffer bufTy Simple lay a) use Single (Specified Initialised)
  = BufferResource
    { bufferObject :: Vulkan.Buffer
    , pokeBuffer   :: a -> IO ()
    }
data instance Resource (Buffer bufTy Arrayed lay a) use Single (Specified Initialised)
  = BufferResourceArr
    { bufferObjectArrayed :: Vulkan.Buffer
    , pokeBufferOff       :: Int -> [a] -> IO ()
    , nbBufferElems       :: Int
    }

data instance Resource (Image Store ) use Single (Specified st) = StorageImage Vulkan.ImageView
data instance Resource (Image Sample) use Single (Specified st) = SampledImage Vulkan.Sampler Vulkan.ImageView

data instance Resource TLAS use Single (Specified st) = TopLevelAS Vulkan.AccelerationStructureKHR

data instance Resource res use (Multiple n) (Specified st)
  = Ixed ( V.Vector n ( Resource res use Single (Specified st) ) )

type Descriptor   res (i :: Nat) st = Resource res DescriptorUse Single       st
type Descriptors  res (i :: Nat) st = Resource res DescriptorUse (Multiple i) st

type UniformBuffer  a i st = Descriptor  ( Buffer Uniform Simple  Extended a ) i st
type UniformBuffers a i st = Descriptors ( Buffer Uniform Simple  Extended a ) i st
type StorageBuffer  a i st = Descriptor  ( Buffer Storage Arrayed Base     a ) i st
type StorageBuffers a i st = Descriptors ( Buffer Storage Arrayed Base     a ) i st
type StorageImage     i st = Descriptor  ( Image Store  ) i st
type StorageImages    i st = Descriptors ( Image Store  ) i st
type SampledImage     i st = Descriptor  ( Image Sample ) i st
type SampledImages    i st = Descriptors ( Image Sample ) i st
type TopLevelAS       i st = Descriptor  TLAS             i st
type TopLevelASes     i st = Descriptors TLAS             i st

type VertexBuffer   a (i :: Nat) st = Resource ( Buffer Vertex Arrayed Locations a ) GeneralUse Single st
type IndexBuffer    a (i :: Nat) st = Resource ( Buffer Index  Arrayed Base      a ) GeneralUse Single st

----------------------------------------------------------------------------

data PostInitialisationResult resources n
  = PostInitialisationResult
      { resourceLayout       :: Vulkan.DescriptorSetLayout
      , resourceDescriptors  :: V.Vector n Vulkan.DescriptorSet
      , bindBuffersCommand   :: forall v. MonadVulkan v => Vulkan.CommandBuffer -> v ()
      , initialisedResources :: resources n Post
      }

initialiseResources
  :: forall
      ( n  :: Nat )
      ( m  :: Type -> Type )
      ( resources :: Nat -> ResourceInfo -> Type )
  . ( KnownNat n
    , MonadVulkan m
    , HasConstraints' HasDescriptorTypeAndFlags ( resources n Named )
    , HasConstraints CanInitialiseResource
         ( resources n Pre  )
         ( resources n Post )
    , HasConstraints' ( CanUpdateResource n ) ( resources n Post )
    )
  => Vulkan.PhysicalDevice
  -> Vulkan.Device
  -> resources n Named
  -> resources n Pre
  -> m (PostInitialisationResult resources n)
initialiseResources physicalDevice device resourceFlags resourcesPre = do
  let
    descriptorTypesAndFlags :: [ (Vulkan.DescriptorType, Vulkan.ShaderStageFlags) ]
    descriptorTypesAndFlags =
      foldrC @HasDescriptorTypeAndFlags
        ( \ res flags -> case descriptorTypeAndFlags res of
          Just flag -> flag : flags
          Nothing   -> flags
        )
        resourceFlags
        []

    descriptorTypes :: [ ( Vulkan.DescriptorType, Int ) ]
    descriptorTypes = map ( \ ( ty, _ ) -> ( ty, 1 ) ) descriptorTypesAndFlags
    n :: Int
    n = fromIntegral ( natVal' @n proxy# )
    nb :: ShortText
    nb = ShortText.pack ( show n )

  descriptorPool      <-
    logDebug ( "Creating descriptor pool for " <> nb <> " sets of descriptors, each with types:\n" <> ShortText.pack ( show descriptorTypes ) )
      *> ( snd <$> createDescriptorPool device n descriptorTypes )
  descriptorSetLayout <-
    logDebug "Creating descriptor set layout"
      *> createDescriptorSetLayout device descriptorTypesAndFlags
  descriptorSets      <-
    logDebug ( "Allocating " <> nb <> " descriptor sets" )
      *>  allocateDescriptorSets @n device descriptorPool descriptorSetLayout

  ( resourcesPost :: resources n Post )
     <- logDebug "Initialising resources" *>
        ( constraints @CanInitialiseResource
            ( initialiseResource physicalDevice device )
        ) resourcesPre

  ( descriptors :: V.Vector n Vulkan.DescriptorSet ) <-
    ( logDebug "Updating descriptor sets" *> ) . ( `evalStateT` descriptorSets ) $ do
        ( theseDescriptorSets, nextDescriptorSets ) <- splitAt n <$> get
        put nextDescriptorSets
        let
          descriptors :: V.Vector n Vulkan.DescriptorSet
          descriptors = fromJust $ V.fromList theseDescriptorSets
        lift ( updateDescriptorSets device descriptors resourcesPost )
        pure descriptors

  let
    indexBuffers  :: [ ( Vulkan.Buffer, Vulkan.IndexType ) ]
    vertexBuffers :: [ Vulkan.Buffer ]
    (indexBuffers, vertexBuffers) =
      foldrC @( CanUpdateResource n )
        ( \ res ( indexBinds, vertexBinds) -> case bindingInformation @n res of
          Just (BindIndexBuffer  buf _ ty) -> ( ( buf, ty ) : indexBinds, vertexBinds )
          Just (BindVertexBuffer buf     ) -> ( indexBinds, buf : vertexBinds )
          Nothing                          -> ( indexBinds, vertexBinds)
        )
        resourcesPost
        ( [], [] )

    bindingCommand :: forall v. MonadVulkan v => Vulkan.CommandBuffer -> v ()
    bindingCommand = bufferBindingCommand ( listToMaybe indexBuffers ) vertexBuffers

  pure
    PostInitialisationResult
      { resourceLayout       = descriptorSetLayout
      , resourceDescriptors  = descriptors
      , bindBuffersCommand   = bindingCommand
      , initialisedResources = resourcesPost
      }

----------------------------------------------------------------------------

createDescriptorSetLayout
  :: MonadVulkan m
  => Vulkan.Device
  -> [ ( Vulkan.DescriptorType, Vulkan.ShaderStageFlags ) ]
  -> m Vulkan.DescriptorSetLayout
createDescriptorSetLayout device descriptorTypes = snd <$> Vulkan.withDescriptorSetLayout device createInfo Nothing allocate

      where
        bindings :: Boxed.Vector Vulkan.DescriptorSetLayoutBinding
        bindings = ( `Boxed.Vector.imap` ( Boxed.Vector.fromList descriptorTypes ) ) \ i ( descType, descStageFlags ) ->
          Vulkan.DescriptorSetLayoutBinding
            { Vulkan.binding           = fromIntegral i
            , Vulkan.descriptorType    = descType
            , Vulkan.descriptorCount   = 1
            , Vulkan.stageFlags        = descStageFlags
            , Vulkan.immutableSamplers = Boxed.Vector.empty
            }
        createInfo :: Vulkan.DescriptorSetLayoutCreateInfo '[]
        createInfo =
          Vulkan.DescriptorSetLayoutCreateInfo
            { Vulkan.next     = ()
            , Vulkan.flags    = Vulkan.zero
            , Vulkan.bindings = bindings
            }

createDescriptorPool
  :: MonadVulkan m
  => Vulkan.Device
  -> Int
  -> [ ( Vulkan.DescriptorType, Int ) ]
  -> m ( ReleaseKey, Vulkan.DescriptorPool )
createDescriptorPool device maxSets descTypes = Vulkan.withDescriptorPool device createInfo Nothing allocate

    where
      poolSizes :: [ Vulkan.DescriptorPoolSize ]
      poolSizes =
        counts descTypes <&> \ ( descType, descCount ) ->
          Vulkan.DescriptorPoolSize
          { Vulkan.type'           = descType
          , Vulkan.descriptorCount = fromIntegral $ maxSets * descCount
          }
      createInfo :: Vulkan.DescriptorPoolCreateInfo '[]
      createInfo =
        Vulkan.DescriptorPoolCreateInfo
          { Vulkan.next      = ()
          , Vulkan.flags     = Vulkan.DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
          , Vulkan.poolSizes = Boxed.Vector.fromList poolSizes
          , Vulkan.maxSets   = fromIntegral maxSets
          }

allocateDescriptorSets
  :: forall n m
  .  ( KnownNat n, MonadVulkan m )
  => Vulkan.Device
  -> Vulkan.DescriptorPool
  -> Vulkan.DescriptorSetLayout
  -> m [ Vulkan.DescriptorSet ]
allocateDescriptorSets dev descriptorPool layout0 = Boxed.Vector.toList . snd <$> Vulkan.withDescriptorSets dev allocateInfo allocate
  where
    count :: Int
    count = fromIntegral ( natVal' @n proxy# )
    allocateInfo :: Vulkan.DescriptorSetAllocateInfo '[]
    allocateInfo =
      Vulkan.DescriptorSetAllocateInfo
        { Vulkan.next           = ()
        , Vulkan.descriptorPool = descriptorPool
        , Vulkan.setLayouts     = Boxed.Vector.replicate count layout0
        }

counts :: ( Ord a, Num i ) => [ ( a, i ) ] -> [ ( a, i ) ]
counts = Map.toList . foldr ( uncurry $ Map.insertWith (+) ) Map.empty


updateDescriptorSets
  :: forall m n resources
  .  ( MonadVulkan m
     , HasConstraints' ( CanUpdateResource n ) ( resources n Post )
     )
  => Vulkan.Device
  -> V.Vector n Vulkan.DescriptorSet
  -> resources n Post
  -> m ()
updateDescriptorSets device descriptorSets resourceSet =
  Vulkan.updateDescriptorSets device
    writes
    Boxed.Vector.empty -- no descriptor copies
    where
      writes :: Boxed.Vector ( Vulkan.SomeStruct Vulkan.WriteDescriptorSet )
      writes = Boxed.Vector.fromList $ V.ifoldl descriptorSetWrites [] descriptorSets
      descriptorSetWrites :: [ Vulkan.SomeStruct Vulkan.WriteDescriptorSet ] -> Finite n -> Vulkan.DescriptorSet -> [ Vulkan.SomeStruct Vulkan.WriteDescriptorSet ]
      descriptorSetWrites writesAcc i descriptorSet
        = writesAcc ++
        ( zipWith (&) [0..] $
          foldrC @( CanUpdateResource n )
            ( \ res descs -> descriptorWrites descriptorSet i res ++ descs )
            resourceSet
            []
        )

bufferBindingCommand
  :: MonadVulkan m
  => Maybe ( Vulkan.Buffer, Vulkan.IndexType )
  -> [ Vulkan.Buffer ]
  -> Vulkan.CommandBuffer
  -> m ()
bufferBindingCommand mbIndexBuffer vertexBuffers commandBuffer = do

  Vulkan.cmdBindVertexBuffers
    commandBuffer
    0
    ( Boxed.Vector.fromList vertexBuffers )
    ( pure 0 )

  case mbIndexBuffer of
    Nothing               -> pure ()
    Just ( buff, ixType ) ->
      liftIO $
        Vulkan.cmdBindIndexBuffer
          commandBuffer
          buff
          0 -- no offset
          ixType

-------------------------------------------------------------------------------------------------
-- Auxiliary classes defined for folds and traversals with generic-lens.

foldrC :: forall c b s. HasConstraints' c s
       => ( forall a. c a => a -> b -> b ) -> s -> b -> b
foldrC f s = coerce foldF
    where
      foldF :: Const (Endo b) s
      foldF = constraints' @c ( \ a -> Const ( Endo ( f a ) ) ) s

-- Constraints used with a generic fold to obtain the resource types and flags.
class KnownDescriptorType (res :: ResourceType) where
  descriptorType :: Vulkan.DescriptorType
instance KnownDescriptorType (Buffer Uniform arr lay a) where
  descriptorType = Vulkan.DESCRIPTOR_TYPE_UNIFORM_BUFFER
instance KnownDescriptorType (Buffer Storage arr lay a) where
  descriptorType = Vulkan.DESCRIPTOR_TYPE_STORAGE_BUFFER
instance KnownDescriptorType (Image Store) where
  descriptorType = Vulkan.DESCRIPTOR_TYPE_STORAGE_IMAGE
instance KnownDescriptorType (Image Sample) where
  descriptorType = Vulkan.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
instance KnownDescriptorType TLAS where
  descriptorType = Vulkan.DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR

class HasDescriptorTypeAndFlags a where
  descriptorTypeAndFlags :: a -> Maybe ( Vulkan.DescriptorType, Vulkan.ShaderStageFlags )
instance ( KnownDescriptorType res )
  => HasDescriptorTypeAndFlags ( Resource res DescriptorUse ct Named )
  where
  descriptorTypeAndFlags (StageFlags flags) = Just ( descriptorType @res, flags )
instance HasDescriptorTypeAndFlags ( Resource res GeneralUse ct Named ) where
  descriptorTypeAndFlags _ = Nothing

-- Constraint used to initialise a set of resources.
class CanInitialiseResource a b | a -> b where
  initialiseResource
    :: MonadVulkan m
    => Vulkan.PhysicalDevice
    -> Vulkan.Device
    -> a
    -> m b

instance ( lay ~ Base, arr ~ Arrayed, Poke a lay, PrimTy a, Poke (Array 1 a) Base ) =>
  CanInitialiseResource
    ( Resource (Buffer Index arr lay a) GeneralUse Single (Specified PreInitialised) )
    ( Resource (Buffer Index arr lay a) GeneralUse Single (Specified Initialised   ) )
  where
  initialiseResource physicalDevice device ( BufferData { bufferData } ) = do
    ( buf, pokeFn, nbElems ) <- createIndexBuffer physicalDevice device bufferData
    pure (BufferResourceArr buf pokeFn nbElems)
instance ( lay ~ Locations, arr ~ Arrayed, Poke a lay, PrimTy a ) =>
  CanInitialiseResource
    ( Resource (Buffer Vertex arr lay a) GeneralUse Single (Specified PreInitialised) )
    ( Resource (Buffer Vertex arr lay a) GeneralUse Single (Specified Initialised   ) )
  where
  initialiseResource physicalDevice device ( BufferData { bufferData } ) = do
    ( buf, pokeFn, nbElems ) <- createVertexBuffer physicalDevice device bufferData
    pure (BufferResourceArr buf pokeFn nbElems)
instance ( lay ~ Extended, arr ~ Simple, Poke a lay, PrimTy a ) =>
  CanInitialiseResource
    ( Resource (Buffer Uniform arr lay a) DescriptorUse Single (Specified PreInitialised) )
    ( Resource (Buffer Uniform arr lay a) DescriptorUse Single (Specified Initialised   ) )
  where
  initialiseResource physicalDevice device ( BufferData { bufferData } ) = do
    uncurry BufferResource <$> createUniformBuffer physicalDevice device bufferData
instance ( lay ~ Base, arr ~ Arrayed, Poke a lay, Poke ( Array 1 a ) lay ) =>
  CanInitialiseResource
    ( Resource (Buffer Storage arr lay a) DescriptorUse Single (Specified PreInitialised) )
    ( Resource (Buffer Storage arr lay a) DescriptorUse Single (Specified Initialised   ) )
  where
  initialiseResource physicalDevice device ( BufferData { bufferData } ) = do
    ( buf, pokeFn, nbElems ) <-
      createBufferFromList @a @lay
        Vulkan.BUFFER_USAGE_STORAGE_BUFFER_BIT
        ( Vulkan.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vulkan.MEMORY_PROPERTY_HOST_COHERENT_BIT )
        Vulkan.zero
        physicalDevice device bufferData
    pure (BufferResourceArr buf pokeFn nbElems)

instance
  CanInitialiseResource
    ( Resource ( Image Store ) DescriptorUse Single ( Specified PreInitialised ) )
    ( Resource ( Image Store ) DescriptorUse Single ( Specified Initialised    ) )
  where
  initialiseResource _ _ ( StorageImage imgView ) = pure ( StorageImage imgView )

instance
  CanInitialiseResource
    ( Resource ( Image Sample ) DescriptorUse Single ( Specified PreInitialised ) )
    ( Resource ( Image Sample ) DescriptorUse Single ( Specified Initialised    ) )
  where
  initialiseResource _ _ ( SampledImage sampler imgView ) = pure ( SampledImage sampler imgView )

instance
  CanInitialiseResource
    ( Resource TLAS DescriptorUse Single ( Specified PreInitialised ) )
    ( Resource TLAS DescriptorUse Single ( Specified Initialised    ) )
  where
  initialiseResource _ _ ( TopLevelAS tlas ) = pure ( TopLevelAS tlas )

instance
  ( CanInitialiseResource
      ( Resource res DescriptorUse Single ( Specified PreInitialised ) )
      ( Resource res DescriptorUse Single ( Specified Initialised    ) )
  ) => CanInitialiseResource
        ( Resource res DescriptorUse ( Multiple n ) ( Specified PreInitialised ) )
        ( Resource res DescriptorUse ( Multiple n ) ( Specified Initialised    ) )
  where
  initialiseResource physicalDevice device (Ixed ixData) =
    Ixed <$> traverse ( initialiseResource physicalDevice device ) ixData

data BindingInformation
  = BindIndexBuffer  Vulkan.Buffer Word32 Vulkan.IndexType
  | BindVertexBuffer Vulkan.Buffer

-- Constraint used to turn resource into updating information:
--    * update the descriptor set with descriptor write
--    * keep track of how to bind resources.
class CanUpdateResource n a where
  descriptorWrites   :: Vulkan.DescriptorSet -> Finite n -> a -> [ Word32 -> Vulkan.SomeStruct Vulkan.WriteDescriptorSet ]
  bindingInformation :: a -> Maybe BindingInformation


instance ( arr ~ Arrayed )
       => CanUpdateResource n ( Resource (Buffer Vertex arr lay a) GeneralUse Single ( Specified Initialised ) ) where
  descriptorWrites _ _ _ = []
  bindingInformation ( BufferResourceArr { bufferObjectArrayed } )
    = Just (BindVertexBuffer bufferObjectArrayed)
instance ( arr ~ Arrayed, ValidIndexingType a )
      => CanUpdateResource n ( Resource (Buffer Index arr lay a) GeneralUse Single ( Specified Initialised ) ) where
  descriptorWrites _ _ _ = []
  bindingInformation ( BufferResourceArr { bufferObjectArrayed, nbBufferElems } )
    = Just (BindIndexBuffer bufferObjectArrayed (fromIntegral nbBufferElems) ( indexType @a ) )

class ValidIndexingType (i :: Type) where
  indexType :: Vulkan.IndexType
instance ValidIndexingType Word32 where
  indexType = Vulkan.INDEX_TYPE_UINT32
instance ValidIndexingType Word16 where
  indexType = Vulkan.INDEX_TYPE_UINT16
instance ValidIndexingType Word8 where
  indexType = Vulkan.INDEX_TYPE_UINT8_EXT

instance
  KnownDescriptorType (Buffer bufType Simple lay a)
  => CanUpdateResource n
      ( Resource (Buffer bufType Simple lay a) DescriptorUse Single (Specified Initialised) )
 where
  descriptorWrites descriptorSet _ ( BufferResource { bufferObject } ) = (:[]) \ bindingNumber ->
    Vulkan.SomeStruct $ mkDescriptorWrite descriptorSet ( descriptorType @(Buffer bufType Simple lay a) ) bindingNumber [bufferDesc] Nothing
      where
        bufferDesc :: Vulkan.DescriptorBufferInfo
        bufferDesc =
          Vulkan.DescriptorBufferInfo
            { Vulkan.buffer = bufferObject
            , Vulkan.offset = 0
            , Vulkan.range  = Vulkan.WHOLE_SIZE
            }
  bindingInformation _ = Nothing

instance
  KnownDescriptorType (Buffer bufType Arrayed lay a)
  => CanUpdateResource n
      ( Resource (Buffer bufType Arrayed lay a) DescriptorUse Single (Specified Initialised) )
 where
  descriptorWrites descriptorSet _ ( BufferResourceArr { bufferObjectArrayed } ) = (:[]) \ bindingNumber ->
    Vulkan.SomeStruct $ mkDescriptorWrite descriptorSet ( descriptorType @(Buffer bufType Arrayed lay a) ) bindingNumber [bufferDesc] Nothing
      where
        bufferDesc :: Vulkan.DescriptorBufferInfo
        bufferDesc =
          Vulkan.DescriptorBufferInfo
            { Vulkan.buffer = bufferObjectArrayed
            , Vulkan.offset = 0
            , Vulkan.range  = Vulkan.WHOLE_SIZE
            }
  bindingInformation _ = Nothing


instance
  CanUpdateResource n
      ( Resource ( Image Store ) DescriptorUse Single (Specified Initialised) )
  where
  descriptorWrites descriptorSet _ ( StorageImage imgView ) = (:[]) $ \bindingNumber ->
    Vulkan.SomeStruct $ mkDescriptorWrite descriptorSet ( descriptorType @(Image Store) ) bindingNumber [] ( Just imageInfo )
      where
        imageInfo :: Vulkan.DescriptorImageInfo
        imageInfo =
          Vulkan.DescriptorImageInfo
            { Vulkan.sampler     = Vulkan.NULL_HANDLE
            , Vulkan.imageView   = imgView
            , Vulkan.imageLayout = Vulkan.IMAGE_LAYOUT_GENERAL
            }
  bindingInformation _ = Nothing

instance
  CanUpdateResource n
      ( Resource ( Image Sample ) DescriptorUse Single (Specified Initialised) )
  where
  descriptorWrites descriptorSet _ ( SampledImage sampler imgView ) = (:[]) $ \bindingNumber ->
    Vulkan.SomeStruct $ mkDescriptorWrite descriptorSet ( descriptorType @(Image Sample) ) bindingNumber [] ( Just imageInfo )
      where
        imageInfo :: Vulkan.DescriptorImageInfo
        imageInfo =
          Vulkan.DescriptorImageInfo
            { Vulkan.sampler     = sampler
            , Vulkan.imageView   = imgView
            , Vulkan.imageLayout = Vulkan.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
            }
  bindingInformation _ = Nothing

instance
  CanUpdateResource n
      ( Resource TLAS DescriptorUse Single (Specified Initialised) )
  where
  descriptorWrites descriptorSet _ ( TopLevelAS tlas ) = (:[]) $ \bindingNumber ->
    Vulkan.SomeStruct $
      ( mkDescriptorWrite descriptorSet ( descriptorType @TLAS ) bindingNumber [] Nothing
        :: Vulkan.WriteDescriptorSet '[] )
        { Vulkan.DescriptorSet.next = ( accelWrite, () ) }
      where
        accelWrite :: Vulkan.WriteDescriptorSetAccelerationStructureKHR
        accelWrite = Vulkan.WriteDescriptorSetAccelerationStructureKHR
          { Vulkan.accelerationStructures = Boxed.Vector.singleton tlas }
  bindingInformation _ = Nothing

instance
     ( CanUpdateResource n
        ( Resource res DescriptorUse Single ( Specified Initialised ) )
     )
  => CanUpdateResource n
        ( Resource res DescriptorUse ( Multiple n ) ( Specified Initialised) )
  where
  descriptorWrites descriptorSet i ( Ixed res ) =
    descriptorWrites descriptorSet i (res `V.index` i)
  bindingInformation _ = Nothing

mkDescriptorWrite
  :: Vulkan.DescriptorSet
  -> Vulkan.DescriptorType
  -> Word32
  -> [Vulkan.DescriptorBufferInfo]
  -> Maybe Vulkan.DescriptorImageInfo
  -> Vulkan.WriteDescriptorSet '[]
mkDescriptorWrite descSet descType bindingNumber bufferInfos imageInfo =
    Vulkan.WriteDescriptorSet
      { Vulkan.next             = ()
      , Vulkan.dstSet           = descSet
      , Vulkan.dstBinding       = bindingNumber
      , Vulkan.descriptorType   = descType
      , Vulkan.texelBufferView  = Boxed.Vector.empty
      , Vulkan.imageInfo        = Boxed.Vector.fromList ( maybeToList imageInfo )
      , Vulkan.bufferInfo       = Boxed.Vector.fromList bufferInfos
      , Vulkan.descriptorCount  = 1
      , Vulkan.dstArrayElement  = 0
      }
