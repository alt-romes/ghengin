{-# OPTIONS_GHC -Wno-orphans #-}
module Vulkan.Linear
  ( module Vulkan.Linear
  , module Vulkan
  ) where

import Data.V.Linear.Internal (V(..))
import Data.Vector (Vector)
import Prelude.Linear
import Control.Functor.Linear as Linear
import Control.Monad.IO.Class.Linear

import Vulkan hiding ( destroyInstance
                     , createDescriptorPool
                     , destroyDescriptorSetLayout
                     , destroyDescriptorPool
                     , createDescriptorSetLayout
                     , allocateDescriptorSets
                     , updateDescriptorSets
                     , freeDescriptorSets
                     )
import Vulkan.CStruct.Extends
import qualified Vulkan as Vk

import Unsafe.Linear

type DeviceM io a = Device ⊸ io (a, Device)

destroyInstance :: MonadIO m
                => Instance ⊸ ("allocator" ::: Maybe AllocationCallbacks) -> m ()
destroyInstance i ac = toLinear liftSystemIO $ toLinear Vk.destroyInstance i ac

createDescriptorPool :: forall a io
                      . ( Extendss DescriptorPoolCreateInfo a
                        , PokeChain a
                        , MonadIO io )
                     => (DescriptorPoolCreateInfo a)
                     -> ("allocator" ::: Maybe AllocationCallbacks)
                     -> Device 
                      ⊸ io (DescriptorPool,Device)
createDescriptorPool info ac
  = toLinear \dev -> (,dev) <$> liftSystemIO (Vk.createDescriptorPool dev info ac)

-- TODO:
-- The real question is can we free the sampler before we free it here too?
-- i.e. do we need to reference count the Samplers in
-- DescriptorSetLayoutCreateInfo, and store them somewhere?
-- For now we always pass an empty list, so ignoring this problem is fine for now
-- (and we say we consume unrestrictedly the dsetlayoutcreate info, meaning it would be unsafe to pass samplers here.
createDescriptorSetLayout
  :: forall a io. (Extendss DescriptorSetLayoutCreateInfo a, PokeChain a, MonadIO io)
  => ("allocator" ::: Maybe AllocationCallbacks)
  -> DescriptorSetLayoutCreateInfo a
  -> DeviceM io DescriptorSetLayout
createDescriptorSetLayout ac info
  = toLinear \dev -> (,dev) <$> liftSystemIO (Vk.createDescriptorSetLayout dev info ac)

destroyDescriptorSetLayout :: forall io. MonadIO io
                           => ("allocator" ::: Maybe AllocationCallbacks)
                           -> DescriptorSetLayout
                            ⊸ Device
                            ⊸ io ((), Device)
destroyDescriptorSetLayout ac
  = toLinear2 \dset dev -> (,dev) <$> liftSystemIO (Vk.destroyDescriptorSetLayout dev dset ac)

destroyDescriptorPool :: forall io. MonadIO io
                      => ("allocator" ::: Maybe AllocationCallbacks)
                      -> DescriptorPool
                       ⊸ Device
                       ⊸ io ((), Device)
destroyDescriptorPool ac
  = toLinear2 \dpool dev -> (,dev) <$> liftSystemIO (Vk.destroyDescriptorPool dev dpool ac)

allocateDescriptorSets
  :: forall io n. MonadIO io
  => DescriptorPool
   ⊸ V n DescriptorSetLayout -- ^ Do we need to free this dset before these layouts, or not? I think not, otherwise the documentation would say so
   ⊸ DeviceM io ("descriptorSets" ::: V n DescriptorSet, V n DescriptorSetLayout, DescriptorPool)
allocateDescriptorSets
  = toLinear3 \pool (V layouts) dev -> (,dev) . (,V @n layouts,pool) . V @n <$> liftSystemIO (Vk.allocateDescriptorSets dev DescriptorSetAllocateInfo{descriptorPool = pool, setLayouts = layouts,next=()})


updateDescriptorSets
  :: forall io. MonadIO io
  -- We need to think about how to handle all these infos that contain loads of
  -- aliases. Perhaps if they were all reference counted... but....
  -- For now they're unrestricted, which makes the caller use unsafe
  => ("descriptorWrites" ::: Vector (SomeStruct WriteDescriptorSet))
  -> ("descriptorCopies" ::: Vector CopyDescriptorSet)
  -> DeviceM io ()
updateDescriptorSets writes copies
  = toLinear \dev ->
    (,dev) <$> liftSystemIO (Vk.updateDescriptorSets dev writes copies)

freeDescriptorSets
  :: forall io. MonadIO io
  => DescriptorPool
   ⊸ ("descriptorSets" ::: Vector DescriptorSet)
   ⊸ DeviceM io DescriptorPool
freeDescriptorSets = toLinear3 \pool sets dev -> (pool,dev) <$ liftSystemIO (Vk.freeDescriptorSets dev pool sets)

-- Orphan Unrestricted instances
instance Consumable ShaderStageFlags where
  consume = toLinear \_ -> ()

instance Consumable DescriptorType where
  consume = toLinear \_ -> ()

instance Dupable ShaderStageFlags where
  dup2 = toLinear \x -> (x,x)

instance Dupable DescriptorType where
  dup2 = toLinear \x -> (x,x)

instance Movable ShaderStageFlags where
  move = toLinear \x -> Ur x

instance Movable DescriptorType where
  move = toLinear \x -> Ur x

