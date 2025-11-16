{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLists #-}
module Ghengin.Vulkan.Renderer.Buffer where

import qualified Control.Category as Category
import Ghengin.Core.Prelude as Linear
import Ghengin.Core.Log
import qualified Prelude

import Foreign.Storable

import Data.Int
import qualified Data.Linear.Alias as Alias

import qualified Data.Vector.Storable as SV
import Foreign.Ptr
import Foreign.Marshal.Utils
import Data.Bits
import Vulkan.Zero (zero)
import qualified Vulkan as Vk

import {-# SOURCE #-} Ghengin.Vulkan.Renderer.Kernel
-- import qualified Ghengin.Vulkan.Renderer.Command as Cmd
import Ghengin.Vulkan.Renderer.Device
import Ghengin.Core.Mesh.Vertex

import qualified Unsafe.Linear as Unsafe

-- Backpack craziness... importing things from the module we're instancing?
-- We might need to duplicate these definitions? If we do, does it work?
-- import {-# SOURCE #-} Ghengin.Vulkan.Renderer.Buffer (Index32Buffer(..), VertexBuffer(..))

-------- Specific buffers --------------

-- inlined from ghengin-core
data Index32Buffer where
  Index32Buffer :: !DeviceLocalBuffer
                 ⊸ Word32                 -- ^ N indices
                -> Index32Buffer

createIndex32Buffer :: SV.Vector Int32 -> Renderer Index32Buffer
createIndex32Buffer vv =
  flip Index32Buffer (fromIntegral $ SV.length vv) <$>
    createDeviceLocalBuffer Vk.BUFFER_USAGE_INDEX_BUFFER_BIT vv

data VertexBuffer where
  VertexBuffer :: !DeviceLocalBuffer
                ⊸ Word32               -- ^ N vertices
               -> VertexBuffer
-- NB: We use Std140 throughout this module for Vertex, which isn't quite
-- right since vertices need to abide by the location/component layout
-- specification... but since Std140 gives some padding this should work fine
-- if you are using Vertices with Vector attributes only.
-- Ghengin.Core.Mesh.Vertex also defines a Storable instance for vertices based on Std140

createVertexBuffer :: ∀ αs. Storable (Vertex αs) => SV.Vector (Vertex αs) -> Renderer VertexBuffer
createVertexBuffer vv =
  flip VertexBuffer (fromIntegral $ SV.length vv) <$>
    createDeviceLocalBuffer @(Vertex αs) Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT vv -- use Locations for vertex buffers

-------- Device-local buffer -----------

data DeviceLocalBuffer where
  DeviceLocalBuffer :: {-# UNPACK #-} !Vk.Buffer
                     ⊸ {-# UNPACK #-} !Vk.DeviceMemory
                     -- ⊸ Word -- Size
                     ⊸ DeviceLocalBuffer

-- | Fills a device (GPU) local buffer with the provided flags and the provided data
-- by first copying the data to a staging buffer and then running a buffer copy
-- one-shot command.
createDeviceLocalBuffer :: ∀ α. SV.Storable α => Vk.BufferUsageFlags -> SV.Vector α -> Renderer DeviceLocalBuffer
createDeviceLocalBuffer flags bufferData = enterD "createDeviceLocalBuffer" Linear.do

  withStagingBuffer bufferData $ \stagingBuffer bufferSize -> Linear.do

    (devBuffer, devMem) <- createBuffer bufferSize (Vk.BUFFER_USAGE_TRANSFER_DST_BIT .|. flags) Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT

    -- Copy data from staging buffer to actual buffer inaccessible by the host
    (stagingBuffer', devBuffer') <- copyBuffer stagingBuffer devBuffer bufferSize

    destroyBuffer stagingBuffer'

    pure $ DeviceLocalBuffer devBuffer' devMem

destroyDeviceLocalBuffer :: DeviceLocalBuffer ⊸ Renderer ()
destroyDeviceLocalBuffer (DeviceLocalBuffer b dm) = enterD "destroyDeviceLocalBuffer" Linear.do
  destroyBuffer b
  freeMemory dm

-------- Mapped Buffer -----------------

-- | A mapped buffer with size equal to the sizeOf of the Storable @a@
--
-- This buffer has e.g. USAGE_UNIFORM_BUFFER_BIT and MEMORY_PROPRTY_HOST_VISIBLE and
-- MEMORY_PROPERTY_HOST_COHERENT_BIT -- we allocate device-local and host-local
-- memory and writing to the mapped buffer entails writing to the host memory
-- which is mapped to device memory and hence synchronized automatically
--
-- This is unlike DeviceLocalBuffers, which are allocated on the device and
-- require a staging buffer and a copy command to be written
data MappedBuffer = MappedBuffer { buffer  :: {-# UNPACK #-} !Vk.Buffer
                                  , devMem  :: {-# UNPACK #-} !Vk.DeviceMemory
                                  , hostMem :: {-# UNPACK #-} !(Ptr ())
                                    -- ^ When `DeviceMemory` is mapped, we get a `hostMem` pointer to it.
                                  , bufSize :: {-# UNPACK #-} !(Ur Word)
                                  }

data BufferType = Uniform | Storage
  deriving Show

-- | Create a uniform buffer with a given size, but don't copy memory to it
-- yet. See 'writeUniformBuffer' for that.
createMappedBuffer :: Word -> BufferType -> Renderer (Alias MappedBuffer)
createMappedBuffer size descriptorType = enterD "createMappedBuffer" Linear.do
  let bsize = fromIntegral size

  (buf, devMem0) <- createBuffer bsize (bufferUsageBit descriptorType) (Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT)

  (devMem1, data') <- mapMemory devMem0 0 bsize zero
  data' <- unsafeUse data' $ \d -> logT $ "Created " <> toLogStr (show descriptorType) <> " mapped region: " <> toLogStr (show d)

  Alias.newAlias destroyMappedBuffer (MappedBuffer buf devMem1 (Unsafe.toLinear castPtr data') (Ur size))

  where
    bufferUsageBit Uniform = Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT
    bufferUsageBit Storage = Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT

-- | Note how the storable must be the same as the storable of the uniform
-- buffer so that the sizes match
writeMappedBuffer :: ∀ α. Block α => Alias MappedBuffer ⊸ α -> Renderer (Alias MappedBuffer)
writeMappedBuffer refcbuf x = enterD "writeMappedBuffer" Linear.do
  (ub, ()) <- Alias.useM refcbuf $ Unsafe.toLinear \ub@(MappedBuffer _ _ ptr (Ur s)) -> Linear.do
    logT $ fromString $
      "Ptr: " ++ show ptr ++
      "; size:" ++ show (sizeOf140 (Proxy @α)) ++
      "; device memory size: " ++ show s
    -- For uniform buffers we use std140 (extended layout)
    liftSystemIO $ write140 @α (castPtr ptr) Category.id x
    logT "Successfully wrote mapped buffer"
    pure (ub, ())
  pure ub

-------- Non-interface details ---------

createBuffer :: Vk.DeviceSize -> Vk.BufferUsageFlags -> Vk.MemoryPropertyFlags -> Renderer (Vk.Buffer, Vk.DeviceMemory)
createBuffer size usage properties = Linear.do
  let bufferInfo = Vk.BufferCreateInfo { next = ()
                                       , flags = zero
                                       , size  = size
                                       , usage = usage
                                       , sharingMode = Vk.SHARING_MODE_EXCLUSIVE
                                       , queueFamilyIndices = []
                                       }
  unsafeUseVulkanDevice (\device -> do
    let dev = device._device
    buffer          <- Vk.createBuffer dev bufferInfo Nothing
    memRequirements <- Vk.getBufferMemoryRequirements dev buffer
    memTypeIndex    <- findMemoryType memRequirements.memoryTypeBits properties device._physicalDevice
    let allocInfo = Vk.MemoryAllocateInfo { next = ()
                                          , allocationSize = memRequirements.size
                                          , memoryTypeIndex = memTypeIndex
                                          }
    devMem          <- Vk.allocateMemory dev allocInfo Nothing

    -- Bind buffer to the memory we allocated (or is it the other way around?)
    Vk.bindBufferMemory dev buffer devMem 0
    Prelude.pure (buffer, devMem)
                     )

-- | Fills a staging buffer with data, uses it with the given function that
-- typically copies the buffer data from the staging buffer to another one
-- (e.g. creating device local buffers and copying textures to the device), and
-- finally frees the staging buffer
withStagingBuffer :: ∀ α (ρ :: Type). SV.Storable α => SV.Vector α -> (Vk.Buffer ⊸ Vk.DeviceSize -> Renderer ρ) ⊸ Renderer ρ
-- ROMES:TODO: nevermind brackets for now, if we ever make this compile we can worry about linear bracket-ing then
withStagingBuffer bufferData f = enterD "withStagingBuffer" Linear.do
  -- Accquire staging buffer
  -- -----------------------
  let !l          = SV.length bufferData
      !bufferSize = fromIntegral $ fromIntegral l * sizeOf @α undefined
  (stagingBuffer0, stagingMem0) <- createBuffer bufferSize Vk.BUFFER_USAGE_TRANSFER_SRC_BIT (Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT)
  
  -- Map the buffer memory into CPU accessible memory
  (stagingMem1, data'ptr) <- mapMemory stagingMem0 0 bufferSize zero
  -- Copy buffer data to data'ptr mapped device memory
  data'ptr <- unsafeUse data'ptr $ \unsafeDataPtr ->
    liftSystemIO $ SV.unsafeWith bufferData $ \ptr ->
      copyBytes unsafeDataPtr (Unsafe.toLinear castPtr ptr) (fromIntegral bufferSize)

  -- Unmap memory (doesn't free the device memory, just unmaps/frees the pointer to the mapped region)
  stagingMem2 <- unmapMemory stagingMem1 data'ptr

  -- Use staging buffer
  -- ------------------
  p <- f stagingBuffer0 bufferSize -- TODO: On exception must free stagingBuffer0 still...

  -- Release things
  -- -----------------------------------
  -- Free associated memory
  freeMemory stagingMem2

  pure p


-- data Buffer where
--   -- | A Uniform buffer with size equal to the sizeOf of the Storable @a@
--   UniformBuffer { buffer :: Vk.Buffer
--                 , devMem :: Vk.DeviceMemory
--                 , hostMem :: Storable a => Ptr a
--                 } :: Buffer

-- UniformBuffer:
--
-- You can then copy data to the mapped memory using 'copyBytes'
--
-- TODO: Enforce pointer is freed with linear types
--
-- The returned Ptr has the sizeOf of the storable type and is mapped to
-- the device buffer. When bytes are copied to this address they are mapped
-- onto the device buffer.  That is, to write to the buffer you should rather
-- write to the pointer


destroyMappedBuffer :: MappedBuffer ⊸ Renderer ()
destroyMappedBuffer (MappedBuffer b dm hostMemory (Ur _size)) = enterD "destroyMappedBuffer" $ Linear.do
  dm' <- unmapMemory dm hostMemory -- unnecessary, freeMemory also unmaps it IUC
  freeMemory dm'
  destroyBuffer b

-- data Buffer = Buffer Vk.Buffer Vk.DeviceMemory

-------- Utils -------------------------

-- | Linear wrapper around Vk.mapMemory.
--
-- It's a bit weird we dont' need to free the host memory, but until we (TODO)
-- make sure, we return the host memory ptr as unrestricted
mapMemory :: Vk.DeviceMemory ⊸ Vk.DeviceSize -> Vk.DeviceSize -> Vk.MemoryMapFlags -> Renderer (Vk.DeviceMemory, (Ptr ()))
mapMemory = Unsafe.toLinear $ \mem offset size flgs -> enterD "mapMemory" $ (mem,) <$> (unsafeUseDevice $ \dev -> Vk.mapMemory dev mem offset size flgs)

-- | Linear wrapper for Vk.unmapMemory
unmapMemory :: Vk.DeviceMemory
             ⊸ Ptr ()
             -- ^ The pointer to the mapped region on the CPU
             ⊸ Renderer Vk.DeviceMemory
unmapMemory = Unsafe.toLinear2 $ \stgMem hostMem -> enterD "unmapMemory" $ Linear.do
  unsafeUseDevice $ \device -> Vk.unmapMemory device stgMem
  -- Host mem is not returned because it becomes unavailable after unmapping.
  pure stgMem

-- -- | Linear wrapper for Vk.freeMemory
freeMemory :: Vk.DeviceMemory ⊸ Renderer ()
freeMemory = Unsafe.toLinear $ \mem -> enterD "freeMemory" $ unsafeUseDevice $ \device -> Vk.freeMemory device mem Nothing 

-- -- | Linear wrapper for Vk.freeMemory
destroyBuffer :: Vk.Buffer ⊸ Renderer ()
destroyBuffer = Unsafe.toLinear $ \buffer -> enterDA "destroyBuffer" buffer $ unsafeUseDevice $ \device -> Vk.destroyBuffer device buffer Nothing 


-- TODO: Can't forget to call this to free buffer memories after meshes (or the related entities) die
-- destroyBufferAndMemory :: Vk.Buffer ⊸ Vk.DeviceMemory ⊸ Renderer ()
-- destroyBufferAndMemory = Unsafe.toLinear2 $ \buffer mem -> Linear.do
--   unsafeUseDevice $ \device -> Vk.destroyBuffer device buffer Nothing
--   unsafeUseDevice $ \device -> Vk.freeMemory device mem Nothing


