{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLists #-}
module Ghengin.Vulkan.Renderer.Buffer where

import Ghengin.Core.Prelude as Linear hiding (zero)
import Ghengin.Core.Log
import qualified Prelude

import Foreign.Storable

import Control.Exception
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
import qualified Data.Linear.Alias as Alias

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
createIndex32Buffer vv = index32buffer <$> createDeviceLocalBuffer Vk.BUFFER_USAGE_INDEX_BUFFER_BIT vv <*> pure (Ur $ fromIntegral $ SV.length vv)
  where
    index32buffer :: DeviceLocalBuffer %1 -> Ur Word32 %1 -> Index32Buffer
    index32buffer d (Ur w) = Index32Buffer d w

data VertexBuffer where
  VertexBuffer :: !DeviceLocalBuffer
                ⊸ Word32               -- ^ N vertices
               -> VertexBuffer

createVertexBuffer :: ∀ αs. SV.Storable (Vertex αs) => SV.Vector (Vertex αs) -> Renderer VertexBuffer
createVertexBuffer vv = vertexBuffer <$> createDeviceLocalBuffer @(Vertex αs) Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT vv <*> pure (Ur $ fromIntegral $ SV.length vv) -- use Locations for vertex buffers
  where
    vertexBuffer :: DeviceLocalBuffer %1 -> Ur Word32 %1 -> VertexBuffer
    vertexBuffer d (Ur w) = VertexBuffer d w

-------- Device-local buffer -----------

data DeviceLocalBuffer where
  DeviceLocalBuffer :: {-# UNPACK #-} !Vk.Buffer
                     ⊸ {-# UNPACK #-} !Vk.DeviceMemory
                     -- ⊸ Word -- Size
                     ⊸ DeviceLocalBuffer

-- | Fills a device (GPU) local buffer with the provided flags and the provided data
-- by first copying the data to a staging buffer and then running a buffer copy
-- one-shot command.
createDeviceLocalBuffer :: ∀ α. (SV.Storable α) => Vk.BufferUsageFlags -> SV.Vector α -> Renderer DeviceLocalBuffer
createDeviceLocalBuffer flags bufferData =

  withStagingBuffer bufferData $ \stagingBuffer bufferSize -> Linear.do

    (devBuffer, devMem) <- createBuffer bufferSize (Vk.BUFFER_USAGE_TRANSFER_DST_BIT .|. flags) Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT

    -- Copy data from staging buffer to actual buffer inaccessible by the host
    (stagingBuffer', devBuffer') <- copyBuffer stagingBuffer devBuffer bufferSize

    destroyBuffer stagingBuffer'

    pure $ DeviceLocalBuffer devBuffer' devMem

destroyDeviceLocalBuffer :: DeviceLocalBuffer ⊸ Renderer ()
destroyDeviceLocalBuffer (DeviceLocalBuffer b dm) = Linear.do
  destroyBuffer b
  freeMemory dm

-------- Mapped Buffer -----------------

-- | A Uniform buffer with size equal to the sizeOf of the Storable @a@
--
-- This buffer has USAGE_UNIFORM_BUFFER_BIT and MEMORY_PROPRTY_HOST_VISIBLE and
-- MEMORY_PROPERTY_HOST_COHERENT_BIT -- we allocate device-local and host-local
-- memory and writing to the mapped buffer entails writing to the host memory
-- which is mapped to device memory and hence synchronized automatically
--
-- This is unlike DeviceLocalBuffers, which are allocated on the device and
-- require a staging buffer and a copy command to be written
data MappedBuffer = UniformBuffer { buffer  :: {-# UNPACK #-} !Vk.Buffer
                                  , devMem  :: {-# UNPACK #-} !Vk.DeviceMemory
                                  , hostMem :: {-# UNPACK #-} !(Ur (Ptr ())) -- See comment on mapMemory wrapper
                                  , bufSize :: {-# UNPACK #-} !(Ur Word)
                                  }
instance Aliasable MappedBuffer where
  countedFields _ = []
  {-# INLINE countedFields #-}

-- | Create a uniform buffer with a given size, but don't copy memory to it
-- yet. See 'writeUniformBuffer' for that yet. See 'writeUniformBuffer' for
-- that yet. See 'writeUniformBuffer' for that yet. See 'writeUniformBuffer'
-- for that.
--
-- The size is given by the type of the storable to store in the uniform buffer.
createMappedBuffer :: Word -> Vk.DescriptorType -> Renderer (Alias MappedBuffer)
createMappedBuffer size descriptorType = Linear.do

  case descriptorType of
    Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER -> Linear.do

      let bsize = fromIntegral size

      (buf, devMem0) <- createBuffer bsize Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT (Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT)

      (devMem1, Ur data') <- mapMemory devMem0 0 bsize zero

      Alias.newAlias destroyMappedBuffer (UniformBuffer buf devMem1 (Ur (castPtr data')) (Ur size))

    t -> error $ "Unexpected/unsupported storage class for descriptor: " <> show t

-- | Note how the storable must be the same as the storable of the uniform buffer so that the sizes match (ROMES:it seems I dropped the type parameter on the buffer, why?)
writeMappedBuffer :: ∀ α. (SV.Storable α) => (Alias MappedBuffer) ⊸ α -> Renderer (Alias MappedBuffer)
-- writeMappedBuffer (UniformBuffer _ _ (castPtr -> ptr) s) x = assert (fromIntegral (sizeOf @α undefined) <= s) $ liftIO $ poke @α ptr x -- <= because the buffer size might be larger than needed due to alignment constraints so the primTySize returned a size bigger than what we pass over
writeMappedBuffer refcbuf x = enterD "writeMappedBuffer" Linear.do
  (ub, ()) <- Alias.useM refcbuf $ Unsafe.toLinear \ub@(UniformBuffer _ _ (Ur ptr) (Ur s)) ->
    Unsafe.toLinear2 assert (fromIntegral (sizeOf @α undefined) Prelude.<= s) $ -- <= because the buffer size might be larger than needed due to alignment constraints so the primTySize returned a size bigger than what we pass over
      Linear.do liftSystemIO $ poke @α (castPtr ptr) x
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
  let l          = SV.length bufferData
      bufferSize = fromIntegral $ fromIntegral l * sizeOf @α undefined
  (stagingBuffer0, stagingMem0) <- createBuffer bufferSize Vk.BUFFER_USAGE_TRANSFER_SRC_BIT (Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT)
  
  -- Map the buffer memory into CPU accessible memory
  (stagingMem1, Ur data'ptr) <- mapMemory stagingMem0 0 bufferSize zero
  -- Copy buffer data to data'ptr mapped device memory
  liftSystemIO $ SV.unsafeWith bufferData $ \ptr -> do
    copyBytes data'ptr (castPtr ptr) (fromIntegral bufferSize)
    -- ROMES:TODO: Do I need to free data'ptr? I think not, it's host memory deallocated automatically somehow

  -- Unmap memory (doesn't free, just unmaps)
  stagingMem2 <- unmapMemory stagingMem1

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
destroyMappedBuffer (UniformBuffer b dm (Ur _hostMemory) (Ur _size)) = enterD "destroyMappedBuffer" $ Linear.do
  dm' <- unmapMemory dm
  freeMemory dm'
  destroyBuffer b

-- data Buffer = Buffer Vk.Buffer Vk.DeviceMemory

-------- Utils -------------------------

-- | Linear wrapper around Vk.mapMemory.
--
-- It's a bit weird we dont' need to free the host memory, but until we (TODO) make sure, we return the host memory ptr as unrestricted
mapMemory :: Vk.DeviceMemory ⊸ Vk.DeviceSize -> Vk.DeviceSize -> Vk.MemoryMapFlags -> Renderer (Vk.DeviceMemory, Ur (Ptr ()))
mapMemory = Unsafe.toLinear $ \mem offset size flgs -> (mem,) <$> (unsafeUseDevice $ \dev -> Ur <$$> Vk.mapMemory dev mem offset size flgs)

-- | Linear wrapper for Vk.unmapMemory
unmapMemory :: Vk.DeviceMemory ⊸ Renderer Vk.DeviceMemory
unmapMemory = Unsafe.toLinear $ \stgMem -> enterD "unmapMemory" $ Linear.do
  unsafeUseDevice $ \device -> Vk.unmapMemory device stgMem 
  pure stgMem

-- -- | Linear wrapper for Vk.freeMemory
freeMemory :: Vk.DeviceMemory ⊸ Renderer ()
freeMemory = Unsafe.toLinear $ \mem -> unsafeUseDevice $ \device -> Vk.freeMemory device mem Nothing 

-- -- | Linear wrapper for Vk.freeMemory
destroyBuffer :: Vk.Buffer ⊸ Renderer ()
destroyBuffer = Unsafe.toLinear $ \buffer -> unsafeUseDevice $ \device -> Vk.destroyBuffer device buffer Nothing 


-- TODO: Can't forget to call this to free buffer memories after meshes (or the related entities) die
-- destroyBufferAndMemory :: Vk.Buffer ⊸ Vk.DeviceMemory ⊸ Renderer ()
-- destroyBufferAndMemory = Unsafe.toLinear2 $ \buffer mem -> Linear.do
--   unsafeUseDevice $ \device -> Vk.destroyBuffer device buffer Nothing
--   unsafeUseDevice $ \device -> Vk.freeMemory device mem Nothing


