{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
module Ghengin.Vulkan.Buffer where

import Data.Int
import Control.Monad.Reader

import qualified Data.Vector.Storable as SV
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Utils
import Data.Bits
import Vulkan.Zero (zero)
import qualified Vulkan as Vk

import qualified Ghengin.Vulkan.Command as Cmd
import Ghengin.Vulkan.Device
import Ghengin.Vulkan

createBuffer :: Vk.DeviceSize -> Vk.BufferUsageFlags -> Vk.MemoryPropertyFlags -> Renderer ext (Vk.Buffer, Vk.DeviceMemory)
createBuffer size usage properties = do
  device <- getDevice
  let bufferInfo = Vk.BufferCreateInfo { next = ()
                                       , flags = zero
                                       , size  = size
                                       , usage = usage
                                       , sharingMode = Vk.SHARING_MODE_EXCLUSIVE
                                       , queueFamilyIndices = []
                                       }
  buffer <- Vk.createBuffer device bufferInfo Nothing
  memRequirements <- Vk.getBufferMemoryRequirements device buffer
  memTypeIndex <- liftIO . findMemoryType memRequirements.memoryTypeBits properties =<< asks (._vulkanDevice._physicalDevice)
  let allocInfo = Vk.MemoryAllocateInfo { next = ()
                                        , allocationSize = memRequirements.size
                                        , memoryTypeIndex = memTypeIndex
                                        }
  devMem <- Vk.allocateMemory device allocInfo Nothing

  -- Bind buffer to the memory we allocated
  Vk.bindBufferMemory device buffer devMem 0
  pure (buffer, devMem)

-- TODO: Can't forget to call this to free buffer memories after meshes (or the related entities) die
destroyBuffer :: Vk.Buffer -> Vk.DeviceMemory -> Renderer ext ()
destroyBuffer buffer mem = do
  device <- getDevice
  Vk.destroyBuffer device buffer Nothing
  Vk.freeMemory device mem Nothing

copyBuffer :: Vk.Buffer -> Vk.Buffer -> Vk.DeviceSize -> Renderer ext ()
copyBuffer src dst size = immediateSubmit $ Cmd.copyFullBuffer src dst size

-- | Fills a device local buffer with the provided flags and the provided data
-- by first copying the data to a staging buffer and then running a buffer copy
-- one-shot command.
createDeviceLocalBuffer :: Storable a => Vk.BufferUsageFlags -> SV.Vector a -> Renderer ext (Vk.Buffer, Vk.DeviceMemory)
createDeviceLocalBuffer flags bufferData = do

  let l          = SV.length bufferData
      bufferSize = fromIntegral $ l * sizeOf (SV.head bufferData)
  (stagingBuffer, stagingMem) <- createBuffer bufferSize Vk.BUFFER_USAGE_TRANSFER_SRC_BIT (Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT)

  device <- getDevice
  
  -- Map the buffer memory into CPU accessible memory
  data' <- Vk.mapMemory device stagingMem 0 bufferSize zero
  -- Copy buffer data to data' mapped device memory
  liftIO $ SV.unsafeWith bufferData $ \ptr -> do
    copyBytes data' (castPtr ptr) (fromIntegral bufferSize)
  -- Unmap memory
  Vk.unmapMemory device stagingMem

  (devBuffer, devMem) <- createBuffer bufferSize (Vk.BUFFER_USAGE_TRANSFER_DST_BIT .|. flags) Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT

  -- Copy data from staging buffer to actual buffer inaccessible by the host
  copyBuffer stagingBuffer devBuffer bufferSize

  -- Free staging buffer
  Vk.destroyBuffer device stagingBuffer Nothing
  Vk.freeMemory    device stagingMem    Nothing

  pure (devBuffer, devMem)

createVertexBuffer :: Storable a => SV.Vector a -> Renderer ext (Vk.Buffer, Vk.DeviceMemory)
createVertexBuffer = createDeviceLocalBuffer Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT

createIndex32Buffer :: SV.Vector Int32 -> Renderer ext (Vk.Buffer, Vk.DeviceMemory)
createIndex32Buffer = createDeviceLocalBuffer Vk.BUFFER_USAGE_INDEX_BUFFER_BIT

-- | A Uniform buffer with size equal to the sizeOf of the Storable @a@
data MappedBuffer a = UniformBuffer { buffer :: Vk.Buffer
                                    , devMem :: Vk.DeviceMemory
                                    , hostMem :: Ptr a
                                    }


data SomeMappedBuffer where
  SomeMappedBuffer :: ∀ a. Storable a => MappedBuffer a -> SomeMappedBuffer

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


-- | Create a uniform buffer with a given size, but don't copy memory to it
-- yet. See 'writeUniformBuffer' for that yet. See 'writeUniformBuffer' for
-- that yet. See 'writeUniformBuffer' for that yet. See 'writeUniformBuffer'
-- for that.
--
-- The size is given by the type of the storable to store in the uniform buffer.
createMappedBuffer :: ∀ a ext. Storable a => Vk.DescriptorType -> Renderer ext (MappedBuffer a)
createMappedBuffer descriptorType = do
  device <- getDevice

  case descriptorType of
    Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER -> do

      let bsize = fromIntegral $ sizeOf @a undefined

      (buf, devMem) <- createBuffer bsize Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT (Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT)

      data' <- Vk.mapMemory device devMem 0 bsize zero

      pure $ UniformBuffer buf devMem (castPtr data')

    t -> error $ "Unexpected/unsupported storage class for descriptor: " <> show t

destroyMappedBuffer :: MappedBuffer a -> Renderer ext ()
destroyMappedBuffer (UniformBuffer b dm _hostMemory) = do
  -- TODO: Is hostMemory freed with unmap? Or with destroyMemory? Or?
  device <- getDevice
  Vk.unmapMemory device dm
  destroyBuffer b dm

-- | Note how the storable must be the same as the storable of the uniform buffer so that the sizes match
writeMappedBuffer :: Storable a => MappedBuffer a -> a -> Renderer ext ()
writeMappedBuffer (UniformBuffer _ _ ptr) x = liftIO $ poke ptr x


