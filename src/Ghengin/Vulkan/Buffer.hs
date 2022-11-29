{-# LANGUAGE LambdaCase #-}
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
import qualified Vulkan.CStruct.Extends as Vk

import qualified Ghengin.Vulkan.Command as Cmd
import Ghengin.Vulkan.Device
import Ghengin.Vulkan

createBuffer :: Vk.DeviceSize -> Vk.BufferUsageFlags -> Vk.MemoryPropertyFlags -> Renderer (Vk.Buffer, Vk.DeviceMemory)
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
destroyBuffer :: Vk.Buffer -> Vk.DeviceMemory -> Renderer ()
destroyBuffer buffer mem = do
  device <- getDevice
  Vk.destroyBuffer device buffer Nothing
  Vk.freeMemory device mem Nothing

copyBuffer :: Vk.Buffer -> Vk.Buffer -> Vk.DeviceSize -> Renderer ()
copyBuffer src dst size = do
  device <- getDevice
  cpool <- asks (._commandPool)
  liftIO (Cmd.createCommandBuffers device cpool 1) >>= \case
    [b] -> do
          Cmd.recordCommandOneShot b $ do
            Cmd.copyFullBuffer src dst size

          graphicsQueue <- asks (._vulkanDevice._graphicsQueue)
          Vk.queueSubmit   graphicsQueue [Vk.SomeStruct $ Vk.SubmitInfo () [] [] [b.commandBufferHandle] []] Vk.NULL_HANDLE
          Vk.queueWaitIdle graphicsQueue

          liftIO $ Cmd.destroyCommandBuffers device cpool [b]

    _ -> liftIO $ fail "Create command buffers expected 1 got something else"
  

-- | Fills a device local buffer with the provided flags and the provided data
-- by first copying the data to a staging buffer and then running a buffer copy
-- one-shot command.
createDeviceLocalBuffer :: Storable a => Vk.BufferUsageFlags -> SV.Vector a -> Renderer (Vk.Buffer, Vk.DeviceMemory)
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

createVertexBuffer :: Storable a => SV.Vector a -> Renderer (Vk.Buffer, Vk.DeviceMemory)
createVertexBuffer = createDeviceLocalBuffer Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT

createIndex32Buffer :: SV.Vector Int32 -> Renderer (Vk.Buffer, Vk.DeviceMemory)
createIndex32Buffer = createDeviceLocalBuffer Vk.BUFFER_USAGE_INDEX_BUFFER_BIT

