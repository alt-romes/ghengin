{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
module Ghengin.Vulkan.Buffer where

import Control.Monad.Reader

import qualified Data.Vector as V
import Data.Bits
import Data.Word
import Vulkan.Zero (zero)
import qualified Vulkan as Vk

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
  memTypeIndex <- findMemoryType memRequirements.memoryTypeBits properties
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

findMemoryType :: Word32 -> Vk.MemoryPropertyFlags -> Renderer Word32
findMemoryType typeFilter properties = asks (._vulkanDevice._physicalDevice) >>= \physicalDevice -> do
  memProperties <- Vk.getPhysicalDeviceMemoryProperties physicalDevice
  pure $ V.head $ V.imapMaybe (\i t -> if ((typeFilter .&. (1 `unsafeShiftL` i)) /= 0) && ((t.propertyFlags .&. properties) == properties)
                                          then pure (fromIntegral i) else Nothing) memProperties.memoryTypes

