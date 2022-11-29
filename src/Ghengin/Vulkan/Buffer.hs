{-# LANGUAGE LambdaCase #-}
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
  

