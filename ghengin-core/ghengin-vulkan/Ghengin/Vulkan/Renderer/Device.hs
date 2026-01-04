{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE LinearTypes         #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QualifiedDo         #-}
{-# LANGUAGE RecordWildCards     #-}
module Ghengin.Vulkan.Renderer.Device
  ( createDevice
  , destroyDevice
  , getDeviceQueue
  , findMemoryType -- what?
  ) where

import Control.Monad.IO.Class.Linear qualified as Linear

import Data.Bits
import Data.ByteString ( ByteString )
import Data.Maybe
import Data.Vector     ( Vector )
import Data.Vector     qualified as V
import Data.Word

import Prelude        hiding ( ($) )
import Prelude.Linear ( ($) )

import Unsafe.Linear qualified as Unsafe

import Vulkan                 qualified as Vk
import Vulkan.Zero            qualified as Vk
import Vulkan.CStruct.Extends qualified as VkC
--------------------------------------------------------------------------------
deviceExtensions :: Vector ByteString
deviceExtensions =
  [
#if defined(darwin_HOST_OS)
    -- required from 1.3 with MoltenVk
    Vk.KHR_PORTABILITY_SUBSET_EXTENSION_NAME
#endif
  ]

createDevice :: Linear.MonadIO m
             => Vk.PhysicalDevice %1
             -> Int               -- ^ Queue family index
             -> Vector ByteString -- ^ Extensions required
             -> m (Vk.Device, Vk.PhysicalDevice)
createDevice = Unsafe.toLinear $ \physicalDevice queueFamilyIndex extensionsRequired -> Linear.liftSystemIO $ do

  physicalDeviceFeatures <- Vk.getPhysicalDeviceFeatures physicalDevice

  let
    queueCreateInfo :: Vk.DeviceQueueCreateInfo '[]
    queueCreateInfo = Vk.DeviceQueueCreateInfo
      { next = ()
      , flags = Vk.zero
      , queueFamilyIndex = fromIntegral queueFamilyIndex
      , queuePriorities  = [ 1.0 :: Float ]
      }

    vk12Features :: Vk.PhysicalDeviceVulkan12Features
    vk12Features = Vk.zero
      { Vk.descriptorIndexing = True
      , Vk.descriptorBindingVariableDescriptorCount = True
      , Vk.runtimeDescriptorArray = True
      , Vk.bufferDeviceAddress = True
      }

    vk13Features :: Vk.PhysicalDeviceVulkan13Features
    vk13Features = Vk.zero
      { Vk.synchronization2 = True
      , Vk.dynamicRendering = True
      }

    deviceCreateInfo :: Vk.DeviceCreateInfo '[Vk.PhysicalDeviceVulkan13Features, Vk.PhysicalDeviceVulkan12Features]
    deviceCreateInfo = Vk.DeviceCreateInfo
      { next = (vk13Features, (vk12Features, ()))
      , flags = Vk.zero
      , queueCreateInfos = [VkC.SomeStruct queueCreateInfo]
      , enabledLayerNames = []
      , enabledExtensionNames = extensionsRequired <> deviceExtensions
      , enabledFeatures = Just physicalDeviceFeatures
      }

  device <- Vk.createDevice physicalDevice deviceCreateInfo Nothing
  pure (device, physicalDevice)

destroyDevice :: Linear.MonadIO m => Vk.Device ⊸ m ()
destroyDevice = Unsafe.toLinear $ \d -> Linear.liftSystemIO (Vk.destroyDevice d Nothing)
--------------------------------------------------------------------------------
getDeviceQueue :: Linear.MonadIO m
               => Vk.Device %1
               -> ("queueFamilyIndex" Vk.::: Word32)
               -> ("queueIndex" Vk.::: Word32)
               -> m (Vk.Queue, Vk.Device)
getDeviceQueue = Unsafe.toLinear \dev famIx ix -> Linear.liftSystemIO $ do
  queue <- Vk.getDeviceQueue dev famIx ix
  pure (queue, dev)
--------------------------------------------------------------------------------
findMemoryType :: Word32 -> Vk.MemoryPropertyFlags -> Vk.PhysicalDevice -> IO Word32
findMemoryType typeFilter properties physicalDevice = do
  memProperties <- Vk.getPhysicalDeviceMemoryProperties physicalDevice
  pure $ V.head $ V.imapMaybe (\i t -> if ((typeFilter .&. (1 `unsafeShiftL` i)) /= 0) && ((t.propertyFlags .&. properties) == properties)
                                          then pure (fromIntegral i) else Nothing) memProperties.memoryTypes


