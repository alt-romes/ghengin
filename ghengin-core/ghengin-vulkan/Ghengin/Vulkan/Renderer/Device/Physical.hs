-- Adapted from fir-examples
module Ghengin.Vulkan.Renderer.Device.Physical
  ( createPhysicalDevice
  , findQueueFamilyIndex
  ) where

import Control.Monad

import Data.Bits        ( Bits ((.&.)) )
import Data.Foldable    ( for_ )
import Data.Traversable ( for )
import Data.Vector      qualified as Boxed ( Vector )
import Data.Vector      qualified as Boxed.Vector

import Ghengin.Core.Prelude ( Ur (..) )
import Ghengin.Core.Prelude qualified as Linear

import Prelude

import Unsafe.Linear qualified as Unsafe

import Vulkan qualified
import Vulkan.Zero qualified as Vulkan

createPhysicalDevice :: Linear.MonadIO m => Vulkan.Instance %1 -> m (Vulkan.PhysicalDevice, Vulkan.Instance)
createPhysicalDevice = Unsafe.toLinear \inst -> Linear.liftSystemIO $ do
  -- TODO: Allow choosing the physical device (e.g. with cli flags)
  physicalDevices <- snd <$> Vulkan.enumeratePhysicalDevices inst

  typedDevices <-
    for physicalDevices \ physicalDevice -> do
      properties <- Vulkan.getPhysicalDeviceProperties physicalDevice
      pure ( physicalDevice, Vulkan.deviceType properties )

  case Boxed.Vector.find ( isSuitableDeviceType . snd ) typedDevices of
    Nothing       -> error "Could not find a suitable physical device"
    Just ( d, _ ) -> pure (d, inst)

  where
    isSuitableDeviceType :: Vulkan.PhysicalDeviceType -> Bool
    isSuitableDeviceType
      = flip elem
          [ Vulkan.PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU
          , Vulkan.PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
          ]

findQueueFamilyIndex
  :: Linear.MonadIO m
  => Vulkan.PhysicalDevice %1
  -> [ Vulkan.QueueFlags ]
  -> m (Ur Int, Vulkan.PhysicalDevice)
findQueueFamilyIndex = Unsafe.toLinear \physicalDevice requiredFlags -> Linear.liftSystemIO $ do
  queueFamilies <- Vulkan.getPhysicalDeviceQueueFamilyProperties physicalDevice
  let
    capableFamilyIndices :: Boxed.Vector Int
    capableFamilyIndices = ( `Boxed.Vector.imapMaybe` queueFamilies ) \ i queueFamily -> do
      let
        flags :: Vulkan.QueueFlags
        flags = Vulkan.queueFlags queueFamily
      for_ requiredFlags
        ( \ f ->
            guard ( flags .&. f > Vulkan.zero )
        )
      pure i
  case capableFamilyIndices Boxed.Vector.!? 0 of
    Nothing -> error "No queue family has sufficient capabilities"
    Just i  -> pure (Ur i, physicalDevice)

