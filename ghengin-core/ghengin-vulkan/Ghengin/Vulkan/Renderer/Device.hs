{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
module Ghengin.Vulkan.Renderer.Device where

import Prelude hiding (($))
import Prelude.Linear (($))
import Data.Ord
import Data.Bits
import Data.Maybe
import Data.Word

import Data.ByteString (ByteString)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.List as L
import qualified Data.Set as S
import qualified System.IO.Linear as Linear
import qualified Control.Monad.IO.Class.Linear as Linear

import qualified Vulkan.CStruct.Extends as VkC
import qualified Vulkan as Vk

import qualified Unsafe.Linear as Unsafe

-- We create a logical device always with a graphics queue and a present queue

type GraphicsQueueFamily = Word32
type PresentQueueFamily  = Word32

-- | Device rating function.
--  The return value is maybe a tuple with three items: the rating associated with the device (higher is better), the graphics queue family and the present queue family
type DeviceRateFunction = (Vk.PhysicalDevice -> IO (Maybe (Int, GraphicsQueueFamily, PresentQueueFamily)))

data VulkanDevice = VulkanDevice { _physicalDevice      :: !Vk.PhysicalDevice
                                 , _device              :: !Vk.Device
                                 , _graphicsQueue       :: !Vk.Queue
                                 , _presentQueue        :: !Vk.Queue
                                 , _graphicsQueueFamily :: !GraphicsQueueFamily
                                 , _presentQueueFamily  :: !PresentQueueFamily
                                 }

createVulkanDevice :: Vk.Instance
                    ⊸ Vector ByteString -- ^ Validation Layers
                   -> Vector ByteString -- ^ Device Extensions
                   -> DeviceRateFunction
                   -> Linear.IO (VulkanDevice, Vk.Instance)
createVulkanDevice = Unsafe.toLinear $ \inst validationLayers deviceExtensions rateFn -> Linear.liftSystemIO $ do

  (physicalDevice, graphicsQF, presentQF) <- pickPhysicalDevice inst rateFn
  physicalDeviceFeatures <- Vk.getPhysicalDeviceFeatures physicalDevice -- currently features aren't considered in the rateFn but they could be; we just read them again afterwards.

  let
    deviceCreateInfo = Vk.DeviceCreateInfo { next = ()
                                           , flags = Vk.DeviceCreateFlags 0
                                           , queueCreateInfos = (V.fromList . map (VkC.SomeStruct . deviceQueueCreateInfo) . S.toList) [graphicsQF, presentQF]
                                           , enabledLayerNames = validationLayers
                                           , enabledExtensionNames = deviceExtensions
                                           , enabledFeatures = Just physicalDeviceFeatures
                                           }

    deviceQueueCreateInfo ix = Vk.DeviceQueueCreateInfo { next = ()
                                                        , flags = Vk.DeviceQueueCreateFlagBits 0
                                                        , queueFamilyIndex = ix
                                                        -- For now all queues have same priority (there should only be one queue anyway?)
                                                        , queuePriorities  = [1]
                                                        }

  device        <- Vk.createDevice physicalDevice deviceCreateInfo Nothing
  graphicsQueue <- Vk.getDeviceQueue device graphicsQF 0
  presentQueue  <- Vk.getDeviceQueue device presentQF  0
  pure (VulkanDevice physicalDevice device graphicsQueue presentQueue graphicsQF presentQF, inst)


destroyVulkanDevice :: VulkanDevice ⊸ Linear.IO ()
destroyVulkanDevice = Unsafe.toLinear $ \d -> Linear.liftSystemIO (Vk.destroyDevice d._device Nothing)


pickPhysicalDevice :: Vk.Instance
                   -> DeviceRateFunction
                   -> IO (Vk.PhysicalDevice, GraphicsQueueFamily, PresentQueueFamily)
pickPhysicalDevice inst rateFn = do
  (_, dvs) <- Vk.enumeratePhysicalDevices inst
  case dvs of
    [] -> fail "Failed to find GPUs with Vulkan support!"
    _  ->
      L.sortOn (Down . fst) . V.toList . V.filter (isJust . fst) . (`V.zip` dvs) <$> traverse rateFn dvs >>= \case
        (Just (_,graphicsQF,presentQF),device):_ -> pure (device, graphicsQF, presentQF)
        (Nothing,_):_ -> fail "Impossible! Failed to find a suitable GPU!"
        [] -> fail "Failed to find a suitable GPU!"


findMemoryType :: Word32 -> Vk.MemoryPropertyFlags -> Vk.PhysicalDevice -> IO Word32
findMemoryType typeFilter properties physicalDevice = do
  memProperties <- Vk.getPhysicalDeviceMemoryProperties physicalDevice
  pure $ V.head $ V.imapMaybe (\i t -> if ((typeFilter .&. (1 `unsafeShiftL` i)) /= 0) && ((t.propertyFlags .&. properties) == properties)
                                          then pure (fromIntegral i) else Nothing) memProperties.memoryTypes


