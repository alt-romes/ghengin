{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Ghengin.Vulkan.Device where

import Data.Ord
import Data.Maybe
import Data.Word
import Control.Monad

import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Data.List as L
import qualified Data.Set as S

import qualified Vulkan.CStruct.Extends as VkC
import qualified Vulkan as Vk

import Ghengin.Vulkan.Device.Instance
import Ghengin.Vulkan.GLFW.Window

validationLayers :: V.Vector BS.ByteString
validationLayers = [ "VK_LAYER_KHRONOS_validation"
                   ]

deviceExtensions :: V.Vector BS.ByteString
deviceExtensions = [ Vk.KHR_SWAPCHAIN_EXTENSION_NAME
                   ]

-- We create a logical device always with a graphics queue and a present queue

type GraphicsQueueFamily = Word32
type PresentQueueFamily  = Word32

-- | Device rating function.
--  The return value is maybe a tuple with three items: the rating associated with the device (higher is better), the graphics queue family and the present queue family
type DeviceRateFunction = (Vk.PhysicalDevice -> IO (Maybe (Int, GraphicsQueueFamily, PresentQueueFamily)))

createDevice :: DeviceRateFunction
             -> IO Vk.Device
createDevice rateFn = do

  inst <- createInstance validationLayers

  (physicalDevice, graphicsQF, presentQF) <- pickPhysicalDevice inst rateFn

  let
    deviceCreateInfo = Vk.DeviceCreateInfo { next = ()
                                           , flags = Vk.DeviceCreateFlags 0
                                           , queueCreateInfos = (V.fromList . map (VkC.SomeStruct . deviceQueueCreateInfo) . S.toList) [graphicsQF, presentQF]
                                           , enabledLayerNames = validationLayers
                                           , enabledExtensionNames = deviceExtensions
                                           , enabledFeatures = Nothing
                                           }

    deviceQueueCreateInfo ix = Vk.DeviceQueueCreateInfo { next = ()
                                                        , flags = Vk.DeviceQueueCreateFlagBits 0
                                                        , queueFamilyIndex = ix
                                                        -- For now all queues have same priority (there should only be one queue anyway?)
                                                        , queuePriorities  = [1]
                                                        }

  Vk.createDevice physicalDevice deviceCreateInfo Nothing


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


destroyLogicalDevice :: Vk.Device -> IO ()
destroyLogicalDevice d = Vk.destroyDevice d Nothing

