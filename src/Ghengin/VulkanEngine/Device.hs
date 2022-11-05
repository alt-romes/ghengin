{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Ghengin.VulkanEngine.Device where

import Data.Word
import Data.Bits
import Data.Ord
import Data.Maybe
import Data.List qualified as L

import Data.Vector     qualified as V
import Data.ByteString qualified as BS

import Control.Exception
import Control.Monad

import GHC.IsList

import Graphics.UI.GLFW qualified as GLFW
import Vulkan qualified as Vk
import Vulkan.CStruct.Extends qualified as VkC

-- ROMES:TODO: Understand this thing's name
data Volcano = D { _inst :: Vk.Instance }

initVulkan :: IO ()
initVulkan = do
  vkInst <- createInstance
  vkPhysicalDevice <- pickPhysicalDevice vkInst
  _vkLogicalDevice <- createLogicalDevice vkPhysicalDevice
  _vkDeviceQueue   <- getDeviceQueue 0
  pure ()

getDeviceQueue :: Int -> IO Vk.Queue
getDeviceQueue i = do
  _
  -- Vk.getDeviceQueue 

createLogicalDevice :: Vk.PhysicalDevice -> IO (Vk.Device)
createLogicalDevice physicalDevice = do
  Just qfi <- findQueueFamilies physicalDevice
  Vk.createDevice physicalDevice (dci (dqci qfi)) Nothing
    where
      dci :: Vk.DeviceQueueCreateInfo '[] -> Vk.DeviceCreateInfo '[]
      dci qci = Vk.DeviceCreateInfo {..} where
                next = ()
                flags = Vk.DeviceCreateFlags 0
                queueCreateInfos = [VkC.SomeStruct qci]
                enabledLayerNames = validationLayers
                enabledExtensionNames = []
                enabledFeatures = Nothing

      dqci :: QueueFamiliesIndices -> Vk.DeviceQueueCreateInfo '[]
      dqci (QFI ix) = Vk.DeviceQueueCreateInfo {..} where
                next = ()
                flags = Vk.DeviceQueueCreateFlagBits 0
                queueFamilyIndex = ix
                queuePriorities  = [1]


data QueueFamiliesIndices = QFI { _graphicsFamily :: Word32 }

findQueueFamilies :: Vk.PhysicalDevice -> IO (Maybe QueueFamiliesIndices)
findQueueFamilies pd = do
  props <- Vk.getPhysicalDeviceQueueFamilyProperties pd
  let suitable = V.filter (isQueueFamilySuitable . snd) (V.indexed props)
  if V.null suitable
     then pure Nothing
     else pure $ Just $ QFI $ fromIntegral $ fst $ V.head suitable

  where
    isQueueFamilySuitable :: Vk.QueueFamilyProperties -> Bool
    isQueueFamilySuitable q = q.queueFlags .&&. Vk.QUEUE_GRAPHICS_BIT


rateDevice :: Vk.PhysicalDevice -> IO (Int, Vk.PhysicalDevice)
rateDevice d = do
  props <- Vk.getPhysicalDeviceProperties d
  feats <- Vk.getPhysicalDeviceFeatures d
  queueFamilies <- findQueueFamilies d

  let
      s1 = if props.deviceType == Vk.PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
             then 1000
             else 0
      s2 = props.limits.maxImageDimension2D

      c1 = isJust queueFamilies

     -- If the app couldn't function without geometry shaders
      _c2 = feats.geometryShader

  if c1
     then pure (s1 + fromIntegral s2, d)
     else pure (0, d)

pickPhysicalDevice :: Vk.Instance -> IO Vk.PhysicalDevice
pickPhysicalDevice vkInst = do
  (_, dvs) <- Vk.enumeratePhysicalDevices vkInst
  L.sortOn (Down . fst) . toList <$> traverse rateDevice dvs >>= \case
    []  -> fail "Failed to find GPUs with Vulkan support!"
    (rating,device):_
      | rating < 1 -> fail "Failed to find a suitable GPU!"
      | otherwise  -> pure device

validationLayers :: V.Vector (BS.ByteString)
validationLayers = [ "VK_LAYER_KHRONOS_validation"
                   ]

-- withDevice :: (Vk.Instance -> IO a) -> IO a
-- withDevice f = bracket createInstance destroyInstance f



(.&&.) :: Bits a => a -> a -> Bool
x .&&. y = (/= zeroBits) (x .&. y)
