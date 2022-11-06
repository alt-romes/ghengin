{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Ghengin.VulkanEngine where

import Data.Kind
import Data.Vector     qualified as V
import Data.ByteString qualified as BS

import Vulkan qualified as Vk

import Ghengin.VulkanEngine.Instance
import Ghengin.VulkanEngine.PhysicalDevice
import Ghengin.VulkanEngine.QueueFamilies
import Ghengin.VulkanEngine.Device
import Ghengin.VulkanEngine.Queue

data VulkanEngine (ps :: [EnginePart])
  = VulkanEngine
    { vkInstance       :: !(HasPart EInstance ps       :=> Vk.Instance)
    , vkPhysicalDevice :: !(HasPart EPhysicalDevice ps :=> Vk.PhysicalDevice)
    , vkDevice         :: !(HasPart EDevice ps         :=> Vk.Device)
    , vkGraphicsQueue  :: !(HasPart EGraphicsQueue ps  :=> Vk.Queue)
    }

data EnginePart
  = EInstance
  | EPhysicalDevice
  | EDevice
  | EGraphicsQueue 

type family HasPart (p :: EnginePart) (ps :: [EnginePart]) :: Bool where
  HasPart _ '[] = False
  HasPart p (p ': xs) = True
  HasPart p (_ ': xs) = HasPart p xs

type family (:=>) (b :: Bool) (t :: Type) :: Type where
  (:=>) False t = ()
  (:=>) True  t = t



validationLayers :: V.Vector (BS.ByteString)
validationLayers = [ "VK_LAYER_KHRONOS_validation"
                   ]

initVulkanEngine :: IO (VulkanEngine [EInstance, EPhysicalDevice, EDevice, EGraphicsQueue])
initVulkanEngine = do
  inst           <- createInstance validationLayers
  physicalDevice <- pickPhysicalDevice inst
  Just (QFI i1)  <- findQueueFamilies physicalDevice
  device         <- createLogicalDevice validationLayers physicalDevice (QFI i1)
  graphicsQueue  <- getDeviceQueue device i1 0
  pure $ VulkanEngine inst physicalDevice device graphicsQueue



-- initVulkan :: IO ()
-- initVulkan = do
--   vkInst <- createInstance
--   vkPhysicalDevice <- pickPhysicalDevice vkInst
--   _vkLogicalDevice <- createLogicalDevice vkPhysicalDevice
--   _vkDeviceQueue   <- getDeviceQueue 0
--   pure ()

-- cleanup :: Vk.Instance -> Vk.Device -> IO ()
-- cleanup i d = do
--   destroyInstance i
  

