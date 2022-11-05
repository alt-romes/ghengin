{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Ghengin.VulkanEngine where

import Data.Kind

import Vulkan qualified as Vk

import Ghengin.VulkanEngine.Instance

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


initVulkanEngine :: IO (VulkanEngine [EInstance, EPhysicalDevice, EDevice, EGraphicsQueue])
initVulkanEngine = do
  addInstance emptyVulkanEngine



emptyVulkanEngine :: VulkanEngine '[]
emptyVulkanEngine = VulkanEngine () () () ()

addInstance :: VulkanEngine '[] -> IO (VulkanEngine '[EInstance])
addInstance (VulkanEngine () () () ()) = do
  inst <- createInstance
  pure $ VulkanEngine inst () () ()


-- cleanup :: Vk.Instance -> Vk.Device -> IO ()
-- cleanup i d = do
--   destroyInstance i
--   Vk.destroyDevice d Nothing
  

