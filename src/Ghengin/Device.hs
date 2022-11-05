{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Ghengin.Device where

import Data.Ord
import Data.List qualified as L

import Data.Vector     qualified as V
import Data.ByteString qualified as BS

import Control.Exception
import Control.Monad

import GHC.IsList

import Foreign.C.String

import Graphics.UI.GLFW qualified as GLFW
import Vulkan qualified as Vk

-- ROMES:TODO: Understand this thing's name
data Volcano = D { _inst :: Vk.Instance }

initVulkan :: IO ()
initVulkan = do
  vkInst <- createInstance
  vkPhysicalDevice <- pickPhysicalDevice vkInst
  pure ()


rateDevice :: Vk.PhysicalDevice -> IO (Int, Vk.PhysicalDevice)
rateDevice d = do
  props <- Vk.getPhysicalDeviceProperties d
  _feats <- Vk.getPhysicalDeviceFeatures d

  let s1 = if props.deviceType == Vk.PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
             then 1000
             else 0
      s2 = props.limits.maxImageDimension2D

  -- If the app can't function without geometry shaders
  -- if feats.geometryShader
  --    then pure (s1 + fromEnum s2, d)
  --    else pure (0, d)

  pure (s1 + fromEnum s2, d)

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

-- | Checks if the validation
checkValidationLayerSupport :: V.Vector (BS.ByteString) -> IO Bool
checkValidationLayerSupport vallys = do
  (_, lps) <- Vk.enumerateInstanceLayerProperties
  all id <$> forM vallys (\vl -> do
    pure $ vl `V.elem` (fmap (.layerName) lps))
    -- False -> fail $ "Required Vulkan layer property (" <> show vl <> ") is not available"

checkRequiredExtensionsSupport :: V.Vector (BS.ByteString) -> IO Bool
checkRequiredExtensionsSupport required_exts = do
  -- Check required extensions exist
  (_, exts) <- Vk.enumerateInstanceExtensionProperties Nothing
  all id <$> forM required_exts (\ext ->
    pure $ ext `V.elem` (fmap (.extensionName) exts))
    -- False -> fail $ "Required Vulkan extension (" <> show ext <> ") by GLFW not available"

-- withDevice :: (Volcano -> IO a) -> IO a
-- withDevice f = bracket createDevice destroyDevice f

createInstance :: IO Vk.Instance
createInstance = do
  glfwExtensions <- GLFW.getRequiredInstanceExtensions >>= cstringListToVector

  checkRequiredExtensionsSupport glfwExtensions >>= \case
    True  -> pure ()
    False -> fail $ "A required Vulkan extension by GLFW not available"

  checkValidationLayerSupport validationLayers >>= \case
    True  -> pure ()
    False -> fail $ "A required Vulkan layer property is not available"

  vkInst         <- Vk.createInstance (ici glfwExtensions) Nothing

  pure vkInst

  where
    ai  :: Vk.ApplicationInfo
    ai = Vk.ApplicationInfo {..} where
           applicationName    = Just "Hello Triangle"
           applicationVersion = Vk.MAKE_API_VERSION 1 0 0
           engineName         = Just "No Engine"
           engineVersion      = Vk.MAKE_API_VERSION 1 0 0
           apiVersion         = Vk.API_VERSION_1_0

    ici :: V.Vector BS.ByteString -> Vk.InstanceCreateInfo '[]
    ici glfwe
      = Vk.InstanceCreateInfo {..} where
          next  = ()
          flags = Vk.InstanceCreateFlagBits 0
          applicationInfo = Just ai
          enabledLayerNames = validationLayers
          enabledExtensionNames = glfwe

    cstringListToVector :: [CString] -> IO (V.Vector BS.ByteString)
    cstringListToVector = fmap fromList . traverse BS.packCString
                        -- . BS.create (newForeignPtr_ extsPtrPtr) count



destroyDevice :: Vk.Instance -> IO ()
destroyDevice vkInst = Vk.destroyInstance vkInst Nothing

