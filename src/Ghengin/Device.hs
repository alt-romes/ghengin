{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Ghengin.Device (Device, withDevice) where

import Data.Vector     qualified as V
import Data.ByteString qualified as BS

import Control.Exception
import Control.Monad

import GHC.IsList

import Foreign.C.String

import Graphics.UI.GLFW qualified as GLFW
import Vulkan qualified as Vk

data Device = D { _inst :: Vk.Instance }

withDevice :: (Device -> IO a) -> IO a
withDevice f = bracket createDevice destroyDevice f

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

createDevice :: IO Device
createDevice = do
  glfwExtensions <- GLFW.getRequiredInstanceExtensions >>= cstringListToVector

  checkRequiredExtensionsSupport glfwExtensions >>= \case
    True  -> pure ()
    False -> fail $ "A required Vulkan extension by GLFW not available"

  checkValidationLayerSupport validationLayers >>= \case
    True  -> pure ()
    False -> fail $ "A required Vulkan layer property is not available"

  vkInst         <- Vk.createInstance (ici glfwExtensions) Nothing

  pure $ D vkInst

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
          flags = Vk.InstanceCreateFlagBits 0 -- Validation layer reported that using this is a bug and that it should start as 0 :) Vk.INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR
          applicationInfo = Just ai
          enabledLayerNames = validationLayers
          enabledExtensionNames = glfwe

    cstringListToVector :: [CString] -> IO (V.Vector BS.ByteString)
    cstringListToVector = fmap fromList . traverse BS.packCString
                        -- . BS.create (newForeignPtr_ extsPtrPtr) count

destroyDevice :: Device -> IO ()
destroyDevice (D vkInst) = Vk.destroyInstance vkInst Nothing

