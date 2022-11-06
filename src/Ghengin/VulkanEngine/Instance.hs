{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
module Ghengin.VulkanEngine.Instance (createInstance, destroyInstance) where

import GHC.IsList (fromList)

import Data.Vector     qualified as V
import Data.ByteString qualified as BS

import Control.Monad

import Foreign.C.String

import Graphics.UI.GLFW qualified as GLFW
import Vulkan qualified as Vk

checkValidationLayerSupport :: V.Vector (BS.ByteString) -> IO Bool
checkValidationLayerSupport vallys = do
  (_, lps) <- Vk.enumerateInstanceLayerProperties
  all id <$> forM vallys (\vl -> do
    pure $ vl `V.elem` (fmap (.layerName) lps))

checkRequiredExtensionsSupport :: V.Vector (BS.ByteString) -> IO Bool
checkRequiredExtensionsSupport required_exts = do
  (_, exts) <- Vk.enumerateInstanceExtensionProperties Nothing
  all id <$> forM required_exts (\ext ->
    pure $ ext `V.elem` (fmap (.extensionName) exts))

createInstance :: V.Vector BS.ByteString -> IO Vk.Instance
createInstance validationLayers = do
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

destroyInstance :: Vk.Instance -> IO ()
destroyInstance vkInst = Vk.destroyInstance vkInst Nothing

