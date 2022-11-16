{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Ghengin.Vulkan.Device.Instance where

import GHC.IsList (fromList)

import Control.Monad
import Foreign.C.String

import qualified Data.ByteString as BS
import qualified Data.Vector as V

import qualified Graphics.UI.GLFW as GLFW
import qualified Vulkan as Vk

createInstance :: V.Vector BS.ByteString -- ^ Validation layers
               -- -> BS.ByteString          -- ^ Application name
               -> IO Vk.Instance
createInstance validationLayers = do
  -- This will only return something if GLFW has been initialized
  glfwExtensions <- GLFW.getRequiredInstanceExtensions >>= cstringListToVector

  checkRequiredExtensionsSupport glfwExtensions >>= \case
    True  -> pure ()
    False -> fail $ "A required Vulkan extension by GLFW not available"

  checkValidationLayerSupport validationLayers >>= \case
    True  -> pure ()
    False -> fail $ "A required Vulkan layer property is not available"

  vkInst <- Vk.createInstance (instanceInfo glfwExtensions) Nothing

  pure vkInst

  where
    appInfo  :: Vk.ApplicationInfo
    appInfo = Vk.ApplicationInfo { applicationName    = Just "Ghengin"
                                 , applicationVersion = 0
                                 , engineName         = Just "Ghengin"
                                 , engineVersion      = 0
                                 , apiVersion         = Vk.API_VERSION_1_2
                                 }

    instanceInfo :: V.Vector BS.ByteString -> Vk.InstanceCreateInfo '[]
    instanceInfo glfwe
      = Vk.InstanceCreateInfo {..} where
          next  = ()
          flags = Vk.InstanceCreateFlagBits 0
          applicationInfo = Just appInfo
          enabledLayerNames = validationLayers
          enabledExtensionNames = glfwe

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

    cstringListToVector :: [CString] -> IO (V.Vector BS.ByteString)
    cstringListToVector = fmap fromList . traverse BS.packCString


destroyInstance :: Vk.Instance -> IO ()
destroyInstance vkInst = Vk.destroyInstance vkInst Nothing

