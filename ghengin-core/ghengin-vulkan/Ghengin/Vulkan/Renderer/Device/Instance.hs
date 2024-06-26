{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
module Ghengin.Vulkan.Renderer.Device.Instance (createInstance, destroyInstance) where

import GHC.IsList (fromList)
import Prelude hiding (($))
import Prelude.Linear (($))

import Data.Bits ((.|.))

import Control.Monad
import Foreign.C.String

import qualified Control.Monad.IO.Class.Linear as Linear

import qualified Data.ByteString as BS
import qualified Data.Vector as V

import qualified Graphics.UI.GLFW as GLFW
import qualified Vulkan.Linear as Vk

instanceExtensions :: V.Vector BS.ByteString
instanceExtensions = [
-- Apple silicon requires this extension at least from 1.3 with MoltenVk
#if defined(darwin_HOST_OS) && defined(aarch64_HOST_ARCH)
                       Vk.KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME
#endif
                     -- , Vk.EXT_METAL_SURFACE_EXTENSION_NAME
                     ]

createInstance :: Linear.MonadIO m
               => V.Vector BS.ByteString -- ^ Validation layers
               -- -> BS.ByteString          -- ^ Application name
               -> m Vk.Instance
createInstance validationLayers = Linear.liftSystemIO $ do
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
#if defined(darwin_HOST_OS)
                    .|. Vk.INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR -- required at least on 1.3 w MoltenVk
#endif
          applicationInfo = Just appInfo
          enabledLayerNames = validationLayers
          enabledExtensionNames = instanceExtensions <> glfwe

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


destroyInstance :: Linear.MonadIO m => Vk.Instance ⊸ m ()
destroyInstance vkInst = Vk.destroyInstance vkInst Nothing

