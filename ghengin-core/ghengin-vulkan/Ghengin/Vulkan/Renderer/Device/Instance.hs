{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE LinearTypes         #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
module Ghengin.Vulkan.Renderer.Device.Instance
  ( createInstance, destroyInstance ) where

import GHC.IsList (fromList)
import Prelude hiding (($))
import Prelude.Linear (($))
import qualified Control.Monad.IO.Class.Linear as Linear

import Data.Bits ((.|.))

import Control.Monad
import Foreign.C.String

import Data.ByteString (ByteString)
import Data.Vector     (Vector)
import qualified Data.ByteString  as BS
import qualified Data.Vector      as V
import qualified Graphics.UI.GLFW as GLFW
import qualified Vulkan           as Vk
import qualified Vulkan.Zero      as Vk

import qualified Unsafe.Linear    as Unsafe

--------------------------------------------------------------------------------

instanceExtensions :: Vector ByteString
instanceExtensions =
  [
#if defined(darwin_HOST_OS) && defined(aarch64_HOST_ARCH)
    -- Apple silicon requires this extension from 1.3 with MoltenVk
    Vk.KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME
    -- , Vk.EXT_METAL_SURFACE_EXTENSION_NAME
#endif
  ]

validationLayers :: Vector ByteString
validationLayers =
  [
-- If we're releasing our game bundled with the dynamic
-- libraries, we cannot use a validation layer because those aren't bundled.
#ifdef DEBUG
    "VK_LAYER_KHRONOS_validation"
#endif
  ]

--------------------------------------------------------------------------------

-- | Create a 'Vk.Instance'. The input string is the application name.
createInstance :: Linear.MonadIO m => ByteString -> m Vk.Instance
createInstance appName = Linear.liftSystemIO $ do
  glfwExtensions <- GLFW.getRequiredInstanceExtensions >>= cstringListToVector

  checkRequiredExtensionsSupport glfwExtensions >>= \case
    True  -> pure ()
    False -> fail $ "A required Vulkan extension by GLFW not available"

  checkValidationLayerSupport validationLayers >>= \case
    True  -> pure ()
    False -> fail $ "A required Vulkan layer property is not available"

  Vk.createInstance (instanceInfo glfwExtensions) Nothing

  where
    appInfo  :: Vk.ApplicationInfo
    appInfo = Vk.ApplicationInfo
      { applicationName    = Just appName
      , applicationVersion = 0
      , engineName         = Just "Ghengin"
      , engineVersion      = 0
      , apiVersion         = Vk.API_VERSION_1_3
      }

    instanceInfo :: Vector ByteString -> Vk.InstanceCreateInfo '[]
    instanceInfo glfwe = Vk.InstanceCreateInfo
      { next                  = ()
      , flags                 = Vk.zero
#if defined(darwin_HOST_OS)
                            .|. Vk.INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR -- required at least on 1.3 w MoltenVk
#endif
      , applicationInfo       = Just appInfo
      , enabledLayerNames     = validationLayers
      , enabledExtensionNames = instanceExtensions <> glfwe
      }

    checkValidationLayerSupport :: Vector (ByteString) -> IO Bool
    checkValidationLayerSupport vallys = do
      (_, lps) <- Vk.enumerateInstanceLayerProperties
      all id <$> forM vallys (\vl -> do
        pure $ vl `V.elem` (fmap (.layerName) lps))

    checkRequiredExtensionsSupport :: Vector (ByteString) -> IO Bool
    checkRequiredExtensionsSupport required_exts = do
      (_, exts) <- Vk.enumerateInstanceExtensionProperties Nothing
      all id <$> forM required_exts (\ext ->
        pure $ ext `V.elem` (fmap (.extensionName) exts))

    cstringListToVector :: [CString] -> IO (Vector ByteString)
    cstringListToVector = fmap fromList . traverse BS.packCString

-- | Destroy the 'Vk.Instance'
destroyInstance :: Linear.MonadIO m => Vk.Instance ⊸ m ()
destroyInstance = Unsafe.toLinear \vkInst ->
  Linear.liftSystemIO $ Vk.destroyInstance vkInst Nothing

