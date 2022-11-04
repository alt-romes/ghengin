{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Ghengin.Device (Device, withDevice) where

import Data.Vector     qualified as V
import Data.ByteString qualified as BS

import Control.Exception

import GHC.IsList

import Foreign.C.String

import Graphics.UI.GLFW qualified as GLFW
import Vulkan qualified as Vk

data Device = D { inst :: Vk.Instance }

withDevice :: (Device -> IO a) -> IO a
withDevice f = bracket createDevice destroyDevice f

createDevice :: IO Device
createDevice = do
  glfwExtensions <- GLFW.getRequiredInstanceExtensions >>= cstringListToVector
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
          flags = Vk.INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR
          applicationInfo = Just ai
          enabledLayerNames = []
          enabledExtensionNames = glfwe

    cstringListToVector :: [CString] -> IO (V.Vector BS.ByteString)
    cstringListToVector = fmap fromList . traverse BS.packCString
                        -- . BS.create (newForeignPtr_ extsPtrPtr) count

destroyDevice :: Device -> IO ()
destroyDevice (D vkInst) = Vk.destroyInstance vkInst Nothing

