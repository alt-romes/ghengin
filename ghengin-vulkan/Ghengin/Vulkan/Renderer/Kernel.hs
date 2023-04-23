{-# LANGUAGE NoImplicitPrelude, LinearTypes, PatternSynonyms #-}
module Ghengin.Vulkan.Renderer.Kernel where

import qualified System.IO.Linear
import qualified Data.Functor.Linear as Data.Linear
import Control.Functor.Linear as Linear
import Control.Monad.IO.Class.Linear as Linear

import Ghengin.Vulkan.Renderer.Device

-- One day abstract over Window API
import Ghengin.Vulkan.Renderer.GLFW.Window

import qualified Vulkan as Vk

data RendererEnv =
  REnv { _instance        :: !Vk.Instance
       , _vulkanDevice    :: !VulkanDevice
       , _vulkanWindow    :: !VulkanWindow
       -- , _vulkanSwapChain :: !VulkanSwapChain
       -- , _commandPool     :: !Vk.CommandPool
       -- , _frames          :: !(Vector VulkanFrameData)
       -- , _frameInFlight   :: !(IORef Int)
       -- , _immediateSubmit :: !ImmediateSubmitCtx
       }
-- ROMES: Worried linear StateT might reduce performance, hope not
newtype Renderer a = Renderer { unRenderer :: Linear.StateT RendererEnv System.IO.Linear.IO a }

deriving instance Data.Linear.Functor Renderer
deriving instance Data.Linear.Applicative Renderer
deriving instance Linear.Functor Renderer
deriving instance Linear.Applicative Renderer
deriving instance Linear.Monad Renderer

instance Linear.MonadIO Renderer where
  liftIO io = Renderer (StateT (\s -> (,s) <$> io))

renderer :: (RendererEnv %1 -> System.IO.Linear.IO (a, RendererEnv)) %1 -> Renderer a
renderer f = Renderer (StateT f)

