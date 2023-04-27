{-# LANGUAGE NoImplicitPrelude, LinearTypes, PatternSynonyms, QualifiedDo,
   OverloadedRecordDot #-}
module Ghengin.Vulkan.Renderer.Kernel where

import qualified Prelude as Unrestricted
import Prelude.Linear
import qualified Control.Monad.IO.Class as Unrestricted
import qualified System.IO.Linear
import qualified Data.Functor.Linear as Data.Linear
import Control.Functor.Linear as Linear
import Control.Monad.IO.Class.Linear as Linear

import Ghengin.Vulkan.Renderer.Device
import Ghengin.Vulkan.Renderer.Command (Command)
import Ghengin.Vulkan.Renderer.ImmediateSubmit

-- One day abstract over Window API
import Ghengin.Vulkan.Renderer.GLFW.Window

import qualified Vulkan as Vk

import Data.Counted
import qualified Unsafe.Linear as Unsafe

type RefC = RefC' Renderer

data RendererEnv =
  REnv { _instance        :: !Vk.Instance
       , _vulkanDevice    :: !VulkanDevice
       , _vulkanWindow    :: !VulkanWindow
       -- , _vulkanSwapChain :: !VulkanSwapChain
       -- , _commandPool     :: !Vk.CommandPool
       -- , _frames          :: !(Vector VulkanFrameData)
       -- , _frameInFlight   :: !(IORef Int)
       , _immediateSubmit :: !ImmediateSubmitCtx
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

-- | Make a renderer computation from a linear IO action that linearly uses a
-- 'RendererEnv'
renderer :: (RendererEnv %1 -> System.IO.Linear.IO (a, RendererEnv)) %1 -> Renderer a
renderer f = Renderer (StateT f)

-- | Unsafely run a Vulkan action on a linear MonadIO that requires a
-- Vulkan.Device reference as a linear action on 'Renderer'.
-- This action assumes the Vk.Device reference is unchanged! If your action,
-- e.g. frees the reference, Bad Things Will Happen
--
-- Note, this is quite unsafe really, but makes usage of non-linear vulkan much easier
unsafeUseDevice :: (Vk.Device -> Unrestricted.IO b)
                   -> Renderer b
unsafeUseDevice f = renderer $ Unsafe.toLinear $ \renv@(REnv _inst device _win _isctx) -> Linear.do
  b <- liftSystemIO $ f (device._device)
  pure $ (b, renv)


-- | Submit a command to the immediate submit command buffer that synchronously
-- submits it to the graphics queue
immediateSubmit :: Command System.IO.Linear.IO -> Renderer () -- ROMES: Command IO, is that OK?
immediateSubmit cmd = renderer $ \(REnv inst dev win imsctx) -> Linear.do
  (dev', imsctx') <- immediateSubmit' dev imsctx cmd
  pure ((), REnv inst dev' win imsctx')
