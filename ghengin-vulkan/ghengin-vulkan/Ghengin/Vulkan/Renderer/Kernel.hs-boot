module Ghengin.Vulkan.Renderer.Kernel where

import Ghengin.Core.Prelude
import Ghengin.Core.Log
import Ghengin.Vulkan.Renderer.Device
import qualified Prelude
import qualified Vulkan as Vk
import qualified Data.Linear.Alias as Alias

type Alias = Alias.Alias Renderer
data Renderer a
instance Functor Renderer
instance Applicative Renderer
instance Monad Renderer
instance MonadIO Renderer
instance HasLogger Renderer

copyBuffer :: Vk.Buffer ⊸ Vk.Buffer ⊸ Vk.DeviceSize -> Renderer (Vk.Buffer, Vk.Buffer)
unsafeUseVulkanDevice :: (VulkanDevice -> Prelude.IO b) -> Renderer b
unsafeUseDevice :: (Vk.Device -> Prelude.IO b) -> Renderer b
