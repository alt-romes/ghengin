{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
module Ghengin.Vulkan.Renderer.Kernel where

import Control.Functor.Linear as Linear
import Control.Monad.IO.Class.Linear as Linear
import Ghengin.Core.Prelude
import Ghengin.Core.Log
import Ghengin.Vulkan.Renderer.Context
import qualified Prelude
import qualified Vulkan as Vk
import qualified Data.Linear.Alias as Alias

type Alias = Alias.Alias Renderer

type Renderer :: Type -> Type
type role Renderer nominal
data Renderer a

instance Functor Renderer
instance Applicative Renderer
instance Monad Renderer
instance MonadIO Renderer
instance HasLogger Renderer

copyBuffer :: Vk.Buffer ⊸ Vk.Buffer ⊸ Vk.DeviceSize -> Renderer (Vk.Buffer, Vk.Buffer)
unsafeWithVulkanContext :: forall b. (VulkanContext WithSwapchain -> Prelude.IO b) -> Renderer b
unsafeUseDevice :: forall b. (Vk.Device -> Prelude.IO b) -> Renderer b
