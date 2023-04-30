{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LinearTypes, UnicodeSyntax, QualifiedDo, NoImplicitPrelude #-}
module Ghengin.Vulkan.Renderer.Frame where

import Prelude.Linear
import Control.Functor.Linear as Linear
import Control.Monad.IO.Class.Linear
import qualified Vulkan as Vk
import qualified Unsafe.Linear as Unsafe

import Ghengin.Vulkan.Renderer.Synchronization
import Ghengin.Vulkan.Renderer.Device

data VulkanFrameData = VulkanFrameData { _renderFence      :: Vk.Fence
                                       , _renderSemaphore  :: Vk.Semaphore
                                       , _presentSemaphore :: Vk.Semaphore
                                       , _commandBuffer    :: Vk.CommandBuffer
                                       }


initVulkanFrameData :: MonadIO m => VulkanDevice -> Vk.CommandBuffer -> m (VulkanFrameData, VulkanDevice)
initVulkanFrameData dev buf = Linear.do
  (inFlightFence    , dev) <- createFence dev True
  (imageAvailableSem, dev) <- createSemaphore dev
  (renderFinishedSem, dev) <- createSemaphore dev
  pure (VulkanFrameData inFlightFence imageAvailableSem renderFinishedSem buf, dev)

destroyVulkanFrameData :: MonadIO m => VulkanDevice ⊸ VulkanFrameData ⊸ m VulkanDevice
destroyVulkanFrameData dev (VulkanFrameData f s1 s2 buf) = Linear.do
  dev <- destroyFence dev f
  dev <- destroySem   dev s1
  dev <- destroySem   dev s2
  Unsafe.toLinear (\_ -> pure dev) buf

