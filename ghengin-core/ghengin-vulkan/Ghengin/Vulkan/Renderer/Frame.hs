{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LinearTypes, UnicodeSyntax, QualifiedDo, NoImplicitPrelude #-}
module Ghengin.Vulkan.Renderer.Frame where

import Prelude.Linear
import Control.Functor.Linear as Linear
import Control.Monad.IO.Class.Linear
import qualified Vulkan as Vk
import qualified Unsafe.Linear as Unsafe

import Ghengin.Vulkan.Renderer.Synchronization
import Ghengin.Vulkan.Renderer.Context

data VulkanFrameData = VulkanFrameData { _renderFence      :: Vk.Fence
                                       , _renderSemaphore  :: Vk.Semaphore
                                       , _presentSemaphore :: Vk.Semaphore
                                       , _commandBuffer    :: Vk.CommandBuffer
                                       }


initVulkanFrameData :: MonadIO m => Vk.CommandBuffer ⊸ VulkanContext ctx ⊸ m (VulkanFrameData, VulkanContext ctx)
initVulkanFrameData buf ctx = Linear.do
  (inFlightFence    , ctx) <- createFence ctx True
  (imageAvailableSem, ctx) <- createSemaphore ctx
  (renderFinishedSem, ctx) <- createSemaphore ctx
  pure (VulkanFrameData inFlightFence imageAvailableSem renderFinishedSem buf, ctx)

destroyVulkanFrameData :: MonadIO m => VulkanFrameData ⊸ VulkanContext ctx ⊸ m (VulkanContext ctx)
destroyVulkanFrameData (VulkanFrameData f s1 s2 buf) ctx = Linear.do
  ctx <- destroyFence ctx f
  ctx <- destroySem   ctx s1
  ctx <- destroySem   ctx s2
  Unsafe.toLinear (\_ -> pure ctx) buf

