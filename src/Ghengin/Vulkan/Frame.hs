module Ghengin.Vulkan.Frame where

import Control.Monad.IO.Class
import qualified Vulkan as Vk

import Ghengin.Vulkan.Renderer.Synchronization

data VulkanFrameData = VulkanFrameData { _renderFence      :: Vk.Fence
                                       , _renderSemaphore  :: Vk.Semaphore
                                       , _presentSemaphore :: Vk.Semaphore
                                       , _commandBuffer    :: Vk.CommandBuffer
                                       }


initVulkanFrameData :: MonadIO m => Vk.Device -> Vk.CommandBuffer -> m VulkanFrameData
initVulkanFrameData dev buf = do
  inFlightFence     <- createFence dev True
  imageAvailableSem <- createSemaphore dev
  renderFinishedSem <- createSemaphore dev
  pure (VulkanFrameData inFlightFence imageAvailableSem renderFinishedSem buf)

destroyVulkanFrameData :: MonadIO m => Vk.Device -> VulkanFrameData -> m ()
destroyVulkanFrameData dev (VulkanFrameData f s1 s2 _) = do
  destroyFence dev f
  destroySem   dev s1
  destroySem   dev s2

