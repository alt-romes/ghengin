module Ghengin.Vulkan.Frame where

import Control.Monad.IO.Class
import qualified Vulkan as Vk

import Ghengin.Vulkan.Synchronization

data FrameData = FrameData { _renderFence      :: Vk.Fence
                           , _renderSemaphore  :: Vk.Semaphore
                           , _presentSemaphore :: Vk.Semaphore
                           , _commandBuffer    :: Vk.CommandBuffer
                           }


initFrameData :: MonadIO m => Vk.Device -> Vk.CommandBuffer -> m FrameData
initFrameData dev buf = do
  inFlightFence     <- createFence dev True
  imageAvailableSem <- createSemaphore dev
  renderFinishedSem <- createSemaphore dev
  pure (FrameData inFlightFence imageAvailableSem renderFinishedSem buf)

destroyFrameData :: MonadIO m => Vk.Device -> FrameData -> m ()
destroyFrameData dev (FrameData f s1 s2 _) = do
  destroyFence dev f
  destroySem   dev s1
  destroySem   dev s2

