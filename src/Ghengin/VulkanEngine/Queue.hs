{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Ghengin.VulkanEngine.Queue where

import Foreign.Ptr
import Data.Word
import Vulkan.CStruct.Extends qualified as Vk
import Vulkan qualified as Vk

import Ghengin.VulkanEngine.QueueFamilies

getDeviceQueue :: Vk.Device
               -> QueueFamily -- Queue family index
               -> Word32      -- Queue ix
               -> IO Vk.Queue
getDeviceQueue d qf i = do
  Vk.getDeviceQueue d qf i

submitQueue :: Vk.Queue -> Vk.CommandBuffer -> Vk.Semaphore -> Vk.Semaphore -> Vk.Fence -> IO ()
submitQueue q cb sem1 sem2 fence = do
  let
    submitInfo = Vk.SubmitInfo { next = ()
                                -- We want to wait with writing colors to the image until it's available
                               , waitSemaphores = [sem1]
                               , waitDstStageMask = [Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
                                -- Semaphores to signal when we are done
                               , signalSemaphores = [sem2]
                               , commandBuffers = [ cb.commandBufferHandle ]
                               }
  Vk.queueSubmit q [Vk.SomeStruct submitInfo] fence

queuePresent :: Vk.Queue -> Vk.Semaphore -> Vk.SwapchainKHR -> Int -> IO ()
queuePresent q sem swpc imageIndex = do
  let
    presentInfo = Vk.PresentInfoKHR { next = ()
                                    , waitSemaphores = [sem]
                                    , swapchains = [swpc]
                                    , imageIndices = [fromIntegral imageIndex]
                                    , results = nullPtr
                                    }
  _ <- Vk.queuePresentKHR q presentInfo
  pure ()
