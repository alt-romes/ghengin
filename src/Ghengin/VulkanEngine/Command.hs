{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
module Ghengin.VulkanEngine.Command where

import qualified Data.Vector as V
import qualified Vulkan.Zero as Vk
import qualified Vulkan as Vk

import Ghengin.VulkanEngine.QueueFamilies

createCommandPool :: Vk.Device -> QueueFamiliesIndices -> IO Vk.CommandPool
createCommandPool device qfi = do

  let
    poolInfo = Vk.CommandPoolCreateInfo { flags = Vk.COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
                                        , queueFamilyIndex = qfi._graphicsFamily
                                        }

  Vk.createCommandPool device poolInfo Nothing


destroyCommandPool :: Vk.Device -> Vk.CommandPool -> IO ()
destroyCommandPool dev pool = Vk.destroyCommandPool dev pool Nothing

createCommandBuffers :: Vk.Device -> Vk.CommandPool -> IO (V.Vector Vk.CommandBuffer)
createCommandBuffers dev cpool = do
  let
    allocInfo = Vk.CommandBufferAllocateInfo { commandPool = cpool
                                             , level = Vk.COMMAND_BUFFER_LEVEL_PRIMARY
                                             , commandBufferCount = 1
                                             }
  Vk.allocateCommandBuffers dev allocInfo


-- | Record to a command buffer commands we want to execute on a given Image which is associated with a framebuffer?
recordCommandBuffer :: Vk.Extent2D -> (V.Vector Vk.Framebuffer) -> Vk.RenderPass -> Vk.Pipeline -> Vk.CommandBuffer -> Int -> IO ()
recordCommandBuffer swapChainExtent swapChainFramebuffers rpass graphicsPipeline commandBuffer imageIndex = do
  let
    beginInfo = Vk.CommandBufferBeginInfo { next = ()
                                          , flags = Vk.zero
                                          , inheritanceInfo = Nothing
                                          }

  Vk.beginCommandBuffer commandBuffer beginInfo

  let
    renderPassInfo = Vk.RenderPassBeginInfo { next = ()
                                            , renderPass = rpass
                                            , framebuffer = swapChainFramebuffers V.! imageIndex -- We created a framebuffer for each swap chain image where it is specified as a color attachment; Thus we need to bind the framebuffer for the swapchain image we want to draw to. Using the imageIndex parameter which was passed in, we can pick the right framebuffer for the current swapchain image
                                            , renderArea  = Vk.Rect2D (Vk.Offset2D 0 0) swapChainExtent
                                            , clearValues = [Vk.Color $ Vk.Float32 1 1 1 1]
                                            }


  -- First recorded command onto the command buffer
  Vk.cmdBeginRenderPass commandBuffer renderPassInfo Vk.SUBPASS_CONTENTS_INLINE

  Vk.cmdBindPipeline commandBuffer Vk.PIPELINE_BIND_POINT_GRAPHICS graphicsPipeline

  let
    -- The region of the framebuffer that the output will be rendered to. We
    -- render from (0,0) to (width, height) i.e. the whole framebuffer
    -- Defines a transformation from image to framebuffer
    viewport = Vk.Viewport {..} where
                 x = 0.0
                 y = 0.0
                 width  = fromIntegral $ swapChainExtent.width
                 height = fromIntegral $ swapChainExtent.height
                 minDepth = 0
                 maxDepth = 1

    -- Defines the region in which pixels will actually be stored. Any pixels
    -- outside of the scissor will be discarded. We keep it as the whole viewport
    scissor = Vk.Rect2D (Vk.Offset2D 0 0) swapChainExtent

  -- Specify the states we set to dynamic
  Vk.cmdSetViewport commandBuffer 0 [viewport]
  Vk.cmdSetScissor  commandBuffer 0 [scissor]

  -- Record draw command
  Vk.cmdDraw commandBuffer 3 1 0 0

  -- Record end render pass cmd
  Vk.cmdEndRenderPass commandBuffer

  -- Finish recording
  Vk.endCommandBuffer commandBuffer


