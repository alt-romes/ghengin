{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Ghengin.VulkanEngine.RenderPass where

import Vulkan.Zero (zero)
import qualified Vulkan as Vk

createRenderPass :: Vk.Device -> Vk.Format -> IO Vk.RenderPass
createRenderPass dev swapChainImageFormat = do

  let
      colorAttachment = Vk.AttachmentDescription {..} where
                            flags   = Vk.AttachmentDescriptionFlagBits 0
                            format  = swapChainImageFormat
                            samples = Vk.SAMPLE_COUNT_1_BIT
                            loadOp  = Vk.ATTACHMENT_LOAD_OP_CLEAR
                            storeOp = Vk.ATTACHMENT_STORE_OP_STORE
                            stencilLoadOp  = Vk.ATTACHMENT_LOAD_OP_DONT_CARE
                            stencilStoreOp = Vk.ATTACHMENT_STORE_OP_DONT_CARE
                            initialLayout  = Vk.IMAGE_LAYOUT_UNDEFINED
                            finalLayout    = Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR


      colorAttachmentRef = Vk.AttachmentReference { attachment = 0, layout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL }

      subpass = Vk.SubpassDescription { pipelineBindPoint = Vk.PIPELINE_BIND_POINT_GRAPHICS
                                      , colorAttachments  = [colorAttachmentRef]
                                      , inputAttachments  = []
                                      , resolveAttachments = []
                                      , preserveAttachments = []
                                      , depthStencilAttachment = Nothing
                                      , flags = Vk.SubpassDescriptionFlagBits 0
                                      }
      colorAttachmentDep = Vk.SubpassDependency { srcSubpass = Vk.SUBPASS_EXTERNAL
                                                , dstSubpass = 0
                                                , srcStageMask = Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
                                                , dstStageMask = Vk.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
                                                , srcAccessMask = Vk.AccessFlagBits 0
                                                , dstAccessMask = Vk.ACCESS_COLOR_ATTACHMENT_WRITE_BIT
                                                , dependencyFlags = zero
                                                }

      renderPassInfo = Vk.RenderPassCreateInfo { attachments = [colorAttachment]
                                               , subpasses   = [subpass]
                                               , dependencies = [colorAttachmentDep]
                                               , flags = Vk.RenderPassCreateFlagBits 0
                                               , next = ()
                                               }

  Vk.createRenderPass dev renderPassInfo Nothing

destroyRenderPass :: Vk.Device -> Vk.RenderPass -> IO ()
destroyRenderPass d rp = Vk.destroyRenderPass d rp Nothing
