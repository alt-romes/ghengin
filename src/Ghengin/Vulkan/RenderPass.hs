{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Ghengin.Vulkan.RenderPass where

-- TODO: DSL

import Data.Bits
import Control.Monad.Reader
import Data.Vector (Vector)
import qualified Data.Vector as V

import Vulkan.Zero (zero)
import qualified Vulkan as Vk

import Ghengin.Vulkan.Image
import Ghengin.Vulkan.Device
import Ghengin.Vulkan.SwapChain
import Ghengin.Vulkan

data VulkanRenderPass = VulkanRenderPass { _renderPass :: Vk.RenderPass
                                         -- | We bundle framebuffer with the 
                                         -- RenderPass because in rendering we have 
                                         -- a fixed SwapChain so the Framebuffer is
                                         -- differentiated just from the rendering pass.
                                         -- That means that we have to create a framebuffer for all of the images in the swap chain and use the one that corresponds to the retrieved image at drawing time.
                                         , _framebuffers :: Vector Vk.Framebuffer
                                         }

withSimpleRenderPass :: (VulkanRenderPass -> Renderer a) -> Renderer a
withSimpleRenderPass f = rendererBracket createSimpleRenderPass destroyRenderPass f

createSimpleRenderPass :: Renderer VulkanRenderPass
createSimpleRenderPass = ask >>= \renv -> do

  let
      colorAttachment = Vk.AttachmentDescription {..} where
                            flags   = Vk.AttachmentDescriptionFlagBits 0
                            format  = renv._vulkanSwapChain._surfaceFormat.format
                            samples = Vk.SAMPLE_COUNT_1_BIT
                            loadOp  = Vk.ATTACHMENT_LOAD_OP_CLEAR
                            storeOp = Vk.ATTACHMENT_STORE_OP_STORE
                            stencilLoadOp  = Vk.ATTACHMENT_LOAD_OP_DONT_CARE
                            stencilStoreOp = Vk.ATTACHMENT_STORE_OP_DONT_CARE
                            initialLayout  = Vk.IMAGE_LAYOUT_UNDEFINED
                            finalLayout    = Vk.IMAGE_LAYOUT_PRESENT_SRC_KHR

      colorAttachmentRef = Vk.AttachmentReference { attachment = 0, layout = Vk.IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL }

      depthAttachment = Vk.AttachmentDescription {..} where
                            flags   = Vk.AttachmentDescriptionFlagBits 0
                            format  = Vk.FORMAT_D32_SFLOAT -- We could have a query like findDepthFormat()
                            samples = Vk.SAMPLE_COUNT_1_BIT
                            loadOp  = Vk.ATTACHMENT_LOAD_OP_CLEAR
                            storeOp = Vk.ATTACHMENT_STORE_OP_DONT_CARE
                            stencilLoadOp  = Vk.ATTACHMENT_LOAD_OP_DONT_CARE
                            stencilStoreOp = Vk.ATTACHMENT_STORE_OP_DONT_CARE
                            initialLayout  = Vk.IMAGE_LAYOUT_UNDEFINED
                            finalLayout    = Vk.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL

      depthAttachmentRef = Vk.AttachmentReference { attachment = 1, layout = Vk.IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL }

      subpass = Vk.SubpassDescription { pipelineBindPoint = Vk.PIPELINE_BIND_POINT_GRAPHICS
                                      , colorAttachments  = [colorAttachmentRef]
                                      , inputAttachments  = []
                                      , resolveAttachments = []
                                      , preserveAttachments = []
                                      , depthStencilAttachment = Just depthAttachmentRef
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

      depthAttachmentDep = Vk.SubpassDependency { srcSubpass = Vk.SUBPASS_EXTERNAL
                                                , dstSubpass = 0
                                                , srcStageMask = Vk.PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT .|. Vk.PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT
                                                , dstStageMask = Vk.PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT .|. Vk.PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT
                                                , srcAccessMask = Vk.AccessFlagBits 0
                                                , dstAccessMask = Vk.ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
                                                , dependencyFlags = zero
                                                }

      renderPassInfo = Vk.RenderPassCreateInfo { attachments = [colorAttachment, depthAttachment]
                                               , subpasses   = [subpass]
                                               , dependencies = [colorAttachmentDep, depthAttachmentDep]
                                               , flags = Vk.RenderPassCreateFlagBits 0
                                               , next = ()
                                               }

  renderPass <- Vk.createRenderPass renv._vulkanDevice._device renderPassInfo Nothing
  framebuffers <- V.mapM (createFramebuffer renderPass renv._vulkanSwapChain._depthImage._imageView) renv._vulkanSwapChain._imageViews

  pure $ VulkanRenderPass renderPass framebuffers

destroyRenderPass :: VulkanRenderPass -> Renderer ()
destroyRenderPass (VulkanRenderPass rp framebuffers) =
  getDevice >>= \d -> do
    V.mapM_ (destroyFramebuffer d) framebuffers
    Vk.destroyRenderPass d rp Nothing


createFramebuffer :: Vk.RenderPass -> Vk.ImageView -> Vk.ImageView -> Renderer Vk.Framebuffer
createFramebuffer rp depthImageView imageView = ask >>= \renv -> do
  let
    frameBufferInfo = Vk.FramebufferCreateInfo { next = ()
                                               , flags = Vk.FramebufferCreateFlagBits 0
                                               , renderPass = rp
                                               , attachments = [imageView, depthImageView]
                                               , width  = renv._vulkanSwapChain._surfaceExtent.width
                                               , height = renv._vulkanSwapChain._surfaceExtent.height
                                               , layers = 1
                                               }
  Vk.createFramebuffer renv._vulkanDevice._device frameBufferInfo Nothing

destroyFramebuffer :: MonadIO m => Vk.Device -> Vk.Framebuffer -> m ()
destroyFramebuffer dev fb = Vk.destroyFramebuffer dev fb Nothing

