{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Ghengin.VulkanEngine.FrameBuffer where

import qualified Vulkan as Vk

createFrameBuffer :: Vk.Device -> Vk.RenderPass -> Vk.Extent2D -> Vk.ImageView -> IO Vk.Framebuffer
createFrameBuffer dev rp swpExt imv = do
  let
    frameBufferInfo = Vk.FramebufferCreateInfo { next = ()
                                               , flags = Vk.FramebufferCreateFlagBits 0
                                               , renderPass = rp
                                               , attachments = [imv]
                                               , width = swpExt.width
                                               , height = swpExt.height
                                               , layers = 1
                                               }
  Vk.createFramebuffer dev frameBufferInfo Nothing

destroyFrameBuffer :: Vk.Device -> Vk.Framebuffer -> IO ()
destroyFrameBuffer dev fb = Vk.destroyFramebuffer dev fb Nothing
