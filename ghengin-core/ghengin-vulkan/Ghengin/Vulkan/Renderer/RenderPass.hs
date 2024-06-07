{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Ghengin.Vulkan.Renderer.RenderPass where

-- TODO: DSL

import Ghengin.Core.Log
import Data.Proxy
import GHC.TypeNats

import Prelude.Linear hiding (zero)
import qualified Data.Functor.Linear as Data.Linear
import Control.Functor.Linear as Linear
import Control.Monad.IO.Class.Linear as Linear
import Data.Bifunctor.Linear

import Data.Bits
import qualified Data.Vector as Vector
import qualified Data.V.Linear as V
import qualified Data.V.Linear.Internal as VI

import Vulkan.Zero (zero)
import qualified Vulkan as Vk

import Ghengin.Vulkan.Renderer.Image
import Ghengin.Vulkan.Renderer.Device
import Ghengin.Vulkan.Renderer.SwapChain
import Ghengin.Vulkan.Renderer.Kernel

import qualified Unsafe.Linear as Unsafe

import Data.Linear.Alias

data RenderPass = VulkanRenderPass { _renderPass :: Vk.RenderPass
                                   -- | We bundle framebuffer with the 
                                   -- RenderPass because in rendering we have 
                                   -- a fixed SwapChain so the Framebuffer is
                                   -- differentiated just from the rendering pass.
                                   -- That means that we have to create a
                                   -- framebuffer for all of the images
                                   -- in the swap chain and use the one
                                   -- that corresponds to the retrieved
                                   -- image at drawing time.
                                   , _framebuffers :: Vector.Vector Vk.Framebuffer
                                   }

-- withSimpleRenderPass :: (RenderPass -> Renderer a) -> Renderer a
-- withSimpleRenderPass f = rendererBracket createSimpleRenderPass destroyRenderPass f

createSimpleRenderPass :: Renderer RenderPass
createSimpleRenderPass = enterD "createSimpleRenderPass" $ Linear.do
  logT "Creating render pass"
  renderPass <- renderer $ Unsafe.toLinear $ \renv -> Linear.do

    let colorAttachment = Vk.AttachmentDescription {..} where
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

    (,renv) <$> liftSystemIO (Vk.createRenderPass renv._vulkanDevice._device renderPassInfo Nothing)

  Ur unsafeRenv <- renderer $ Unsafe.toLinear $ \renv -> pure (Ur renv, renv)

  logT "Creating framebuffers"
  case someNatVal (fromIntegral $ Vector.length unsafeRenv._vulkanSwapChain._imageViews) of
    SomeNat (Proxy @n) -> Linear.do
      ((VI.V framebuffers, imageViews'), (renderPass', depthImage'))
          <- createFramebuffers renderPass unsafeRenv._vulkanSwapChain._depthImage._imageView
                                (VI.V @n unsafeRenv._vulkanSwapChain._imageViews)

      Unsafe.toLinear2 (\_ _ -> pure ()) imageViews' depthImage' -- forget, their part of the unsafeRenv which wasn't changed
      pure $ (VulkanRenderPass renderPass' framebuffers)

destroyRenderPass :: RenderPass ⊸ Renderer ()
destroyRenderPass = Unsafe.toLinear $ \(VulkanRenderPass rp framebuffers) -> enterD "destroyRenderPass" $ Linear.do
  Ur dev <- unsafeGetDevice
  dev'   <- case someNatVal (fromIntegral $ Vector.length framebuffers) of
              SomeNat (Proxy @n) -> destroyFramebuffers dev (VI.V @n framebuffers)
  Unsafe.toLinear (\dev'' -> liftSystemIO $ Vk.destroyRenderPass dev'' rp Nothing) dev'

-- | Create multiple frame buffers (that share the depth image image view?) from multiple imageViews?
createFramebuffers :: KnownNat n => Vk.RenderPass ⊸ Vk.ImageView ⊸ V.V n Vk.ImageView ⊸ Renderer ((V.V n Vk.Framebuffer, V.V n Vk.ImageView), (Vk.RenderPass, Vk.ImageView))
createFramebuffers rp depthImgV imgVs = runStateT ((Unsafe.toLinear \case (VI.V fbsimgs) -> bimap VI.V VI.V (Vector.unzip fbsimgs)) <$> Data.Linear.traverse createFramebufferState imgVs) (rp, depthImgV)
  where
    -- Hope this threading doesn't impact performance too much. TODO: Benchmarks....
    createFramebufferState :: Vk.ImageView ⊸ StateT (Vk.RenderPass, Vk.ImageView) Renderer (Vk.Framebuffer, Vk.ImageView)
    createFramebufferState imageView = StateT (\(rp', depthImgV') -> createFramebuffer rp' depthImgV' imageView >>= \case (fb, (rp'',dimgv',imgv')) -> pure ((fb, imgv'), (rp'', dimgv')))
-- ROMES:force inline everywhere with this stateT?

createFramebuffer :: Vk.RenderPass ⊸ Vk.ImageView ⊸ Vk.ImageView ⊸ Renderer (Vk.Framebuffer, (Vk.RenderPass, Vk.ImageView, Vk.ImageView))
createFramebuffer = Unsafe.toLinear3 $ \rp depthImageView imageView -> Linear.do
  Ur frameBufferInfo <- renderer $ Unsafe.toLinear $ \renv -> pure
    (Ur Vk.FramebufferCreateInfo { next = ()
                              , flags = Vk.FramebufferCreateFlagBits 0
                              , renderPass = rp
                              , attachments = [imageView, depthImageView]
                              , width  = renv._vulkanSwapChain._surfaceExtent.width
                              , height = renv._vulkanSwapChain._surfaceExtent.height
                              , layers = 1
                              }, renv)
  (, (rp, depthImageView, imageView)) <$> unsafeUseDevice (\dev -> Vk.createFramebuffer dev frameBufferInfo Nothing)

destroyFramebuffers :: MonadIO m => KnownNat n => Vk.Device ⊸ V.V n Vk.Framebuffer ⊸ m Vk.Device
destroyFramebuffers dev fb = Linear.do
  (units, dev')<- runStateT (Data.Linear.traverse destroyFramebufferState fb) dev
  pure $ consumeUnits units
  pure dev'
  where
    destroyFramebufferState :: MonadIO m => Vk.Framebuffer ⊸ StateT Vk.Device m ()
    destroyFramebufferState f = StateT (fmap ((),) . destroyFramebuffer f)

destroyFramebuffer :: MonadIO m => Vk.Framebuffer ⊸ Vk.Device ⊸ m Vk.Device
destroyFramebuffer = Unsafe.toLinear2 $ \fb dev -> dev <$ liftSystemIO (Vk.destroyFramebuffer dev fb Nothing)

