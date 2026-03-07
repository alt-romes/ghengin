{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
module Ghengin.DearImGui.Vulkan
  ( module Ghengin.DearImGui.Vulkan
  -- * Re-export all the functions needed to construct an immediate-mode GUI.
  -- Use them with 'imguiCmd'.
  , module DearImGui
  ) where

import qualified Prelude
import Data.Proxy
import GHC.TypeLits
import Prelude.Linear
import Control.Functor.Linear as Linear
import Control.Monad.IO.Class.Linear
import qualified Unsafe.Linear as Unsafe

import Foreign
import qualified Control.Monad as Base

import qualified Vulkan.Zero as Vk
import qualified Vulkan as Vk

import DearImGui -- for re-exporting im UI
import qualified DearImGui as IM
import qualified DearImGui.Vulkan as IM
import qualified DearImGui.GLFW   as IM
import qualified DearImGui.GLFW.Vulkan as IM

import Ghengin.Vulkan.Renderer.Command
import Ghengin.Vulkan.Renderer.Context
import Ghengin.Vulkan.Renderer.Context.Swapchain
import Ghengin.Vulkan.Renderer.Kernel

data ImCtx = IMCtx Vk.DescriptorPool IM.Context (FunPtr (Vk.Result -> IO ()), Bool)

-- | Init ImGui (for some renderpass?)
initImGui :: Renderer ImCtx
initImGui =
  withVulkanContext $ Unsafe.toLinear \vkCtx -> liftSystemIO $ do
    imCtx <- withSwapchainInfo vkCtx.aSwapchainInfo (kont vkCtx)
    Prelude.pure (imCtx, vkCtx)
      where
        -- Quite big descriptors but is taken from example
        poolSizes =
          [ Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_SAMPLER 1000
          , Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER 1000
          , Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_SAMPLED_IMAGE 1000
          , Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE 1000
          , Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER 1000
          , Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER 1000
          , Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER 1000
          , Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER 1000
          , Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC 1000
          , Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC 1000
          , Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_INPUT_ATTACHMENT 1000
          ]

        poolInfo = Vk.DescriptorPoolCreateInfo
          { flags = Vk.DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
          , maxSets = 1000
          , poolSizes = poolSizes
          , next = ()
          }

        kont :: ∀ swpImgs. KnownNat swpImgs => VulkanContext WithSwapchain %1 -> SwapchainInfo swpImgs %1 -> IO ImCtx
        kont = Unsafe.toLinear2 \VulkanContext{..} _ -> do

          -- Create descriptor pool
          imGuiDPool <- Vk.createDescriptorPool device poolInfo Nothing

          -- Setup imgui context
          imCtx <- IM.createContext

          -- Setup platform/renderer backends (glfw+vulkan)
          _ <- IM.glfwInitForVulkan (case window of ContextWindow w -> w) True
          let initInfo = IM.InitInfo
                { instance' = vkInstance
                , physicalDevice = physicalDevice
                , device = device
                , queueFamily = fromIntegral queueFamilyIndex
                , queue = queue
                , pipelineCache = Vk.zero
                , descriptorPool = imGuiDPool
                , subpass = 0
                , minImageCount = fromIntegral $ natVal (Proxy @swpImgs)
                , imageCount    = fromIntegral $ natVal (Proxy @swpImgs)
                , msaaSamples = Vk.SAMPLE_COUNT_1_BIT
                , mbAllocator = Nothing
                , checkResult = \x -> Base.when (x Prelude./= Vk.SUCCESS) (Base.fail $ show x)

                , rendering = Right Vk.PipelineRenderingCreateInfo
                    { colorAttachmentFormats = [ Vk.FORMAT_B8G8R8A8_UNORM ]
                    , depthAttachmentFormat = Vk.zero
                    , stencilAttachmentFormat = Vk.zero
                    , viewMask = Vk.zero
                    }
                }

          initRes <- IM.vulkanInit initInfo

          _ok     <- IM.vulkanCreateFontsTexture

          Prelude.pure (IMCtx imGuiDPool imCtx initRes)

-- | Shutdown ImGui
destroyImCtx :: ImCtx ⊸ Renderer ()
destroyImCtx = Unsafe.toLinear $ \(IMCtx pool imCtx initRes) -> Linear.do
  liftSystemIO $ IM.vulkanShutdown initRes
  liftSystemIO $ IM.glfwShutdown
  liftSystemIO $ IM.destroyContext imCtx
  unsafeUseDevice (\device -> Vk.destroyDescriptorPool device pool Nothing)

-- | Register the start of a new frame and write here your immediate mode GUI
-- by using the re-exported functions. Prepares the ImGui structures at the end
-- by calling @ImGui::Render()@.
--
-- == Example
--
-- @
-- import qualified Ghengin.DearImGui.Vulkan as ImGui
-- ...
--
-- loop = ...
--   (pollWindowEvents ↑)
--
--   ImGui.withNewFrame $ do
--    ImGui.showDemoWindow
--
--   (rp, rq) <- renderWith $ Linear.do
--      ...
--      renderPassCmd ... $ Linear.do
--
--        renderQueueCmd rq
--
--        ImGui.renderDrawData
--   loop
-- @
withNewFrame :: MonadIO n => Prelude.IO () -> n ()
withNewFrame do_it = Linear.do
  registerNewFrame
  liftSystemIO do_it
  imguiRender

-- | This needs to be called as part of a custom render pass to draw the data
-- prepared by 'imguiRender'. See 'Ghengin.Core.renderWith'. An example usage
-- is in @examples/dear-imgui@.
renderDrawData :: MonadIO m => RenderCmd m
renderDrawData = unsafeRenderCmd_ $ \b -> do
  dd <- IM.getDrawData
  IM.vulkanRenderDrawData dd b Nothing -- this Maybe Pipeline might serve for vertex processing on top of imgui

--------------------------------------------------------------------------------
-- * Internals that may eventually be useful
--------------------------------------------------------------------------------

-- | Register the start of a new frame
registerNewFrame :: MonadIO m => m ()
registerNewFrame = Linear.do
  liftSystemIO IM.glfwNewFrame
  liftSystemIO IM.vulkanNewFrame
  liftSystemIO IM.newFrame

-- | Make dear-imgui calculate the draw structures that will be rendered on the
-- 'renderDrawData' render pass command. This will use all the immediate mode
-- commands you used before. (i.e. @ImGui::Render()@)
imguiRender :: MonadIO m => m ()
imguiRender = liftSystemIO IM.render

