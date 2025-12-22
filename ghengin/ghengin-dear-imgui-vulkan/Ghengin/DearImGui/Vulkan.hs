{-# LANGUAGE CPP #-}
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
import Ghengin.Vulkan.Renderer.GLFW.Window
import Ghengin.Vulkan.Renderer.SwapChain
import Ghengin.Vulkan.Renderer.Device
import Ghengin.Vulkan.Renderer.Kernel
import Ghengin.Vulkan.Renderer.RenderPass

data ImCtx = IMCtx Vk.DescriptorPool IM.Context (FunPtr (Vk.Result -> IO ()), Bool)

-- | Init ImGui (for some renderpass?)
initImGui :: RenderPass %1 -> Renderer (RenderPass, ImCtx)
initImGui = Unsafe.toLinear \rp -> Linear.do -- rp is only used for the initialization
  -- Quite big descriptors but is taken from example
  let poolSizes = [ Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_SAMPLER 1000
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

      poolInfo = Vk.DescriptorPoolCreateInfo { flags = Vk.DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
                                             , maxSets = 1000
                                             , poolSizes = poolSizes
                                             , next = ()
                                             }

  -- Create descriptor pool
  Ur imGuiDPool <- unsafeUseDevice (\device -> Ur Prelude.<$> Vk.createDescriptorPool device poolInfo Nothing)

  -- Setup imgui context
  imCtx <- liftSystemIO IM.createContext

  -- Setup platform/renderer backends (glfw+vulkan)
  Ur renv <- renderer $ Unsafe.toLinear $ \renv -> pure (Ur renv, renv)
  Ur _booj <- liftSystemIOU $ IM.glfwInitForVulkan renv._vulkanWindow._window True
  let initInfo = IM.InitInfo { instance' = renv._instance
                             , physicalDevice = renv._vulkanDevice._physicalDevice
                             , device = renv._vulkanDevice._device
                             , queueFamily = renv._vulkanDevice._graphicsQueueFamily
                             , queue = renv._vulkanDevice._graphicsQueue
                             , pipelineCache = Vk.zero
                             , descriptorPool = imGuiDPool
                             , subpass = 0
                             , minImageCount = fromIntegral $ Prelude.length renv._vulkanSwapChain._imageViews
                             , imageCount    = fromIntegral $ Prelude.length renv._vulkanSwapChain._imageViews
                             , msaaSamples = Vk.SAMPLE_COUNT_1_BIT
                             , mbAllocator = Nothing
                             , checkResult = \x -> Base.when (x Prelude./= Vk.SUCCESS) (Base.fail $ show x)
                             , rendering = Left rp._renderPass
                             }
  
  initRes <- liftSystemIO $ IM.vulkanInit initInfo

  Ur _ok  <- liftSystemIOU $ IM.vulkanCreateFontsTexture

  pure (rp, IMCtx imGuiDPool imCtx initRes)

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
renderDrawData :: MonadIO m => RenderPassCmd m
renderDrawData = unsafeRenderPassCmd_ $ \b -> do
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

