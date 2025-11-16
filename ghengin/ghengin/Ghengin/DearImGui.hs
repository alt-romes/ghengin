{-# LANGUAGE CPP #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
module Ghengin.DearImGui
  ( module Ghengin.DearImGui
  , IM.vulkanNewFrame
  , IM.glfwNewFrame
  , module DearImGui
  ) where

-- #define IMGUI_DEBUG

import qualified Prelude
import Prelude.Linear
import Control.Functor.Linear as Linear
import Control.Monad.IO.Class.Linear
import qualified Unsafe.Linear as Unsafe

import Foreign
import qualified Control.Monad as Base
import qualified Control.Monad.IO.Class as Base

import qualified Vulkan.Zero as Vk
import qualified Vulkan as Vk
import DearImGui
import qualified DearImGui as IM
import qualified DearImGui.Vulkan as IM
import qualified DearImGui.GLFW   as IM
import qualified DearImGui.GLFW.Vulkan as IM

import Ghengin.Vulkan.Renderer.Command
import Ghengin.Vulkan.Renderer.GLFW.Window
import Ghengin.Vulkan.Renderer.SwapChain
import Ghengin.Vulkan.Renderer.Device
import Ghengin.Vulkan.Renderer.Kernel
import {-# SOURCE #-} Ghengin (Ghengin)

import Ghengin.Component.UI

data ImCtx = IMCtx Vk.DescriptorPool IM.Context (FunPtr (Vk.Result -> IO ()), Bool)

-- | Init ImGui (for some renderpass?)
initImGui :: Vk.RenderPass ⊸ Renderer (ImCtx, Vk.RenderPass)
initImGui = Unsafe.toLinear $ \renderPass' -> Linear.do
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
                                             , maxSets = fromIntegral $ 1000*Prelude.length poolSizes
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
                             }
  
  initRes <- liftSystemIO $ IM.vulkanInit initInfo renderPass'

  immediateSubmit $ withCmdBuffer (\x -> liftSystemIO $ () Prelude.<$ IM.vulkanCreateFontsTexture x)

  liftSystemIO $ IM.vulkanDestroyFontUploadObjects

  pure (IMCtx imGuiDPool imCtx initRes, renderPass')

destroyImCtx :: ImCtx ⊸ Renderer ()
destroyImCtx = Unsafe.toLinear $ \(IMCtx pool imCtx initRes) -> Linear.do
  liftSystemIO $ IM.vulkanShutdown initRes
  liftSystemIO $ IM.destroyContext imCtx
  unsafeUseDevice (\device -> Vk.destroyDescriptorPool device pool Nothing)


renderDrawData :: MonadIO m => IM.DrawData %p -> RenderPassCmd m
renderDrawData = Unsafe.toLinear $ \dd -> makeRenderPassCmd $ \b ->
  liftSystemIO $ IM.vulkanRenderDrawData dd b Nothing -- this Maybe Pipeline might serve for vertex processing on top of imgui


-- | Returns a list of booleans indicating whether each component was changed
-- in the previous frame
pushWindow :: UIWindow w -> Ghengin w ()
pushWindow (UIWindow wname act) = do
  beginnt <- Base.liftIO $ IM.begin wname
  if beginnt then do
    act
    Base.liftIO IM.end
  else do
    -- Optimization, if window is closed we end the the window and avoid
    -- drawing the ui inside it.
    Base.liftIO IM.end


