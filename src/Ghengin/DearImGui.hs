{-# LANGUAGE CPP #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
module Ghengin.DearImGui
  ( module Ghengin.DearImGui
  , IM.vulkanNewFrame
  , IM.glfwNewFrame
  , module DearImGui
  ) where

-- #define IMGUI_DEBUG

import Foreign
import Control.Monad.Reader

import qualified Vulkan.Zero as Vk
import qualified Vulkan as Vk
import DearImGui
import qualified DearImGui as IM
import qualified DearImGui.Vulkan as IM
import qualified DearImGui.GLFW   as IM
import qualified DearImGui.GLFW.Vulkan as IM

import Ghengin.Vulkan.Command
import Ghengin.Vulkan.GLFW.Window
import Ghengin.Vulkan.SwapChain
import Ghengin.Vulkan.Device
import Ghengin.Vulkan

import Ghengin.Component.UI

data ImCtx = IMCtx Vk.DescriptorPool IM.Context (FunPtr (Vk.Result -> IO ()), Bool)

-- | Init ImGui (for some renderpass?)
initImGui :: Vk.RenderPass -> Renderer ImCtx
initImGui renderPass' = do
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
                                             , maxSets = fromIntegral $ 1000*length poolSizes
                                             , poolSizes = poolSizes
                                             , next = ()
                                             }

  -- Create descriptor pool
  device <- getDevice
  imGuiDPool <- Vk.createDescriptorPool device poolInfo Nothing

  -- Setup imgui context
  imCtx <- IM.createContext

  -- Setup platform/renderer backends (glfw+vulkan)
  renv <- ask
  _booj <- IM.glfwInitForVulkan renv._vulkanWindow._window True
  let initInfo = IM.InitInfo { instance' = renv._instance
                             , physicalDevice = renv._vulkanDevice._physicalDevice
                             , device = renv._vulkanDevice._device
                             , queueFamily = renv._vulkanDevice._graphicsQueueFamily
                             , queue = renv._vulkanDevice._graphicsQueue
                             , pipelineCache = Vk.zero
                             , descriptorPool = imGuiDPool
                             , subpass = 0
                             , minImageCount = fromIntegral $ length renv._vulkanSwapChain._imageViews
                             , imageCount = fromIntegral $ length renv._vulkanSwapChain._imageViews
                             , msaaSamples = Vk.SAMPLE_COUNT_1_BIT
                             , mbAllocator = Nothing
                             , checkResult = \x -> when (x /= Vk.SUCCESS) (fail $ show x)
                             }
  
  initRes <- IM.vulkanInit initInfo renderPass'

  immediateSubmit $ withCmdBuffer ((() <$) . IM.vulkanCreateFontsTexture)

  IM.vulkanDestroyFontUploadObjects

  pure (IMCtx imGuiDPool imCtx initRes)

destroyImCtx :: ImCtx -> Renderer ()
destroyImCtx (IMCtx pool imCtx initRes) = do
  IM.vulkanShutdown initRes
  IM.destroyContext imCtx
  device <- getDevice
  Vk.destroyDescriptorPool device pool Nothing


renderDrawData :: IM.DrawData -> RenderPassCmd
renderDrawData dd = makeRenderPassCmd $ \b -> do
  IM.vulkanRenderDrawData dd b Nothing -- this Maybe Pipeline might serve for vertex processing on top of imgui


-- | Returns a list of booleans indicating whether each component was changed
-- in the previous frame
pushWindow :: UIWindow -> Renderer Bool
pushWindow (UIWindow wname act) = do
  beginnt <- IM.begin wname
  if beginnt then do
    b <- liftIO $ act

    IM.end
    pure b
  else do
    -- Optimization, if window is closed we end the the window and avoid
    -- drawing the ui inside it.
    IM.end
    pure False


-- | Returns a boolean indicating whether the component was changed in the previous frame
-- pushComp :: UIComponent -> Renderer Bool
-- pushComp = \case
--   ColorPicker t ref -> IM.colorPicker3 t (unsafeCoerce ref :: IORef ImVec3) -- Unsafe coerce Vec3 to ImVec3. They have the same representation. Right?

--     -- get ref >>= \case
--     --   WithVec3 x y z -> do
--     --     tmpRef <- liftIO $ newIORef (ImVec3 x y z)
--     --     b <- IM.colorPicker3 t tmpRef -- Unsafe coerce Vec3 to ImVec3. They have the same representation. Right?
--     --     ImVec3 x' y' z' <- get tmpRef
--     --     ref $= vec3 x' y' z'
--     --     pure b

--   SliderFloat t ref f1 f2 -> IM.sliderFloat t ref f1 f2
--   SliderVec3  t ref f1 f2 -> do
--     v <- get ref
--     withVec3 v $ \x y z -> do
--       tmpR <- liftIO $ newIORef (x,y,z)
--       b <- IM.sliderFloat3 t tmpR f1 f2
--       (x',y',z') <- get tmpR
--       ref $= vec3 x' y' z'
--       pure b
--   DragFloat   t ref f1 f2 -> trace "DragFloat is behaving weird..." $ IM.dragFloat t ref 0.05 f1 f2
--   SliderInt   t ref f1 f2 -> IM.sliderInt t ref f1 f2
--   Checkbox    t ref -> IM.checkbox t ref
--   WithTree    t cmops     -> do
--     b <- IM.treeNode t
--     if b then do
--       b' <- mapM pushComp cmops
--       IM.treePop
--       pure $ any id b'
--     else
--       pure False

--   WithCombo t ref (opt:|opts) -> do

--     b <- IM.beginCombo t (pack $ show opt)
--     if b then do

--       currIx <- liftIO $ newIORef 0
--       bs <- forM (zip (opt:opts) [0..]) $ \(o, n) -> do
--               currIx' <- get currIx
--               let is_selected = currIx' == n
--               b' <- selectableWith (defSelectableOptions{selected=is_selected}) (pack $ show o)
--               when b' $ currIx $= n
--               -- when is_selected (IM.setItemDefaultFocus)
--               pure b'
--       currIx' <- get currIx
--       ref $= ((opt:opts) !! currIx')
--       IM.endCombo
--       pure $ any id bs
--     else
--       pure False


  -- TabBar      t sc        -> IM.beginTabBar t IM.ImGuiTabBarFlags_None >>= \case
  --                             False -> do
  --                                 b' <- mapM (\(t',b'',cs) -> do
  --                                   br <- liftIO $ newIORef b''
  --                                   bt <- IM.beginTabItem t' br IM.ImGuiTabBarFlags_None
  --                                   if bt then do
  --                                     b''' <- mapM pushComp cs
  --                                     IM.endTabItem
  --                                     pure $ any id b'''
  --                                   else do
  --                                     b''' <- mapM pushComp cs
  --                                     pure $ any id b''') sc
  --                                 pure $ any id b'
  --                             True  -> do
  --                               b' <- mapM (\(t',b'',cs) -> do
  --                                 br <- liftIO $ newIORef b''
  --                                 bt <- IM.beginTabItem t' br IM.ImGuiTabBarFlags_None
  --                                 if bt then do
  --                                     b''' <- mapM pushComp cs
  --                                     IM.endTabItem
  --                                     pure $ any id b'''
  --                                 else do
  --                                     b''' <- mapM pushComp cs
  --                                     pure $ any id b'''

  --                                                                   ) sc
  --                               IM.endTabBar
  --                               pure $ any id b'



