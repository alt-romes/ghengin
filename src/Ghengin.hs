{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLists #-}
module Ghengin
  ( module Ghengin
  , module Geomancy.Vec3
  , module Geomancy.Mat4
  ) where

import GHC.Records

import Control.Monad.Reader

import Data.IORef

import Data.Time.Clock
import qualified Vulkan as Vk
import Apecs
import Geomancy.Vec3
import Geomancy.Mat4

import Ghengin.Vulkan.RenderPass
import Ghengin.Vulkan.GLFW.Window
import Ghengin.Vulkan
import Ghengin.Render.Packet
import Ghengin.Scene.Graph

import qualified Ghengin.DearImGui as IM

import Ghengin.Component.Camera
import Ghengin.Component.Transform
import Ghengin.Component.UI
import Ghengin.Render


-- TODO: Somehow systems that want to delete entities should call a special
-- destructor function that gets rid of resources stuck in components such as
-- meshes


type Ghengin w = SystemT w (Renderer GEnv)

data GEnv = GEnv { _renderPipelines :: IORef [SomeRenderPipeline]
                 }

initGEnv :: MonadIO m => m GEnv
initGEnv = do
  GEnv <$> liftIO (newIORef [])

windowLoop :: Ghengin w Bool -> Ghengin w ()
windowLoop action = do
  win <- lift (asks (._vulkanWindow._window))
  loopUntilClosedOr win action

type DeltaTime = Float -- Converted from NominalDiffTime

type WorldConstraints w = ( HasField "transforms" w (Storage Transform)
                          , HasField "renderPackets" w (Storage RenderPacket)
                          , HasField "modelMatrices" w (Storage ModelMatrix)
                          , HasField "entityParents" w (Storage Parent)
                          , HasField "cameras" w (Storage Camera)
                          , HasField "uiwindows" w (Storage UIWindow)
                          )

ghengin :: WorldConstraints w
        => w           -- ^ World
        -> Ghengin w a -- ^ Init
        -> Ghengin w b -- ^ Run every simulation step (currently ignored)
        -> (a -> DeltaTime -> [Bool] -> Ghengin w Bool) -- ^ Run every game
                                                          -- loop? iteration. The list of list of bools indicates whether the components in
                                                          -- a UI window were changed. The returned Bool indicates whether we should exit the gameloop
        -- -> Ghengin w c -- ^ Run every draw step?
        -> Ghengin w c -- ^ Run once the game is quit (for now that is when the window closed)
        -> IO ()
ghengin world initialize _simstep loopstep finalize = (\x -> initGEnv >>= flip runVulkanRenderer x) . (`runSystem` world) $ do

  -- TODO: If the materials added to each entity are ordered, when we get all
  -- the materials will the materials be ordered? If so, we can simply render
  -- with them sequentially without being afraid of changing back and forwards
  a <- initialize

  -- BIG:TODO: Bundle descriptor sets, render passes, and pipelines into a single abstraction
  -- ^^ This is going well, they are created in Render.Packet
  --
  -- TODO: Use linear types. Can I make the monad stack over a multiplicity polymorphic monad?

  -- Init ImGui for this render pass (should eventually be tied to the UI render pass)
  -- BIG:TODO: Don't hardcode the renderpass from the first renderpacket...
  (head -> pp) <- liftIO . readIORef =<< lift (asks (._extension._renderPipelines))
  imCtx <- lift $ IM.initImGui (case pp of SomeRenderPipeline pp' _ -> pp'._renderPass._renderPass)

  currentTime <- liftIO (getCurrentTime >>= newIORef)
  lastFPSTime <- liftIO (getCurrentTime >>= newIORef)
  frameCounter <- liftIO (newIORef (0 :: Int))

  windowLoop $ do

    newTime <- liftIO getCurrentTime

    -- FPS Counter
    lastFPS <- liftIO (readIORef lastFPSTime)
    liftIO (modifyIORef' frameCounter (+1))
    when (diffUTCTime newTime lastFPS > 1) (liftIO $ do
      frames <- readIORef frameCounter
      putStrLn $ "FPS: " <> show frames
      writeIORef frameCounter 0
      writeIORef lastFPSTime newTime
      )

    -- Fix Your Timestep: A Very Hard Thing To Get Right. For now, the simplest approach:
    frameTime <- diffUTCTime newTime <$> liftIO(readIORef currentTime)
    liftIO(writeIORef currentTime newTime)

    -- DearImGui frame
    -- TODO: Draw UI (define all UI components in the frame)
    bs <- drawUI

    -- We're currently drawing two equal frames in a row... we probably want all of this to be done on each frame

    -- Game loop step
    b <- loopstep a (min MAX_FRAME_TIME $ realToFrac frameTime) bs

    -- Currently render is called here because it traverses the scene graph and
    -- populates the render queue, and renders!
    render =<< liftIO (readIORef frameCounter)

    -- Render frame
    -- drawFrame

    pure b

  Vk.deviceWaitIdle =<< lift getDevice

  lift $ do
    IM.destroyImCtx imCtx
    -- BIG:TODO: Destroy allocated 'RenderPipeline's
    -- destroyDescriptorPool dpool
    -- mapM_ destroyUniformBuffer objUBs
    -- mapM_ destroyDescriptorSetLayout descriptorSetLayouts
    -- destroyRenderPass simpleRenderPass
    -- destroyPipeline pipeline

  _ <- finalize

  pure ()

drawUI :: WorldConstraints w => Ghengin w [Bool]
drawUI = do
    IM.vulkanNewFrame
    IM.glfwNewFrame
    IM.newFrame

    bs <- cfoldM (\acc (uiw :: UIWindow) -> do
      bs <- lift $ IM.pushWindow uiw
      pure (bs:acc)) []

    IM.render

    pure bs

pattern MAX_FRAME_TIME :: Float
pattern MAX_FRAME_TIME = 0.5
