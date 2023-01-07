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
  -- | TODO: Export Vecs from other module
  , module Geomancy.Vec3
  , module Geomancy.Mat4
  ) where

import GHC.Records
import Type.Reflection

import Data.String
import Control.Logger.Simple
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
import qualified Ghengin.Render.Queue as RQ
import Ghengin.Scene.Graph

import qualified Ghengin.DearImGui as IM

import Ghengin.Component.Mesh
import Ghengin.Component.Material
import Ghengin.Component.Camera
import Ghengin.Component.Transform
import Ghengin.Component.UI
import Ghengin.Render


-- TODO: Somehow systems that want to delete entities should call a special
-- destructor function that gets rid of resources stuck in components such as
-- meshes


type Ghengin w = SystemT w (Renderer ())

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
                          , HasField "uiwindows" w (Storage (UIWindow w))
                          , Typeable w
                          )

ghengin :: WorldConstraints w
        => w           -- ^ World
        -> Ghengin w a -- ^ Init
        -> Ghengin w b -- ^ Run every simulation step (currently ignored)
        -> (a -> DeltaTime -> Ghengin w Bool) -- ^ Run every game
                                                          -- loop? iteration.
                                                          -- The returned Bool indicates whether we should exit the gameloop
        -- -> Ghengin w c -- ^ Run every draw step?
        -> Ghengin w c -- ^ Run once the game is quit (for now that is when the window closed)
        -> IO ()
ghengin world initialize _simstep loopstep finalize = withGlobalLogging (LogConfig (Just "log.ghengin.log") True) . (runVulkanRenderer ()) . (`runSystem` world) $ do

  logDebug "Started Ghengin"

  a <- initialize

  -- TODO: Use linear types. Can I make the monad stack over a multiplicity polymorphic monad?

  -- Init ImGui for this render pass (should eventually be tied to the UI render pass)
  -- BIG:TODO: Don't hardcode the renderpass from the first renderpacket...
  rps <- cfold (\acc (RenderPacket _ _ pp _) -> pp._renderPass._renderPass:acc) []
  imCtx <- lift $ IM.initImGui (head rps)

  currentTime <- liftIO (getCurrentTime >>= newIORef)
  lastFPSTime <- liftIO (getCurrentTime >>= newIORef)
  frameCounter <- liftIO (newIORef (0 :: Int))

  windowLoop $ do

    logTrace "New frame"

    newTime <- liftIO getCurrentTime

    -- FPS Counter
    lastFPS <- liftIO (readIORef lastFPSTime)
    liftIO (modifyIORef' frameCounter (+1))
    when (diffUTCTime newTime lastFPS > 1) (liftIO $ do
      frames <- readIORef frameCounter
      logInfo $ "FPS: " <> (fromString $ show frames)
      writeIORef frameCounter 0
      writeIORef lastFPSTime newTime
      )

    -- Fix Your Timestep: A Very Hard Thing To Get Right. For now, the simplest approach:
    frameTime <- diffUTCTime newTime <$> liftIO(readIORef currentTime)
    liftIO(writeIORef currentTime newTime)

    logTrace "Drawing UI"

    -- DearImGui frame
    -- TODO: Draw UI (define all UI components in the frame)
    -- DrawUI will run the associated onchange functions the imgui way right
    -- away instead of returning any boolean whatsoever.
    drawUI

    logTrace "Simulating a step"

    -- TODO: We're currently drawing two equal frames in a row... we probably want all of this to be done on each frame

    -- Game loop step
    b <- loopstep a (min MAX_FRAME_TIME $ realToFrac frameTime)

    logTrace "Rendering"

    -- Currently render is called here because it traverses the scene graph and
    -- populates the render queue, and renders!
    render =<< liftIO (readIORef frameCounter)

    logTrace "Done frame"

    pure b

  Vk.deviceWaitIdle =<< lift getDevice

  lift $ do
    IM.destroyImCtx imCtx

  -- Destroy all render packets
  -- We can't do it by traversing the existing render packets because pipelines are shared across them.
  -- We instead create a last render queue and free it top-down
  rq <- cfold (flip $ \p -> RQ.insert p ()) mempty
  RQ.traverseRenderQueue rq
    (const id)
    (\(RQ.SomePipeline p) -> lift $ destroyRenderPipeline p)
    (\_ (RQ.SomeMaterial m) -> lift $ freeMaterial m)
    (\_ m _ -> lift $ freeMesh m
    )
    (pure ())

  _ <- finalize

  pure ()

drawUI :: WorldConstraints w => Ghengin w ()
drawUI = do
    IM.vulkanNewFrame
    IM.glfwNewFrame
    IM.newFrame

    cmapM $ \(uiw :: UIWindow w) -> IM.pushWindow uiw

    IM.render

pattern MAX_FRAME_TIME :: Float
pattern MAX_FRAME_TIME = 0.5
