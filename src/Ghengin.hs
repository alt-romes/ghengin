{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLists #-}
module Ghengin
  ( module Ghengin

  -- * Meshes
  , Mesh, createMesh, createMeshWithIxs
  , module Ghengin.Component.Mesh.Vertex

  -- * Materials
  , Material(..)
  , material

  -- * Render pipelines
  , RenderPipeline, makeRenderPipeline

  -- * Render properties
  , PropertyBinding(..)
  , HasPropertyAt(..)

  -- * Compatible
  , Compatible

  -- | TODO: Export Vecs from other module
  , module Geomancy.Vec3
  , module Geomancy.Mat4

  , module Control.Lens

  , lift, liftIO
  ) where

-- Re-exports
import Ghengin.Core.Type.Compatible
import Ghengin.Core.Mesh.Vertex
import Ghengin.Core.Render.Property
-- End re-exports

import Apecs.Linear
import Control.Logger.Simple
import Control.Monad.Reader
import Data.IORef
import Data.Maybe
import Data.String
import Data.Time.Clock
import Geomancy.Mat4
import Geomancy.Vec3
import Ghengin.Core.Material
import Ghengin.Core.Mesh
import Ghengin.Component.UI
import Ghengin.Render
import Ghengin.Core.Render.Packet
import Ghengin.Core.Renderer.Kernel
import Ghengin.Vulkan.GLFW.Window
import Ghengin.Vulkan.RenderPass
import Ghengin.World
import Type.Reflection
import qualified Ghengin.DearImGui as IM
import qualified Ghengin.Render.Queue as RQ
import qualified Vulkan as Vk
import Control.Lens ((^.), (.~), (%~), Lens, Lens', (&))
import Ghengin.Utils (Ref(..))

-- TODO: Somehow systems that want to delete entities should call a special
-- destructor function that gets rid of resources stuck in components such as
-- meshes

type Ghengin w = SystemT (World w) Renderer

windowLoop :: Ghengin w Bool -> Ghengin w ()
windowLoop action = do
  win <- lift (asks (._vulkanWindow._window))
  loopUntilClosedOr win action

type DeltaTime = Float -- Converted from NominalDiffTime

initWorld :: MonadIO m => w -> m (World w)
initWorld w = World <$> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> pure w

ghengin :: Typeable w
        => w           -- ^ World
        -> Ghengin w a -- ^ Init
        -> Ghengin w b -- ^ Run every simulation step (currently ignored)
        -> (a -> DeltaTime -> Ghengin w Bool) -- ^ Run every game
                                                          -- loop? iteration.
                                                          -- The returned Bool indicates whether we should exit the gameloop
        -- -> Ghengin w c -- ^ Run every draw step?
        -> (a -> Ghengin w c) -- ^ Run once the game is quit (for now that is when the window closed)
        -> IO ()
                                                                                   -- (Just "log.ghengin.log")
ghengin world initialize _simstep loopstep finalize = do

  world' <- initWorld world

  withGlobalLogging (LogConfig Nothing True) . runVulkanRenderer () . (`runSystem` world') $ do

    logDebug "Started Ghengin"

    a <- initialize

    -- TODO: Use linear types. Can I make the monad stack over a multiplicity polymorphic monad?

    -- Init ImGui for this render pass (should eventually be tied to the UI render pass)
    -- BIG:TODO: Don't hardcode the renderpass from the first renderpacket...
    rps <- cfoldM (\acc (RenderPacket _ _ (Ref pp_ref) _) ->
                    Apecs.get pp_ref >>= \(SomePipeline pp) -> pure ((pp ^. renderPass)._renderPass:acc)) []
    imCtx <- case listToMaybe rps of
               Nothing -> pure Nothing
               Just x  -> Just <$> lift (IM.initImGui x)

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
        logInfo $ "FPS: " <> fromString (show frames)
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
      -- TODO: Draw UI should only draw if UI is enabled (imCtx was set...
      -- tODO: better way to create imCtx).
      drawUI imCtx

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

    case imCtx of
      Nothing -> pure ()
      Just x  -> lift $ IM.destroyImCtx x

    -- Destroy all render packets
    -- We can't do it by traversing the existing render packets because pipelines are shared across them.
    -- We instead create a last render queue and free it top-down
    rq <- cfold (flip $ \p -> RQ.insert p ()) mempty
    RQ.traverseRenderQueue rq
      (const id)
      (\(RQ.SomePipelineRef (Ref p_ref)) -> do
        SomePipeline p <- Apecs.get p_ref
        lift $ destroyRenderPipeline p
        pure $ SomePipeline p
        )
      (\_ (RQ.SomeMaterialRef (Ref m_ref)) -> do
        SomeMaterial m <- Apecs.get m_ref
        lift $ freeMaterial m)
      (\_ (SomeMesh m) _ -> do
        -- liftIO $ print =<< readIORef (m.referenceCount)
        lift $ freeMesh m
      )
      (pure ())

    _ <- finalize a

    pure ()

drawUI :: ∀ w. Typeable w => Maybe IM.ImCtx -> Ghengin w ()
drawUI Nothing = pure ()
drawUI (Just _ctx) = do
    IM.vulkanNewFrame
    IM.glfwNewFrame
    IM.newFrame

    cmapM $ \(uiw :: UIWindow w) -> IM.pushWindow uiw
    IM.showDemoWindow

    IM.render

pattern MAX_FRAME_TIME :: Float
pattern MAX_FRAME_TIME = 0.5
