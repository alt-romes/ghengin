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
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE LinearTypes #-}
module Ghengin
  ( module Ghengin

  -- * Meshes
  , Mesh, createMesh, createMeshWithIxs
  , module Ghengin.Core.Mesh.Vertex

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

  -- | TODO: Export Vecs from other module (Ghengin.Prelude)
  , module Geomancy.Vec3
  , module Geomancy.Mat4
  ) where

import System.Log.FastLogger
import Ghengin.Core.Log
import Prelude.Linear hiding (IO)
import qualified Prelude
import Control.Functor.Linear as Linear
import Control.Monad.IO.Class.Linear


import System.IO.Linear

-- Re-exports
import Ghengin.Core.Type.Compatible
import Ghengin.Core.Mesh.Vertex
import Ghengin.Core.Render.Property
-- End re-exports

import Apecs.Linear as Apecs
-- import Control.Logger.Simple
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
import Ghengin.Vulkan.Renderer.Kernel -- ROMES:TODO: One day we won't need to depend on Vulkan here
import Ghengin.Vulkan.Renderer.GLFW.Window
import Ghengin.Vulkan.Renderer.RenderPass
import Ghengin.Vulkan.Renderer.Kernel (unsafeUseDevice)
import Ghengin.Vulkan.Renderer

import Ghengin.World
import Type.Reflection
import qualified Ghengin.DearImGui as IM
import qualified Ghengin.Render.Queue as RQ
import qualified Vulkan as Vk
import Control.Lens ((^.), (.~), (%~), Lens, Lens', (&))
import Data.Counted
import qualified Data.Counted.Unsafe

import qualified Unsafe.Linear as Unsafe

-- TODO: Somehow systems that want to delete entities should call a special
-- destructor function that gets rid of resources stuck in components such as
-- meshes

type Ghengin w = SystemT (World w) Renderer
instance Dupable w => HasLogger (Ghengin w) where
  -- Unsafe, whateverrrrr, it's easier. This is bad...
  getLogger = SystemT $ ReaderT $ Unsafe.toLinear $ \w -> getLogger
  {-# INLINE getLogger #-}
  withLevelUp (SystemT (ReaderT fma)) = SystemT $ ReaderT \w -> withLevelUp (fma w)
  {-# INLINE withLevelUp #-}

windowLoop :: Dupable w => Ur s ⊸ (Ur s ⊸ Ghengin w (Bool, Ur s)) -> Ghengin w (Ur s)
windowLoop s action = Linear.do
  Ur win <- lift (renderer $ Unsafe.toLinear $ \renv -> pure (Ur renv._vulkanWindow._window, renv))
  (win', s) <- loopUntilClosedOr win s action
  Unsafe.toLinear (\_ -> pure s) win'

type DeltaTime = Float -- Converted from NominalDiffTime

initWorld :: MonadIO m => w -> m (Ur (World w))
initWorld w = Linear.do
  world <- World <$> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> pure w
  pure (Unsafe.toLinear Ur world) -- worry about it later, this won't stay like this for long since the whole linear apecs interface is quite broken

ghengin :: ∀ w a b c
         . Typeable w
        => Dupable w
        => w           -- ^ World
        -> Ghengin w (Ur a) -- ^ Init
        -> Ghengin w b -- ^ Run every simulation step (currently ignored)
        -> (a -> DeltaTime -> Ghengin w Bool) -- ^ Run every game
                                                          -- loop? iteration.
                                                          -- The returned Bool indicates whether we should exit the gameloop
        -- -> Ghengin w c -- ^ Run every draw step?
        -> (a -> Ghengin w ()) -- ^ Run once the game is quit (for now that is when the window closed)
        -> IO ()
                                                                                   -- (Just "log.ghengin.log")
-- this bit of the code can still be linear (just wrap with withLinearIO the
-- whole thing), but the interface to Ghengin should be reverted to be unrestricted
ghengin world initialize _simstep loopstep finalize = Linear.do

  Ur world' <- initWorld world

  runRenderer $ (`runSystem` world') $ Linear.do

    logD "Started Ghengin"

    Ur a <- initialize

    logT "Initialized"

    -- TODO: Use linear types (was this was meant for `a`?). Can I make the monad stack over a multiplicity polymorphic monad? (Yes, you could, past me!)

    -- Init ImGui for this render pass (should eventually be tied to the UI render pass)
    -- BIG:TODO: Don't hardcode the renderpass from the first renderpacket...
    Ur rps <- cfoldM (\acc (RenderPacket _ _ (Ref pp_ref) _) ->
                    Apecs.get (Entity pp_ref) >>= \(Ur (SomePipeline pp)) -> Linear.do
                      Ur (rrp,pp') <- Unsafe.toLinear Ur <$> getRenderPass pp
                      Ur rp <- Unsafe.toLinear Ur <$> Data.Counted.Unsafe.dec rrp
                      pure (Ur (rp._renderPass:acc))
                           ) []
    logT "Got renderpass from first packet"

    -- Very unsafe imCtx... won't matter when we revert linear types from the frontend
    Ur imCtx <- case listToMaybe rps of
                 Nothing -> pure (Ur Nothing)
                 Just x  -> Linear.do
                   Ur (imct, _uns) <- Unsafe.toLinear Ur <$> lift (IM.initImGui x)
                   pure (Ur (Just imct))
    logT "Got imCtx"

    Ur currentTime <- liftSystemIOU getCurrentTime
    -- Ur lastFPSTime <- liftSystemIOU getCurrentTime
    let frameCounter = 0 :: Int

    Ur _ <- windowLoop (Ur (currentTime,frameCounter)) (ghengin_loop a imCtx)

    lift (unsafeUseDevice Vk.deviceWaitIdle)

    case imCtx of
      Nothing -> pure ()
      Just x  -> lift $ IM.destroyImCtx x

    -- Destroy all render packets
    -- We can't do it by traversing the existing render packets because pipelines are shared across them.
    -- We instead create a last render queue and free it top-down
    Ur rq <- cfold (flip $ \p -> RQ.insert p ()) Prelude.mempty
    RQ.traverseRenderQueue rq
      (const id)
      (\(RQ.SomePipelineRef (Ref p_ref)) -> Linear.do
        Ur (SomePipeline p) <- Apecs.get (Entity p_ref)
        -- lift $ destroyRenderPipeline p -- ROMES:TODO:
        pure $ SomePipeline p
        )
      (\_ (RQ.SomeMaterialRef (Ref m_ref)) -> Linear.do
        Ur (SomeMaterial m) <- Apecs.get (Entity m_ref)
        -- lift $ freeMaterial m -- ROMES:TODO:
        pure ()
        )
      (\_ (SomeMesh m) () -> Linear.do
        pure ()
        -- liftIO $ print =<< readIORef (m.referenceCount)
        lift $ freeMesh m -- ROMES:TODO:
      )
      (pure ())

    finalize a

    pure ()
  where
    ghengin_loop :: a -> Maybe IM.ImCtx ⊸ LoopState ⊸ Ghengin w (Bool, LoopState)
    ghengin_loop a imCtx (Ur (currentTime, frameCounter)) = Linear.do

      logT "New frame"

      Ur newTime <- liftSystemIOU getCurrentTime

      -- Fix Your Timestep: A Very Hard Thing To Get Right. For now, the simplest approach:
      let frameTime = diffUTCTime newTime currentTime

      logT "Drawing UI"

      -- DearImGui frame
      -- TODO: Draw UI (define all UI components in the frame)
      -- DrawUI will run the associated onchange functions the imgui way right
      -- away instead of returning any boolean whatsoever.
      -- TODO: Draw UI should only draw if UI is enabled (imCtx was set...
      -- tODO: better way to create imCtx).
      drawUI imCtx

      logT "Simulating a step"

      -- BIG:TODO: We're currently drawing two equal frames in a row... we probably want all of this to be done on each frame

      -- Game loop step
      b <- loopstep a (Prelude.min MAX_FRAME_TIME $ realToFrac frameTime)

      logT "Rendering"

      -- Currently render is called here because it traverses the scene graph and
      -- populates the render queue, and renders!
      render frameCounter

      logT "Done frame"

      pure (b, Ur (newTime, frameCounter+1))

type LoopState = Ur (UTCTime, Int)


-- Unsafe, doesn't matter, we'll rework this all
drawUI :: ∀ w. (Typeable w, Dupable w) => Maybe IM.ImCtx -> Ghengin w ()
drawUI Nothing = pure ()
drawUI (Just _ctx) = Linear.do
    liftSystemIO $ do
      IM.vulkanNewFrame
      IM.glfwNewFrame
      IM.newFrame

    cmapM $ \(uiw :: UIWindow w) -> move <$> IM.pushWindow uiw

    liftSystemIO $ do
      IM.showDemoWindow

      IM.render

pattern MAX_FRAME_TIME :: Float
pattern MAX_FRAME_TIME = 0.5
