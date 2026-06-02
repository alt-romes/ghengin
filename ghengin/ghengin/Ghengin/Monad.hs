{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Ghengin.Monad where

-- linear-base
import qualified System.IO.Linear as Linear
import qualified Control.Functor.Linear as Linear

-- reference-counting
import qualified Data.Linear.Alias as Alias

-- ghengin-core
import qualified Ghengin.Core.Prelude as Linear
import Ghengin.Vulkan.Renderer
import Ghengin.Core.Render
import Ghengin.Core.Render.Pipeline
import Ghengin.Core.Render.Property
import Ghengin.Core.Render.Queue
import Ghengin.Core.Input

-- ghengin:dear-imgui
import qualified Ghengin.DearImGui.Vulkan as ImGui
import qualified Ghengin.DearImGui.UI as ImGui

import Ghengin.Prelude

-- | The top-level monad for using the engine.
-- Wraps the linear Renderer core monad and provides most engine capabilities
-- on by default.
--
-- If you need finer-grained control over the renderer (without bringing in any
-- ghengin-level capabilities) you may want to check out @ghengin-core@'s
-- 'Renderer' monad.
newtype Ghengin a = Ghengin
  { unGhengin :: ReaderT GhenginReader (UrT (Linear.StateT RenderState Renderer)) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader GhenginReader)

-- | The reader environment for the game engine monad 'Ghengin'
data GhenginReader = GhenginReader
  { conf :: !GhenginConf
  , charStream :: !CharStream
  , mouseDragStream :: !MouseDragStream
  }

data RenderState = RenderState
  { renderQueue :: !(RenderQueue ())
  -- ^ Evolve this, as necessary, into a cooler better render graph.
  }

-- | Make a new 'GhenginReader' environment from the a 'GhenginConf' configuration
newGhenginReader :: GhenginConf -> Renderer (Ur GhenginReader)
newGhenginReader conf = Linear.do
  Ur charStream <- registerCharStream
  Ur mouseDragStream <- registerMouseDragStream $ Linear.do
    -- Makes sure dragging events don't occur if an ImGui window is being used
    if enableImGui conf then
      not <$> ImGui.wantCaptureMouse -- imgui is already using the mouse
    else
      pure True
  Linear.pure (Ur GhenginReader{..})

-- | Run the engine
runGhengin :: GhenginConf -> Ghengin a -> IO a
runGhengin conf@GhenginConf{..} (Ghengin act) =
  Linear.withLinearIO $
    runRenderer (frameWidth, frameHeight) $ Linear.do

      -- Register input streams BEFORE ImGui so that ImGui's GLFW callbacks
      -- (installed by initImGui) chain on top of ours instead of being clobbered
      -- by GLFW.setCursorPosCallback / setMouseButtonCallback. Otherwise ImGui
      -- never sees mouse events and widgets like sliders can't be dragged.
      Ur ghenginReader <- newGhenginReader conf

      -- Init imgui
      mimctx <-
        if enableImGui
          then Just Linear.<$> ImGui.initImGui
          else Linear.pure Nothing

      (x, RenderState{..}) <-
        Linear.runStateT
          (runUrT (runReaderT act ghenginReader))
          RenderState
            { renderQueue = emptyRenderQueue
            }

      freeRenderQueue renderQueue

      case mimctx of
        Nothing -> Linear.pure ()
        Just imctx -> ImGui.destroyImCtx imctx

      Linear.pure x

-- | Run the game loop given a step function. The step function iterates over
-- some game state @a@. The initial game state is the second argument to
-- 'runGameLoop'. The step also receives the same index args as 'newFrame'.
--
-- This function updates the engine time(lines), updates window events, and
-- registers a new render frame ('newFrame' / 'newRenderFrame') for each
-- iteration of the game loop.
--
-- The loop exits if the user clicked to close the window.
runGameLoop
  :: forall a.
     ( forall swpcImgs. Linear.KnownNat swpcImgs
     => a
     -- TODO: we should cache these and then provide "renderWith" which receives the renderqueue and more...
     -> Linear.Finite FramesInFlight
     -> Linear.Finite swpcImgs
     -> Ghengin a  )
  -> a
  -> Ghengin a
runGameLoop act ini = go ini where
  go :: a -> Ghengin a
  go gs = do
    liftRenderer (Ur () Linear.<$ pollWindowEvents)
    should_close <- liftRenderer shouldCloseWindow
    if should_close then
      return gs
    else do
      gs' <- newRenderFrame (act gs)
      go gs'

-- | This can be used instead of 'newFrame' when you want to do more than just
-- 'Renderer' actions when rendering the frame.
newRenderFrame
  :: forall a.
   ( forall swpcImgs. Linear.KnownNat swpcImgs
     => Linear.Finite FramesInFlight
     -> Linear.Finite swpcImgs
     -> Ghengin a )
  -> Ghengin a
newRenderFrame k = Ghengin $ ReaderT $ \gr -> UrT $ Linear.StateT $ \s ->
  let go :: forall swpcImgs. Linear.KnownNat swpcImgs
         => Linear.Finite FramesInFlight
         -> Linear.Finite swpcImgs
         -> Renderer (Ur a, RenderState)
      go ff si =
        let inner = Linear.runStateT (runUrT (runReaderT (unGhengin (k ff si)) gr)) s
         in Linear.do
          if gr.conf.enableImGui
            then Linear.liftIO ImGui.registerNewFrame
            else Linear.pure () -- register new imgui frame
          inner
   in newFrame go

-- | Prepares the ImGui data to be rendered by the current frame's 'renderDrawData' render pass command.
-- This MUST be called before 'renderDrawData' and after all the immediate mode commands.
renderImGuiData :: Ghengin ()
renderImGuiData = liftRenderer (Ur () Linear.<$ Linear.liftIO ImGui.imguiRender )

--------------------------------------------------------------------------------
-- On Renderer
--------------------------------------------------------------------------------

renderState :: (RenderState %1 -> Renderer (Ur a, RenderState)) -> Ghengin a
renderState act = Ghengin (ReaderT \_ -> (UrT (Linear.StateT \s -> act s)))

editRenderQueue :: (RenderQueue () %1 -> Renderer (RenderQueue ())) -> Ghengin ()
editRenderQueue f = renderState $ \RenderState{..} -> Linear.do
  renderQueue <- f renderQueue
  Linear.return (Ur (), RenderState{..})

liftRenderer :: Renderer (Ur a) %1 -> Ghengin a
liftRenderer r = Ghengin (ReaderT \_ -> (UrT (Linear.StateT \s -> (,s) Linear.<$> r)))

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configure the engine
data GhenginConf = GhenginConf
  { frameWidth  :: !Int
    -- ^ Frame width in pixels
  , frameHeight :: !Int
    -- ^ Frame height in pixels
  , enableImGui :: !Bool
    -- ^ Whether to set-up dear-imgui. @True@ by default.
  }

-- | The default settings for running the engine
defaultGhenginConf :: GhenginConf
defaultGhenginConf = GhenginConf
  { frameWidth  = 1920
  , frameHeight = 1080
  , enableImGui = True
  }

