module Ghengin.Monad where

-- linear-base
import qualified System.IO.Linear as Linear
import qualified Control.Functor.Linear as Linear

-- reference-counting
import qualified Data.Linear.Alias as Alias

-- ghengin-core
import qualified Ghengin.Core.Prelude as Linear
import Ghengin.Core.Renderer
import Ghengin.Core.Render
import Ghengin.Core.Render.Pipeline
import Ghengin.Core.Render.Property
import Ghengin.Core.Render.Queue

-- ghengin:dear-imgui
import qualified Ghengin.DearImGui.Backend as ImGui
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
  { unGhengin :: ReaderT GhenginReader (UrT (Linear.StateT GhenginState Renderer)) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader GhenginReader)

-- | The reader environment for the game engine monad 'Ghengin'
data GhenginReader = GhenginReader
  { conf :: !GhenginConf
  }

data GhenginState = GhenginState
  { mainRenderPass :: !(Alias RenderPass)
  }

-- | Make a new 'GhenginReader' environment from the a 'GhenginConf' configuration
newGhenginReader :: GhenginConf -> GhenginReader
newGhenginReader conf = GhenginReader conf

-- | Run the engine
runGhengin :: GhenginConf -> Ghengin a -> IO a
runGhengin conf@GhenginConf{..} (Ghengin act) =
  Linear.withLinearIO $
    runRenderer (frameWidth, frameHeight) $ Linear.do

      -- Figure out how to (and why) we can have more than a single default
      -- render pass
      rp <- createSimpleRenderPass

      -- Init imgui
      (rp, mimctx) <-
        if enableImGui
          then Alias.useM rp $ \rp -> Linear.do
               (rp, imctx) <- ImGui.initImGui rp
               Linear.pure (rp, Just imctx)
          else Linear.pure (rp, Nothing)

      (x, GhenginState{..}) <-
        Linear.runStateT
          (runUrT (runReaderT act (newGhenginReader conf)))
          GhenginState
            { mainRenderPass = rp
            }

      Alias.forget mainRenderPass

      case mimctx of
        Nothing -> Linear.pure ()
        Just imctx -> ImGui.destroyImCtx imctx

      Linear.pure x

--------------------------------------------------------------------------------
-- Lifting
--------------------------------------------------------------------------------

liftRenderer :: Renderer (Ur a) %1 -> Ghengin a
liftRenderer r = Ghengin (ReaderT \_ -> (UrT (Linear.StateT \s -> (,s) Linear.<$> r)))

-- | Get the "main" render pass to use in a Renderer action.
-- Right now, there's a single "main" render pass.
-- Why, and how, should we have something other than a single built-in one?
withRenderPass :: (RenderPass %1 -> Renderer (Ur a)) -> Ghengin a
withRenderPass = _

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

