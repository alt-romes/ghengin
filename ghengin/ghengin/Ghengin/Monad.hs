module Ghengin.Monad where

-- linear-base
import qualified System.IO.Linear as Linear

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
newtype Ghengin a = Ghengin { unGhengin :: ReaderT GhenginReader (UrT Renderer) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader GhenginReader)

-- | The reader environment for the game engine monad 'Ghengin'
data GhenginReader = GhenginReader
  { conf :: !GhenginConf
  }

-- | Make a new 'GhenginReader' environment from the a 'GhenginConf' configuration
newGhenginReader :: GhenginConf -> GhenginReader
newGhenginReader conf = GhenginReader conf

-- | Run the engine
runGhengin :: GhenginConf -> Ghengin a -> IO a
runGhengin conf@GhenginConf{..} (Ghengin act) =
  Linear.withLinearIO $
    runRenderer (frameWidth, frameHeight) $ Linear.do

      (rp1, rp2) <- Alias.share Linear.=<< createSimpleRenderPass

      -- Init imgui
      (rp1, imctx) <-
        if enableImGui then Alias.useM rp1 ImGui.initImGui
                       else Linear.pure (rp1, undefined)

      runUrT (runReaderT act (newGhenginReader conf))

--------------------------------------------------------------------------------
-- Lifting
--------------------------------------------------------------------------------

liftRenderer :: Renderer (Ur a) %1 -> Ghengin a
liftRenderer r = Ghengin (ReaderT \_ -> (UrT r))

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

