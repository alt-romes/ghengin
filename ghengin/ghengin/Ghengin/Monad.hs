module Ghengin.Monad where

import Prelude

-- linear-base
import Data.Unrestricted.Linear
import qualified System.IO.Linear as Linear

import Ghengin.Core.Renderer

-- | The top-level monad for using the engine.
-- Wraps the linear Renderer core monad.
newtype Ghengin a = Ghengin { unGhengin :: UrT Renderer a }

-- | Configure the engine
data GhenginConf = GhenginConf
  { frameResolution :: (Int, Int)
    -- ^ Frame resolution in pixels (width x height)
  }

-- | Run the engine
runGhengin :: GhenginConf -> Ghengin a -> IO a
runGhengin GhenginConf{..} (Ghengin m) =
  Linear.withLinearIO $
    runRenderer frameResolution (runUrT m)

