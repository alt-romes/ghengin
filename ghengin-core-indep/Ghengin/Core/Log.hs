{-# LANGUAGE CPP #-}
module Ghengin.Core.Log where

import Data.Bifunctor
import Ghengin.Core.Prelude as G
import System.Log.FastLogger
import qualified Prelude

class MonadIO m => HasLogger m where
  -- | Get a logger. Don't forget to add an inline pragma!
  getLogger :: m (Ur FastLogger)

-- | Returns a new logger and an IO cleanup action
newLogger :: MonadIO m => m (Ur (FastLogger, IO ()))
newLogger = liftSystemIOU (second (liftSystemIO) Prelude.<$> newFastLogger (LogStdout defaultBufSize))

-- | Unconditionally log a message to the default logger
log :: (ToLogStr msg, HasLogger m) => msg -> m ()
log msg = getLogger >>= \(Ur logger) -> G.do
  liftSystemIO $ logger (toLogStr msg)
{-# INLINE log #-}

-- | Log if debug level is set
logD :: (ToLogStr msg, HasLogger m) => msg -> m ()
#ifdef DEBUG
logD = log
#else
logD = const (pure ())
#endif
