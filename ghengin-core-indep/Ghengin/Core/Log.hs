{-# LANGUAGE CPP #-}
module Ghengin.Core.Log
  ( module Ghengin.Core.Log
  , FastLogger, toLogStr
  ) where

import Data.Bifunctor
import Ghengin.Core.Prelude as G
import System.Log.FastLogger
import qualified Prelude

class MonadIO m => HasLogger m where
  -- | Get a logger. Don't forget to add an inline pragma!
  getLogger :: m (Ur FastLogger)

-- | Returns a new logger and an IO cleanup action
newLogger :: MonadIO m => m (Ur FastLogger, IO ())
newLogger = G.do
  Ur (logger,clean) <- liftSystemIOU (second (liftSystemIO) Prelude.<$> newFastLogger (LogStdout defaultBufSize))
  pure (Ur logger, clean)

-- | Unconditionally log a message to the default logger
log :: (ToLogStr msg, HasLogger m) => msg -> m ()
log msg = getLogger >>= \(Ur logger) -> G.do
  liftSystemIO $ logger (toLogStr msg <> toLogStr "\n")
{-# INLINE log #-}

-- | Log if debug level is set
logD :: HasLogger m => LogStr -> m ()
#ifdef DEBUG
logD = log
#else
logD = const (pure ())
#endif
{-# INLINE logD #-}

-- | Log if trace level is set
logT :: HasLogger m => LogStr -> m ()
#ifdef DEBUG_TRACE
logT = log
#else
logT = const (pure ())
#endif
{-# INLINE logT #-}
