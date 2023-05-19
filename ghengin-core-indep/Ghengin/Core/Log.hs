{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Ghengin.Core.Log
  ( module Ghengin.Core.Log
  , FastLogger, toLogStr
  ) where

import Data.Bifunctor
import Ghengin.Core.Prelude as G
import System.Log.FastLogger
import qualified Data.ByteString as BS
import qualified Prelude
import qualified System.IO

data Logger
  = Logger { _log :: FastLogger
           , _depth :: Int
           }

class MonadIO m => HasLogger m where
  -- | Get a logger. Don't forget to add an inline pragma!
  getLogger :: m (Ur Logger)
  -- | Increment the depth of the logging. This makes it quite hard to instance
  -- HasLogger for all MonadTrans over a HasLogger m.
  withLevelUp  :: m a ⊸ m a

-- | Returns a new logger and an IO cleanup action
newLogger :: MonadIO m => m (Ur Logger, IO ())
newLogger = G.do
  Ur (logger,clean) <- liftSystemIOU (second (liftSystemIO) Prelude.<$> newFastLogger (LogStdout defaultBufSize))
  pure (Ur (Logger logger 0), clean)

-- | Unconditionally log a message to the default logger
log :: (ToLogStr msg, HasLogger m) => msg -> m ()
log msg = getLogger >>= \(Ur logger) -> G.do
  let white = replicate (logger._depth*2) ' '
      full_msg = toLogStr white <> toLogStr msg <> toLogStr "\n"
  liftSystemIO $
#ifdef THINGS_ARE_GOING_THAT_BAD
    do BS.putStr (fromLogStr full_msg); System.IO.hFlush System.IO.stdout
#else
    liftSystemIO $ logger._log full_msg
#endif
{-# INLINE log #-}

-- | Log if debug level is set
logD :: HasLogger m => LogStr -> m ()
#ifdef DEBUG
logD = log
#else
logD = const (pure ())
#endif
{-# INLINE logD #-}

-- | Log and enter if debug level is set
enterD :: HasLogger m => LogStr -> m a ⊸ m a
#ifdef DEBUG
enterD msg ma = G.do
  log (toLogStr "Entering: " <> msg)
  a <- withLevelUp ma
  log "Done."
  pure a

#else
enterD _ = pure ()
#endif
{-# INLINE enterD #-}

-- | Log if trace level is set
logT :: HasLogger m => LogStr -> m ()
#ifdef DEBUG_TRACE
logT = log
#else
logT = const (pure ())
#endif
{-# INLINE logT #-}
