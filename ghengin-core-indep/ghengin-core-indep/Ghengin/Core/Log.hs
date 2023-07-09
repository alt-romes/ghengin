{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-|
   Ghengin logging capabilities.
-}
module Ghengin.Core.Log
  ( module Ghengin.Core.Log

  -- * Fast-logger re-exports
  , FastLogger, toLogStr, LogType'(..), defaultBufSize
  ) where

import Data.Bifunctor
import Ghengin.Core.Prelude as G
import System.Log.FastLogger
import qualified Prelude (take)

import Control.Functor.Linear as Linear

#ifdef THINGS_ARE_GOING_THAT_BAD
-- In that case we always flush and use BS.putStr
import qualified Data.ByteString as BS
import qualified System.IO
#endif

data Logger
  = Logger { _log :: FastLogger
           , _depth :: Int }

class MonadIO m => HasLogger m where
  -- | Get a logger. Don't forget to add an inline pragma!
  getLogger :: m (Ur Logger)
  -- | Increment the depth of the logging. This makes it quite hard to instance
  -- HasLogger for all MonadTrans over a HasLogger m.
  withLevelUp  :: m a ⊸ m a

instance (MonadIO m, HasLogger m) => HasLogger (StateT s m) where
  getLogger = lift getLogger
  withLevelUp (StateT m) = StateT $ \s -> withLevelUp (m s)

-- | Returns a new logger and an IO cleanup action
newLogger :: MonadIO m => LogType -> m (Ur Logger, IO ())
{-# INLINE newLogger #-}
newLogger logt = G.do
  Ur (logger,clean) <- liftSystemIOU (second liftSystemIO <$$> newFastLogger logt)
  pure (Ur (Logger logger 0), clean)

-- | Unconditionally log a message to the default logger
log :: (ToLogStr msg, HasLogger m) => msg -> m ()
{-# INLINE log #-}
log msg = getLogger >>= \(Ur logger) -> G.do
  let -- Log with preceeding unicode symbols
      leading_syms = Prelude.take (logger._depth*2) (cycle ['│',' '])
      full_msg = toLogStr leading_syms <> toLogStr msg <> toLogStr "\n"
  liftSystemIO $
#ifndef THINGS_ARE_GOING_THAT_BAD
    logger._log full_msg
#else
    do BS.putStr (fromLogStr full_msg); System.IO.hFlush System.IO.stdout
#endif

-- | Log if debug level (@-DDEBUG@) is set
logD :: HasLogger m => LogStr -> m ()
{-# INLINE logD #-}
#ifdef DEBUG
logD = log
#else
logD = const (pure ())
#endif

-- | Log and increase logging depth until action is left if debug level
-- (@-DDEBUG@) is set
enterD :: HasLogger m => LogStr -> m a ⊸ m a
{-# INLINE enterD #-}
#ifdef DEBUG
enterD msg ma = G.do
  log (toLogStr "Entering: " <> msg)
  a <- withLevelUp ma
  log "Done."
  pure a
#else
enterD _ = pure ()
#endif

-- | Log if trace level (@-DDEBUG_TRACE@) is set
logT :: HasLogger m => LogStr -> m ()
{-# INLINE logT #-}
#ifdef DEBUG_TRACE
logT = log
#else
logT = const (pure ())
#endif
