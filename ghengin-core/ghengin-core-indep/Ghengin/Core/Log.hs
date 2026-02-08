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

import qualified Data.Functor.Linear as Data
import Data.Bifunctor
import Ghengin.Core.Prelude as G
import System.Log.FastLogger
import qualified Prelude (take, cycle)


#ifdef THINGS_ARE_GOING_THAT_BAD
-- In that case we always flush and use BS.putStr
import qualified Data.ByteString as BS
import qualified System.IO
#endif

data Logger
  = Logger { log   :: !FastLogger
           , depth :: !Int }

class MonadIO m => HasLogger m where
  -- | Get a logger. Don't forget to add an inline pragma!
  getLogger :: m (Ur Logger)
  -- | Increment the depth of the logging.
  withLevelUp  :: m a ⊸ m a

instance (MonadIO m, HasLogger m) => HasLogger (StateT s m) where
  getLogger = lift getLogger
  {-# INLINE getLogger #-}
  withLevelUp (StateT m) = StateT $ \s -> withLevelUp (m s)

--------------------------------------------------------------------------------
newtype WithLogger m a = WithLogger { unWithLogger :: ReaderT (Ur Logger) m a }
  deriving (Data.Functor, Data.Applicative, Functor, Applicative, Monad)

instance MonadIO m => MonadIO (WithLogger m) where
  liftIO io = WithLogger (ReaderT \(Ur _) -> (liftIO io))
  {-# INLINE liftIO #-}

instance MonadIO m => HasLogger (WithLogger m) where
  getLogger = WithLogger $ ReaderT $ \r -> pure r
  {-# INLINE getLogger #-}
  withLevelUp (WithLogger (ReaderT r)) = WithLogger $ ReaderT $ \(Ur (Logger l d)) -> r (Ur (Logger l (d+1)))
  {-# INLINE withLevelUp #-}

runWithLogger :: Logger -> WithLogger m a %1 -> m a
runWithLogger logger (WithLogger (ReaderT r)) = r (Ur logger)
--------------------------------------------------------------------------------

-- | Returns a new logger and an IO cleanup action
newLogger :: MonadIO m => LogType -> m (Ur Logger, IO ())
{-# INLINE newLogger #-}
newLogger logt = G.do
  Ur (logger,clean) <- liftSystemIOU (second liftSystemIO <$$> newFastLogger logt)
  pure (Ur (Logger logger 0), clean)

-- | Unconditionally log a message to the default logger
logI, logInfo :: (ToLogStr msg, HasLogger m) => msg -> m ()
logDebug, logD :: HasLogger m => LogStr -> m ()
{-# INLINE logInfo #-}
{-# INLINE logDebug #-}
{-# INLINE logD #-}
logI msg = getLogger >>= \(Ur logger) -> G.do
  let -- Log with preceeding unicode symbols
      leading_syms = Prelude.take (logger.depth*2) (Prelude.cycle ['│',' '])
      full_msg = toLogStr leading_syms <> toLogStr msg <> toLogStr "\n"
  liftSystemIO $
#ifndef THINGS_ARE_GOING_THAT_BAD
    logger.log full_msg
#else
    do BS.putStr (fromLogStr full_msg); !_ <- System.IO.hFlush System.IO.stdout; Prelude.return ()
#endif

-- | Log if debug level (@-DDEBUG@) is set
#ifdef DEBUG
logDebug = logI
#else
logDebug = const (pure ())
#endif

logInfo = logI
logD = logDebug

-- | Log and increase logging depth until action is left if debug level
-- (@-DDEBUG@) is set
enterD :: HasLogger m => LogStr -> m a ⊸ m a
{-# INLINE enterD #-}
#ifdef DEBUG
enterD msg ma = G.do
  () <- logD (toLogStr "Entering: " <> msg)
  !a <- withLevelUp ma
  () <- logD (toLogStr "Done.")
  pure a
#else
enterD _ x = x
#endif

-- | Log @message(show arg)@ and increase logging depth until action is left if debug level
-- (@-DDEBUG@) is set
enterDA :: HasLogger m => Show b => LogStr -> b -> m a ⊸ m a
{-# INLINE enterDA #-}
#ifdef DEBUG
enterDA msg arg ma = G.do
  () <- logD (toLogStr "Entering: " <> msg <> toLogStr ("(" <> show arg <> ")"))
  !a <- withLevelUp ma
  () <- logD (toLogStr "Done.")
  pure a
#else
enterDA _ _ x = x
#endif

-- | Log if trace level (@-DDEBUG_TRACE@) is set
logT :: HasLogger m => LogStr -> m ()
{-# INLINE logT #-}
#ifdef DEBUG_TRACE
logT = logI
#else
logT = const (pure ())
#endif
