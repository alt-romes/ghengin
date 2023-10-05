{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Unrestricted.Linear.Orphans where

import Prelude()
import Prelude.Linear
import Control.Monad.IO.Class.Linear
import Data.Unrestricted.Linear
import qualified Control.Monad.IO.Class as Base
import Data.Unique
import qualified Data.Map as M
import Data.Typeable

import Unsafe.Linear

instance Consumable (M.Map Unique ()) where
  consume = toLinear $ \_ -> ()

instance Consumable (M.Map TypeRep ()) where
  consume = toLinear $ \_ -> ()

instance Consumable (M.Map Unique [()]) where
  consume = toLinear $ \_ -> ()

instance MonadIO m => Base.MonadIO (UrT m) where
  liftIO io = UrT (liftSystemIOU io)

