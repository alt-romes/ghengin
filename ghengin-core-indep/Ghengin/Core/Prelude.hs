module Ghengin.Core.Prelude
  ( module Prelude.Linear
  , module Control.Functor.Linear
  , module Control.Monad.IO.Class.Linear
  , module System.IO.Linear
  )
  where

import Prelude.Linear hiding (IO, log)
import Control.Functor.Linear hiding (get,modify)
import Control.Monad.IO.Class.Linear
import System.IO.Linear

