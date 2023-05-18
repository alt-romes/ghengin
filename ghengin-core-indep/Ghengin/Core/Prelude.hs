module Ghengin.Core.Prelude
  ( module Prelude.Linear
  , module Control.Functor.Linear
  , module Control.Monad.IO.Class.Linear
  , module System.IO.Linear
  , module Prelude
  )
  where

import Prelude.Linear hiding (IO, log, Semigroup(..), Monoid(..), mappend, mconcat)
import Control.Functor.Linear hiding (get,modify)
import Control.Monad.IO.Class.Linear
import System.IO.Linear
import Prelude (Semigroup(..), Monoid(..), mappend, mconcat)
