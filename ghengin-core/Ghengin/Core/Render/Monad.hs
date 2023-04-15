{-# LANGUAGE LinearTypes, QualifiedDo, AllowAmbiguousTypes #-}
module Ghengin.Core.Render.Monad where

import Data.Kind

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Prelude ()
import Prelude.Linear
import Control.Functor.Linear as Linear
import Control.Monad.IO.Class.Linear as Linear
import qualified Unsafe.Linear as Unsafe

class Linear.MonadIO m => MonadRenderer m where

