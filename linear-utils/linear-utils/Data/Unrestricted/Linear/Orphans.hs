{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Unrestricted.Linear.Orphans where

import Prelude.Linear
import Data.Unique
import qualified Data.Map as M

import Unsafe.Linear

instance Consumable (M.Map Unique ()) where
  consume = toLinear $ \_ -> ()



