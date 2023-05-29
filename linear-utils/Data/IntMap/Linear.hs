{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BlockArguments #-}
module Data.IntMap.Linear
  ( module Data.IntMap.Linear
  -- reexports
  , IM.empty
  ) where

import Data.Linear.Alias as Alias

import Data.Bits
import Prelude.Linear
import Control.Functor.Linear as Linear
import Control.Monad.IO.Class.Linear
import Data.IntMap.Internal (IntMap(..))
import qualified Data.IntMap.Internal as IM

import Data.IntSet.Internal (Key)
import qualified Data.IntSet.Internal as IntSet
import Utils.Containers.Internal.BitUtil

import Unsafe.Linear

insert :: Int %p -> a ⊸ IntMap a ⊸ IntMap a
insert = toLinear3 IM.insert

elems :: IntMap a ⊸ [a]
elems = toLinear IM.elems

lookupM :: (MonadIO m, Shareable m a) => Int %p -> IntMap a ⊸ m (Maybe a, IntMap a)
lookupM = toLinear2 \k m ->
  -- We *unsafely* increment the reference because we also return
  -- `resourcemap` which retains one reference, which makes it *all work out safely*
  -- This is also why resourcemap is used twice (and hence is unsafe).
  --
  -- In short, we unsafely increment a linear resource for one we unsafely keep
  case IM.lookup k m of
    Nothing -> pure (Nothing, m)
    Just x  -> share x >>= toLinear \case (x1,_x2) -> pure (Just x1,m)

instance Consumable (IM.IntMap ()) where
  consume = toLinear $ \_ -> ()

