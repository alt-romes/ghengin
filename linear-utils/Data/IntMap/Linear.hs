{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Data.IntMap.Linear
  ( module Data.IntMap.Linear
  -- reexports
  , IM.empty
  ) where

import Prelude.Linear
import Data.IntMap.Internal (IntMap(..))
import qualified Data.IntMap as IM
import Unsafe.Linear

insert :: Int %p -> a ⊸ IntMap a ⊸ IntMap a
insert = toLinear3 IM.insert

elems :: IntMap a ⊸ [a]
elems = toLinear IM.elems

-- modified from containers
-- lookup :: Int -> IntMap a ⊸ (Maybe a, IntMap a)
-- lookup = undefined

instance Consumable (IM.IntMap ()) where
  consume = toLinear $ \_ -> ()
