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

import Data.V.Linear.Internal (V(..))
import qualified Data.Vector as V
import qualified Prelude
import Data.Linear.Alias as Alias

import Prelude.Linear
import Control.Functor.Linear as Linear
import qualified Data.Functor.Linear as Data.Linear
import Control.Monad.IO.Class.Linear
import Data.IntMap.Internal (IntMap(..))
import qualified Data.IntMap.Internal as IM

import Data.IntSet.Internal (Key)

import Unsafe.Linear


insert :: Key %p -> a ⊸ IntMap a ⊸ IntMap a
insert = toLinear3 IM.insert

elems :: IntMap a ⊸ [a]
elems = toLinear IM.elems

toList :: IntMap a ⊸ [(Key,a)]
toList = toLinear IM.toList

fromList :: [(Key,a)] ⊸ IntMap a
fromList = toLinear IM.fromList

lookup :: Dupable a => Key %p -> IntMap a ⊸ Maybe a
lookup = toLinear2 \k m ->
  case IM.lookup k m of
    Nothing -> m `lseq` Nothing
    Just x  ->
      case dup x of
       (x1,x2) -> -- forget it, one of the copies will be consumed with the map
                  toLinear (\_ -> ()) x2 `lseq`
                  m `lseq` -- consume the map
                  Just x1

lookupM :: (MonadIO m, Shareable m a) => Key %p -> IntMap a ⊸ m (Maybe a, IntMap a)
lookupM = toLinear2 \k m ->
  -- We *unsafely* increment the reference because we also return
  -- `resourcemap` which retains one reference, which makes it *all work out safely*
  -- This is also why resourcemap is used twice (and hence is unsafe).
  --
  -- In short, we unsafely increment a linear resource for one we unsafely keep
  case IM.lookup k m of
    Nothing -> pure (Nothing, m)
    Just x  -> share x >>= toLinear \case (x1,_x2) -> pure (Just x1,m)

-- | \(O(n)\). Partition the map according to some predicate. The first
-- map contains all elements that satisfy the predicate, the second all
-- elements that fail the predicate.
--
-- > partitionWithKey (\ k _ -> k > 3) (fromList [(5,"a"), (3,"b")]) == (singleton 5 "a", singleton 3 "b")
-- > partitionWithKey (\ k _ -> k < 7) (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], empty)
-- > partitionWithKey (\ k _ -> k > 7) (fromList [(5,"a"), (3,"b")]) == (empty, fromList [(3, "b"), (5, "a")])
partitionByKeys :: V n Key %π -> IntMap a ⊸ (IntMap a, IntMap a)
partitionByKeys = toLinear2 \(V vec) im -> IM.partitionWithKey (\k _ -> k `V.elem` vec) im

unionWith :: (a ⊸ a ⊸ a) -> IntMap a ⊸ IntMap a ⊸ IntMap a
unionWith f = toLinear2 (IM.unionWith (toLinear2 f))

traverseWithKey :: Applicative t => (Key -> a ⊸ t b) -> IntMap a ⊸ t (IntMap b)
traverseWithKey f = toLinear go
  where
    go Nil = pure Nil
    go (Tip k v) = Tip k <$> f k v
    go (Bin p m l r)
      | m < 0     = liftA2 (flip (Bin p m)) (go r) (go l)
      | otherwise = liftA2 (Bin p m) (go l) (go r)
{-# INLINE traverseWithKey #-}


instance {-# OVERLAPPABLE #-} Consumable a => Consumable (IntMap a) where
  consume = \case
    Bin p m im1 im2 -> consume (p,m,im1,im2)
    Tip k a -> consume (k,a)
    Nil -> ()

-- Aren't these like actually unsafe, not just the implementation ? :)

instance Data.Linear.Functor IntMap where
  fmap f x = toLinear2 Prelude.fmap (toLinear f) x
  {-# INLINE fmap #-}

instance Data.Linear.Traversable IntMap where
  traverse :: ∀ t a b. Data.Linear.Applicative t => (a %1 -> t b) -> IntMap a %1 -> t (IntMap b)
  traverse f = go
    where
      go :: IntMap a %1 -> t (IntMap b)
      go IM.Nil = Data.Linear.pure IM.Nil
      go (IM.Tip k v) = toLinear (\k' -> IM.Tip k' Data.Linear.<$> f v) k
      go bin = toLinear (\(IM.Bin p m l r) ->
          if m < 0
             then Data.Linear.liftA2 (flip (IM.Bin p m)) (go r) (go l)
             else Data.Linear.liftA2 (IM.Bin p m) (go l) (go r)
        ) bin
  {-# INLINE traverse #-}

