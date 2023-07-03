{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BlockArguments #-}
module Data.Map.Linear
  ( module Data.Map.Linear
  -- reexports
  , M.empty
  ) where

import Data.V.Linear.Internal (V(..))
import qualified Data.Vector as V
import qualified Prelude
import Data.Linear.Alias as Alias

import Prelude.Linear
import Control.Functor.Linear as Linear
import qualified Data.Functor.Linear as Data.Linear
import Control.Monad.IO.Class.Linear
import Data.Map.Internal (Map(..))
import qualified Data.Map.Internal as M

import Unsafe.Linear

-- | \(O(n+m)\). An unsafe general combining function.
--
-- WARNING: This function can produce corrupt maps and its results
-- may depend on the internal structures of its inputs. Users should
-- prefer 'merge' or 'mergeA'.
--
-- When 'mergeWithKey' is given three arguments, it is inlined to the call
-- site. You should therefore use 'mergeWithKey' only to define custom
-- combining functions. For example, you could define 'unionWithKey',
-- 'differenceWithKey' and 'intersectionWithKey' as
--
-- > myUnionWithKey f m1 m2 = mergeWithKey (\k x1 x2 -> Just (f k x1 x2)) id id m1 m2
-- > myDifferenceWithKey f m1 m2 = mergeWithKey f id (const empty) m1 m2
-- > myIntersectionWithKey f m1 m2 = mergeWithKey (\k x1 x2 -> Just (f k x1 x2)) (const empty) (const empty) m1 m2
--
-- When calling @'mergeWithKey' combine only1 only2@, a function combining two
-- 'Map's is created, such that
--
-- * if a key is present in both maps, it is passed with both corresponding
--   values to the @combine@ function. Depending on the result, the key is either
--   present in the result with specified value, or is left out;
--
-- * a nonempty subtree present only in the first map is passed to @only1@ and
--   the output is added to the result;
--
-- * a nonempty subtree present only in the second map is passed to @only2@ and
--   the output is added to the result.
--
-- The @only1@ and @only2@ methods /must return a map with a subset (possibly empty) of the keys of the given map/.
-- The values can be modified arbitrarily. Most common variants of @only1@ and
-- @only2@ are 'id' and @'const' 'empty'@, but for example @'map' f@,
-- @'filterWithKey' f@, or @'mapMaybeWithKey' f@ could be used for any @f@.

mergeWithKey :: Ord k
             => (k -> a ⊸ b ⊸ Maybe c)
             -> (Map k a ⊸ Map k c)
             -> (Map k b ⊸ Map k c)
             -> Map k a ⊸ Map k b ⊸ Map k c
mergeWithKey f g h = toLinearN @5 mergeWithKey (toLinear3 f) (toLinear g) (toLinear h)

-- | \(O(n)\).
-- @'traverseWithKey' f m == 'fromList' \<$\> 'traverse' (\\(k, v) -> (,) k \<$\> f k v) ('toList' m)@
-- That is, behaves exactly like a regular 'traverse' except that the traversing
-- function also has access to the key associated with a value.
--
-- > traverseWithKey (\k v -> if odd k then Just (succ v) else Nothing) (fromList [(1, 'a'), (5, 'e')]) == Just (fromList [(1, 'b'), (5, 'f')])
-- > traverseWithKey (\k v -> if odd k then Just (succ v) else Nothing) (fromList [(2, 'c')])           == Nothing
traverseWithKey :: Applicative t => (k -> a ⊸ t b) -> Map k a ⊸ t (Map k b)
traverseWithKey f = toLinear go
  where
    go Tip = pure Tip
    go (Bin 1 k v _ _) = (\v' -> Bin 1 k v' Tip Tip) <$> f k v
    go (Bin s k v l r) = (flip (Bin s k)) <$> (go l) <*> (f k v) <*> (go r)
{-# INLINE traverseWithKey #-}

instance Data.Linear.Functor (Map k) where
  fmap f = toLinear2 M.map (toLinear f)
  {-# INLINE fmap #-}

-- | Traverses in order of increasing key.
instance Data.Linear.Traversable (Map k) where
  traverse f = traverseWithKey (\_ -> f)
  {-# INLINE traverse #-}
