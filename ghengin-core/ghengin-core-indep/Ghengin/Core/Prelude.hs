{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE QuantifiedConstraints #-}
module Ghengin.Core.Prelude
  (
  -- * Re-exports
    module Prelude.Linear
  , module Data.Tuple.Linear
  , module Data.Bifunctor.Linear
  , module Control.Functor.Linear
  , module Control.Monad.IO.Class.Linear
  , module System.IO.Linear
  , module Prelude
  , module Data.Finite
  -- , module Data.Unrestricted.Linear.Orphans

  -- base
  , Generic(..), NE.NonEmpty(..), Type, Constraint
  , Word32, Int32, IORef, KnownNat, Proxy(..)

  -- linear-base
  , UrT(..)
  , withResource
  -- linear vectors
  , VL.V(..)
  , withSized
  , genSizedM
  , SomeV(..)
  , focusV
  , consumeV

  -- containers
  , IM.IntMap, M.Map, S.Set

  -- vector
  , V.Vector
  , toSV
  , SVector

  -- reference-counting
  , Forgettable, Shareable

  -- gl-block
  , Block(..)

  -- * Re-exports under different names
  , (<$$>)
  -- ** With multiplicity generalization
  , vmap, vtraverse

  -- * Our own things
  , GHList(..), (=<<), (<=<), (>=>), v2vec, l2vec, vec2l

  , vzipWith

  , assertM
  , expectLeft
  , unsafeUse
  )
  where

import GHC.Stack
import Control.Functor.Linear        hiding ( get, modify )
import Control.Functor.Linear        qualified as Linear
import Control.Monad.IO.Class.Linear

import Data.Finite
import Data.Bifunctor.Linear            ( bimap )
import Data.Functor.Linear              qualified as Data.Linear
import Data.Int
import Data.IntMap                      qualified as IM
import Data.IORef                       ( IORef )
import Data.Kind
import Data.Linear.Alias                as Alias
import Data.List.NonEmpty               qualified as NE
import Data.Map                         qualified as M
import Data.Proxy
import Data.Set                         qualified as S
import Data.Tuple.Linear
import Data.Unrestricted.Linear
import Data.Unrestricted.Linear.Orphans ()
import Data.V.Linear.Internal           qualified as VL
import Data.V.Linear.Internal.Instances qualified ()
import Data.Vector                      qualified as V
import Data.Vector.Storable             qualified as SV
import Data.Word

import GHC.Generics
import GHC.Records
import GHC.TypeLits

import Geomancy.Vec2 ( Vec2, pattern WithVec2 )
import Geomancy.Vec3 ( Vec3, pattern WithVec3 )
import Geomancy.Vec4 ( Vec4, pattern WithVec4 )

import Graphics.Gl.Block

import Prelude        ( Monoid (..), Num (..), Semigroup (..), mappend,
                        mconcat )
import Prelude qualified
import Prelude.Linear hiding ( AddIdentity (..), Additive (..),
                        AdditiveGroup (..), FromInteger (..), IO, Monoid (..),
                        MultIdentity (..), Multiplicative (..), Num (..), Ring,
                        Semigroup (..), Semiring, fst, log, mappend, mconcat,
                        snd, transpose )

import System.IO.Linear

import Unsafe.Linear qualified as Unsafe

--------------------------------------------------------------------------------
-- * Linear utilities
--------------------------------------------------------------------------------

-- | Thread a resource through a computation
withResource :: res %1 -> (StateT res m a) %1 -> m (a, res)
withResource res use = runStateT use res

--------------------------------------------------------------------------------
-- * Vec2, Vec3, Vec4 accessors
--------------------------------------------------------------------------------

instance HasField "x" Vec2 Float where
  getField (WithVec2 x _) = x

instance HasField "y" Vec2 Float where
  getField (WithVec2 _ y) = y

instance HasField "x" Vec3 Float where
  getField (WithVec3 x _ _) = x

instance HasField "y" Vec3 Float where
  getField (WithVec3 _ y _) = y

instance HasField "z" Vec3 Float where
  getField (WithVec3 _ _ z) = z

instance HasField "x" Vec4 Float where
  getField (WithVec4 x _ _ _) = x

instance HasField "y" Vec4 Float where
  getField (WithVec4 _ y _ _) = y

instance HasField "z" Vec4 Float where
  getField (WithVec4 _ _ z _) = z

instance HasField "w" Vec4 Float where
  getField (WithVec4 _ _ _ w) = w

--------------------------------------------------------------------------------
-- * Vector/V utils
--------------------------------------------------------------------------------

type SVector = SV.Vector

-- | Convert a list in a storable vector. Useful when creating meshes from lists
toSV :: SV.Storable a => [a] -> SV.Vector a
toSV = SV.fromList

withSized :: forall a r. V.Vector a -> (forall n. KnownNat n => VL.V n a -> r) -> r
withSized v f = case someNatVal (fromIntegral (V.length v)) of
  Just (SomeNat (Proxy :: Proxy n)) -> f (VL.V v :: VL.V n a)
  Nothing -> error "impossible: Vector has negative length"

genSizedM
  :: forall n m a.
     (KnownNat n, Monad m)
  => (Int -> m a)
  -> m (VL.V n a)
genSizedM mk =
    go (VL.theLength @n)
    >>= Unsafe.toLinear (\v -> return (VL.V (V.fromList v)))
  where
    go :: Int -> m [a]
    go 0 = pure []
    go i = Linear.do
      as <- go (i-1)
      a  <- mk (i-1{-0-indexed-})
      pure (a:as)

-- | Existential linear sized vectors
data SomeV a = forall n. KnownNat n => SomeV (VL.V n a)

-- | Focus on an element in the Vec and return an action that reconstructs the
-- original vector with a potentially updated element at the same position.
focusV :: KnownNat n => Finite n -> VL.V n a %1 -> (a, a %1 -> VL.V n a)
focusV f = Unsafe.toLinear \(VL.V v) -> (v V.! i, Unsafe.toLinear \a -> VL.V (v `V.unsafeUpd` [(i, a)]))
  where
    i = fromIntegral (getFinite f)

-- | Consume sized vector. The overloaded instance is ambiguous.
consumeV :: KnownNat n => VL.V n () %1 -> ()
consumeV (VL.V v) = consume (Unsafe.toLinear V.toList v)

--------------------------------------------------------------------------------

-- Worry about performance of doing things safely later.
-- For now, simply strive for correctness.

-- | Unrestricted 'fmap' over unrestricted Functor.
-- Equivalent to Prelude.<$> over Prelude.Functor.
(<$$>) :: Prelude.Functor f => (a -> b) -> f a -> f b
{-# INLINE (<$$>) #-}
(<$$>) = (Prelude.<$>)

expectLeft :: HasCallStack => String -> Either a b %1 -> a
expectLeft _ (Left a)  = a
expectLeft s (Right r) = error s r

--------------------------------------------------------------------------------
-- * Generic HList (aka "Product"), but this one is linear!
--------------------------------------------------------------------------------

-- | Generic HList
-- Perhaps move to its own module?
data GHList c xs where
    GHNil :: GHList c '[]
    (:##) :: c a ⊸ GHList c as ⊸ GHList c (a ': as)
infixr 6 :##

instance Consumable (GHList c '[]) where
  consume GHNil = ()

instance Dupable (GHList c '[]) where
  dup2 GHNil = (GHNil, GHNil)

instance (Consumable (c a), Consumable (GHList c as)) => Consumable (GHList c (a:as)) where
  consume (a :## as) = a `lseq` consume as

instance (Dupable (c a), Dupable (GHList c as)) => Dupable (GHList c (a:as)) where
  dup2 (a :## as) = case dup2 a of
                      (a1,a2) -> case dup2 as of
                                   (as1, as2) -> (a1:##as1, a2:##as2)

instance (forall a. Forgettable m (c a)) => Forgettable m (GHList c as) where
  forget GHNil      = pure ()
  forget (a :## as) = Alias.forget a >> Alias.forget as

instance (forall a. Shareable m (c a)) => Shareable m (GHList c as) where
  share GHNil = pure (GHNil, GHNil)
  share (a :## as) = Linear.do
    (a1,a2)    <- Alias.share a
    (as1, as2) <- Alias.share as
    pure (a1:##as1, a2:##as2)

--------------------------------------------------------------------------------

(=<<) :: Monad m => (a ⊸ m b) ⊸ m a ⊸ m b
f =<< x = x >>= f
{-# INLINE (=<<) #-}

(<=<) :: Monad m => (b ⊸ m c) ⊸ (a ⊸ m b) ⊸ a ⊸ m c
(<=<) = flip (>=>)
{-# INLINE (<=<) #-}

(>=>) :: Monad m => (a ⊸ m b) ⊸ (b ⊸ m c) ⊸ a ⊸ m c
f >=> g = \x -> f x >>= g
{-# INLINE (>=>) #-}

-- | 'map' but this is polymorphic in the multiplicity (for some reason the default isn't)
vmap :: (a %p -> b) -> VL.V n a %p -> VL.V n b
vmap f (VL.V xs) = VL.V $ Unsafe.toLinear (V.map (\x -> f x)) xs

-- | Like 'Data.Linear.traverse', but polymorphic multiplicity (for some reason, not the default)
vtraverse :: (KnownNat n, Applicative f) => (a %p -> f b) -> VL.V n a %p -> f (VL.V n b)
vtraverse = Unsafe.toLinear2 Data.Linear.traverse . Unsafe.toLinear
          -- I really think this is safe, for these vectors at least.
          -- If we consume the $a$s linearly, we consume V linearly
          -- If we consume $a$s unrestrictedly, we consume V unrestrictedly...

vzipWith :: (a %p -> b %p -> c) -> VL.V n a %p -> VL.V n b %p -> VL.V n c
vzipWith f (VL.V va) (VL.V vb) = VL.V (Unsafe.toLinear3 V.zipWith (Unsafe.toLinear2 f) va vb)

v2vec :: VL.V n a ⊸ V.Vector a
v2vec (VL.V v) = v

l2vec :: [a] ⊸ V.Vector a
l2vec = Unsafe.toLinear V.fromList

vec2l :: V.Vector a ⊸ [a]
vec2l = Unsafe.toLinear V.toList

assertM :: Monad m => String -> Bool -> m ()
{-# INLINE assertM #-}
#ifdef DEBUG
assertM s b = if b then pure () else error ("Failed assertion: " ++ s)
#else
assertM _ _ = pure ()
#endif

unsafeUse :: Monad m => a ⊸ (a -> m ()) %l -> m a
unsafeUse x f = Unsafe.toLinear (\y -> f y >> pure y) x

--- More orphans

instance MonadIO m => MonadIO (StateT s m) where
  liftIO io = StateT $ \s -> (,s) <$> liftIO io
