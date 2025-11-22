{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE CPP #-}
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
  -- , module Data.Unrestricted.Linear.Orphans

  -- base
  , Generic(..), NE.NonEmpty(..), Type, Constraint
  , Word32, Int32, IORef, KnownNat, Proxy(..)

  -- linear-base
  , UrT(..)
  -- linear vectors
  , VL.V(..)

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
  , unsafeUse
  )
  where

import Data.Proxy
import Data.Unrestricted.Linear
import GHC.TypeLits
-- Perhaps it would be better to re-export explicit modules instead of their prelude
import Prelude.Linear hiding ( IO, log
                             , Semigroup(..), Monoid(..), mappend, mconcat
                             , fst, snd
                             -- For now, we don't adhere to the "better Num" from linear base
                             , Num(..), FromInteger(..), Additive(..), AddIdentity(..), AdditiveGroup(..), Multiplicative(..), MultIdentity(..), Semiring, Ring, FromInteger(..)
                             , transpose
                             )
import Control.Functor.Linear hiding (get,modify)
import qualified Control.Functor.Linear as Linear
import Control.Monad.IO.Class.Linear
import System.IO.Linear
import Prelude (Semigroup(..), Monoid(..), mappend, mconcat
               -- For now, we don't adhere to the "better Num" from linear base
               , Num(..)
               )
import qualified Prelude

import Data.Tuple.Linear
import Data.Bifunctor.Linear (bimap)
import Data.Unrestricted.Linear.Orphans ()
import qualified Data.Functor.Linear as Data.Linear

import qualified Data.IntMap as IM
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.V.Linear.Internal as VL
import qualified Data.V.Linear.Internal.Instances ()

import GHC.Generics
import Data.Kind
import Data.Word
import Data.Int
import Data.IORef (IORef)

import Graphics.Gl.Block

import Data.Linear.Alias as Alias

import qualified Unsafe.Linear as Unsafe

--------------------------------------------------------------------------------
-- * Storable vector utils
--------------------------------------------------------------------------------

type SVector = SV.Vector

-- | Convert a list in a storable vector. Useful when creating meshes from lists
toSV :: SV.Storable a => [a] -> SV.Vector a
toSV = SV.fromList

--------------------------------------------------------------------------------

-- Worry about performance of doing things safely later.
-- For now, simply strive for correctness.

-- | Unrestricted 'fmap' over unrestricted Functor.
-- Equivalent to Prelude.<$> over Prelude.Functor.
(<$$>) :: Prelude.Functor f => (a -> b) -> f a -> f b
{-# INLINE (<$$>) #-}
(<$$>) = (Prelude.<$>)

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

instance (∀ a. Forgettable m (c a)) => Forgettable m (GHList c as) where
  forget GHNil = pure ()
  forget (a :## as) = Alias.forget a >> Alias.forget as

instance (∀ a. Shareable m (c a)) => Shareable m (GHList c as) where
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
