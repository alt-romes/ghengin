{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE QuantifiedConstraints #-}
module Ghengin.Core.Prelude
  (
  -- * Re-exports
    module Prelude.Linear
  , module Control.Functor.Linear
  , module Control.Monad.IO.Class.Linear
  , module System.IO.Linear
  , module Prelude

  -- base
  , Generic(..), NE.NonEmpty(..), Type, Constraint
  , Word32, IORef

  -- linear-base
  , bimap

  -- containers
  , IM.IntMap, M.Map, S.Set

  -- vector
  , V.Vector

  -- reference-counting
  , Aliasable, Forgettable, Shareable, SomeAlias(..)

  -- * Re-exports under different names
  , (<$$>)

  -- * Our own things
  , GHList(..)
  )
  where

import Prelude.Linear hiding ( IO, log
                             , Semigroup(..), Monoid(..), mappend, mconcat
                             )
import Control.Functor.Linear hiding (get,modify)
import qualified Control.Functor.Linear as Linear
import Control.Monad.IO.Class.Linear
import System.IO.Linear
import Prelude (Semigroup(..), Monoid(..), mappend, mconcat)
import qualified Prelude

import Data.Bifunctor.Linear (bimap)

import qualified Data.IntMap as IM
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V

import GHC.Generics
import Data.Kind
import Data.Word
import Data.IORef (IORef)

import Data.Unique (Unique)

import Data.Linear.Alias as Alias

import qualified Unsafe.Linear as Unsafe

-- Worry about performance of doing things safely later.
-- For now, simply strive for correctness.

-- | Unrestricted 'fmap' over unrestricted Functor.
-- Equivalent to Prelude.<$> over Prelude.Functor.
(<$$>) :: Prelude.Functor f => (a -> b) -> f a -> f b
{-# INLINE (<$$>) #-}
(<$$>) = (Prelude.<$>)

-- | Generic HList
-- Perhaps move to its own module?
data GHList c xs where
    GHNil :: GHList c '[]
    (:##) :: c a ⊸ GHList c as ⊸ GHList c (a ': as)
infixr 6 :##

-- GHList instances

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

-- TODO: Some special linear lenses to use propertyAt ... import Control.Lens ((^.), Lens', lens)
-- ROMES:TODO: For the lens to be used as a getter, I think we will need this definition of functor rather than the control one.
-- Otherwise, I think we can assume the lens is always over something which is
-- Consumable, (we only ever deal with properties which are consumable, btut I
-- suppose we could have properties which aren't, and are always updated).
-- Perhaps just the setter lens could be over the thing if it is Consumable

-- * Orphan instances

instance Consumable (M.Map Unique ()) where
  consume = Unsafe.toLinear $ \_ -> ()
