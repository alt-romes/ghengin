{-# OPTIONS_GHC -Wno-orphans #-}
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

import Data.Linear.Alias

import qualified Unsafe.Linear as Unsafe

-- Worry about performance of doing things safely later.
-- For now, simply strive for correctness.

-- | Unrestricted 'fmap' over unrestricted Functor.
-- Equivalent to Prelude.<$> over Prelude.Functor.
(<$$>) :: Prelude.Functor f => (a -> b) -> f a -> f b
{-# INLINE (<$$>) #-}
(<$$>) = (Prelude.<$>)

-- | Generic HList
data GHList c xs where
    GHNil :: GHList c '[]
    (:##) :: c a ⊸ GHList c as ⊸ GHList c (a ': as)
infixr 6 :##

-- * Orphan instances

instance Consumable (M.Map Unique ()) where
  consume = Unsafe.toLinear $ \_ -> ()

instance Consumable (IM.IntMap ()) where
  consume = Unsafe.toLinear $ \_ -> ()
