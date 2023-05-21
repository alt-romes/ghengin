{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Ghengin.Core.Type.Utils
  ( module Ghengin.Core.Type.Utils
  , module Data.Type.Map
  , module Data.Type.List
  , module Data.Type.Maybe
  ) where

import Prelude
import GHC.TypeLits
import Data.Proxy

import Data.Word (Word32)
import Data.Type.Map
import Data.Type.List hiding (Zip)
import Data.Type.Maybe

type (:<|>:) :: Maybe a -> Maybe a -> Maybe a
type family (:<|>:) mx my where
  (:<|>:) ('Just x) _ = 'Just x
  (:<|>:) 'Nothing ('Just y) = 'Just y
  (:<|>:) 'Nothing 'Nothing  = 'Nothing

type Zip :: [a] -> [b] -> [(a,b)]
type family Zip xs ys where
  Zip '[] _ = '[]
  Zip _ '[] = '[]
  Zip (a ': as) (b ': bs) = '(a, b) ': Zip as bs

type NumbersFromTo :: Nat -> Nat -> [Nat]
type family NumbersFromTo from to where
  NumbersFromTo to to = '[]
  NumbersFromTo from to = from ': NumbersFromTo (from+1) to

data Some f where
  Some :: f a %p -> Some f

nat :: ∀ m. KnownNat m => Int
nat = fromIntegral (natVal (Proxy @m))

w32 :: ∀ n. KnownNat n => Word32
w32 = fromInteger (natVal (Proxy @n))

