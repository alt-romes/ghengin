{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
module Ghengin.Utils
  ( module Ghengin.Utils
  , module Data.StateVar
  , module Foreign.Storable
  , module Foreign.Storable.Generic
  , module Data.Hashable
  , module Control.Logger.Simple
  , (.|.)
  , Proxy(..)
  ) where

import Control.Logger.Simple
import Data.Hashable
import Data.Kind
import Geomancy.Vec3
import Data.StateVar
import Foreign.Storable
import Foreign.Storable.Generic
import Data.Proxy

import Data.Bits

import GHC.TypeLits

import Math.Linear (V(..))
import Data.Type.Map
import FIR.Prim.Struct

-- TODO Eventually move to its own module
class Sized a where
  type SizeOf a :: Nat

instance Sized Float where
  type SizeOf Float = 4

instance Sized a => Sized (V n a) where
  type SizeOf (V n a) = n * SizeOf a

instance Sized (Struct '[]) where
  type SizeOf (Struct '[]) = 0

instance Sized a => Sized (Struct ((k ':-> a) ': xs)) where
  type SizeOf (Struct ((_ ':-> a) ': xs)) = SizeOf a + SizeOf (Struct xs)

instance Sized Vec3 where
  type SizeOf Vec3 = 3 * SizeOf Float

instance Hashable Vec3 where
  hashWithSalt i (WithVec3 x y z) = hashWithSalt i (x,y,z)

data HList xs where
    HNil :: HList '[]
    (:#) :: a -> HList as -> HList (a ': as)

infixr 6 :#

type Size = Word

(.&&.) :: Bits a => a -> a -> Bool
x .&&. y = (/= zeroBits) (x .&. y)

-- | Returns the first element in a foldable structure for that the
-- monadic predicate holds true, and @Nothing@ if no such element
-- exists.
findM :: ∀ m t a. (Monad m, Foldable t)
      => (a -> m Bool) -> t a -> m (Maybe a)
findM p = foldr go (pure Nothing)
  where
    go :: a -> m (Maybe a) -> m (Maybe a)
    go x acc = do
      b <- p x
      if b then pure (Just x) else acc

data EnginePart
  = EInstance
  | EPhysicalDevice
  | EDevice
  | EGraphicsQueue 

type family HasPart (p :: EnginePart) (ps :: [EnginePart]) :: Bool where
  HasPart _ '[] = False
  HasPart p (p ': xs) = True
  HasPart p (_ ': xs) = HasPart p xs

type family (:=>) (b :: Bool) (t :: Type) :: Type where
  (:=>) False t = ()
  (:=>) True  t = t

type family FromMaybe (a :: k) (m :: Maybe k) :: k where
  FromMaybe a 'Nothing = a
  FromMaybe _ ('Just a) = a

type family Concat (as :: [[k]]) :: [k] where
  Concat '[] = '[]
  Concat (x ': xs) = x ++ Concat xs

type family (++) as bs where
  (++) '[] bs = bs
  (++) (x ': xs) ys = x : xs ++ ys

-- | Provides a fairly subjective test to see if a quantity is near zero.
--
-- >>> nearZero (1e-11 :: Double)
-- False
--
-- >>> nearZero (1e-17 :: Double)
-- True
--
-- >>> nearZero (1e-5 :: Float)
-- False
--
-- >>> nearZero (1e-7 :: Float)
-- True
class Num a => Epsilon a where
  -- | Determine if a quantity is near zero.
  nearZero :: a -> Bool

-- | @'abs' a '<=' 1e-6@
instance Epsilon Float where
  nearZero a = abs a <= 1e-6

-- | @'abs' a '<=' 1e-12@
instance Epsilon Double where
  nearZero a = abs a <= 1e-12

instance Epsilon Vec3 where
  nearZero a = nearZero (dot a a)
