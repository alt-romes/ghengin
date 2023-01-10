-- {-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE OverloadedRecordDot #-}
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
  , module Control.Logger.Simple
  , module Control.Monad.Trans
  , (.|.)
  , Proxy(..)
  ) where

import GHC.Records
import Data.IORef
import GHC.TypeLits
import Control.Monad
import Control.Monad.Trans
import Control.Logger.Simple
import Data.Kind
import Geomancy.Vec3
import Geomancy.Mat4
import Data.StateVar
import Foreign.Storable
import Foreign.Storable.Generic
import Data.Proxy

import Data.Bits

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

data HList xs where
    HNil :: HList '[]
    (:#) :: a -> HList as -> HList (a ': as)
infixr 6 :#

headHList :: HList (a ': as) -> a
headHList (a :# _) = a

tailHList :: HList (a ': as) -> HList as
tailHList (_ :# as) = as

type Size = Word

(.&&.) :: Bits a => a -> a -> Bool
x .&&. y = (/= zeroBits) (x .&. y)

whenM :: Monad m => m Bool -> m () -> m ()
whenM c t = (`when` t) =<< c

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

type family FromMaybe (a :: k) (m :: Maybe k) :: k where
  FromMaybe a 'Nothing = a
  FromMaybe _ ('Just a) = a

type family Concat (as :: [[k]]) :: [k] where
  Concat '[] = '[]
  Concat (x ': xs) = x ++ Concat xs

type family (++) as bs where
  (++) '[] bs = bs
  (++) (x ': xs) ys = x : xs ++ ys

type (!!) :: [Type] -> Nat -> Type
type family (!!) as n where
  (!!) as n = Index as n 0 (Text "Getting out of bounds index " :<>: ShowType n :<>: Text " of list " :<>: ShowType as)

type Index :: [Type] -> Nat -> Nat -> ErrorMessage -> Type
type family Index xs n m e where
  Index (x ': xs) n n _ = x
  Index (x ': xs) n m e = Index xs n (m+1) e
  Index '[] n m e       = TypeError e

type family Length α :: Nat where
  Length '[] = 0
  Length (_ ': as) = Length as + 1

type (:<|>:) :: Maybe a -> Maybe a -> Maybe a
type family (:<|>:) mx my where
  (:<|>:) ('Just x) _ = 'Just x
  (:<|>:) 'Nothing ('Just y) = 'Just y
  (:<|>:) 'Nothing 'Nothing  = 'Nothing

type family Reverse xs acc where
  Reverse '[] acc = acc
  Reverse (x ': xs) acc = Reverse xs (x ': acc)

type Zip :: [a] -> [b] -> [(a,b)]
type family Zip xs ys where
  Zip '[] _ = '[]
  Zip _ '[] = '[]
  Zip (a ': as) (b ': bs) = '(a, b) ': Zip as bs

type NumbersFromTo :: Nat -> Nat -> [Nat]
type family NumbersFromTo from to where
  NumbersFromTo to to = '[]
  NumbersFromTo from to = from ': NumbersFromTo (from+1) to


posFromMat4 :: Mat4 -> Vec3
posFromMat4 = flip withColMajor (\_ _ _ x _ _ _ y _ _ _ z _ _ _ _ -> vec3 x y z)


-- * Known nats

class KnownNats (ns :: [Nat]) where
  natVals :: [Int]
  natList :: NatList ns

instance KnownNats '[] where
  natVals = []
  natList = ØNL

instance (KnownNat n, KnownNats ns) => KnownNats (n ': ns) where
  natVals = fromIntegral (natVal (Proxy @n)) : natVals @ns
  natList = Proxy @n :<# natList @ns

data NatList :: [Nat] -> Type where
    ØNL   :: NatList '[]
    (:<#) :: (KnownNat n, KnownNats ns)
          => !(Proxy n) -> !(NatList ns) -> NatList (n ': ns)

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


incRefCount :: HasField "referenceCount" a (IORef Int) => MonadIO m => a -> m ()
incRefCount x = do
  liftIO $ atomicModifyIORef' x.referenceCount (\c -> (c+1,()))

decRefCount :: HasField "referenceCount" a (IORef Int) => MonadIO m => a -> m ()
decRefCount x = do
  liftIO $ atomicModifyIORef' x.referenceCount (\c -> (c-1,()))
