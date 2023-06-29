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
  , module Control.Monad.Trans
  , (.|.)
  , Proxy(..)
  ) where

import Prelude
import GHC.TypeLits
import Control.Monad
import Control.Monad.Trans
import Data.Kind
import Geomancy.Vec3
import Data.StateVar
import Foreign.Storable
import Foreign.Storable.Generic
import Data.Proxy

import Data.Bits

-- | Typed reference
-- newtype Ref α = Ref { unRef :: Apecs.Entity } -- iso to Int

data HList xs where
    HNil :: HList '[]
    (:#) :: a -> HList as -> HList (a ': as)
infixr 6 :#

{-
Note [Coerce HList to List]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
They have the same representation ^_^, so unsafeCoerce is safe ^_^
-}

headHList :: HList (a ': as) -> a
headHList (a :# _) = a

tailHList :: HList (a ': as) -> HList as
tailHList (_ :# as) = as

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

instance Show (HList '[]) where
  show _ = "HNil"

instance (Show (HList xs), Show x) => Show (HList (x ': xs)) where
  show (x :# xs) = show x <> " :# " <> show xs

