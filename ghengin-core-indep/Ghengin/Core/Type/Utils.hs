{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Ghengin.Core.Type.Utils
  ( module Ghengin.Core.Type.Utils
  -- , module Data.Type.Map
  -- , module Data.Type.List
  ) where

import Prelude
import GHC.TypeLits
import Data.Proxy

import Data.Word (Word32)
import Math.Linear (V(..), M(..))
import FIR.Prim.Struct
import qualified SPIRV.Image as SPIRV
import Data.Type.Map
-- import Data.Type.List hiding (Zip)

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

-- TODO Eventually move to its own module?
class Sized a where
  type SizeOf a :: Nat

instance Sized Float where
  type SizeOf Float = 4

instance Sized a => Sized (V n a) where
  type SizeOf (V n a) = n * SizeOf a

instance Sized a => Sized (M m n a) where
  type SizeOf (M m n a) = m * n * SizeOf a

instance Sized (Struct '[]) where
  type SizeOf (Struct '[]) = 0

instance Sized a => Sized (Struct ((k ':-> a) ': xs)) where
  type SizeOf (Struct ((_ ':-> a) ': xs)) = SizeOf a + SizeOf (Struct xs)

-- Depend on Geomancy in Core?
-- instance Sized Vec3 where
--   type SizeOf Vec3 = 3 * SizeOf Float

instance Sized ('SPIRV.ImageFormat c '[]) where
  type SizeOf ('SPIRV.ImageFormat _ '[]) = 0

instance Sized ('SPIRV.ImageFormat c ((x ': xs) :: [Nat])) where
  type SizeOf ('SPIRV.ImageFormat c (x ': xs)) = (x `Div` 8) + SizeOf ('SPIRV.ImageFormat c xs)
  -- Division by 8 because image format size is in bits

nat :: ∀ m. KnownNat m => Int
nat = fromIntegral (natVal (Proxy @m))

w32 :: ∀ n. KnownNat n => Word32
w32 = fromInteger (natVal (Proxy @n))

data Some f where
  Some :: f a %p -> Some f


