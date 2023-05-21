{-# LANGUAGE UndecidableInstances #-}
module Ghengin.Core.Type.Sized where

import GHC.TypeLits
import GHC.Float

import FIR.Prim.Struct
import qualified SPIRV.Image as SPIRV
import Math.Linear (V(..), M(..))
import Data.Type.Map

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

instance Sized ('SPIRV.ImageFormat c '[]) where
  type SizeOf ('SPIRV.ImageFormat _ '[]) = 0

instance Sized ('SPIRV.ImageFormat c ((x ': xs) :: [Nat])) where
  type SizeOf ('SPIRV.ImageFormat c (x ': xs)) = (x `Div` 8) + SizeOf ('SPIRV.ImageFormat c xs)
  -- Division by 8 because image format size is in bits

-- Depend on Geomancy in Core? Or be abstract over the matrix multiplication library?
-- instance Sized Vec3 where
--   type SizeOf Vec3 = 3 * SizeOf Float
