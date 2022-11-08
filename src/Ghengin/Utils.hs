{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
module Ghengin.Utils where

import Data.Kind

import Data.Bits

(.&&.) :: Bits a => a -> a -> Bool
x .&&. y = (/= zeroBits) (x .&. y)

-- | Returns the first element in a foldable structure for that the
-- monadic predicate holds true, and @Nothing@ if no such element
-- exists.
findM :: forall m t a. (Monad m, Foldable t)
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


