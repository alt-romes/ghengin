{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module BugRepro where

import GHC.TypeLits
import Data.Kind
import Data.Type.Bool

data C where
  CC :: Compatible' α β => A α -> B β -> C

data T2
data A s
data B s

class Sized α where
  type SizeOf α :: Nat

instance Sized T2 where
  type SizeOf T2 = 4

type family ListSize a where
  ListSize '[] = 0
  ListSize (_ ': xs) = 1 + ListSize xs

type family Compatible' α β :: Constraint where
  Compatible' a b = Compatible a b (ListSize a)

type family Compatible (α :: [Type]) (β :: [(Nat,Type)]) (n :: Nat) :: Constraint where
  Compatible '[] _ _ = ()
  Compatible (x ': xs) ys n = (SizeOf x ~ SizeOf (FromMaybe (TypeError (Text "err")) (Find (n-1) ys)), Compatible xs ys n)

type family Find n α :: Maybe Type where
  Find n '[] = 'Nothing
  Find n '[ '(n,x) ] = 'Just x
  Find n ( '(m,x) ': xs) = If (Match n m) ('Just x) (Find n xs)

type family Match x y where
  Match x x = 'True
  Match x y = 'False

type family FromMaybe a mb :: Type where
  FromMaybe a 'Nothing = a
  FromMaybe _ ('Just b) = b


