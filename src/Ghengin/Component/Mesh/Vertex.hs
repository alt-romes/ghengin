{-# LANGUAGE MagicHash #-}
module Ghengin.Component.Mesh.Vertex where

import Data.Kind
import GHC.Exts

-- TODO: benchmark with SmallArray# too.

type Vertex :: [Type] -> Type
newtype Vertex ls = Vertex { array# :: TYPE ('TupleRep (WhatRuntimeReps ls)) }

instance Show (Vertex a) where
  show _ = "show Vertex"

type family WhatRuntimeReps (ls :: [Type]) :: [RuntimeRep] where
  WhatRuntimeReps '[] = '[]
  WhatRuntimeReps ((x :: TYPE LiftedRep)': xs) = r ': WhatRuntimeReps xs
