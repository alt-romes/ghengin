module Ghengin.Component.Mesh.Vertex
  ( module Ghengin.Component.Mesh.Vertex
  , HList(..)
  ) where

import Data.Kind
import qualified Data.Vector.Storable as SV
import Ghengin.Utils

data VertexArray (ts :: [Type]) where
  VertexArray :: SV.Vector (HList ts) -> VertexArray ts

