{-# LANGUAGE PatternSynonyms #-}
module Ghengin.Component.Mesh.Vertex
  ( module Ghengin.Component.Mesh.Vertex
  , HList(..)
  ) where

import Foreign.Storable
import Foreign.Ptr
import Data.Kind
import qualified Data.Vector.Storable as SV
import Ghengin.Utils

data VertexArray (ts :: [Type]) where
  VertexArray :: SV.Vector (Vertex ts) -> VertexArray ts

data Vertex vs where
  SiVe :: v -> Vertex '[v]
  (:&) :: v -> Vertex (v':vs) -> Vertex (v:v':vs)
infixr 6 :&

pattern (:&:) :: v -> v' -> Vertex '[v, v']
pattern (:&:) a b = a :& SiVe b
infixr 6 :&:

instance Storable x => Storable (Vertex '[x]) where
  sizeOf _ = sizeOf @x undefined
  alignment _ = 0
  peek p = SiVe <$> peek (castPtr @(Vertex '[x]) @x p)
  poke p (SiVe a) = poke (castPtr @(Vertex '[x]) @x p) a

instance (Storable (Vertex (y:xs)), Storable x) => Storable (Vertex (x:y:xs)) where
  sizeOf _ = sizeOf @x undefined + sizeOf @(Vertex (y : xs)) undefined
  alignment _ = 0
  peek p = do
    a  <- peekByteOff (castPtr @(Vertex (x:y:xs)) @x p) 0
    as <- peekByteOff (castPtr @(Vertex (x:y:xs)) @(Vertex (y:xs)) p) (sizeOf a)
    pure $ a :& as
  poke p (a :& as) = do
    pokeByteOff (castPtr @(Vertex (x:y:xs)) @x p)          0 a
    pokeByteOff (castPtr @(Vertex (x:y:xs)) @(Vertex (y:xs)) p) (sizeOf a) as

instance Show x => Show (Vertex '[x]) where
  show (SiVe x) = show x

instance (Show (Vertex (y : xs)), Show x) => Show (Vertex (x : y : xs)) where
  show (x :& xs) = show x <> " :&: " <> show xs

