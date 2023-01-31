{-# LANGUAGE PatternSynonyms #-}
module Ghengin.Component.Mesh.Vertex
  ( Vertex(Sin,(:&),(:&:))
  ) where

import Foreign.Ptr
import Foreign.Storable

data Vertex vs where

  -- | A vertex with a single property
  Sin :: v -> Vertex '[v]

  -- | Add a property to a vertex
  (:&) :: v -> Vertex (v':vs) -> Vertex (v:v':vs)

infixr 6 :&

-- | A vertex with two properties
pattern (:&:) :: v -> v' -> Vertex '[v, v']
pattern (:&:) a b = a :& Sin b
infixr 6 :&:

instance Storable x => Storable (Vertex '[x]) where
  sizeOf _ = sizeOf @x undefined
  alignment _ = 0
  peek p = Sin <$> peek (castPtr @(Vertex '[x]) @x p)
  poke p (Sin a) = poke (castPtr @(Vertex '[x]) @x p) a

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
  show (Sin x) = show x

instance (Show (Vertex (y : xs)), Show x) => Show (Vertex (x : y : xs)) where
  show (x :& xs) = show x <> " :& " <> show xs

