{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
-- Using template Haskell will break build, because the dynlinker dependencies are not quite right.
-- I think this may be solved with my fix for vulkan-utils (https://github.com/expipiplus1/vulkan/pull/502)
-- I've fixed this locally by adding an rpath to dylib..., but it turns out
-- that we don't need template haskell because deriveGeneric is not supported
-- for existentials...
-- {-# LANGUAGE TemplateHaskell #-}
module Ghengin.Core.Mesh.Vertex
  ( Vertex(Sin,(:&),(:&:))
  ) where

import GHC.TypeNats
import Data.Kind
import Data.Proxy
import Graphics.Gl.Block
import Foreign.Ptr.Diff (Diff(..), peekDiffOff, pokeDiffOff)
import Foreign.Storable
import Prelude

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

-- It is kind of precarious to copy over the generic implementation here.
-- Would be better to implement Generic for Vertex, and derive generically Block.
-- May need this https://ryanglscott.github.io/2018/02/11/how-to-derive-generic-for-some-gadts/
-- But failed my attempts because I had to bring into scope type variables in the RHS of the type family and couldn't...

instance Block x => Block (Vertex '[x]) where
  type PackedSize (Vertex '[x]) = PackedSize x
  alignment140 _ = lcm 16 $ alignment140 (Proxy @x)
  alignment430 _ = alignment430 (Proxy @x)
  sizeOf140    _ = roundUp (sizeOf140 (Proxy @x)) (alignment140 (Proxy @x))
  sizeOf430    _ = roundUp (sizeOf430 (Proxy @x)) (alignment430 (Proxy @x))
  sizeOfPacked _ = sizeOfPacked (Proxy @x)
  read140 p (Diff o) = Sin <$> read140 p (Diff o)
  read430 p (Diff o) = Sin <$> read430 p (Diff o)
  readPacked p (Diff o) = Sin <$> readPacked p (Diff o)
  write140 p (Diff o) (Sin a) = write140 p (Diff o) a
  write430 p (Diff o) (Sin a) = write430 p (Diff o) a
  writePacked p (Diff o) (Sin a) = writePacked p (Diff o) a

instance (Block (Vertex (y:ys)), Block x) => Block (Vertex (x:y:ys)) where
  type PackedSize (Vertex (x ': y ': ys)) = PackedSize x + PackedSize (Vertex (y:ys))
  alignment140 _ = max (alignment140 (Proxy @(Vertex '[x]))) (alignment140 (Proxy @(Vertex (y:ys))))
  alignment430 _ = max (alignment430 (Proxy @(Vertex '[x]))) (alignment430 (Proxy @(Vertex (y:ys))))
  sizeOf140    _ = roundUp (sizeOf140 (Proxy @(Vertex '[x]))) (alignment140 (Proxy @(Vertex (y:ys))))
                   + sizeOf140 (Proxy @(Vertex (y:ys)))
  sizeOf430    _ = roundUp (sizeOf430 (Proxy @(Vertex '[x]))) (alignment430 (Proxy @(Vertex (y:ys))))
                   + sizeOf430 (Proxy @(Vertex (y:ys)))
  sizeOfPacked _ = sizeOfPacked (Proxy @(Vertex '[x])) + sizeOfPacked (Proxy @(Vertex (y:ys)))
  read140 p (Diff o) =
    (:&)
      <$> ((\case Sin x -> x) <$> read140 p (Diff @_ @(Vertex '[x]) o))
      <*> read140 p (Diff $ o + roundUp (sizeOf140 (Proxy @(Vertex '[x]))) (alignment140 (Proxy @(Vertex (y:ys)))))
  read430 p (Diff o) =
    (:&)
      <$> ((\case Sin x -> x) <$> read430 p (Diff @_ @(Vertex '[x]) o))
      <*> read430 p (Diff $ o + roundUp (sizeOf430 (Proxy @(Vertex '[x]))) (alignment430 (Proxy @(Vertex (y:ys)))))
  readPacked p (Diff o) =
    (:&)
      <$> ((\case Sin x -> x) <$> readPacked p (Diff @_ @(Vertex '[x]) o))
      <*> readPacked p (Diff $ o + sizeOfPacked (Proxy @(Vertex '[x])))
  write140 p (Diff o) (a :& b) = do
    write140 p (Diff o) (Sin a)
    write140 p (Diff $ o + roundUp (sizeOf140 (Proxy @(Vertex '[x]))) (alignment140 (Proxy @(Vertex (y:ys))))) b
  write430 p (Diff o) (a :& b) = do
    write140 p (Diff o) (Sin a)
    write140 p (Diff $ o + roundUp (sizeOf430 (Proxy @(Vertex '[x]))) (alignment430 (Proxy @(Vertex (y:ys))))) b
  writePacked p (Diff o) (a :& b) = do
    writePacked p (Diff o) (Sin a)
    writePacked p (Diff $ o + sizeOfPacked (Proxy @(Vertex '[x]))) b

-- deriving via () instance Storable (Vertex '[x])

instance Show x => Show (Vertex '[x]) where
  show (Sin x) = show x
  {-# INLINE show #-}

instance (Show (Vertex (y : xs)), Show x) => Show (Vertex (x : y : xs)) where
  show (x :& xs) = show x <> " :& " <> show xs
  {-# INLINE show #-}


