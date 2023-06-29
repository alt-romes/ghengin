{-# LANGUAGE TypeApplications, LinearTypes, QualifiedDo, NoImplicitPrelude, BlockArguments, UndecidableInstances #-}
module Ghengin.Apecs
  ( newEntity
  , newEntity_
  , UnsafeRenderer -- temp?
  , module Subset
  ) where

import qualified Prelude
import Prelude.Linear
import Control.Functor.Linear as Linear
import qualified Data.Functor.Linear as Data.Linear
import Control.Monad.IO.Class.Linear
import qualified Unsafe.Linear as Unsafe

import qualified Data.Functor
import qualified Control.Applicative
import qualified Control.Monad

import qualified Control.Monad.State as Mtl
import qualified Control.Monad.Reader as Mtl
import qualified GHC.IO

import Apecs as Subset hiding (newEntity, set, Set)
import qualified Apecs

import Ghengin.Core.Renderer.Kernel
import Unsafe.Coerce (unsafeCoerce)

newtype UnsafeRenderer a = UnsafeUnrestrictedRenderer (Mtl.StateT RendererEnv GHC.IO.IO a) deriving (Data.Functor.Functor, Control.Applicative.Applicative, Control.Monad.Monad, Mtl.MonadIO)

instance Data.Linear.Functor m => Data.Linear.Functor (SystemT w m) where
  fmap f (SystemT (Mtl.ReaderT w2m)) = SystemT $ Mtl.ReaderT \w -> f Data.Linear.<$> w2m w
  {-# INLINE fmap #-}

instance Functor m => Functor (SystemT w m) where
  fmap f (SystemT (Mtl.ReaderT w2m)) = SystemT $ Mtl.ReaderT \w -> f <$> w2m w
  {-# INLINE fmap #-}

type Get     w m c = (Has w m c, ExplGet     m (Storage c))
type Set     w m c = (Has w m c, ExplSet     m (Storage c))
type Members w m c = (Has w m c, ExplMembers m (Storage c))
type Destroy w m c = (Has w m c, ExplDestroy m (Storage c))

-- lol
-- instance (Has w Renderer c, Component c) => Has w UnsafeRenderer c where
--   getStore = unsafeCoerce @(SystemT w Renderer (Storage c)) @(SystemT w UnsafeRenderer (Storage c)) getStore

-- | Writes the given components to a new entity, and yields that entity.
-- The return value is often ignored.
{-# INLINE newEntity #-}
newEntity :: ∀ w c. (Set w UnsafeRenderer c, Get w UnsafeRenderer EntityCounter)
          => c -> SystemT w Renderer (Ur Entity)
newEntity c = Unsafe.toLinear Ur <$> unsafeCoerce @(SystemT w UnsafeRenderer Entity) @(SystemT w Renderer Entity) (Apecs.newEntity c)

set :: ∀ w m c. Set w m c => Entity -> c -> SystemT w m ()

-- cmapM :: forall w m cx cy. (Get w m cx, Set w m cy, Members w m cx) => (cx -> SystemT w m cy) -> SystemT w m ()

