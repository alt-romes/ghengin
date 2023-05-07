{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QualifiedDo #-}
module Ghengin.World where

import Prelude.Linear
import qualified Prelude
import Control.Functor.Linear as Linear
import Data.Unrestricted.Linear
import Apecs.Linear
import Ghengin.Core.Render.Packet (RenderPacket)
import Ghengin.Core.Render.Pipeline (SomePipeline)
import Ghengin.Core.Material (SomeMaterial)
import Ghengin.Component.Transform (Transform)
import Ghengin.Component.Transform.Animation (TransformAnimation)
import Ghengin.Component.Camera (Camera)
import Ghengin.Component.UI (UIWindow)
import Ghengin.Scene.Graph (ModelMatrix, Parent)

import qualified Unsafe.Linear as Unsafe

data World w =
  World { renderPackets   :: !(Storage RenderPacket)
        , renderPipelines :: !(Storage SomePipeline)
        , materials       :: !(Storage SomeMaterial)
        , transforms      :: !(Storage Transform)
        , cameras         :: !(Storage Camera)
        , uiwindows       :: !(Storage (UIWindow w))
        , modelMatrices   :: !(Storage ModelMatrix)
        , entityParents   :: !(Storage Parent)
        , entityCounter   :: !(Storage EntityCounter)
        , world           :: !w
        }

instance Consumable w => Consumable (World w) where
  consume = Unsafe.toLinear $ \_ -> ()
    -- ROMES:TODO: Get back to this. If everything is reference counted then
    -- a valid instance is possible.
    -- Must instance Dupable (RefCounted X) and Dupable for all Stores in
    -- linear-apecs

instance Dupable w => Dupable (World w) where
  dup2 = Unsafe.toLinear $ \w -> (w,w)


-- TODO: Can I move these out so that there are no cyclic dependencies yet we
-- can still use the instances?


-- ROMES:TODO: This is really unsafe, and wrong. Get back to this ASAP
-- I'm not sure I like this World approach instances like this either. It's not
-- very much composable. Perhaps a type-list would be better
-- Also consider getting rid of linearity altogether from the Renderer upwards

instance Monad m => Has (World w) m RenderPacket where
  getStore = SystemT (asks (Unsafe.toLinear $ Ur Prelude.. renderPackets))

instance Monad m => Has (World w) m SomePipeline where
  getStore = SystemT (asks (Unsafe.toLinear $ Ur Prelude.. renderPipelines))

instance Monad m => Has (World w) m SomeMaterial where
  getStore = SystemT (asks (Unsafe.toLinear $ Ur Prelude.. materials))

instance Monad m => Has (World w) m Transform where
  getStore = SystemT (asks (Unsafe.toLinear $ Ur Prelude.. transforms))

instance Monad m => Has (World w) m ModelMatrix where
  getStore = SystemT (asks (Unsafe.toLinear $ Ur Prelude.. modelMatrices))

instance Monad m => Has (World w) m Camera where
  getStore = SystemT (asks (Unsafe.toLinear $ Ur Prelude.. cameras))

instance Monad m => Has (World w) m (UIWindow w) where
  getStore = SystemT (asks (Unsafe.toLinear $ Ur Prelude.. uiwindows))

instance Monad m => Has (World w) m Parent where
  getStore = SystemT (asks (Unsafe.toLinear $ Ur Prelude.. entityParents))

instance Monad m => Has (World w) m EntityCounter where
  getStore = SystemT (asks (Unsafe.toLinear $ Ur Prelude.. entityCounter))

instance (Dupable w, Monad m, Has w m (TransformAnimation w)) => Has (World w) m (TransformAnimation w) where
  getStore = SystemT $ Linear.do
    Ur w <- asks (Unsafe.toLinear $ Ur Prelude.. world)
    withReaderT (Unsafe.toLinear2 const w) $ unSystem getStore

-- Overlapping instances issues
-- instance (Monad m, Has w m c) => Has (World w) m c where
--   getStore = SystemT $ do
--     w <- asks (.world)
--     withReaderT (const w) $ unSystem getStore

