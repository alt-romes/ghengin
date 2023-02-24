{-# LANGUAGE OverloadedRecordDot #-}
module Ghengin.World where

import Control.Monad.Reader
import Apecs
import Ghengin.Core.Render.Packet (RenderPacket)
import Ghengin.Core.Render.Pipeline (SomePipeline)
import Ghengin.Core.Material (SomeMaterial)
import Ghengin.Component.Transform (Transform)
import Ghengin.Component.Transform.Animation (TransformAnimation)
import Ghengin.Component.Camera (Camera)
import Ghengin.Component.UI (UIWindow)
import Ghengin.Scene.Graph (ModelMatrix, Parent)

data World w =
  World { renderPackets :: !(Storage RenderPacket)
        , renderPipelines :: !(Storage SomePipeline)
        , materials     :: !(Storage SomeMaterial)
        , transforms    :: !(Storage Transform)
        , cameras       :: !(Storage Camera)
        , uiwindows     :: !(Storage (UIWindow w))
        , modelMatrices :: !(Storage ModelMatrix)
        , entityParents :: !(Storage Parent)
        , entityCounter :: !(Storage EntityCounter)
        , world         :: !w
        }


-- TODO: Can I move these out so that there are no cyclic dependencies yet we
-- can still use the instances?

instance Monad m => Has (World w) m RenderPacket where
  getStore = SystemT (asks (.renderPackets))

instance Monad m => Has (World w) m SomePipeline where
  getStore = SystemT (asks (.renderPipelines))

instance Monad m => Has (World w) m SomeMaterial where
  getStore = SystemT (asks (.materials))

instance Monad m => Has (World w) m Transform where
  getStore = SystemT (asks (.transforms))

instance Monad m => Has (World w) m ModelMatrix where
  getStore = SystemT (asks (.modelMatrices))

instance Monad m => Has (World w) m Camera where
  getStore = SystemT (asks (.cameras))

instance Monad m => Has (World w) m (UIWindow w) where
  getStore = SystemT (asks (.uiwindows))

instance Monad m => Has (World w) m Parent where
  getStore = SystemT (asks (.entityParents))

instance Monad m => Has (World w) m EntityCounter where
  getStore = SystemT (asks (.entityCounter))

instance (Monad m, Has w m (TransformAnimation w)) => Has (World w) m (TransformAnimation w) where
  getStore = SystemT $ do
    w <- asks (.world)
    withReaderT (const w) $ unSystem getStore

-- Overlapping instances issues
-- instance (Monad m, Has w m c) => Has (World w) m c where
--   getStore = SystemT $ do
--     w <- asks (.world)
--     withReaderT (const w) $ unSystem getStore


