{-# LANGUAGE OverloadedRecordDot #-}
module Ghengin.World where

import GHC.Records
import Prelude.Linear
import qualified Prelude
import Control.Functor.Linear as Linear
import Apecs
import Ghengin.Core.Render.Packet (RenderPacket)
import Ghengin.Core.Render.Pipeline (SomePipeline)
import Ghengin.Core.Material (Material)
import Ghengin.Core.Type.Utils (Some)
import Ghengin.Component.Transform (Transform)
import Ghengin.Component.Transform.Animation (TransformAnimation)
import Ghengin.Component.Camera (Camera)
import Ghengin.Component.UI (UIWindow)
import Ghengin.Scene.Graph (ModelMatrix, Parent)
import System.Log.FastLogger
import qualified System.IO.Linear as Linear

import qualified Unsafe.Linear as Unsafe

data World w =
  -- IT seems really bad that these are unrestricted. Big ouch. Makes this
  -- linearity pretty much invalid in Ghengin (outside of Core).
  -- Perhaps we can still make a safe linear API around this?
  World { renderPackets   :: !((Storage RenderPacket))
        , renderPipelines :: !((Storage SomePipeline))
        , materials       :: !((Storage (Some Material)))
        , transforms      :: !((Storage Transform))
        , cameras         :: !((Storage Camera))
        , uiwindows       :: !((Storage (UIWindow w)))
        , modelMatrices   :: !((Storage ModelMatrix))
        , entityParents   :: !((Storage Parent))
        , entityCounter   :: !((Storage EntityCounter))
        , world           :: !w
        }

-- ROMES:TODO: This is really unsafe, and wrong. Get back to this ASAP
-- I'm not sure I like this World approach instances like this either. It's not
-- very much composable. Perhaps a type-list would be better
-- Also consider getting rid of linearity altogether from the Renderer upwards

instance Monad m => Has (World w) m RenderPacket where
  getStore = SystemT (asks (Unsafe.toLinear renderPackets))

instance Monad m => Has (World w) m SomePipeline where
  getStore = SystemT (asks (Unsafe.toLinear renderPipelines))

instance Monad m => Has (World w) m (Some Material) where
  getStore = SystemT (asks (Unsafe.toLinear materials))

instance Monad m => Has (World w) m Transform where
  getStore = SystemT (asks (Unsafe.toLinear transforms))

instance Monad m => Has (World w) m ModelMatrix where
  getStore = SystemT (asks (Unsafe.toLinear modelMatrices))

instance Monad m => Has (World w) m Camera where
  getStore = SystemT (asks (Unsafe.toLinear cameras))

instance Monad m => Has (World w) m (UIWindow w) where
  getStore = SystemT (asks (Unsafe.toLinear uiwindows))

instance Monad m => Has (World w) m Parent where
  getStore = SystemT (asks (Unsafe.toLinear entityParents))

instance Monad m => Has (World w) m EntityCounter where
  getStore = SystemT (asks (Unsafe.toLinear entityCounter))

instance (Monad m, Has w m (TransformAnimation w)) => Has (World w) m (TransformAnimation w) where
  getStore = SystemT $ Linear.do
    Ur w <- asks (Unsafe.toLinear $ Ur Prelude.. world)
    withReaderT (Unsafe.toLinear2 const w) $ unSystem getStore

-- Overlapping instances issues
-- instance (Monad m, Has w m c) => Has (World w) m c where
--   getStore = SystemT $ do
--     w <- asks (.world)
--     withReaderT (const w) $ unSystem getStore

-- instance HasField "logger" (World w) (FastLogger, Prelude.IO ())

