{-# LANGUAGE OverloadedStrings, DefaultSignatures #-}
module Planet.UI where

-- ToDo: introduce UI management abstraction in the
-- not-yet-existent:unrestricted-ghengin-monad which wraps Core.

import qualified Data.IORef as Base
import qualified Control.Monad as Base
import Ghengin.Core.Prelude as Linear
import Ghengin.Core.Render

import qualified Ghengin.DearImGui.Vulkan as ImGui
import Ghengin.DearImGui.UI

import Planet

-- This function is almost all generically derived from 'Planet', where we
-- always create a function: @forall a. a -> Core (Ur (a, Bool))@, where the
-- result is the new value of type @a@ and the bool indicates whether it has
-- changed. See `Widget`
preparePlanetUI :: Planet -> Renderer (Ur (Planet, Bool, Bool))
preparePlanetUI Planet{..} = Linear.do

  Ur changedShapeRef   <- liftIO (newIORef False)
  Ur newPlanetShapeRef <- liftIO (newIORef planetShape)

  Ur changedColorRef   <- liftIO (newIORef False)
  Ur newPlanetColorRef <- liftIO (newIORef planetColor)

  ImGui.withNewFrame $ do
    ImGui.withWindowOpen "Planet" $ do
      (p, b) <- widget planetShape
      Base.when b $ do
        Base.writeIORef changedShapeRef True
        Base.writeIORef newPlanetShapeRef p

      (p', b') <- widget planetColor
      Base.when b' $ do
        Base.writeIORef changedColorRef True
        Base.writeIORef newPlanetColorRef p'

  Ur newPlanetShape <- liftIO (readIORef newPlanetShapeRef)
  Ur didChangeShape <- liftIO (readIORef changedShapeRef)
  Ur newPlanetColor <- liftIO (readIORef newPlanetColorRef)
  Ur didChangeColor <- liftIO (readIORef changedColorRef)

  return $ Ur (Planet{planetColor=newPlanetColor, planetShape=newPlanetShape}, didChangeShape, didChangeColor)

