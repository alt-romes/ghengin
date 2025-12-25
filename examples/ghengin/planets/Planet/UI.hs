{-# LANGUAGE OverloadedStrings, DefaultSignatures #-}
module Planet.UI where

-- ToDo: introduce UI management abstraction in the
-- not-yet-existent:unrestricted-ghengin-monad which wraps Core.

import Data.IORef
import Control.Monad
import Control.Monad.IO.Class

-- ghengin:dear-imgui
import qualified Ghengin.DearImGui.Vulkan as ImGui
import Ghengin.DearImGui.UI

-- ghengin
import Ghengin.Monad

import Planet

-- This function is almost all generically derived from 'Planet', where we
-- always create a function: @forall a. a -> Core (Ur (a, Bool))@, where the
-- result is the new value of type @a@ and the bool indicates whether it has
-- changed. See `Widget`
preparePlanetUI :: Planet -> Ghengin (Planet, Bool, Bool)
preparePlanetUI Planet{..} = do

  changedShapeRef   <- liftIO (newIORef False)
  newPlanetShapeRef <- liftIO (newIORef planetShape)

  changedColorRef   <- liftIO (newIORef False)
  newPlanetColorRef <- liftIO (newIORef planetColor)

  -- ImGui.withNewFrame $ do
  --   -- ImGui.showIDStackToolWindow
  --   ImGui.withWindowOpen "Planet" $ do
  --     (p, b) <- widget planetShape
  --     when b $ do
  --       writeIORef changedShapeRef True
  --       writeIORef newPlanetShapeRef p
  --
  --     (p', b') <- widget planetColor
  --     when b' $ do
  --       writeIORef changedColorRef True
  --       writeIORef newPlanetColorRef p'

  newPlanetShape <- liftIO (readIORef newPlanetShapeRef)
  didChangeShape <- liftIO (readIORef changedShapeRef)
  newPlanetColor <- liftIO (readIORef newPlanetColorRef)
  didChangeColor <- liftIO (readIORef changedColorRef)

  return (Planet{planetColor=newPlanetColor, planetShape=newPlanetShape}, didChangeShape, didChangeColor)

