{-# LANGUAGE OverloadedStrings, DefaultSignatures #-}
module Planet.UI where

-- ToDo: introduce UI management abstraction in the
-- not-yet-existent:unrestricted-ghengin-monad which wraps Core.

import qualified Prelude as Base
import qualified Data.IORef as Base
import qualified Control.Monad as Base
import Ghengin.Core
import Ghengin.Core.Prelude as Linear
import Ghengin.Core.Render
import Ghengin.Core.Render.Pipeline
import Ghengin.Core.Render.Queue

import qualified Ghengin.DearImGui.Vulkan as ImGui
import Ghengin.DearImGui.UI

import Planet

-- This function is almost all generically derived from 'Planet', where we
-- always create a function: @forall a. a -> Core (Ur (a, Bool))@, where the
-- result is the new value of type @a@ and the bool indicates whether it has
-- changed. See `Widget`
preparePlanetUI :: Planet -> Core (Ur (Planet, Bool))
preparePlanetUI planet = Linear.do

  Ur changedRef   <- liftIO (newIORef False)
  Ur newPlanetRef <- liftIO (newIORef planet)

  ImGui.withNewFrame $ do
    ImGui.withWindowOpen "Planet" $ do
      (p, b) <- widget planet
      Base.when b $ do
        Base.writeIORef changedRef True
        Base.writeIORef newPlanetRef p

  Ur newPlanet <- liftIO (readIORef newPlanetRef)
  Ur didChange <- liftIO (readIORef changedRef)

  return $ Ur (newPlanet, didChange)

