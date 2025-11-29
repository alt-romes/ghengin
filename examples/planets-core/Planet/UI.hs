{-# LANGUAGE OverloadedStrings #-}
module Planet.UI where

-- ToDo: introduce UI management abstraction in the
-- not-yet-existent:unrestricted-ghengin-monad which wraps Core.

import qualified Prelude
import Ghengin.Core
import Ghengin.Core.Prelude as Linear
import Ghengin.Core.Render
import Ghengin.Core.Render.Pipeline
import Ghengin.Core.Render.Queue

import qualified Ghengin.DearImGui.Vulkan as ImGui

import Planet

preparePlanetUI :: Planet -> Core (Ur (Planet, Bool))
preparePlanetUI Planet{..} = Linear.do

  Ur changedRef      <- liftIO (newIORef False)
  Ur planetRadiusRef <- liftIO (newIORef (planetRadius planetShape))

  ImGui.withNewFrame $ do
    ImGui.withWindowOpen "Planet" $ do

      ImGui.sliderFloat "Planet Radius" planetRadiusRef 0.1 10

      Prelude.pure ()
      

  Ur newPlanetRadius <- liftIO (readIORef planetRadiusRef)
  Ur didChange       <- liftIO (readIORef changedRef)

  return $ Ur (Planet{planetShape=planetShape{planetRadius=newPlanetRadius},..}, didChange)


