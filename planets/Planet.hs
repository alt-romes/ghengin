{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}
module Planet where

import Control.Monad

import Ghengin
import Ghengin.Vulkan
import Ghengin.Component.Mesh.Sphere
import Ghengin.Component.Mesh
-- import Ghengin.Component.UI


newPlanet :: Int -> Renderer Mesh
newPlanet = newSphere


