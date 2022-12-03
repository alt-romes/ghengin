{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
module Planet where

import Data.IORef
import Control.Monad

import Ghengin
import Ghengin.Vulkan
import Ghengin.Component.Mesh.Sphere
import Ghengin.Component.Mesh
import Ghengin.Component.UI


-- data PlanetSettings = PlanetSettings { radius :: Float
--                                      , color  :: Vec3
--                                      }

planetSettings :: IORef Float -- ^ Resolution
               -> IORef Vec3  -- ^ Color
               -> [UIComponent]
planetSettings res color =
  [ SliderFloat "Resolution" res 2 256
  , ColorPicker "Color" color
  ]

newPlanet :: Int -> Renderer Mesh
newPlanet = newSphere


