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

planetSettings :: IORef Int   -- ^ Resolution
               -> IORef Vec3  -- ^ Color
               -> [UIComponent]
planetSettings res color =
  [ SliderInt "Resolution" res 2 256
  , ColorPicker "Color" color
  ]

newPlanet :: Int -> Vec3 -> Renderer Mesh
newPlanet i c = newSphere i (Just c)


