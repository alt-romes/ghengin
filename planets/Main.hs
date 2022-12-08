{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}

import Data.IORef
import Control.Monad

import Ghengin
import Ghengin.Component.Mesh
import Ghengin.Component.Camera
import Ghengin.Component.Transform
import Ghengin.Component.UI
import Ghengin.Utils
import Ghengin.Vulkan

import Planet

data World = World { meshes     :: !(Storage Mesh)
                   , transforms :: !(Storage Transform)
                   , cameras :: !(Storage Camera)
                   , uiwindows :: !(Storage UIWindow)
                   , entityCounter :: !(Storage EntityCounter)
                   }

instance Has World Renderer EntityCounter where getStore = SystemT (asks entityCounter)

initG :: Ghengin World PlanetSettings
initG = do

  ps <- liftIO $ makeSettings @PlanetSettings
  newEntity ( UIWindow "Planet" (makeComponents ps) )
  -- mapM (\ns -> newEntity ( UIWindow "Noise"  (makeComponents ns) )) ps.noiseSettings

  s <- lift $ newPlanet ps

  newEntity ( s, Transform (vec3 0 0 4) (vec3 1 1 1) (vec3 0 0 0) )
  newEntity ( Camera (Perspective (radians 65) 0.1 100) ViewTransform
            , Transform (vec3 0 0 0) (vec3 1 1 1) (vec3 0 0 0) )

  pure ps


updateG :: PlanetSettings -> DeltaTime -> [Bool] -> Ghengin World Bool
updateG ps dt uichanges = do

  cmapM $ \(_ :: Camera, tr :: Transform) -> lift $ updateFirstPersonCameraTransform dt tr

  -- TODO: perhaps all UI colors could be combined with the uichanges variables and be always provided on request depending on whether they were changed or not
  -- something like: getChanged :: Ghengin w (PlanetSettings Maybe) or (Maybe Color, Maybe Resolution) or ...
  when (or uichanges) $
    cmapM $ \(m :: Mesh) -> lift $ do
      x <- newPlanet ps
      freeMesh m -- Can we hide/enforce this somehow?
      pure x

  -- cmap $ \(_ :: Mesh, tr :: Transform) -> (tr{rotation = withVec3 tr.rotation (\x y z -> vec3 x (y+0.5*dt) z) } :: Transform)

  pure False

endG :: Ghengin World ()
endG = do
  cmapM $ \(m :: Mesh) -> lift $ freeMesh m
  liftIO $ putStrLn "Goodbye"

main :: IO ()
main = do
  w <- World <$> explInit <*> explInit <*> explInit <*> explInit <*> explInit
  ghengin w initG undefined updateG endG

radians d = d * (pi/180)
