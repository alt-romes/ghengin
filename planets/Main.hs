{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}

import qualified Graphics.UI.GLFW as GLFW
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

initG :: Ghengin World (IORef Int, IORef Vec3, IORef Vec3)
initG = do

  resR   <- liftIO $ newIORef 2
  colorR <- liftIO $ newIORef (vec3 1 0 0)
  oldColorR <- liftIO $ newIORef (vec3 1 0 0)
  newEntity ( UIWindow "Planet" (planetSettings resR colorR) )

  s <- lift $ newPlanet 15 (vec3 0.5 0.5 0.5)

  newEntity ( s, Transform (vec3 0 0 4) (vec3 1 1 1) (vec3 0 0 0) )
  newEntity ( s, Transform (vec3 0 0 (-4)) (vec3 1 1 1) (vec3 0 0 0) )
  newEntity ( s, Transform (vec3 4 0 0) (vec3 1 1 1) (vec3 0 0 0) )
  newEntity ( s, Transform (vec3 (-4) 0 0) (vec3 1 1 1) (vec3 0 0 0) )
  newEntity ( Camera (Perspective (radians 65) 0.1 10) ViewTransform
            , Transform (vec3 0 0 0) (vec3 1 1 1) (vec3 0 0 0) )

  pure (resR, colorR, oldColorR)


updateG :: (IORef Int, IORef Vec3, IORef Vec3) -> DeltaTime -> Ghengin World Bool
updateG (resR, colorR, oldColorR) dt = do

  cmapM $ \(_ :: Camera, tr :: Transform) -> lift $ updateFirstPersonCameraTransform dt tr

  res <- liftIO $ readIORef resR
  color <- liftIO $ readIORef colorR
  oldColor <- liftIO $ readIORef oldColorR

  when (color /= oldColor) $
    cmapM $ \(m :: Mesh) -> lift $ newPlanet res color

  liftIO $ writeIORef oldColorR color

  -- cmap $ \(_ :: Mesh, tr :: Transform) -> (tr{rotation = withVec3 tr.rotation (\x y z -> vec3 x (y+1*dt) z) } :: Transform)

  pure False

endG :: Ghengin World ()
endG = liftIO $ putStrLn "Goodbye"

main :: IO ()
main = do
  w <- World <$> explInit <*> explInit <*> explInit <*> explInit <*> explInit
  ghengin w initG undefined updateG endG

radians d = d * (pi/180)
