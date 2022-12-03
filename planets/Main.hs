{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}

import qualified Graphics.UI.GLFW as GLFW

import Ghengin
import Ghengin.Component.Mesh
import Ghengin.Component.Camera
import Ghengin.Component.Transform
import Ghengin.Utils
import Ghengin.Vulkan

import Planet

data World = World { meshes     :: !(Storage Mesh)
                   , transforms :: !(Storage Transform)
                   , cameras :: !(Storage Camera)
                   , entityCounter :: !(Storage EntityCounter)
                   }

instance Has World Renderer EntityCounter where getStore = SystemT (asks entityCounter)

initG :: Ghengin World ()
initG = do

  s <- lift $ newPlanet 15

  newEntity ( s, Transform (vec3 0 0 4) (vec3 1 1 1) (vec3 0 0 0) )
  newEntity ( s, Transform (vec3 0 0 (-4)) (vec3 1 1 1) (vec3 0 0 0) )
  newEntity ( s, Transform (vec3 4 0 0) (vec3 1 1 1) (vec3 0 0 0) )
  newEntity ( s, Transform (vec3 (-4) 0 0) (vec3 1 1 1) (vec3 0 0 0) )
  newEntity ( Camera (Perspective (radians 65) 0.1 10) ViewTransform
            , Transform (vec3 0 0 0) (vec3 1 1 1) (vec3 0 0 0) )

  pure ()


updateG :: () -> DeltaTime -> Ghengin World Bool
updateG () dt = do

  cmapM $ \(_ :: Camera, tr :: Transform) -> lift $ updateFirstPersonCameraTransform dt tr

  -- cmap $ \(_ :: Mesh, tr :: Transform) -> (tr{rotation = withVec3 tr.rotation (\x y z -> vec3 x (y+1*dt) z) } :: Transform)

  pure False

endG :: Ghengin World ()
endG = liftIO $ putStrLn "Goodbye"

main :: IO ()
main = do
  w <- World <$> explInit <*> explInit <*> explInit <*> explInit
  ghengin w initG undefined updateG endG

radians d = d * (pi/180)
