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

  s <- lift $ newSphere 2

  newEntity ( s, Transform (vec3 0 0 4) (vec3 1 1 1) (vec3 0 0 0) )
  newEntity ( Camera (Perspective (radians 65) 0.1 10) ViewTransform
            , Transform (vec3 0 0 0) (vec3 1 1 1) (vec3 0 0 0) )

  pure ()


updateG :: () -> DeltaTime -> Ghengin World Bool
updateG () dt = do

  updateCameraTransform dt

  cmap $ \(_ :: Mesh, tr :: Transform) -> (tr{rotation = withVec3 tr.rotation (\x y z -> vec3 x (y+1*dt) z) } :: Transform)

  pure False

endG :: Ghengin World ()
endG = liftIO $ putStrLn "Goodbye"

main :: IO ()
main = do
  w <- World <$> explInit <*> explInit <*> explInit <*> explInit
  ghengin w initG undefined updateG endG

updateCameraTransform :: DeltaTime -> Ghengin World ()
updateCameraTransform dt =
  cmapM $ \(_ :: Camera, tr :: Transform) -> do
    r <- ifPressed GLFW.Key'Right (pure $ vec3 0 1 0) (pure $ vec3 0 0 0)
    l <- ifPressed GLFW.Key'Left (pure $ vec3 0 (-1) 0) (pure $ vec3 0 0 0)
    u <- ifPressed GLFW.Key'Up   (pure $ vec3 1 0 0) (pure $ vec3 0 0 0)
    d <- ifPressed GLFW.Key'Down (pure $ vec3 (-1) 0 0) (pure $ vec3 0 0 0)

    let rotateV = normalize (r + l + u + d)
    -- TODO: mod of y rotation with 2*pi

    let tr'  = tr{rotation = if nearZero rotateV then tr.rotation else tr.rotation + rotateV ^* dt ^* lookSpeed}

        WithVec3 rx ry rz = tr'.rotation

        forwardDir = vec3 (sin ry) 0 (cos ry)
        rightDir = vec3 (cos ry) 0 (-sin ry)
        upDir = vec3 0 (-1) 0

    mf <- ifPressed GLFW.Key'W (pure forwardDir) (pure $ vec3 0 0 0)
    mb <- ifPressed GLFW.Key'S (pure (-forwardDir)) (pure $ vec3 0 0 0)
    mr <- ifPressed GLFW.Key'D (pure rightDir) (pure $ vec3 0 0 0)
    ml <- ifPressed GLFW.Key'A (pure (-rightDir)) (pure $ vec3 0 0 0)
    mu <- ifPressed GLFW.Key'Space (pure upDir) (pure $ vec3 0 0 0)
    md <- ifPressed GLFW.Key'LeftShift (pure (-upDir)) (pure $ vec3 0 0 0)

    let moveDir = mf + mb + mr + ml + mu + md

        tr'' = tr'{position = if nearZero moveDir then tr'.position else tr'.position + moveDir ^* dt ^* moveSpeed} :: Transform

    pure (tr'' :: Transform)
  where
    moveSpeed = 3
    lookSpeed = 2

radians d = d * (pi/180)
