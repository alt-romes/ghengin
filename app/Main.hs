{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Data.Fixed (mod')
import Control.Monad.Reader
import qualified Data.Vector as V

import Data.IORef

import Vulkan.Zero (zero)
import qualified Vulkan as Vk

import qualified Graphics.UI.GLFW as GLFW
import Ghengin.Vulkan.GLFW.Window
import Ghengin.Vulkan.Command
import Ghengin.Vulkan.Device
import Ghengin.Vulkan.Pipeline
import Ghengin.Vulkan.RenderPass
import Ghengin.Vulkan.Synchronization
import Ghengin.Vulkan
import Ghengin.Utils
import Ghengin

import qualified Ghengin.Shaders.SimpleShader as SimpleShader
import Ghengin.Shaders

import Ghengin.Component.Mesh
import Ghengin.Component.Camera
import Ghengin.Component.Mesh.Cube
import Ghengin.Component.Mesh.Obj
import Ghengin.Component.Transform

import Apecs
import Main.Apecs

main :: IO ()
main = do
  w <- initWorld
  ghengin w initG undefined loopStepG endG


loopStepG :: () -> DeltaTime -> [[Bool]] -> Ghengin World Bool
loopStepG () dt _ = do

  cmapM $ \(_ :: Camera, tr :: Transform) -> lift $ updateFirstPersonCameraTransform dt tr
  cmap  $ \(_ :: Mesh, tr :: Transform) -> (tr{rotation = withVec3 tr.rotation (\x y z -> vec3 x (y+1*dt) z) } :: Transform)

  pure False

initG :: Ghengin World ()
initG = do

  cube       <- lift $ cubeMesh
  vikingRoom <- lift $ loadObjMesh "assets/viking_room.obj"
  mambus     <- lift $ loadObjMesh "assets/Mambus.obj"

  newEntity (cube, Transform (vec3 0 0 2.5) (vec3 0.5 0.5 0.5) (vec3 0 0 0))
  newEntity (vikingRoom, Transform (vec3 (-2.5) 0 0.5) (vec3 0.5 0.5 0.5) (vec3 (pi/2) 0 0))
  newEntity (mambus, Transform (vec3 1 0 (-2.5)) (vec3 0.5 0.5 0.5) (vec3 pi 0 pi))

  -- newEntity (Camera (Perspective (radians 50) 0.1 10) (ViewDirection (vec3 0 0 1)), Transform (vec3 0 0 0) (vec3 1 1 1) (vec3 0 0 0))
  newEntity (Camera (Perspective (radians 65) 0.1 10) ViewTransform, Transform (vec3 0 0 0) (vec3 1 1 1) (vec3 0 0 0))

  return ()

endG :: Ghengin World ()
endG = do
  liftIO $ putStrLn "Goodbye"


radians d = d * (pi/180)

