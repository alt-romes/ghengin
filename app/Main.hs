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

import Ghengin.Vulkan.GLFW.Window
import Ghengin.Vulkan.Command
import Ghengin.Vulkan.Device
import Ghengin.Vulkan.Pipeline
import Ghengin.Vulkan.RenderPass
import Ghengin.Vulkan.Synchronization
import Ghengin.Vulkan
import Ghengin

import qualified Ghengin.Shaders.SimpleShader as SimpleShader
import Ghengin.Shaders

import Ghengin.Component.Mesh
import Ghengin.Component.Mesh.Cube
import Ghengin.Component.Transform

import Geomancy.Transform hiding (Transform)
import Geomancy hiding (Transform)
import Apecs
import Main.Apecs

main :: IO ()
main = do
  w <- initWorld
  ghengin w initG undefined loopStepG endG


loopStepG :: () -> Ghengin World Bool
loopStepG () = do
  cmap $ \(tr :: Transform) -> (tr{rotation = withVec3 tr.rotation (\x y z -> vec3 x (y + 0.005) z) } :: Transform)
  pure False

initG :: Ghengin World ()
initG = do
  -- (_, nExts) <- Vk.enumerateInstanceExtensionProperties Nothing
  -- liftIO $ putStr "Extensions: " >> print nExts

  m1 <- lift $ createMesh [ Vertex (vec3 0.0 (-0.6) 1) (vec3 0 0 0) (vec3 1 0 0)
                          , Vertex (vec3 (-0.6) 0.6 1) (vec3 0 0 0) (vec3 0 0 1)
                          , Vertex (vec3 0.6 0.6 1) (vec3 0 0 0) (vec3 0 1 0)
                          ]

  m2 <- lift $ createMesh [ Vertex (vec3 (-0.5) (-0.5) 1) (vec3 0 0 0) (vec3 1 0 0)
                          , Vertex (vec3 (-0.5) 0.5 1) (vec3 0 0 0) (vec3 1 0 1)
                          , Vertex (vec3 0.5 (-0.5) 1) (vec3 0 0 0) (vec3 0 0 1)
                          , Vertex (vec3 0.5 0.5 1) (vec3 0 0 0) (vec3 0 1 0)
                          ]
  cube <- lift $ cubeMesh
  newEntity (Position 0, Velocity 1, cube, Transform (vec3 0 0 0) (vec3 0.5 0.5 0.5) (vec3 0 0 0))
  newEntity (Position 2, Velocity 1)
  newEntity (Position 1, Velocity 2, Flying)

  pure ()

endG :: Ghengin World ()
endG = do
  liftIO $ putStrLn "Goodbye"


