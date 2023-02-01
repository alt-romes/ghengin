{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}

import Data.Typeable
import System.Mem
import System.Random
import GHC.TypeLits
import Data.IORef
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans

import Ghengin
import Ghengin.Asset.Texture
import Ghengin.Component.Material
import Ghengin.Component.Mesh
import Ghengin.Component.Camera
import Ghengin.Component.Transform
import Ghengin.Component.UI
import Ghengin.Render.Packet
import Ghengin.Utils
import Ghengin.Vulkan
import Ghengin.Vulkan.Sampler
import Ghengin.Scene.Graph
import Ghengin.Component (Storage, EntityCounter, explInit, cmap, cmapM)

import qualified Shader
import Planet

-- | A better program:
initG :: Ghengin () ()
initG = do

  ps <- liftIO $ makeSettings @PlanetSettings
  ps2 <- liftIO $ makeSettings @PlanetSettings

  -- sampler <- lift $ createSampler FILTER_NEAREST SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
  -- tex <- lift $ texture "assets/planet_gradient.png" sampler

  planetPipeline <- lift $ makeRenderPipeline Shader.shaderPipeline

  p1 <- newPlanet ps planetPipeline
  p2 <- newPlanet ps2 planetPipeline

  sceneGraph do

    -- TODO: register global pipeline data newEntity ( PipelineData a planetPipeline )?
    -- which can be later modified. this data is bound once per pipeline.?

    (e1,e2) <- newEntity' ( p1, Transform (vec3 0 0 0) (vec3 1 1 1) (vec3 0 (pi/2) 0) ) do
                 newEntity ( p2, Transform (vec3 0 0 10) (vec3 1 1 1) (vec3 0 0 0) ) 

    -- The global data in this game in specific is actually the Camera transform?
    newEntity ( Camera (Perspective (radians 65) 0.1 100) ViewTransform
              , Transform (vec3 0 0 0) (vec3 1 1 1) (vec3 0 0 0))

    -- : UI
    newEntityUI "Planet" $ makeComponents ps e1
    newEntityUI "Planet2" $ makeComponents ps2 e2

  pure ()

updateG :: () -> DeltaTime -> Ghengin () Bool
updateG () dt = do

  cmapM $ \(_ :: Camera, tr :: Transform) -> lift $ updateFirstPersonCameraTransform dt tr
  cmap $ \(_ :: RenderPacket, tr :: Transform) -> (tr{rotation = withVec3 tr.rotation (\x y z -> vec3 x (y+0.5*dt) z) } :: Transform)

  pure False

endG :: () -> Ghengin () ()
endG () = do
  liftIO $ putStrLn "Goodbye"

main :: IO ()
main = do
  setLogLevel LogDebug
  ghengin () initG undefined updateG endG

radians d = d * (pi/180)
