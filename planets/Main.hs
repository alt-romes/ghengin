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
import Ghengin.Component.Material
import Ghengin.Component.Mesh
import Ghengin.Component.Camera
import Ghengin.Component.Transform
import Ghengin.Component.UI
import Ghengin.Utils
import Ghengin.Render.Packet
import Ghengin.Vulkan

import qualified Ghengin.Shaders.SimpleShader as SimpleShader
import qualified Shader
import Planet

data World = World { meshes :: !(Storage Mesh)
                   , materials    :: !(Storage SomeMaterial)
                   , transforms    :: !(Storage Transform)
                   , cameras       :: !(Storage Camera)
                   , uiwindows     :: !(Storage UIWindow)
                   , entityCounter :: !(Storage EntityCounter)
                   }

-- TODO: Move to somewhere within the engine and put together requirements on record fields
instance Monad m => Has World m EntityCounter where getStore = SystemT (asks entityCounter)

initG :: Ghengin World PlanetSettings
initG = do

  -- vikingRoom <- lift $ loadObjMesh "assets/viking_room.obj"

  ps <- liftIO $ makeSettings @PlanetSettings
  newEntity ( UIWindow "Planet" (makeComponents ps) )

  planetPipeline <- lift $ makeRenderPipeline Shader.shaderPipeline
  (planetMesh,minmax) <- lift $ newPlanet ps -- TODO: Also require shader pipeline to validate it
  minmaxMaterial <- lift $ makeMaterial planetPipeline (makeMinMaxMaterial minmax)

  -- TODO: register global pipeline data newEntity ( PipelineData a planetPipeline )
  -- which can be later modified. this data is bound once per pipeline.
  -- The global data in this example is actually the Camera transform

  -- planetRenderPacket <- lift $ newRenderPacket planetPipeline planetMesh undefined  -- also take a type that instances material (that passes the parameters for this shader?)

  -- let planetMaterial = Material planetPipeline

  newEntity ( planetMesh, minmaxMaterial, Transform (vec3 0 0 4) (vec3 1 1 1) (vec3 0 0 0) )

  newEntity ( Camera (Perspective (radians 65) 0.1 100) ViewTransform
            , Transform (vec3 0 0 0) (vec3 1 1 1) (vec3 0 0 0))
            -- , PipelineData @Transform self planetPipeline)

  pure ps

updateG :: PlanetSettings -> DeltaTime -> [Bool] -> Ghengin World Bool
updateG ps dt uichanges = do

  cmapM $ \(_ :: Camera, tr :: Transform) -> lift $ updateFirstPersonCameraTransform dt tr

  -- TODO: perhaps all UI colors could be combined with the uichanges variables and be always provided on request depending on whether they were changed or not
  -- something like: getChanged :: Ghengin w (PlanetSettings Maybe) or (Maybe Color, Maybe Resolution) or ...
  when (or uichanges) $
    cmapM $ \(oldMesh :: Mesh) -> lift $ do
      (newMesh,_TODOuseme) <- newPlanet ps
      freeMesh (oldMesh) -- Can we hide/enforce this somehow?
      pure (newMesh)

  -- cmap $ \(_ :: Mesh, tr :: Transform) -> (tr{rotation = withVec3 tr.rotation (\x y z -> vec3 x (y+0.5*dt) z) } :: Transform)

  pure False

endG :: Ghengin World ()
endG = do
  cmapM $ \(m :: Mesh) -> lift $ freeMesh m
  liftIO $ putStrLn "Goodbye"

main :: IO ()
main = do
  w <- World <$> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit
  ghengin w initG undefined updateG endG

radians d = d * (pi/180)
