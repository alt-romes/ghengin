{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}

import Data.IORef
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans

import Ghengin
import Ghengin.Component.Material
import Ghengin.Component.Mesh
import Ghengin.Component.Camera
import Ghengin.Component.Transform
import Ghengin.Component.UI
import Ghengin.Utils
import Ghengin.Render.Packet
import Ghengin.Vulkan
import Ghengin.Scene.Graph
import Ghengin.Component (Storage, EntityCounter, explInit, cmap, cmapM)

import qualified Ghengin.Shaders.SimpleShader as SimpleShader
import qualified Shader
import Planet

-- TODO: EngineWorld data type in engine-land and then only one field must be of that type
data World = World { meshes :: !(Storage Mesh)
                   , materials    :: !(Storage SharedMaterial)
                   , transforms    :: !(Storage Transform)
                   , modelMatrices :: !(Storage ModelMatrix)
                   , cameras       :: !(Storage Camera)
                   , uiwindows     :: !(Storage UIWindow)
                   , entityParents :: !(Storage Parent)
                   , entityCounter :: !(Storage EntityCounter)
                   }

-- | A better program:
-- @
-- init :: Ghengin World PlanetSettings
-- init = do
--  -- Planet's pipeline
--  planetPipeline <- makeRenderPipeline Shader.shaderPipeline
--  ps             <- makeSettings @PlanetSettings
--
--  (planetMesh,minmax)   <- newPlanet ps
--  (planetMesh2,minmax2) <- newPlanet ps
--
--  let rp1 = RenderPacket planetMesh (makeMinMaxMaterial minmax) planetPipeline
--      rp2 = RenderPacket planetMesh2 (makeMinMaxMaterial minmax2) planetPipeline
--
--  sceneGraph do
--    newEntity ( UIWindow "Planet" (makeComponents ps) )
--  
--    newEntity' ( rp1, Transform (vec3 0 0 0) (vec3 1 1 1) (vec3 0 (pi/2) 0) ) do
--      newEntity ( rp2, Transform (vec3 0 0 10) (vec3 1 1 1) (vec3 0 0 0) ) 
--  
--    newEntity ( Camera (Perspective (radians 65) 0.1 100) ViewTransform
--              , Transform (vec3 0 0 0) (vec3 1 1 1) (vec3 0 0 0))
-- @

initG :: Ghengin World PlanetSettings
initG = do

  ps <- liftIO $ makeSettings @PlanetSettings

  planetPipeline <- lift $ makeRenderPipeline Shader.shaderPipeline
  (planetMesh,minmax) <- newPlanet ps -- TODO: Also require shader pipeline to validate it
  (planetMesh2,_minmax2) <- newPlanet ps -- TODO: Also require shader pipeline to validate it
  minmaxMaterial <- lift $ makeMaterial planetPipeline (makeMinMaxMaterial minmax)

  -- TODO: Currently we can't share meshes, we're freeing them multiple times
  -- causing a segmentation fault, and even worse if we free it to create a new
  -- one when someone else is using it

  sceneGraph do
    newEntity ( UIWindow "Planet" (makeComponents ps) )


    -- TODO: register global pipeline data newEntity ( PipelineData a planetPipeline )
    -- which can be later modified. this data is bound once per pipeline.?
    -- The global data in this example is actually the Camera transform

    newEntity' ( planetMesh, minmaxMaterial, Transform (vec3 0 0 0) (vec3 1 1 1) (vec3 0 (pi/2) 0) ) do
      newEntity (planetMesh2, minmaxMaterial, Transform (vec3 0 0 10) (vec3 1 1 1) (vec3 0 0 0) ) 

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
    cmapM $ \(oldMesh :: Mesh, sm :: SharedMaterial) -> do
      (newMesh,newMinMax) <- newPlanet ps
      lift $ do
        freeMesh (oldMesh) -- Can we hide/enforce this somehow?
        writeMaterial sm (makeMinMaxMaterial newMinMax)
      pure (newMesh)

  -- cmap $ \(_ :: Mesh, tr :: Transform) -> (tr{rotation = withVec3 tr.rotation (\x y z -> vec3 x (y+0.5*dt) z) } :: Transform)

  pure False

endG :: Ghengin World ()
endG = do
  cmapM $ \(m :: Mesh) -> lift $ freeMesh m
  liftIO $ putStrLn "Goodbye"

main :: IO ()
main = do
  w <- World <$> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit
  ghengin w initG undefined updateG endG

radians d = d * (pi/180)
