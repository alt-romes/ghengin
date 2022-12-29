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

import System.Random
import Unsafe.Coerce
import GHC.TypeLits
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
import Ghengin.Render.Packet
import Ghengin.Utils
import Ghengin.Vulkan
import Ghengin.Scene.Graph
import Ghengin.Shaders
import Ghengin.Component (Storage, EntityCounter, explInit, cmap, cmapM)

import qualified Ghengin.Shaders.SimpleShader as SimpleShader
import qualified Shader
import Planet

-- TODO: EngineWorld data type in engine-land and then only one field must be of that type
data World = World { renderPackets :: !(Storage RenderPacket)
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
  ps2 <- liftIO $ makeSettings @PlanetSettings

  planetPipeline <- lift $ makeRenderPipeline Shader.shaderPipeline
  (planetMesh,minmax) <- newPlanet ps -- TODO: Also require shader pipeline to validate it
  (planetMesh2,minmax2) <- newPlanet ps2 -- TODO: Also require shader pipeline to validate it
  let p1 = renderPacket planetMesh (makeMinMaxMaterial (vec3 1 0 0) minmax) planetPipeline
      p2 = renderPacket planetMesh2 (makeMinMaxMaterial (vec3 0 0 1) minmax2) planetPipeline

  -- TODO: Currently we can't share meshes, we're freeing them multiple times
  -- causing a segmentation fault, and even worse if we free it to create a new
  -- one when someone else is using it

  sceneGraph do
    -- TODO: creating the settings should also define how to react to changes
    newEntity ( UIWindow "Planet" (makeComponents ps) )
    newEntity ( UIWindow "Planet2" (makeComponents ps2) )


    -- TODO: register global pipeline data newEntity ( PipelineData a planetPipeline )?
    -- which can be later modified. this data is bound once per pipeline.?
    -- The global data in this example is actually the Camera transform

    newEntity' ( p1, Transform (vec3 0 0 0) (vec3 1 1 1) (vec3 0 (pi/2) 0) ) do
      newEntity ( p2, Transform (vec3 0 0 10) (vec3 1 1 1) (vec3 0 0 0) ) 

    newEntity ( Camera (Perspective (radians 65) 0.1 100) ViewTransform
              , Transform (vec3 0 0 0) (vec3 1 1 1) (vec3 0 0 0))
              -- , PipelineData @Transform self planetPipeline)

  pure ps

updateG :: PlanetSettings -> DeltaTime -> [Bool] -> Ghengin World Bool
updateG ps dt uichanges = do

  cmapM $ \(_ :: Camera, tr :: Transform) -> lift $ updateFirstPersonCameraTransform dt tr

  -- TODO: perhaps all UI colors could be combined with the uichanges variables and be always provided on request depending on whether they were changed or not
  -- something like: getChanged :: Ghengin w (PlanetSettings Maybe) or (Maybe Color, Maybe Resolution) or ...
  -- TODO: Which UI changed?... Maybe we should embrace dear-imgui and make all
  -- the decisions the imgui way. that is, when defining the UI defining what
  -- happens when something is changed
  when (or uichanges) $
    cmapM $ \x ->
      case x of
        (RenderPacket oldMesh mat pp _) -> do
          (newMesh,newMinMax) <- newPlanet ps
          lift $ do
            freeMesh (oldMesh) -- Can we hide/enforce this somehow? Meshes aren't automatically freed when switched! We should make "switching" explicit?
          case Shader.shaderPipeline of
            (spp :: GShaderPipeline i)
                 -- GIGANTIC:TODO: For some reason I have yet to better
                 -- understand, the pipeline associated to the render packet
                 -- can't be used to validate Compatibility with a new material again.
                 -- It's somehow related to being an existential type and therefore the type not carrying enough information?
                 -- How can I make the existential type carry enough information to pass Compatible again?
            --
            --
            -- The Solution might be defining a function that edits the content of dynamic
            -- bindings (by comparing Typeable instances?) because (and this is the key) if
            -- the pipeline was already created then it was already compatible, and
            -- therefore changing the value of the dynamic binding will not affect
            -- compatibility
            --
            -- Also: TODO: With the typeable constraint, we are able to inspect at runtime the material type (as if it were a simple tag) and depending on the value updating the material
             -> do
               (x,y,z) <- liftIO randomIO
               pure (renderPacket @_ @i newMesh (makeMinMaxMaterial (vec3 x y z) newMinMax) (unsafeCoerce pp))

  cmap $ \(_ :: RenderPacket, tr :: Transform) -> (tr{rotation = withVec3 tr.rotation (\x y z -> vec3 x (y+0.5*dt) z) } :: Transform)

  pure False

endG :: Ghengin World ()
endG = do
  liftIO $ putStrLn "Goodbye"

main :: IO ()
main = do
  -- setLogLevel LogTrace
  w <- World <$> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit
  ghengin w initG undefined updateG endG

radians d = d * (pi/180)
