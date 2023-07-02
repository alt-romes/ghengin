{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}

import Prelude.Linear hiding (IO,(*),(+),(-),(/))
import qualified Prelude
import Data.Typeable
-- import Data.Maybe
import System.Mem
import System.Random
import GHC.TypeLits
import Data.IORef
import Control.Functor.Linear as Linear
import Control.Monad.IO.Class.Linear
import System.IO.Linear

import Ghengin
import Ghengin.Vulkan.Renderer.Texture
import Ghengin.Core.Mesh
import Ghengin.Component.Camera
import Ghengin.Component.Transform
import Ghengin.Component.UI
import Ghengin.Core.Render.Packet
-- import Ghengin.Utils
import Ghengin.Vulkan.Renderer
import Ghengin.Vulkan.Renderer.Kernel
import Ghengin.Vulkan.Renderer.Sampler
import Ghengin.Scene.Graph
-- import Ghengin.Component (Storage, EntityCounter, explInit, cmap, cmapM)
-- import qualified Ghengin.Component as C
import Apecs (Storage, EntityCounter, explInit, cmap, cmapM, Entity(..))
import qualified Apecs as C
import Ghengin.Core.Render.Property

import Prelude ((*),(+),(-),(/))
import qualified Shader
import Planet
import qualified Unsafe.Linear as Unsafe
import Ghengin.Core.Log

-- | A better program:
initG :: Ghengin () ()
initG = enterD "initG" $ Linear.do

  Ur ps  <- liftIO $ makeSettings @PlanetSettings
  Ur ps2 <- liftIO $ makeSettings @PlanetSettings

  -- sampler <- lift $ createSampler FILTER_NEAREST SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
  -- tex <- lift $ texture "assets/planet_gradient.png" sampler

  logT "Making Render pipeline"
  planetPipeline :: RenderPipeline p a <- lift (makeRenderPipeline Shader.shaderPipeline (DynamicBinding (Ur $ Shader.CameraProperty identity identity (vec3 0 0 0)) :## GHNil))
  (Ur planetPipeline') <- pure $ Unsafe.toLinear (\x -> Ur x) planetPipeline -- Unsafe, we need to fix or drop linearity at the surface level
  Ur (Entity pipelineRef) <- Unsafe.toLinear C.newEntity (SomePipeline planetPipeline')

  logT "New planet"
  p1 <- newPlanet ps  planetPipeline' (Ref pipelineRef)
  p2 <- newPlanet ps2 planetPipeline' (Ref pipelineRef)

  sceneGraph Linear.do

    -- TODO: register global pipeline data newEntity ( PipelineData a planetPipeline )?
    -- which can be later modified. this data is bound once per pipeline.?

                    -- fix or drop linearity, linear apecs is currently just wrong
    (Ur e1,Ur e2) <- Unsafe.toLinear2 newEntity' ( p1, Transform (vec3 0 0 0) (vec3 1 1 1) (vec3 0 (pi/2) 0) ) do
                       Unsafe.toLinear newEntity ( p2, Transform (vec3 0 0 10) (vec3 1 1 1) (vec3 0 0 0) ) 

    -- The global data in this game in specific is actually the Camera transform?
    Ur _ <- newEntity ( Camera (Perspective (radians 65) 0.1 100) ViewTransform
                      , Transform (vec3 0 0 0) (vec3 1 1 1) (vec3 0 0 0))

    -- : UI
    Ur _ <- newEntityUI "Planet" $ makeComponents ps e1
    Ur _ <- newEntityUI "Planet2" $ makeComponents ps2 e2
    pure ()

  pure ()

updateG :: () -> DeltaTime -> Ghengin () Bool
updateG () dt = Linear.do

  -- TODO: This next bit has got to be re-done
  -- TODO: Bind pipeline-global data to descriptor set #0
  -- Get main camera, for the time being it's the only possible pipeline data for the shader
  -- The last camera will override the write buffer

  cmapM $ \(Camera proj view, fromMaybe (ModelMatrix identity 0) -> ModelMatrix camTr _) -> Linear.do

    Ur projM <- lift $ makeProjection proj
    let viewM = makeView camTr view
        ubo   = Shader.CameraProperty viewM projM (posFromMat4 camTr)
     in Linear.do
      cmapM $ \(SomePipeline (rp :: RenderPipeline π props)) ->
        case eqT @props @'[Shader.CameraProperty] of
          Just Refl -> Linear.do
            rp' <- lift $ rp & propertyAt @0 (\(Ur _) -> pure (Ur ubo))
            pure $ Unsafe.toLinear Ur (SomePipeline rp') -- romes:todo: frontend linearity..., linear apecs bad,... either fix or drop
          Nothing -> pure $ Unsafe.toLinear Ur $ SomePipeline rp
      pure (Ur ())


  cmapM $ \(_ :: Camera, tr :: Transform) -> lift $ updateFirstPersonCameraTransform dt tr
  cmap $ \(_ :: RenderPacket, tr :: Transform) -> (tr{rotation = withVec3 tr.rotation (\x y z -> vec3 x (y+0.5*dt) z) } :: Transform)

  pure False

endG :: () -> Ghengin () ()
endG () = do
  liftSystemIO $ putStrLn "Goodbye"

main :: Prelude.IO ()
main = do
  withLinearIO $ move <$> ghengin () (move <$> initG) undefined updateG endG

radians d = d * (pi/180)
