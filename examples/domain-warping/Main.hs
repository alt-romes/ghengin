{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import GHC.Float
import Data.Coerce
import Data.Time
import Foreign.Storable
import Geomancy.Mat4
import Geomancy.Transform
import Geomancy.Vec2
import Geomancy.Vec3
import Geomancy.Vec4
import Geomancy.Vulkan.Projection (perspective)
import Ghengin.Core
import Ghengin.Core.Log
import Ghengin.Core.Mesh
import Ghengin.Core.Mesh.Vertex
import Ghengin.Core.Material
import Ghengin.Core.Prelude as Linear
import Ghengin.Core.Render
import Ghengin.Core.Render.Packet
import Ghengin.Core.Render.Pipeline
import Ghengin.Core.Render.Property
import Ghengin.Core.Render.Queue
import Ghengin.Vulkan.Renderer.Sampler
import Ghengin.Core.Shader (StructVec2(..), StructVec3(..), StructMat4(..), StructFloat(..))
import Vulkan.Core10.FundamentalTypes (Extent2D(..))
import qualified Data.Monoid.Linear as LMon
import qualified FIR
import FIR.Generics
import qualified Math.Linear as FIR
import qualified Prelude
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC
import Data.Time.Clock.POSIX

import Shaders
import Common

gameLoop :: forall (_s :: FIR.PipelineInfo). Vec2 -> PipelineKey _s PipelineProps -> RenderQueue () ⊸ Core (RenderQueue ())
gameLoop (WithVec2 previousPosX previousPosY) pkey rq = Linear.do
 should_close <- (shouldCloseWindow ↑)
 if should_close then return rq else Linear.do
  (pollWindowEvents ↑)

  Ur (double2Float -> newPosX, double2Float -> newPosY) <- (getMousePos ↑)
  liftSystemIO $ print (newPosX, newPosY)

  let Ur pos = Ur $ vec2 (0.5 * (previousPosX + newPosX)) (0.5 * (previousPosY + newPosY))

  rq' <- (editPipeline pkey rq (propertyAt @1 (\(Ur (Time time)) -> pure $ Ur $ Time ((time+0.001))) <=< propertyAt @0 (\(Ur _) -> pure $ Ur $ MousePos pos)) ↑)

  Ur prev <- liftSystemIOU getCurrentTime
  rq'' <- render rq'
  Ur post <- liftSystemIOU getCurrentTime
  liftSystemIO $
    print ("Frame time " ++ show (diffUTCTime post prev))

  gameLoop (vec2 newPosX newPosY) pkey rq''

main :: Prelude.IO ()
main = do
 withLinearIO $
  runCore WINDOW_SIZE Linear.do
    (rq, Ur pkey) <- (renderQueueWithViewport shaderPipeline ↑)

    Ur (x,y)   <- (getMousePos ↑)
    rq <- gameLoop (vec2 (double2Float x) (double2Float y)) pkey rq

    (freeRenderQueue rq ↑)

    -- In fact, freeing these again is a type error. Woho!
    -- (destroyRenderPipeline pipeline ↑)

    return (Ur ())

