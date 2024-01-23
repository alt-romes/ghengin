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
import Data.Time.Clock.POSIX
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

gameLoop :: forall (_s :: FIR.PipelineInfo). PipelineKey _s PipelineProps -> UTCTime -> RenderQueue () ⊸ Core (RenderQueue ())
gameLoop pkey lastTime rq = Linear.do
 should_close <- (shouldCloseWindow ↑)
 if should_close then return rq else Linear.do
  (pollWindowEvents ↑)

  Ur ( double2Float -> newPosX
     , double2Float -> newPosY
     ) <- (getMousePos ↑)
  Ur nowTime <- liftSystemIOU getCurrentTime
  let diffTime = diffUTCTime nowTime lastTime
  liftSystemIO $ do
    print ("Frame time " ++ show diffTime)

  -- rq' <- Core $ lift $
  --   editPipeline pkey rq $
  --         propertyAt @1 (\(Ur (Time time)) -> pure $ Ur $ Time $ time + (realToFrac diffTime / 2))
  --     <=< propertyAt @0 (\(Ur _) -> pure $ Ur $ MousePos $ vec2 newPosX newPosY)

  rq'' <- render rq'

  gameLoop pkey nowTime rq''

main :: Prelude.IO ()
main = do
 withLinearIO $
  runCore WINDOW_SIZE Linear.do
    pipeline             <- makeMainPipeline sp
    (emptyMat, pipeline) <- material GHNil pipeline
    (mesh, pipeline)     <- createMeshWithIxs pipeline GHNil viewportVertices viewportIndices
    (rq, Ur pkey)        <- pure (insertPipeline pipeline LMon.mempty)
    (rq, Ur mkey)        <- pure (insertMaterial pkey emptyMat rq)
    (rq, Ur mshkey)      <- pure (insertMesh mkey mesh rq)

    rq <- gameLoop pkey now rq

    (freeRenderQueue rq ↑)

    -- In fact, freeing these again is a type error. Woho!
    -- (destroyRenderPipeline pipeline ↑)

    return (Ur ())

