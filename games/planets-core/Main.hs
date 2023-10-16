{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PostfixOperators #-}
{-|

The planets-core demo:

A planet (render packet) is defined through a mesh, material, and pipeline,
which in turn are made up of multiple things, as represented in the following picture.

┌─────────────┐┌─────────────┐┌────────────┐┌──────┐┌───────────┐
│Position (DP)││Colormap (TP)││Min Max (SP)││Shader││Camera (DP)│
└┬────────────┘└┬────────────┘└┬───────────┘└┬─────┘└┬──────────┘
┌▽───┐┌─────────▽──────────────▽┐┌───────────▽───────▽┐          
│Mesh││Material                 ││Pipeline            │          
└┬───┘└┬────────────────────────┘└┬───────────────────┘          
┌▽─────▽──────────────────────────▽┐                             
│Planet (RenderPacket)             │                             
└──────────────────────────────────┘                             

The function `newPlanet` handles the creation of planet render packets...

-}
module Main where

import Data.Coerce
import Data.Time
import Foreign.Storable
import Geomancy.Mat4
import Geomancy.Transform
import Geomancy.Vec3
import Geomancy.Vec4
import Geomancy.Vulkan.Projection (perspective)
import Ghengin.Core
import Ghengin.Core.Log
import Ghengin.Core.Mesh
import Ghengin.Core.Material
import Ghengin.Core.Prelude as Linear
import Ghengin.Core.Render
import Ghengin.Core.Render.Packet
import Ghengin.Core.Render.Pipeline
import Ghengin.Core.Render.Property
import Ghengin.Vulkan.Renderer.Sampler
import Ghengin.Core.Render.Queue
import Ghengin.Core.Shader (StructVec3(..), StructMat4(..))
import Vulkan.Core10.FundamentalTypes (Extent2D(..))
import qualified Data.Monoid.Linear as LMon
import qualified FIR
import qualified Math.Linear as FIR
import qualified Prelude

import Shaders -- planet shaders
import Planet

newtype ProjectionM = ProjectionM Transform
  deriving Storable -- use GlBlock instead...
  deriving FIR.Syntactic via (StructMat4 "m")

newtype ViewM       = ViewM Transform
  deriving Storable
  deriving FIR.Syntactic via (StructMat4 "m")

newtype CameraPos   = CameraPos Vec3
  deriving Storable
  deriving FIR.Syntactic via (StructVec3 "v")

type CameraProperties = [ProjectionM, ViewM, CameraPos]

pattern MAX_FRAME_TIME :: Float
pattern MAX_FRAME_TIME = 0.5

-- We should use Alexander's gl-block library instead of Storable, and
-- Geomancy.Transform.Tree for the node tree...

makeMainPipeline :: Renderer (RenderPipeline _ CameraProperties)
makeMainPipeline = Linear.do
  Ur extent <- getRenderExtent

  let radians d = d * (pi/180)
      -- By making the extent into a static binding, when we update the extent
      -- we must also explicitely update the static binding
      projM = perspective @Word32 (radians 65) 0.1 100 extent.width extent.height

  makeRenderPipeline shaders
    (   StaticBinding  (Ur (coerce projM))
    :## DynamicBinding (Ur (coerce $ Transform identity))
    :## DynamicBinding (Ur (coerce $ vec3 0 0 0))
    :## GHNil                       )


gameLoop :: UTCTime -> RenderQueue () ⊸ Core (RenderQueue ())
gameLoop currentTime rq = Linear.do
 logT "New frame" 
 should_close <- (shouldCloseWindow ↑)
 if should_close then return rq else Linear.do
  (pollWindowEvents ↑)

  Ur newTime <- liftSystemIOU getCurrentTime

  -- Fix Your Timestep: A Very Hard Thing To Get Right. For now, the simplest approach:
  let frameTime = diffUTCTime newTime currentTime
      deltaTime = Prelude.min MAX_FRAME_TIME $ realToFrac frameTime

  -- Render the rendering queue!
  rq' <- render rq

  -- Loop!
  gameLoop newTime rq'


main :: Prelude.IO ()
main = do
 currTime <- getCurrentTime
 withLinearIO $
  runCore Linear.do
    sampler <- ( createSampler FILTER_NEAREST SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE ↑)
    tex     <- ( texture "assets/planet_gradient.png" sampler ↑)

    pipeline            <- (makeMainPipeline ↑)
    ((p1mesh, pipeline), Ur minmax) <- newPlanetMesh pipeline defaultPlanetSettings
    (pmat, pipeline)    <- newPlanetMaterial minmax tex pipeline
    (rq, Ur pkey)       <- pure (insertPipeline pipeline LMon.mempty)
    (rq, Ur mkey)       <- pure (insertMaterial pkey pmat rq)
    (rq, Ur mshkey)     <- pure (insertMesh mkey p1mesh rq)

    rq <- gameLoop currTime rq

    (freeRenderQueue rq ↑)
    -- This is all done in the freeRenderQueue!
    -- In fact, freeing these again is a type error. Woho!
    -- (freeMesh p1mesh ↑)
    -- (freeMaterial pmat ↑)
    -- (destroyRenderPipeline pipeline ↑)

    return (Ur ())

