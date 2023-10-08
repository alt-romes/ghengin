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

The function `newPlanet` handles the creation of planet render packets.

The main function

-}
module Main where

import qualified Prelude
import Data.Time
import Data.Coerce
import Foreign.Storable
import Ghengin.Core.Prelude as Linear
import Ghengin.Core.Render.Packet
import Ghengin.Core.Render.Property
import Ghengin.Core.Render
import Ghengin.Core.Shader (StructVec3(..), StructMat4(..))
import Ghengin.Core.Log
import Ghengin.Core
import Geomancy.Mat4
import Geomancy.Vec4
import Geomancy.Vec3
import Geomancy.Transform
import Geomancy.Vulkan.Projection (perspective)
import Vulkan.Core10.FundamentalTypes (Extent2D(..))
import qualified FIR
import qualified Math.Linear as FIR

import Shaders -- planet shaders

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

-- We should use Alexander's gl-block library instead of Storable, and
-- Geomancy.Transform.Tree for the node tree...

-- newPlanet :: _
-- newPlanet = _

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

main :: Prelude.IO ()
main = do
 currTime <- getCurrentTime
 withLinearIO $
  runCore Linear.do
    pipeline <- (makeMainPipeline ↑)
    gameLoop currTime
    (destroyRenderPipeline pipeline ↑)
    return (Ur ())

gameLoop :: UTCTime -> Core ()
gameLoop currentTime = Linear.do
 logT "New frame" 
 should_close <- (shouldCloseWindow ↑)
 if should_close then return () else Linear.do
  (pollWindowEvents ↑)

  Ur newTime <- liftSystemIOU getCurrentTime

  -- Fix Your Timestep: A Very Hard Thing To Get Right. For now, the simplest approach:
  let frameTime = diffUTCTime newTime currentTime
      deltaTime = Prelude.min MAX_FRAME_TIME $ realToFrac frameTime

  -- Loop!
  gameLoop newTime


pattern MAX_FRAME_TIME :: Float
pattern MAX_FRAME_TIME = 0.5
