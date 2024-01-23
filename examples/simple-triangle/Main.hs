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
import Geomancy.Vec2

import Shaders

-- ROMES:TODO: Using Vec2 instead of Vec3 breaks the simple triangle drawing.
-- Why? Because of alignment issues. I'm unsure if the fault is in my handling
-- of Storable Vectors, or in FIR. It is easy to see the issue is alignment by
-- using this set of vertices for example:
--   [ vec2 0 (-0.5)
--   , vec2 0 0
--   , vec2 0.5 0.5
--   , vec2 0 0
--   , vec2 (-0.5) 0.5
--   , vec2 0 0
--   ]
-- This way we get the correct triangle, because the intermediate "vec2 0 0"
-- are ignored because of alignment.
triangleVertices :: [Vertex '[Vec3]]
triangleVertices =
  [ Sin $ vec3 0 (-0.5) 1
  , Sin $ vec3 0.5 0.5 1
  , Sin $ vec3 (-0.5) 0.5 1
  ]

gameLoop :: RenderQueue () ⊸ Core (RenderQueue ())
gameLoop rq = Linear.do
 should_close <- (shouldCloseWindow ↑)
 if should_close then return rq else Linear.do
  (pollWindowEvents ↑)

  rq <- render rq

  gameLoop rq

main :: Prelude.IO ()
main = do
 withLinearIO $
  runCore (640, 480) Linear.do
    pipeline             <- (makeRenderPipeline shaderPipeline GHNil ↑)
    (emptyMat, pipeline) <- (material GHNil pipeline ↑)
    (mesh, pipeline)     <- (createMeshWithIxs pipeline GHNil triangleVertices [0, 1, 2] ↑)
    (rq, Ur pkey)        <- pure (insertPipeline pipeline LMon.mempty)
    (rq, Ur mkey)        <- pure (insertMaterial pkey emptyMat rq)
    (rq, Ur mshkey)      <- pure (insertMesh mkey mesh rq)

    rq <- gameLoop rq

    (freeRenderQueue rq ↑)

    -- In fact, freeing these again is a type error. Woho!
    -- (destroyRenderPipeline pipeline ↑)

    return (Ur ())

