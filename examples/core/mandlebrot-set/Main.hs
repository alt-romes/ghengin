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
import Ghengin.Core.Shader (StructVec2(..), StructVec3(..), StructMat4(..))
import Vulkan.Core10.FundamentalTypes (Extent2D(..))
import qualified Data.Monoid.Linear as LMon
import qualified FIR
import qualified Math.Linear as FIR
import qualified Prelude

import Shaders

pattern MAX_FRAME_TIME :: Float
pattern MAX_FRAME_TIME = 0.5

newtype MousePos = MousePos Vec2
  deriving Storable

viewportVertices :: [ Vertex '[Vec3] ]
viewportVertices =
  [ Sin ( vec3 (-1) (-1) 0 )
  , Sin ( vec3 (-1)   1  0 )
  , Sin ( vec3   1 (-1)  0 )
  , Sin ( vec3   1   1   0 )
  ]

viewportIndices :: [ Int ]
viewportIndices
  = [ 0, 1, 2
    , 2, 1, 3
    ]

-- | we should be getting the window size dynamically (in ghengin utils we can even pass it automatically)
pattern WINDOW_SIZE = (2560, 1600)
-- pattern WINDOW_SIZE = (1920, 1200)

makeMainPipeline :: Renderer (RenderPipeline _ '[MousePos])
makeMainPipeline = makeRenderPipeline (shaderPipeline WINDOW_SIZE)
  ( DynamicBinding (Ur (MousePos $ vec2 0 0))
  :## GHNil
  )

gameLoop :: forall (_s :: FIR.PipelineInfo). Vec2 -> PipelineKey _s '[MousePos] -> RenderQueue () ⊸ Core (RenderQueue ())
gameLoop (WithVec2 previousPosX previousPosY) pkey rq = Linear.do
 Ur should_close <- (shouldCloseWindow ↑)
 if should_close then return rq else Linear.do
  (pollWindowEvents ↑)

  Ur (double2Float -> newPosX, double2Float -> newPosY) <- (getMousePos ↑)
  liftSystemIO $ print (newPosX, newPosY)

  let pos = vec2 (0.5 * (previousPosX + newPosX)) (0.5 * (previousPosY + newPosY))

  rq'  <- (editPipeline pkey rq (propertyAt @0 (\(Ur _) -> pure $ Ur $ MousePos pos)) ↑)

  rq'' <- render rq'

  gameLoop (vec2 newPosX newPosY) pkey rq''

main :: Prelude.IO ()
main = do
 withLinearIO $
  runRenderer WINDOW_SIZE Linear.do
    pipeline             <- (makeMainPipeline ↑)
    -- perhaps we should allow a way to bind meshes without materials? no! they
    -- have to be compatible, and it turns out in this shader pipeline every
    -- empty material is compatible. this explicitness is good...
    (emptyMat, pipeline) <- (material GHNil pipeline ↑)
    (mesh, pipeline)     <- (createMeshWithIxs pipeline GHNil viewportVertices viewportIndices ↑)
    (rq, Ur pkey)        <- pure (insertPipeline pipeline LMon.mempty)
    (rq, Ur mkey)        <- pure (insertMaterial pkey emptyMat rq)
    (rq, Ur mshkey)      <- pure (insertMesh mkey mesh rq)

    Ur (x,y) <- (getMousePos ↑)
    rq <- gameLoop (vec2 (double2Float x) (double2Float y)) pkey rq

    (freeRenderQueue rq ↑)

    -- In fact, freeing these again is a type error. Woho!
    -- (destroyRenderPipeline pipeline ↑)

    return (Ur ())

