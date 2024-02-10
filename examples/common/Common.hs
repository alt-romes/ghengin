{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Common where

import Data.Coerce
import Data.Time
import Data.Time.Clock.POSIX
import FIR.Generics
import Foreign.Storable
import GHC.Float
import Geomancy.Mat4
import Geomancy.Transform
import Geomancy.Vec2
import Geomancy.Vec3
import Geomancy.Vec4
import Geomancy.Vulkan.Projection (perspective)
import Ghengin.Core
import Ghengin.Core.Log
import Ghengin.Core.Material
import Ghengin.Core.Mesh
import Ghengin.Core.Mesh.Vertex
import Ghengin.Core.Prelude as Linear
import Ghengin.Core.Render
import Ghengin.Core.Render.Packet
import Ghengin.Core.Render.Pipeline
import Ghengin.Core.Render.Property
import Ghengin.Core.Render.Queue
import Ghengin.Core.Shader (StructVec2(..), StructVec3(..), StructMat4(..), StructFloat(..))
import Ghengin.Core.Shader.Data
import Ghengin.Core.Type.Compatible
import Ghengin.Vulkan.Renderer.Sampler
import Vulkan.Core10.FundamentalTypes (Extent2D(..))
import qualified Data.Monoid.Linear as LMon
import qualified FIR
import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP
import qualified Ghengin.Core.Shader as G
import qualified Math.Linear as FIR
import qualified Prelude
import Data.Typeable

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

newtype MousePos = MousePos Vec2
  deriving ShaderData via (StructVec2 "mousePos")

newtype Time = Time Float
  deriving ShaderData via (StructFloat "val")

type PipelineProps = [MousePos, Time]


makeMainPipeline :: _ => ((Word32, Word32) -> G.ShaderPipeline _w) -> Renderer (RenderPipeline _w PipelineProps)
makeMainPipeline shaderPipeline = makeRenderPipeline (shaderPipeline WINDOW_SIZE)
  ( DynamicBinding (Ur (MousePos $ vec2 0 0))
  :## DynamicBinding (Ur (Time 0))
  :## GHNil
  )

-- | we should be getting the window size dynamically (in ghengin utils we can even pass it automatically)
-- pattern WINDOW_SIZE = (3840, 2160)
-- pattern WINDOW_SIZE = (2560, 1600)
-- pattern WINDOW_SIZE = (1920, 1200)
pattern WINDOW_SIZE = (640, 480)

renderQueueWithViewport :: (Typeable w, Compatible '[Vec3] '[] '[] PipelineProps w, _) => ((Word32, Word32) -> G.ShaderPipeline w) -> Renderer (RenderQueue (), Ur (PipelineKey w PipelineProps))
renderQueueWithViewport sp = Linear.do
    pipeline             <- makeMainPipeline sp
    -- perhaps we should allow a way to bind meshes without materials? no! they
    -- have to be compatible, and it turns out in this shader pipeline every
    -- empty material is compatible. this explicitness is good...
    (emptyMat, pipeline) <- material GHNil pipeline
    (mesh, pipeline)     <- createMeshWithIxs pipeline GHNil viewportVertices viewportIndices
    (rq, Ur pkey)        <- pure (insertPipeline pipeline LMon.mempty)
    (rq, Ur mkey)        <- pure (insertMaterial pkey emptyMat rq)
    (rq, Ur mshkey)      <- pure (insertMesh mkey mesh rq)
    return (rq, Ur pkey)
