{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import GHC.Generics
import Geomancy.Vec3
import Geomancy.Mat4
import Geomancy.Transform
import Geomancy.Vulkan.View
import Geomancy.Vulkan.Projection
import Ghengin.Core
import Ghengin.Core.Shader.Data
import Ghengin.Core.Mesh
import Ghengin.Core.Material
import Ghengin.Core.Prelude as Linear
import Ghengin.Core.Render
import Ghengin.Core.Render.Property
import Ghengin.Core.Render.Pipeline
import Ghengin.Core.Render.Queue
import Data.List.Linear ()
import qualified Data.Monoid.Linear as LMon
import qualified Prelude
import qualified Math.Linear as FIR
import qualified FIR

-- ghengin:camera
import Ghengin.Camera

import qualified Prelude
import Shaders

type IcosahedronMesh = Mesh '[Vec3, Vec3] '[]

icosahedronVerts :: [Vertex '[Vec3, Vec3]]
icosahedronVerts =
  [ ( vec3  0     1    phi  )   :&: ( vec3 0    1    0    )
  , ( vec3    0   (-1)   phi  ) :&: ( vec3 0    0.75 0.25 )
  , ( vec3    0     1  (-phi) ) :&: ( vec3 0    0.25 0.75 )
  , ( vec3    0   (-1) (-phi) ) :&: ( vec3 0    0    1    )
  , ( vec3    1    phi    0   ) :&: ( vec3 1    0    0    )
  , ( vec3  (-1)   phi    0   ) :&: ( vec3 0.75 0.25 0    )
  , ( vec3    1  (-phi)   0   ) :&: ( vec3 0.25 0.75 0    )
  , ( vec3  (-1) (-phi)   0   ) :&: ( vec3 0    1    0    )
  , ( vec3   phi    0     1   ) :&: ( vec3 1    0    0    )
  , ( vec3   phi    0   (-1)  ) :&: ( vec3 0.75 0    0.25 )
  , ( vec3 (-phi)   0     1   ) :&: ( vec3 0.25 0    0.75 )
  , ( vec3 (-phi)   0   (-1)  ) :&: ( vec3 0    0    1    )
  ]
    where
      phi = 0.5 + sqrt 1.25

icosahedronIndices :: [ Int32 ]
icosahedronIndices
  = [ 0,  1,  8
    , 0, 10,  1
    , 0,  4,  5
    , 0,  8,  4
    , 0,  5, 10
    , 1,  7,  6
    , 1,  6,  8
    , 1, 10,  7
    , 2,  9,  3
    , 2,  3, 11
    , 2,  5,  4
    , 2,  4,  9
    , 2, 11,  5
    , 3,  6,  7
    , 3,  9,  6
    , 3,  7, 11
    , 4,  8,  9
    , 5, 11, 10
    , 6,  9,  8
    , 7, 10, 11
    ]

gameLoop :: PipelineKey _ '[Camera "view" "proj"] -- ^ rq key to camera
         -> MeshKey _ _ _ _ '[] -- ^ rq key to cube mesh
         -> RenderQueue ()
          ⊸ Core (RenderQueue ())
gameLoop ckey mkey rq = Linear.do
 should_close <- (shouldCloseWindow ↑)
 if should_close then return rq else Linear.do
  (pollWindowEvents ↑)

  rq <- render rq

  gameLoop ckey mkey rq

main :: Prelude.IO ()
main = 
 withLinearIO $
  runCore (640, 480) Linear.do
    pipeline :: RenderPipeline π ps <- (makeRenderPipeline shaderPipeline (StaticBinding (Ur (defaultCamera @"view" @"proj")) :## GHNil) ↑)
    (emptyMat, pipeline) <- (material GHNil pipeline ↑)
    (mesh :: IcosahedronMesh, pipeline) <- (createMeshWithIxs pipeline GHNil icosahedronVerts icosahedronIndices ↑)

    (rq, Ur pkey)    <- pure (insertPipeline @π @ps pipeline LMon.mempty)
    (rq, Ur mkey)    <- pure (insertMaterial @π @ps pkey emptyMat rq)
    (rq, Ur mshkey)  <- pure (insertMesh @π @ps mkey mesh rq)

    rq <- gameLoop pkey mshkey rq

    (freeRenderQueue rq ↑)

    return (Ur ())
