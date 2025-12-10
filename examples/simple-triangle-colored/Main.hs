{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PostfixOperators #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import Geomancy.Vec3
import Ghengin.Core
import Ghengin.Core.Mesh
import Ghengin.Core.Material
import Ghengin.Core.Prelude as Linear
import Ghengin.Core.Render
import Ghengin.Core.Render.Pipeline
import Ghengin.Core.Render.Queue
import qualified Data.Monoid.Linear as LMon
import qualified Prelude
import qualified Data.Linear.Alias as Alias
import Shaders

triangleVerticesWithColors :: [Vertex '[Vec3, Vec3]]
triangleVerticesWithColors =
  [ vec3 0 (-0.5)   1 :&: vec3 1 0 0
  , vec3 (-0.5) 0.5 1 :&: vec3 0 0 1
  , vec3 0.5 0.5    1 :&: vec3 0 1 0
  ]

gameLoop :: Alias RenderPass ⊸ RenderQueue () ⊸ Renderer (RenderQueue ())
gameLoop rp rq = Linear.do
 should_close <- shouldCloseWindow
 if should_close then Alias.forget rp >> return rq else Linear.do
  pollWindowEvents

  (rp, rq) <- render rp rq

  gameLoop rp rq

main :: Prelude.IO ()
main = do
 withLinearIO $
  runRenderer (640, 480) Linear.do
    (rp1, rp2) <- Alias.share =<< createSimpleRenderPass

    pipeline <- makeRenderPipeline rp1 shaderPipelineColors GHNil
    (emptyMat, pipeline) <- material GHNil pipeline
    (mesh, pipeline) <- createMeshWithIxs pipeline GHNil triangleVerticesWithColors [0, 1, 2]
    (rq, Ur pkey)    <- pure (insertPipeline pipeline LMon.mempty)
    (rq, Ur mkey)    <- pure (insertMaterial pkey emptyMat rq)
    (rq, Ur mshkey)  <- pure (insertMesh mkey mesh rq)

    rq <- gameLoop rp2 rq

    freeRenderQueue rq

    -- In fact, freeing these again is a type error. Woho!
    -- destroyRenderPipeline pipeline

    return (Ur ())

