{-# LANGUAGE OverloadedLists #-}
module Ghengin.Component.Mesh.Cube where

import Data.Vector.Storable as V
import Geomancy

import Ghengin.Component.Mesh
import Ghengin.Vulkan (Renderer)



cubeMeshVertices :: V.Vector Vertex
cubeMeshVertices =
  [
      -- left face (white)
      vertex' (-0.5) (-0.5) (-0.5) (0.9) (0.9) (0.9)
  ,   vertex' (-0.5) (0.5)  ( 0.5) (0.9) (0.9) (0.9)
  ,   vertex' (-0.5) (-0.5) ( 0.5) (0.9) (0.9) (0.9)
  ,   vertex' (-0.5) (-0.5) (-0.5) (0.9) (0.9) (0.9)
  ,   vertex' (-0.5) (0.5)  (-0.5) (0.9) (0.9) (0.9)
  ,   vertex' (-0.5) (0.5)  ( 0.5) (0.9) (0.9) (0.9)
 
      -- right face (yellow)
  ,   vertex' (0.5) (-0.5) (-0.5) (0.8) (0.8) (0.1)
  ,   vertex' (0.5) ( 0.5) ( 0.5) (0.8) (0.8) (0.1)
  ,   vertex' (0.5) (-0.5) ( 0.5) (0.8) (0.8) (0.1)
  ,   vertex' (0.5) (-0.5) (-0.5) (0.8) (0.8) (0.1)
  ,   vertex' (0.5) ( 0.5) (-0.5) (0.8) (0.8) (0.1)
  ,   vertex' (0.5) ( 0.5) ( 0.5) (0.8) (0.8) (0.1)
 
      -- top face (orange, remember y axis points down)
  ,   vertex' (-0.5) (-0.5) (-0.5) (0.9) (0.6) (0.1)
  ,   vertex' ( 0.5) (-0.5) ( 0.5) (0.9) (0.6) (0.1)
  ,   vertex' (-0.5) (-0.5) ( 0.5) (0.9) (0.6) (0.1)
  ,   vertex' (-0.5) (-0.5) (-0.5) (0.9) (0.6) (0.1)
  ,   vertex' ( 0.5) (-0.5) (-0.5) (0.9) (0.6) (0.1)
  ,   vertex' ( 0.5) (-0.5) ( 0.5) (0.9) (0.6) (0.1)
 
      -- bottom face (red)
  ,   vertex' (-0.5) (0.5) (-0.5) (0.8) (0.1) (0.1)
  ,   vertex' ( 0.5) (0.5) ( 0.5) (0.8) (0.1) (0.1)
  ,   vertex' (-0.5) (0.5) ( 0.5) (0.8) (0.1) (0.1)
  ,   vertex' (-0.5) (0.5) (-0.5) (0.8) (0.1) (0.1)
  ,   vertex' ( 0.5) (0.5) (-0.5) (0.8) (0.1) (0.1)
  ,   vertex' ( 0.5) (0.5) ( 0.5) (0.8) (0.1) (0.1)
 
      -- nose face (blue)
  ,   vertex' (-0.5) (-0.5) (0.5) (0.1) (0.1) (0.8)
  ,   vertex' ( 0.5) ( 0.5) (0.5) (0.1) (0.1) (0.8)
  ,   vertex' (-0.5) ( 0.5) (0.5) (0.1) (0.1) (0.8)
  ,   vertex' (-0.5) (-0.5) (0.5) (0.1) (0.1) (0.8)
  ,   vertex' ( 0.5) (-0.5) (0.5) (0.1) (0.1) (0.8)
  ,   vertex' ( 0.5) ( 0.5) (0.5) (0.1) (0.1) (0.8)
 
      -- tail face (green)
  ,   vertex' (-0.5) (-0.5) (-0.5) (0.1) (0.8) (0.1)
  ,   vertex' ( 0.5) ( 0.5) (-0.5) (0.1) (0.8) (0.1)
  ,   vertex' (-0.5) ( 0.5) (-0.5) (0.1) (0.8) (0.1)
  ,   vertex' (-0.5) (-0.5) (-0.5) (0.1) (0.8) (0.1)
  ,   vertex' ( 0.5) (-0.5) (-0.5) (0.1) (0.8) (0.1)
  ,   vertex' ( 0.5) ( 0.5) (-0.5) (0.1) (0.8) (0.1)
  ]
 where
   vertex' a b c d e f = Vertex (vec3 a b c) (vec3 0 0 0) (vec3 d e f)

cubeMesh :: Renderer Mesh
cubeMesh = createMesh cubeMeshVertices

