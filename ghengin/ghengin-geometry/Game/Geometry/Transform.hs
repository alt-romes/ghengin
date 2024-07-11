module Game.Geometry.Transform
  ( transform
  , module Geomancy.Transform
  , module Geomancy.Vec3
  )
  where

import Ghengin.Core.Mesh.Vertex
import Geomancy.Vec3
import Geomancy.Transform

-- | Transform a list of vertices.
-- Use with functions re-exported from Geomancy.Transform
--
-- Recall, from Geomancy's documentation:
--   CPU-side matrices compose in MVP order, optimized for mconcat (local1 : local2 : ... : root) operation.
--   GPU-side, in GLSL, it is PVM * v.
transform :: Transform -> [Vertex '[Vec3]] -> [Vertex '[Vec3]]
transform tr vs = [ Sin (v `apply` tr) | Sin v <- vs ]

