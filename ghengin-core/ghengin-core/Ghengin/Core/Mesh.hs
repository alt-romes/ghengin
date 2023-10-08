{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Ghengin.Core.Mesh
  ( Mesh(..) -- Export these from an Internals module, not from here
  , Some(..)
  , createMesh
  , createMeshWithIxs
  -- , calculateFlatNormals
  -- , calculateSmoothNormals
  , freeMesh

  -- * Vertices
  , module Ghengin.Core.Mesh.Vertex
  ) where

import Prelude.Linear

import Data.Kind

import Control.Functor.Linear as Linear
import Data.Vector.Storable (Storable)
import qualified Data.Vector.Storable as SV

import Ghengin.Core.Mesh.Vertex

import Ghengin.Core.Renderer.Kernel
import Ghengin.Core.Renderer.Buffer

import Ghengin.Core.Log
import Ghengin.Core.Type.Utils (Some(..))

{-
Note [Meshes]
~~~~~~~~~~~~~

OUTDATED!

All meshes are freed when the window is closed. However, if you change/discard a
mesh during the game you must free it explicitly. TODO: Enforce it somehow: linear types+reference counting

 -}

type Mesh :: [Type] -> Type
data Mesh ts = SimpleMesh { vertexBuffer       :: !VertexBuffer -- a vector of vertices in buffer format

                          -- We don't need to keep the Vector Vertex that was originally (unless we wanted to regenerate it every time?)
                          -- used to create this Mesh, bc having the vertex buffer and
                          -- the device memory is morally equivalent
                          -- , vertices :: Vector Vertex

                          }
             | IndexedMesh { vertexBuffer       :: !VertexBuffer -- a vector of vertices in buffer format
                           , indexBuffer        :: !Index32Buffer
                           }

      -- TODO: Various kinds of meshes: indexed meshes, strip meshes, just triangles...

-- | Create a Mesh given a vector of vertices
--
-- Outdated:
-- Initially there exist no references to this mesh, and if we never assign it
-- to a render entity it will never be freed. However, when a mesh is added to
-- a render packet using 'renderPacket', a reference count is added.
--
-- A mesh will only be freed when its reference count reaches zero.
--
-- If you delete/change meshes at runtime you must ensure they are freed
-- because we simply free them at the end. One must free the same meshes as
-- many times as they are shared for they will only be freed with the last
-- reference
createMesh :: Storable (Vertex ts) => [Vertex ts] -> Renderer (Mesh ts)
createMesh (SV.fromList -> vs) = enterD "createMesh" Linear.do
  vertexBuffer <- createVertexBuffer vs
  pure (SimpleMesh vertexBuffer)

createMeshWithIxs :: Storable (Vertex ts) => [Vertex ts] -> [Int] -> Renderer (Mesh ts)
createMeshWithIxs (SV.fromList -> vertices) (SV.fromList -> ixs) = enterD "createMeshWithIxs" Linear.do
  logT "Creating Vertex Buffer"
  vertexBuffer <- createVertexBuffer vertices
  logT "Creating Index Buffer"
  indexBuffer  <- createIndex32Buffer (SV.map fromIntegral ixs)
  pure (IndexedMesh vertexBuffer indexBuffer)


freeMesh :: Mesh ts ⊸ Renderer ()
freeMesh mesh = Linear.do
  logD "Freeing mesh..."
  case mesh of
    SimpleMesh (VertexBuffer vb _) -> destroyDeviceLocalBuffer vb
    IndexedMesh (VertexBuffer vb _) (Index32Buffer ib _) -> destroyDeviceLocalBuffer vb >> destroyDeviceLocalBuffer ib


-- TODO: Nub vertices (make indexes pointing at different vertices which are equal to point at the same vertice and remove the other)
