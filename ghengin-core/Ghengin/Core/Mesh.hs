{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QualifiedDo #-}
module Ghengin.Core.Mesh
  ( Mesh(..) -- Export these from an Internals module, not from here
  , SomeMesh(..)
  , createMesh
  , createMeshWithIxs
  -- , calculateFlatNormals
  -- , calculateSmoothNormals
  -- , renderMesh
  -- , freeMesh
  -- , chunksOf
  ) where

import Prelude.Linear

import Data.Typeable
import Data.Kind

import Control.Functor.Linear as Linear

-- import Data.List.Split (chunksOf)
import Data.List (sort, foldl')

import qualified Data.Map as M
import qualified Data.IntMap as IM

import Data.Vector (Vector)
import Data.Vector.Storable (Storable)
import qualified Data.Vector.Storable as SV

-- import Geomancy.Vec3

-- import qualified Apecs as A

import Ghengin.Core.Mesh.Vertex

import Ghengin.Core.Renderer.Kernel
import Ghengin.Core.Renderer.Buffer

{-
Note [Meshes]
~~~~~~~~~~~~~

All meshes are freed when the window is closed. However, if you change/discard a
mesh during the game you must free it explicitly. TODO: Enforce it somehow: linear types+reference counting

 -}

-- TODO: Renderable components should be Cache instead of Map
-- TODO: APECS ORPHAN INSTANCE OUOTSIDE OF CORE
-- instance A.Component (Mesh ts) where
--   type Storage (Mesh ts) = A.Map (Mesh ts)

type Mesh :: [Type] -> Type
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

data SomeMesh = forall ts. SomeMesh (Mesh ts)

-- Render a mesh command
-- ROMES:TODO: Certainly must get back to this
-- renderMesh :: MonadIO m => Mesh a -> RenderPassCmd m
-- renderMesh = \case
--   SimpleMesh buf _ nverts _ -> do
--       let buffers = [buf] :: Vector Vk.Buffer
--           offsets = [0]
--       bindVertexBuffers 0 buffers offsets
--       draw nverts
--   IndexedMesh vbuf _ ibuf _ nixs _ -> do
--       let buffers = [vbuf] :: Vector Vk.Buffer
--           offsets = [0]
--       bindVertexBuffers 0 buffers offsets
--       bindIndex32Buffer ibuf 0
--       drawIndexed nixs


-- | Create a Mesh given a vector of vertices
--
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
createMesh (SV.fromList -> vs) = Linear.do
  vertexBuffer <- createVertexBuffer vs
  pure (SimpleMesh vertexBuffer)

createMeshWithIxs :: Storable (Vertex ts) => [Vertex ts] -> [Int] -> Renderer (Mesh ts)
createMeshWithIxs (SV.fromList -> vertices) (SV.fromList -> ixs) = Linear.do
  vertexBuffer <- createVertexBuffer vertices
  indexBuffer  <- createIndex32Buffer (SV.map fromIntegral ixs)
  pure (IndexedMesh vertexBuffer indexBuffer)


-- ROMES:TODO
-- freeMesh :: Mesh ts -> Renderer ()
-- freeMesh mesh = do
--   () <- decRefCount mesh
--   count <- get mesh.referenceCount

--   when (count == 0) $ do
--     logDebug "Freeing mesh..."
--     case mesh of
--       SimpleMesh a b _ _ -> destroyBuffer a b
--       IndexedMesh a b c d _ _ -> destroyBuffer a b >> destroyBuffer c d

--   -- TODO: Don't include this when built for production somehow
--   -- ROMES: Make assertion instead of when
--   when (count < 0) $ do
--     logError "Destroying mesh more times than the number of assignments..."


-- TODO: Nub vertices (make indexes pointing at different vertices which are equal to point at the same vertice and remove the other)

-- ROMES:TODO: MOVE THESE TWO FUNCTIONS OUT OF CORE

-- | Calculate normals of vertices given vertex positions and the indices that describe the faces
-- The returned list has a normal for each position in the input positions, in the same order
-- calculateFlatNormals :: [Int] -> [Vec3] -> [Vec3]
-- calculateFlatNormals ixs (SV.fromList -> pos) =

--   let m = foldl' (\acc [a,b,c] ->
--             let vab = (pos SV.! b) - (pos SV.! a)
--                 vbc = (pos SV.! c) - (pos SV.! b)
--                 n = normalize $ cross vbc vab -- vbc X vab gives the normal facing up for clockwise faces
--              in IM.insertWith const a n $ IM.insertWith const b n $ IM.insertWith const c n acc) mempty (chunksOf 3 ixs)

--    in map snd $ sort (IM.toList m)

-- | Calculate smooth normals of vertices given vertex positions and the
-- indices that describe the faces The returned list has a normal for each
-- position in the input positions, in the same order
--
-- TODO: Take into consideration the angles or provide alternative that does
-- calculateSmoothNormals :: [Int] -> [Vec3] -> [Vec3]
-- calculateSmoothNormals ixs pos =

--   let fns = calculateFlatNormals ixs pos

--       smoothNormalsMap = foldl' (\acc (p,n) -> M.insertWith (\(na, i) (nb, j) -> (na + nb, i + j)) p (n,1) acc) mempty (zip pos fns)

--    in map (\p -> case smoothNormalsMap M.! p of (n,b) -> n^/b) pos

