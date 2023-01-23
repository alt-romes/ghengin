{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-} -- instance Has w m Mesh
module Ghengin.Component.Mesh
  ( Mesh(..) -- Export these from an Internals module, not from here
  , SomeMesh(..)
  , createMesh
  , createMeshWithIxs
  , calculateFlatNormals
  , calculateSmoothNormals
  , renderMesh
  , freeMesh
  , chunksOf
  ) where

import Data.Kind
import Control.Monad
import Data.IORef
import GHC.TypeLits
import GHC.Records
import Data.List.Split (chunksOf)
import Data.List (sort, foldl')
import Control.Monad.IO.Class

import Foreign.Ptr
import qualified Foreign.Storable as S
import Data.Word
import qualified Data.Map as M
import qualified Data.IntMap as IM

import Data.Vector (Vector)
import qualified Data.Vector.Storable as SV

import Geomancy.Vec3

import qualified Apecs as A

import qualified Vulkan as Vk

import Ghengin.Vulkan.Command
import Ghengin.Vulkan.Buffer
import Ghengin.Vulkan
import Ghengin.Component.Mesh.Vertex
import Ghengin.Utils

{-
Note [Meshes]
~~~~~~~~~~~~~

All meshes are freed when the window is closed. However, if you change/discard a
mesh during the game you must free it explicitly. TODO: Enforce it somehow

 -}

instance (Monad m, HasField "meshes" w (A.Storage (Mesh ts))) => A.Has w m (Mesh ts) where
  getStore = A.SystemT (A.asks (.meshes))

-- TODO: Renderable components should be Cache instead of Map
instance A.Component (Mesh ts) where
  type Storage (Mesh ts) = A.Map (Mesh ts)

type Mesh :: [Type] -> Type
data Mesh ts = SimpleMesh { vertexBuffer       :: {-# UNPACK #-} !Vk.Buffer -- a vector of vertices in buffer format
                          , vertexBufferMemory :: {-# UNPACK #-} !Vk.DeviceMemory
                          , nVertices          :: {-# UNPACK #-} !Word32 -- ^ TODO: With the type level information this is no longer needed. We save the number of vertices to pass to the draw function

                          -- We don't need to keep the Vector Vertex that was originally (unless we wanted to regenerate it every time?)
                          -- used to create this Mesh, bc having the vertex buffer and
                          -- the device memory is morally equivalent
                          -- , vertices :: Vector Vertex

                          , referenceCount     :: !(IORef Int) -- ^ Count the number of references to this mesh so freeMesh can take it into account
                          }
             | IndexedMesh { vertexBuffer       :: {-# UNPACK #-} !Vk.Buffer -- a vector of vertices in buffer format
                           , vertexBufferMemory :: {-# UNPACK #-} !Vk.DeviceMemory -- vertices device memoy
                           , indexBuffer        :: {-# UNPACK #-} !Vk.Buffer -- vertices indexes in buffer
                           , indexBufferMemory  :: {-# UNPACK #-} !Vk.DeviceMemory -- indexes device memory
                           , nIndexes           :: {-# UNPACK #-} !Word32 -- ^ We save the number of indexes to pass to the draw function
                           , referenceCount     :: !(IORef Int) -- ^ Count the number of references to this mesh so freeMesh can take it into account
                           }

      -- TODO: Various kinds of meshes: indexed meshes, strip meshes, just triangles...

data SomeMesh = forall ts. SomeMesh (Mesh ts)

-- Render a mesh command
renderMesh :: MonadIO m => Mesh a -> RenderPassCmd m
renderMesh = \case
  SimpleMesh buf _ nverts _ -> do
      let buffers = [buf] :: Vector Vk.Buffer
          offsets = [0]
      bindVertexBuffers 0 buffers offsets
      draw nverts
  IndexedMesh vbuf _ ibuf _ nixs _ -> do
      let buffers = [vbuf] :: Vector Vk.Buffer
          offsets = [0]
      bindVertexBuffers 0 buffers offsets
      bindIndex32Buffer ibuf 0
      drawIndexed nixs


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
createMesh :: Storable (Vertex ts) => VertexArray ts -> Renderer ext (Mesh ts)
createMesh (VertexArray vs) = do
  let nverts = SV.length vs
  (buffer, devMem) <- createVertexBuffer vs
  ref <- liftIO $ newIORef 0
  pure (SimpleMesh buffer devMem (fromIntegral nverts) ref)

createMeshWithIxs :: Storable a => [a] -> [Int] -> Renderer ext (Mesh ts)
createMeshWithIxs (SV.fromList -> vertices) (SV.fromList -> ixs) = do
  let nixs = SV.length ixs
  (vbuffer, vbMem) <- createVertexBuffer vertices
  (ibuffer, ibMem) <- createIndex32Buffer (SV.map fromIntegral ixs)

  ref <- liftIO $ newIORef 0
  pure (IndexedMesh vbuffer vbMem ibuffer ibMem (fromIntegral nixs) ref)


freeMesh :: Mesh ts -> Renderer ext ()
freeMesh mesh = do
  () <- decRefCount mesh
  count <- get mesh.referenceCount

  when (count == 0) $ do
    logDebug "Freeing mesh..."
    case mesh of
      SimpleMesh a b _ _ -> destroyBuffer a b
      IndexedMesh a b c d _ _ -> destroyBuffer a b >> destroyBuffer c d

  -- TODO: Don't include this when built for production somehow
  when (count < 0) $ do
    logError "Destroying mesh more times than the number of assignments..."


-- TODO: Nub vertices (make indexes pointing at different vertices which are equal to point at the same vertice and remove the other)

-- | Calculate normals of vertices given vertex positions and the indices that describe the faces
-- The returned list has a normal for each position in the input positions, in the same order
calculateFlatNormals :: [Int] -> [Vec3] -> [Vec3]
calculateFlatNormals ixs (SV.fromList -> pos) =

  let m = foldl' (\acc [a,b,c] ->
            let vab = (pos SV.! b) - (pos SV.! a)
                vbc = (pos SV.! c) - (pos SV.! b)
                n = normalize $ cross vbc vab -- vbc X vab gives the normal facing up for clockwise faces
             in IM.insertWith const a n $ IM.insertWith const b n $ IM.insertWith const c n acc) mempty (chunksOf 3 ixs)

   in map snd $ sort (IM.toList m)

-- | Calculate smooth normals of vertices given vertex positions and the
-- indices that describe the faces The returned list has a normal for each
-- position in the input positions, in the same order
--
-- TODO: Take into consideration the angles or provide alternative that does
calculateSmoothNormals :: [Int] -> [Vec3] -> [Vec3]
calculateSmoothNormals ixs pos =

  let fns = calculateFlatNormals ixs pos

      smoothNormalsMap = foldl' (\acc (p,n) -> M.insertWith (\(na, i) (nb, j) -> (na + nb, i + j)) p (n,1) acc) mempty (zip pos fns)

   in map (\p -> case smoothNormalsMap M.! p of (n,b) -> n^/b) pos

