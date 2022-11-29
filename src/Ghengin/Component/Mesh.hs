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
  ( Mesh
  , Vertex(..)
  , createMesh
  , createMeshWithIxs
  , calculateFlatNormals
  , calculateSmoothNormals
  , renderMesh
  , vertexInputBindingDescription
  , vertexInputAttributeDescriptions
  , chunksOf
  ) where

import GHC.Records
import Data.List.Split (chunksOf)
import Data.List (sort, foldl')

import Foreign.Ptr
import Foreign.Storable
import Data.Word
import qualified Data.Map as M
import qualified Data.IntMap as IM

import Data.Vector (Vector)
import qualified Data.Vector.Storable as SV

import Geomancy.Vec3

import Apecs

import qualified Vulkan as Vk

import Ghengin.Vulkan.Command
import Ghengin.Vulkan.Buffer
import Ghengin.Vulkan

data Vertex = Vertex { position :: {-# UNPACK #-} !Vec3
                     , normal   :: {-# UNPACK #-} !Vec3
                     , color    :: {-# UNPACK #-} !Vec3
                     } deriving Show

instance Storable Vertex where
  sizeOf _ = 3 * sizeOf @Vec3 undefined
  alignment _ = 4
  peek (castPtr -> p) = do
    pos <- peek p
    normal <- peekElemOff p 1
    color <- peekElemOff p 2
    pure $ Vertex pos normal color
  poke (castPtr -> p) (Vertex pos normal color) = do
    poke p pos
    pokeElemOff p 1 normal
    pokeElemOff p 2 color

data Mesh = SimpleMesh { vertexBuffer       :: {-# UNPACK #-} !Vk.Buffer -- a vector of vertices in buffer format
                       , vertexBufferMemory :: {-# UNPACK #-} !Vk.DeviceMemory -- we later need to free this as well
                       , nVertices :: Word32 -- ^ We save the number of vertices to pass to the draw function
                       -- We don't need to keep the Vector Vertex that was originally (unless we wanted to regenerate it every time?)
                       -- used to create this Mesh, bc having the vertex buffer and
                       -- the device memory is morally equivalent
                       -- , vertices :: Vector Vertex
                       }
          | IndexedMesh { vertexBuffer       :: {-# UNPACK #-} !Vk.Buffer -- a vector of vertices in buffer format
                        , vertexBufferMemory :: {-# UNPACK #-} !Vk.DeviceMemory -- vertices device memoy -- we later need to free this as well
                        , indexBuffer        :: {-# UNPACK #-} !Vk.Buffer -- vertices indexes in buffer
                        , indexBufferMemory  :: {-# UNPACK #-} !Vk.DeviceMemory -- indexes device memory -- TODO TO BE FREED AND BUFFER TOO
                        , nIndexes           :: Word32 -- ^ We save the number of indexes to pass to the draw function
                        }
          deriving Show

      -- TODO: Various kinds of meshes: indexed meshes, strip meshes, just triangles...

instance Component Mesh where
  type Storage Mesh = Map Mesh

-- TODO: Instructions on having a World record with "meshes"

instance (Monad m, HasField "meshes" w (Storage Mesh)) => Has w m Mesh where
  getStore = SystemT (asks (.meshes))


-- Render a mesh command
renderMesh :: Mesh -> RenderPassCmd
renderMesh = \case
  SimpleMesh buf _ nverts -> do
      let buffers = [buf] :: Vector Vk.Buffer
          offsets = [0]
      bindVertexBuffers 0 buffers offsets
      draw nverts
  IndexedMesh vbuf _ ibuf _ nixs -> do
      let buffers = [vbuf] :: Vector Vk.Buffer
          offsets = [0]
      bindVertexBuffers 0 buffers offsets
      bindIndex32Buffer ibuf 0
      drawIndexed nixs


-- | Create a Mesh given a vector of vertices
-- TODO: Clean all Mesh vertex buffers
createMesh :: SV.Vector Vertex -> Renderer Mesh
createMesh vs = do
  let nverts = SV.length vs
  (buffer, devMem) <- createVertexBuffer vs
  pure (SimpleMesh buffer devMem (fromIntegral nverts))

createMeshWithIxs :: [Vertex] -> [Int] -> Renderer Mesh
createMeshWithIxs (SV.fromList -> vertices) (SV.fromList -> ixs) = do
  let nixs = SV.length ixs
  (vbuffer, vbMem) <- createVertexBuffer vertices
  (ibuffer, ibMem) <- createIndex32Buffer (SV.map fromIntegral ixs)

  pure (IndexedMesh vbuffer vbMem ibuffer ibMem (fromIntegral nixs))


-- TODO: Nub vertices (make indexes pointing at different vertices which are equal to point at the same vertice and remove the other)

-- | Calculate normals of vertices given vertex positions and the indices that describe the faces
-- The returned list has a normal for each position in the input positions, in the same order
calculateFlatNormals :: [Int] -> [Vec3] -> [Vec3]
calculateFlatNormals ixs (SV.fromList -> pos) =

  let m = foldl' (\acc [a,b,c] ->
            let vab = (pos SV.! b) - (pos SV.! a)
                vbc = (pos SV.! c) - (pos SV.! b)
                n = normalize $ cross vbc vab -- vbc X vab gives the normal facing up for clockwise faces
             in IM.insertWith (\x _ -> x) a n $ IM.insertWith (\x _ -> x) b n $ IM.insertWith (\x _ -> x) c n acc) mempty (chunksOf 3 ixs)

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


vertexInputBindingDescription :: Vk.VertexInputBindingDescription
vertexInputBindingDescription = Vk.VertexInputBindingDescription { binding = 0 -- We use only one binding for now?, its index is always 0
                                                                 , stride  = fromIntegral $ sizeOf @Vertex undefined
                                                                 , inputRate = Vk.VERTEX_INPUT_RATE_VERTEX -- Move to the next data entry after each vertex
                                                                 }

vertexInputAttributeDescriptions :: Vector Vk.VertexInputAttributeDescription
vertexInputAttributeDescriptions = [ Vk.VertexInputAttributeDescription { binding = 0
                                                                        , location = 0
                                                                        , format = Vk.FORMAT_R32G32B32_SFLOAT
                                                                        , offset = 0
                                                                        }
                                   , Vk.VertexInputAttributeDescription { binding = 0
                                                                        , location = 1
                                                                        , format = Vk.FORMAT_R32G32B32_SFLOAT
                                                                        , offset = fromIntegral $ sizeOf @Vec3 undefined
                                                                        }
                                   , Vk.VertexInputAttributeDescription { binding = 0
                                                                        , location = 2
                                                                        , format = Vk.FORMAT_R32G32B32_SFLOAT
                                                                        , offset = fromIntegral $ 2 * sizeOf @Vec3 undefined
                                                                        }
                                   ]

