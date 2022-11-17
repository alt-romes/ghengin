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
module Ghengin.Component.Mesh where

import GHC.Records
import Foreign.Storable
import Data.Vector (Vector)

import Geomancy

import Apecs

import qualified Vulkan as Vk

data Vertex = Vertex { position :: {-# UNPACK #-} !Vec3
                     , normal   :: {-# UNPACK #-} !Vec3
                     , color    :: {-# UNPACK #-} !Vec3
                     } deriving Show

instance Storable Vertex where
  sizeOf _ = 3 * sizeOf @Vec3 undefined
  alignment _ = 4
  peek _   = fail "peek: Vertex"
  poke _ _ = fail "poke: Vertex"

newtype Mesh = Mesh { vertices :: Vector Vertex }
  deriving Show

instance Component Mesh where
  type Storage Mesh = Map Mesh

-- TODO: Instructions on having a World record with "meshes"

instance (Monad m, HasField "meshes" w (Storage Mesh)) => Has w m Mesh where
  getStore = SystemT (asks (.meshes))


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


