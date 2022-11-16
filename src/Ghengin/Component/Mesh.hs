{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
module Ghengin.Component.Mesh where

import Foreign.Storable
import Data.Vector (Vector)

import Geomancy

import qualified Vulkan as Vk

data Vertex = Vertex { position :: {-# UNPACK #-} !Vec3
                     , normal   :: {-# UNPACK #-} !Vec3
                     , color    :: {-# UNPACK #-} !Vec3
                     }

instance Storable Vertex where
  sizeOf _ = 3 * sizeOf @Vec3 undefined
  alignment _ = 4
  peek _   = fail "peek: Vertex"
  poke _ _ = fail "poke: Vertex"


newtype Mesh = Mesh { vertices :: Vector Vertex }

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


