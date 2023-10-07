-- | This module currently duplicates Buffer.hsig in ghengin-core, so it can be used in ghengin-vulkan
module Ghengin.Vulkan.Renderer.Buffer where

import Data.Word (Word, Word32)
import Data.Int (Int32)
import qualified Data.Vector.Storable as SV
-- TODO:Exchange Vk specific types to renderer agnostic enumerations
import qualified Vulkan as Vk (DescriptorType, BufferUsageFlags, Buffer, DeviceMemory)

import {-# SOURCE #-} Ghengin.Vulkan.Renderer.Kernel
import Ghengin.Core.Mesh.Vertex

import Data.Linear.Alias (Aliasable)

-------- Specific buffers --------------

data Index32Buffer where
  Index32Buffer :: !DeviceLocalBuffer
                 ⊸ Word32                 -- ^ N indices
                -> Index32Buffer

data VertexBuffer where
  VertexBuffer :: !DeviceLocalBuffer
                ⊸ Word32               -- ^ N vertices
               -> VertexBuffer

-------- Device-local buffer -----------

data DeviceLocalBuffer where
  DeviceLocalBuffer :: {-# UNPACK #-} !Vk.Buffer
                     ⊸ {-# UNPACK #-} !Vk.DeviceMemory
                     -- ⊸ Word -- Size
                     ⊸ DeviceLocalBuffer

-------- Mapped Buffer -----------------

-- | A mapped (uniform) buffer. See Note [Mapped vs Device-local Buffers]
data MappedBuffer

