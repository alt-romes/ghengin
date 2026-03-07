{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLists #-}
module Ghengin.Vulkan.Renderer.Buffer where

import qualified Control.Category as Category
import Ghengin.Core.Prelude as Linear
import Ghengin.Core.Log
import qualified Prelude

import Foreign.Storable

import qualified Data.Linear.Alias as Alias

import qualified Data.Vector.Storable as SV
import Foreign.Ptr
import Foreign.Ptr.Diff (Diff(..))
import Foreign.Marshal.Utils
import Data.Bits
import Vulkan.Zero (zero)
import qualified Vulkan as Vk
import qualified Vulkan.Zero as Vk
import qualified Data.V.Linear as V

import FIR.Vulkan.Memory
import Ghengin.Vulkan.Renderer.Kernel
import Ghengin.Vulkan.Renderer.Context
import Ghengin.Core.Mesh.Vertex
import Ghengin.Vulkan.Renderer.Command

import qualified Unsafe.Linear as Unsafe

--------------------------------------------------------------------------------
-- * Commands
--------------------------------------------------------------------------------

drawVertexBuffer :: Linear.MonadIO m => VertexBuffer ⊸ RenderCmdM m VertexBuffer
drawVertexBuffer (VertexBuffer (DeviceLocalBuffer buf mem) nverts) = Linear.do
  let offsets = V.make 0
  buffers' <- bindVertexBuffers 0 (V.make buf :: V.V 1 Vk.Buffer) offsets
  draw nverts 1
  pure (VertexBuffer (DeviceLocalBuffer (V.elim (\x -> x) buffers') mem) nverts)

drawVertexBufferIndexed :: Linear.MonadIO m => VertexBuffer ⊸ Index32Buffer ⊸ RenderCmdM m (VertexBuffer, Index32Buffer)
drawVertexBufferIndexed (VertexBuffer (DeviceLocalBuffer vbuf mem) nverts) (Index32Buffer (DeviceLocalBuffer ibuf imem) nixs) = Linear.do
  let offsets = V.make 0
  buffers' <- bindVertexBuffers 0 (V.make vbuf) offsets
  ibuf'    <- bindIndex32Buffer ibuf 0
  drawIndexed nixs 1
  pure ( VertexBuffer (DeviceLocalBuffer (V.elim (\x -> x) buffers') mem) nverts
       , Index32Buffer (DeviceLocalBuffer ibuf' imem) nixs
       )

-------- Specific buffers --------------

-- inlined from ghengin-core
data Index32Buffer where
  Index32Buffer :: !DeviceLocalBuffer
                 ⊸ Word32 -- ^ N indices
                -> Index32Buffer

createIndex32Buffer :: SV.Vector Int32 -> Renderer Index32Buffer
createIndex32Buffer vv =
  flip Index32Buffer (fromIntegral $ SV.length vv) <$>
    createDeviceLocalBuffer Vk.BUFFER_USAGE_INDEX_BUFFER_BIT vv

data VertexBuffer where
  VertexBuffer :: !DeviceLocalBuffer
                ⊸ Word32 -- ^ N vertices
               -> VertexBuffer

createVertexBuffer :: ∀ αs. Storable (Vertex αs) => SV.Vector (Vertex αs) -> Renderer VertexBuffer
createVertexBuffer vv =
  flip VertexBuffer (fromIntegral $ SV.length vv) <$>
    createDeviceLocalBuffer @(Vertex αs) Vk.BUFFER_USAGE_VERTEX_BUFFER_BIT vv -- use Locations for vertex buffers

-------- Device-local buffer -----------

data DeviceLocalBuffer where
  DeviceLocalBuffer :: {-# UNPACK #-} !Vk.Buffer
                     ⊸ {-# UNPACK #-} !Vk.DeviceMemory
                     ⊸ DeviceLocalBuffer

-- | Fills a device (GPU) local buffer with the provided flags and the provided data
-- by first copying the data to a staging buffer and then running a buffer copy
-- one-shot command.
createDeviceLocalBuffer :: ∀ α. SV.Storable α => Vk.BufferUsageFlags -> SV.Vector α -> Renderer DeviceLocalBuffer
createDeviceLocalBuffer flags bufferData = enterD "createDeviceLocalBuffer" Linear.do

  withStagingBuffer bufferData $ \stagingBuffer bufferSize -> Linear.do

    (devBuffer, devMem) <- createBuffer bufferSize (Vk.BUFFER_USAGE_TRANSFER_DST_BIT .|. flags) Vk.MEMORY_PROPERTY_DEVICE_LOCAL_BIT

    -- Copy data from staging buffer to actual buffer inaccessible by the host
    (stagingBuffer', devBuffer') <- copyBuffer stagingBuffer devBuffer bufferSize

    destroyBuffer stagingBuffer'

    pure $ DeviceLocalBuffer devBuffer' devMem

destroyDeviceLocalBuffer :: DeviceLocalBuffer ⊸ Renderer ()
destroyDeviceLocalBuffer (DeviceLocalBuffer b dm) = enterD "destroyDeviceLocalBuffer" Linear.do
  destroyBuffer b
  freeMemory dm

-------- Mapped Buffer -----------------

-- | A mapped buffer with size equal to the sizeOf of the Storable @a@
--
-- This buffer has e.g. USAGE_UNIFORM_BUFFER_BIT and MEMORY_PROPRTY_HOST_VISIBLE and
-- MEMORY_PROPERTY_HOST_COHERENT_BIT -- we allocate device-local and host-local
-- memory and writing to the mapped buffer entails writing to the host memory
-- which is mapped to device memory and hence synchronized automatically
--
-- This is unlike DeviceLocalBuffers, which are allocated on the device and
-- require a staging buffer and a copy command to be written
data MappedBuffer = MappedBuffer { buffer  :: {-# UNPACK #-} !Vk.Buffer
                                 , devMem  :: {-# UNPACK #-} !Vk.DeviceMemory
                                 , hostMem :: {-# UNPACK #-} !(Ptr ())
                                   -- ^ When `DeviceMemory` is mapped, we get a `hostMem` pointer to it.
                                 , bufSize :: {-# UNPACK #-} !(Ur Word)
                                 }

data BufferType = Uniform | Storage
  deriving Show

-- | Create a uniform buffer with a given size, but don't copy memory to it
-- yet. See 'writeMappedBuffer' for that.
createMappedBuffer :: Word -> BufferType -> Renderer (Alias MappedBuffer)
createMappedBuffer size descriptorType = enterD "createMappedBuffer" Linear.do
  let bsize = fromIntegral size

  (buf, devMem0) <- createBuffer bsize (bufferUsageBit descriptorType) (Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT)

  (devMem1, data') <- mapMemory devMem0 0 bsize zero
  data' <- unsafeUse data' $ \d -> logT $ "Created " <> toLogStr (show descriptorType) <> " mapped region: " <> toLogStr (show d)

  Alias.newAlias destroyMappedBuffer (MappedBuffer buf devMem1 (Unsafe.toLinear castPtr data') (Ur size))

  where
    bufferUsageBit Uniform = Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT
    bufferUsageBit Storage = Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT

-- | Write to a 'MappedBuffer' using the given write action.
--
-- Typically, the poke action should use @gl-block@'s @'write140'@ or
-- @'write430'@ to guarantee the alignment expected by the shader is respected.
--
-- === __Example__
--
-- @
-- writeMappedBuffer write140 mbuf (vec3 1 2 3) -- for uniform buffers
-- writeMappedBuffer write430 mbuf ...          -- for storage buffers
-- @
writeMappedBuffer :: forall a. (Ptr a -> Diff a a -> a -> Prelude.IO ())
                  -> Alias MappedBuffer ⊸ a -> Renderer (Alias MappedBuffer)
writeMappedBuffer writeIt refcbuf x = enterD "writeMappedBuffer" Linear.do
  (ub, ()) <- Alias.useM refcbuf $ Unsafe.toLinear \ub@(MappedBuffer _ _ ptr (Ur _)) -> Linear.do
    liftSystemIO $
      writeIt (castPtr ptr) Category.id x
    pure (ub, ())
  pure ub

-------- Non-interface details ---------

createBuffer :: Vk.DeviceSize -> Vk.BufferUsageFlags -> Vk.MemoryPropertyFlags -> Renderer (Vk.Buffer, Vk.DeviceMemory)
createBuffer size usage properties = Linear.do
  let bufferInfo = Vk.BufferCreateInfo
        { next = ()
        , flags = zero
        , size  = size
        , usage = usage
        , sharingMode = Vk.SHARING_MODE_EXCLUSIVE
        , queueFamilyIndices = []
        }
  unsafeWithVulkanContext $ \vkContext -> do
    let dev = vkContext.device
    buffer  <- Vk.createBuffer dev bufferInfo Nothing
    memReqs <- Vk.getBufferMemoryRequirements dev buffer
    devMem  <- allocateMemory vkContext.physicalDevice dev memReqs properties Vk.zero

    -- Bind buffer to the memory we allocated (or is it the other way around?)
    Vk.bindBufferMemory dev buffer devMem 0
    Prelude.pure (buffer, devMem)

-- | Fills a staging buffer with data, uses it with the given function that
-- typically copies the buffer data from the staging buffer to another one
-- (e.g. creating device local buffers and copying textures to the device), and
-- finally frees the staging buffer
withStagingBuffer :: ∀ α (ρ :: Type). SV.Storable α => SV.Vector α -> (Vk.Buffer ⊸ Vk.DeviceSize -> Renderer ρ) ⊸ Renderer ρ
withStagingBuffer bufferData f = enterD "withStagingBuffer" Linear.do
  -- Accquire staging buffer
  let !l          = SV.length bufferData
      !bufferSize = fromIntegral $ fromIntegral l * sizeOf @α undefined
  (stagingBuffer0, stagingMem0) <- createBuffer bufferSize Vk.BUFFER_USAGE_TRANSFER_SRC_BIT (Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT)
  
  -- Map the buffer memory into CPU accessible memory
  (stagingMem1, data'ptr) <- mapMemory stagingMem0 0 bufferSize zero

  -- Copy buffer data to data'ptr mapped device memory
  data'ptr <- unsafeUse data'ptr $ \unsafeDataPtr ->
    liftSystemIO $ SV.unsafeWith bufferData $ \ptr ->
      copyBytes unsafeDataPtr (Unsafe.toLinear castPtr ptr) (fromIntegral bufferSize)

  -- Unmap memory (doesn't free the device memory, just unmaps/frees the pointer to the mapped region)
  stagingMem2 <- unmapMemory stagingMem1 data'ptr

  -- Use staging buffer
  -- ------------------
  !p <- f stagingBuffer0 bufferSize

  -- Release things
  -- -----------------------------------
  -- Free associated memory
  freeMemory stagingMem2

  pure p

destroyMappedBuffer :: MappedBuffer ⊸ Renderer ()
destroyMappedBuffer (MappedBuffer b dm hostMemory (Ur _size)) = enterD "destroyMappedBuffer" $ Linear.do
  dm' <- unmapMemory dm hostMemory -- unnecessary, freeMemory also unmaps it IUC
  freeMemory dm'
  destroyBuffer b

-------- Utils -------------------------

-- | Create a CPU mapped region to the given device memory
mapMemory :: Vk.DeviceMemory ⊸ Vk.DeviceSize -> Vk.DeviceSize -> Vk.MemoryMapFlags -> Renderer (Vk.DeviceMemory, (Ptr ()))
mapMemory = Unsafe.toLinear $ \mem offset size flgs -> enterD "mapMemory" $ (mem,) <$> (unsafeUseDevice $ \dev -> Vk.mapMemory dev mem offset size flgs)

-- | Free a CPU mapped region
unmapMemory :: Vk.DeviceMemory
             ⊸ Ptr () -- ^ This mapped region on the CPU is freed
             ⊸ Renderer Vk.DeviceMemory
unmapMemory = Unsafe.toLinear2 $ \stgMem _hostMem -> enterD "unmapMemory" $ Linear.do
  unsafeUseDevice $ \device -> Vk.unmapMemory device stgMem
  -- Host mem is not returned because it becomes unavailable after unmapping.
  pure stgMem

-- | Free device memory
freeMemory :: Vk.DeviceMemory ⊸ Renderer ()
freeMemory = Unsafe.toLinear $ \mem -> enterD "freeMemory" $ unsafeUseDevice $ \device -> Vk.freeMemory device mem Nothing 

-- | Destroy a Vk.Buffer
destroyBuffer :: HasVulkanContext m => Vk.Buffer ⊸ m ()
destroyBuffer = Unsafe.toLinear $ \buffer -> withDevice $ Unsafe.toLinear \device ->
  ((), device) <$ liftSystemIO (Vk.destroyBuffer device buffer Nothing)
