{-# LANGUAGE UndecidableInstances #-}
module Ghengin.Render.Class where

import Control.Monad.IO.Class
import Control.Monad.Trans

import qualified Vulkan as Vk
import Foreign.Storable
import qualified Data.Vector.Storable as SV

class MonadIO μ => MonadRender μ where

  -- | Create a vertex buffer. TODO: The return type can't be Vulkan specific.
  createVertexBuffer :: Storable α => SV.Vector α -> μ (Vk.Buffer, Vk.DeviceMemory)
-- TODO

instance (MonadIO (t m), MonadTrans t, MonadRender m) => MonadRender (t m) where
  createVertexBuffer = lift . createVertexBuffer

