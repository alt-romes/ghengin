{-# LANGUAGE LinearTypes, QualifiedDo, AllowAmbiguousTypes #-}
-- TODO: Move module to .Class instead of .Monad
module Ghengin.Core.Render.Monad where

import Data.Kind

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import qualified Ghengin.Asset.Texture as T
import Prelude ()
import Prelude.Linear
import Control.Functor.Linear as Linear
import Control.Monad.IO.Class.Linear as Linear
import qualified Unsafe.Linear as Unsafe

import qualified Vulkan as Vk


-- class MappedBuffer buffer where
--   -- | TODO: Drop dependency on Vulkan and make DescriptorType a data type renderer agnostic
--   createMappedBuffer :: MonadRenderer m => Word -> Vk.DescriptorType -> m buffer
--   writeMappedBuffer :: ∀ α m. (MonadRenderer m, SV.Storable α) => buffer ⊸ α -> m buffer
--   -- ROMES: worry about performance and specialization later, it'll be much easier after this all works with linear types

class Linear.MonadIO m => MonadRenderer m where

-- class DescriptorSetC dset where
--   updateDescriptorSet :: dset -- Vk.DescriptorSet -- ^ The descriptor set we're writing with these resources
--                       ⊸ ResourceMap               -- ^ The resources we're updating the descriptor set with
--                       ⊸ m (dset, ResourceMap)

-- getUniformBuffer :: ResourceMap ⊸ Int -> (SomeMappedBuffer, ResourceMap)
-- getUniformBuffer = Unsafe.toLinear2 $ \resourcemap i ->
--   case IM.lookup i resourcemap of
--     Just (UniformResource b) -> (SomeMappedBuffer b, resourcemap)
--     Nothing -> error $ "Expecting a uniform descriptor resource at binding " <> show i <> " but found nothing!"
--     _ -> error $ "Expecting the descriptor resource at binding " <> show i <> " to be a uniform!"

