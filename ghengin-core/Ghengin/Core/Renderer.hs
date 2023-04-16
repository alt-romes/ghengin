{-# LANGUAGE LinearTypes #-}
module Ghengin.Core.Renderer
  ( module Ghengin.Core.Renderer.DescriptorSet
  , module Ghengin.Core.Renderer.Buffer
  , module Ghengin.Core.Renderer
  ) where

import qualified Data.IntMap as IM
import qualified Prelude
import Prelude.Linear

import Control.Functor.Linear
import Control.Monad.IO.Class.Linear

import qualified Unsafe.Linear as Unsafe

import Ghengin.Core.Render.Monad
import Ghengin.Core.Renderer.DescriptorSet
import Ghengin.Core.Renderer.Buffer

import Data.Counted
import qualified Data.Counted.Unsafe as Unsafe.Counted

getUniformBuffer :: MonadIO lm => ResourceMap ⊸ Int -> lm (RefC MappedBuffer, ResourceMap)
getUniformBuffer = Unsafe.toLinear $ \resourcemap i ->
  case IM.lookup i resourcemap of
    -- We *unsafely* increment the reference because we also return
    -- `resourcemap` which retains one reference, which makes it *all work out safely*
    -- This is also why resourcemap is used twice (and hence is unsafe).
    --
    -- In short, we unsafely increment a linear resource for one we unsafely keep
    Just (UniformResource b) -> (,resourcemap) <$> Unsafe.Counted.inc b
    Nothing -> error $ "Expecting a uniform descriptor resource at binding " <> show i <> " but found nothing!"
    _ -> error $ "Expecting the descriptor resource at binding " <> show i <> " to be a uniform!"
