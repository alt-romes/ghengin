{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Ghengin.Core.Renderer
  ( module Ghengin.Core.Renderer.DescriptorSet
  , module Ghengin.Core.Renderer.Buffer
  , module Ghengin.Core.Renderer.Kernel
  , module Ghengin.Core.Renderer
  ) where

import qualified Data.IntMap as IM
import Ghengin.Core.Log
import Prelude.Linear

import Control.Functor.Linear

import qualified Unsafe.Linear as Unsafe

import Ghengin.Core.Renderer.Buffer
import Ghengin.Core.Renderer.Kernel
import Ghengin.Core.Renderer.DescriptorSet

import Data.Counted
import qualified Data.Counted.Unsafe as Unsafe.Counted

instance Counted DescriptorResource where
  countedFields (UniformResource x) = [SomeRefC x]
  countedFields (Texture2DResource x) = [SomeRefC x]

getDescriptorResource :: ResourceMap ⊸ Int -> Renderer (DescriptorResource, ResourceMap)
getDescriptorResource = Unsafe.toLinear $ \resourcemap i -> enterD "getUniformBuffer" $
  case IM.lookup i resourcemap of
    -- We *unsafely* increment the reference because we also return
    -- `resourcemap` which retains one reference, which makes it *all work out safely*
    -- This is also why resourcemap is used twice (and hence is unsafe).
    --
    -- In short, we unsafely increment a linear resource for one we unsafely keep
    Just (UniformResource b) -> (,resourcemap) . UniformResource <$> Unsafe.Counted.inc b
    Just (Texture2DResource t) -> (,resourcemap) . Texture2DResource <$> Unsafe.Counted.inc t
    Nothing -> error $ "Expecting a uniform descriptor resource at binding " <> show i <> " but found nothing!"
