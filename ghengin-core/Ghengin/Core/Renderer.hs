{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Ghengin.Core.Renderer
  ( module Ghengin.Core.Renderer.DescriptorSet
  , module Ghengin.Core.Renderer.Buffer
  , module Ghengin.Core.Renderer.Kernel
  , module Ghengin.Core.Renderer
  ) where

import qualified Data.IntMap.Linear as IM
import Ghengin.Core.Log
import Prelude.Linear

import Control.Functor.Linear as Linear

import Ghengin.Core.Renderer.Buffer
import Ghengin.Core.Renderer.Kernel
import Ghengin.Core.Renderer.DescriptorSet

import qualified Data.Linear.Alias as Alias

getDescriptorResource :: ResourceMap ⊸ Int -> Renderer (DescriptorResource, ResourceMap)
getDescriptorResource resourcemap i = enterD "getUniformBuffer" $
  IM.lookupM i resourcemap >>= \case
    (Just x, rmap1) -> pure (x, rmap1)
    (Nothing, rmap1) -> Linear.do
      Alias.forget rmap1
      error $ "Expecting a uniform descriptor resource at binding " <> show i <> " but found nothing!"
