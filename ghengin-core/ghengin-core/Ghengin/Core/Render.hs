{-# LANGUAGE OverloadedStrings #-}
module Ghengin.Core.Render
  ( module Ghengin.Core.Render
  , module Ghengin.Vulkan.Renderer
  , module Ghengin.Vulkan.Renderer.Kernel
  , module Ghengin.Vulkan.Renderer.Pipeline
  )
  where

import qualified Data.IntMap.Linear as IM
import Ghengin.Core.Log
import Prelude.Linear

import Control.Functor.Linear as Linear

import Ghengin.Vulkan.Renderer.Buffer
import Ghengin.Vulkan.Renderer.Kernel
import Ghengin.Vulkan.Renderer.Pipeline
import Ghengin.Vulkan.Renderer

import qualified Data.Linear.Alias as Alias

-- Backend agnostic rendering functions?

-- I don't know yet what the purpose of this module is.

-- I don't know where exactly to put this, so put it here for now
getDescriptorResource :: ResourceMap ⊸ Int -> Renderer (DescriptorResource, ResourceMap)
getDescriptorResource resourcemap i = enterD "getDescriptorResource" $
  IM.lookupM i resourcemap >>= \case
    (Just x, rmap1) -> pure (x, rmap1)
    (Nothing, rmap1) -> Linear.do
      Alias.forget rmap1
      error $ "Expecting a descriptor resource at binding " <> show i <> " but found nothing!"

