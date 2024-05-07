{-# LANGUAGE OverloadedStrings #-}
module Ghengin.Core.Render
  ( module Ghengin.Core.Render
  , module Ghengin.Core.Renderer
  , module Ghengin.Core.Renderer.Kernel
  )
  where

import qualified Data.IntMap.Linear as IM
import Ghengin.Core.Log
import Prelude.Linear

import Control.Functor.Linear as Linear

import Ghengin.Core.Renderer.Buffer
import Ghengin.Core.Renderer.Kernel
import Ghengin.Core.Renderer

import qualified Data.Linear.Alias as Alias

-- Backend agnostic rendering functions?

-- I don't know yet what the purpose of this module is.

-- I don't know where exactly to put this, so put it here for now
getDescriptorResource :: ResourceMap ⊸ Int -> Renderer (DescriptorResource, ResourceMap)
getDescriptorResource resourcemap i = enterD "getUniformBuffer" $
  IM.lookupM i resourcemap >>= \case
    (Just x, rmap1) -> pure (x, rmap1)
    (Nothing, rmap1) -> Linear.do
      Alias.forget rmap1
      error $ "Expecting a uniform descriptor resource at binding " <> show i <> " but found nothing!"
