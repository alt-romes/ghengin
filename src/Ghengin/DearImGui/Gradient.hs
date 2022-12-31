{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BlockArguments #-}
module Ghengin.DearImGui.Gradient where

import DearImGui.Context
  ( imguiContext )

import Control.Monad.IO.Class
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import qualified Language.C.Inline.Cpp as Cpp

data ImGradient = ImGradient (Ptr ImGradient)

Cpp.context (Cpp.cppCtx <> Cpp.bsCtx <> imguiContext <>
  Cpp.cppTypePairs [
    ("ImGradient", [t|ImGradient|])
    ]
  )
Cpp.include "imgui_color_gradient.hpp"
Cpp.using "namespace ImGui"


gradient :: (MonadIO m) => CString -> Ptr CFloat -> m ImGradient
gradient _descPtr _refPtr = liftIO do
  pt <- [Cpp.block| ImGradient* { return new ImGradient();} |]
  pure (ImGradient pt)


