module Ghengin.Input
  ( module Ghengin.Input
  -- Re-exports
  , Core.MouseDrag(..)
  ) where

-- ghengin-core
import qualified Ghengin.Core.Prelude as Linear
import qualified Ghengin.Core.Input as Core

-- ghengin:dear-imgui
import qualified Ghengin.DearImGui.Vulkan as ImGui
import qualified Ghengin.DearImGui.UI as ImGui

import Ghengin.Monad
import Ghengin.Prelude

-- | Like 'Core.readCharInput'
readCharInput :: Ghengin (Maybe Char)
readCharInput = asks charStream >>= \s -> liftRenderer (Core.readCharInput s)

-- | Like 'Core.readMouseDrag'
readMouseDrag :: Ghengin (Maybe Core.MouseDrag)
readMouseDrag = asks mouseDragStream >>= \s -> liftRenderer (Core.readMouseDrag s)

