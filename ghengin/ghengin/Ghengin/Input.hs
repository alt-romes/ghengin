module Ghengin.Input where

-- ghengin-core
import qualified Ghengin.Core.Prelude as Linear
import qualified Ghengin.Core.Input as Core

-- ghengin:dear-imgui
import qualified Ghengin.DearImGui.Backend as ImGui
import qualified Ghengin.DearImGui.UI as ImGui

import Ghengin.Monad
import Ghengin.Prelude

-- | Like 'Core.registerCharStream'
registerCharStream :: Ghengin Core.CharStream
registerCharStream = liftRenderer Core.registerCharStream

-- | Like 'Core.registerMouseDragStream', but makes sure dragging events don't
-- occur if an ImGui window is being used. The first element allows one to
-- additionally filter out mouse drag events by returning @False@.
--
-- To allow all mouse drag events except for when an ImGui window is in use:
-- @
-- registerMouseDragStream (pure True)
-- @
registerMouseDragStream :: IO Bool -> Ghengin Core.MouseDragStream
registerMouseDragStream isDragAllowed = do
  hasImgui <- asks (enableImGui . conf)
  liftRenderer $ Core.registerMouseDragStream $ do
    let imguiAllowsDrag
          | hasImgui
          -- Don't register drag if imgui is already using the mouse
          = not <$> ImGui.wantCaptureMouse
          | otherwise
          = pure True
    -- Is drag allowed?
    (&&) <$> isDragAllowed <*> imguiAllowsDrag

