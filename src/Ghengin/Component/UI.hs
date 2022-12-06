{-# LANGUAGE GADTs #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-} -- HasField Has Transform
{-# LANGUAGE OverloadedRecordDot #-}
module Ghengin.Component.UI where

import GHC.Records
import Data.List.NonEmpty (NonEmpty)
import Data.IORef
import Data.Text (Text)
import Geomancy.Vec3
import Apecs

data UIWindow = UIWindow Text [UIComponent]

-- TODO: Sooner rather than later this should be moved to a IO () drawfunction
-- instead of components. If we ever need fine grained control of what was
-- clicked when.

data UIComponent where
  ColorPicker :: !Text -> !(IORef Vec3)  -> UIComponent
  SliderFloat :: !Text -> !(IORef Float) -> !Float -> !Float -> UIComponent
  DragFloat   :: !Text -> !(IORef Float) -> !Float -> !Float -> UIComponent
  SliderVec3  :: !Text -> !(IORef Vec3)  -> !Float -> !Float -> UIComponent
  SliderInt   :: !Text -> !(IORef Int)   -> !Int   -> !Int -> UIComponent
  WithTree    :: !Text -> ![UIComponent] -> UIComponent
  Checkbox    :: !Text -> !(IORef Bool)  -> UIComponent
  WithCombo   :: Show a => !Text -> !(IORef a) -> !(NonEmpty a) -> UIComponent
                 -- | TabBar      Text [(Text, Bool, [UIComponent])]
                 -- | Menu        Text [UIComponent]

instance Component UIWindow where
  type Storage UIWindow = Map UIWindow

-- TODO: Instructions on having a World record with "transforms"
instance (Monad m, HasField "uiwindows" w (Storage UIWindow)) => Has w m UIWindow where
  getStore = SystemT (asks (.uiwindows))


class UISettings a where
  makeSettings   :: IO a
  makeComponents :: a -> [UIComponent]

