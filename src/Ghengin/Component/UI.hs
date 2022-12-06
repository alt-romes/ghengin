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
import Data.IORef
import Data.Text (Text)
import Geomancy.Vec3
import Apecs

data UIWindow = UIWindow Text [UIComponent]

data UIComponent = ColorPicker Text (IORef Vec3)
                 | SliderFloat Text (IORef Float) Float Float
                 | DragFloat   Text (IORef Float) Float Float
                 | SliderVec3  Text (IORef Vec3)  Float Float
                 | SliderInt   Text (IORef Int)   Int   Int
                 | WithTree    Text [UIComponent]
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

