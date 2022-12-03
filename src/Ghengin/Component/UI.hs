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


instance Component UIWindow where
  type Storage UIWindow = Map UIWindow

-- TODO: Instructions on having a World record with "transforms"
instance (Monad m, HasField "uiwindows" w (Storage UIWindow)) => Has w m UIWindow where
  getStore = SystemT (asks (.uiwindows))


