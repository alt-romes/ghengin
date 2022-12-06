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

import Debug.Trace
import GHC.Records
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.List.NonEmpty (NonEmpty(..))
import Data.IORef
import Data.StateVar
import Data.Text (Text, pack)
import Geomancy.Vec3
import Apecs (Component(..), Map, Storage(..), Has(..), SystemT(..))
import Unsafe.Coerce

import qualified DearImGui as IM

data UIWindow = UIWindow Text UI

-- TODO: Sooner rather than later this should be moved to a IO () drawfunction
-- instead of components. If we ever need fine grained control of what was
-- clicked when.

type UI = IO Bool

data IOSelectRef a = IOSelectRef (IORef a) (IORef Int)
newIOSelectRef :: MonadIO m => a -> m (IOSelectRef a)
newIOSelectRef x = IOSelectRef <$> liftIO (newIORef x) <*> liftIO (newIORef 0)

readIOSelectRef :: MonadIO m => IOSelectRef a -> m a
readIOSelectRef (IOSelectRef r _) = liftIO $ readIORef r

instance HasGetter (IOSelectRef a) a where
  get = liftIO . readIOSelectRef

-- Returns a boolean indicating whether the component was changed in the previous frame


colorPicker :: Text -> IORef Vec3 -> UI
colorPicker t ref = IM.colorPicker3 t (unsafeCoerce ref :: IORef IM.ImVec3) -- Unsafe coerce Vec3 to ImVec3. They have the same representation. Right?

sliderFloat :: Text -> IORef Float -> Float -> Float -> UI
sliderFloat = IM.sliderFloat

sliderInt :: Text -> IORef Int -> Int -> Int -> UI
sliderInt = IM.sliderInt

sliderVec3 :: Text -> IORef Vec3 -> Float -> Float -> UI
sliderVec3 t ref f1 f2 = do
  v <- get ref
  withVec3 v $ \x y z -> do
    tmpR <- liftIO $ newIORef (x,y,z)
    b <- IM.sliderFloat3 t tmpR f1 f2
    (x',y',z') <- get tmpR
    ref $= vec3 x' y' z'
    pure b

dragFloat :: Text -> IORef Float -> Float -> Float -> UI
dragFloat t ref f1 f2 = trace "DragFloat is behaving weird..." $ IM.dragFloat t ref 0.05 f1 f2

checkBox :: Text -> IORef Bool -> UI
checkBox = IM.checkbox

    -- get ref >>= \case
    --   WithVec3 x y z -> do
    --     tmpRef <- liftIO $ newIORef (ImVec3 x y z)
    --     b <- IM.colorPicker3 t tmpRef -- Unsafe coerce Vec3 to ImVec3. They have the same representation. Right?
    --     ImVec3 x' y' z' <- get tmpRef
    --     ref $= vec3 x' y' z'
    --     pure b

withTree :: Text -> UI -> UI
withTree t act = do
  b <- IM.treeNode t
  if b then do
    b' <- act
    IM.treePop
    pure b'
  else
    pure False


withCombo :: Show a
          => Text      -- ^ Combo label
          -> IOSelectRef a   -- ^ Reference to current item
          -> NonEmpty a -- ^ List of possible items
          -> UI
withCombo t (IOSelectRef ref currIx) (opt:|opts) = do

  currSelected <- get currIx

  b <- IM.beginCombo t (pack . show $ (opt:opts) !! currSelected)
  if b then do

    bs <- forM (zip (opt:opts) [0..]) $ \(o, n) -> do
            currIx' <- get currIx
            let is_selected = currIx' == n
            b' <- IM.selectableWith (IM.defSelectableOptions{IM.selected=is_selected}) (pack $ show o)
            when b' $ currIx $= n
            -- when is_selected (IM.setItemDefaultFocus)
            pure b'
    currIx' <- get currIx
    ref $= ((opt:opts) !! currIx')
    IM.endCombo
    pure $ or bs

  else
    pure False


instance Component UIWindow where
  type Storage UIWindow = Map UIWindow

-- TODO: Instructions on having a World record with "transforms"
instance (Monad m, HasField "uiwindows" w (Storage UIWindow)) => Has w m UIWindow where
  getStore = SystemT (asks (.uiwindows))


class UISettings a where
  makeSettings   :: IO a
  -- | Makes the UI components for these UI settings. Returns true if the settings were modified.
  makeComponents :: a -> UI


