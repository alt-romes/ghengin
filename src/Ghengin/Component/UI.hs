{-# LANGUAGE GADTs #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Ghengin.Component.UI where

import Data.Kind
import Control.Monad
import Control.Monad.IO.Class
import Data.List.NonEmpty (NonEmpty(..))
import Data.IORef
import Data.StateVar
import Data.Text (Text, pack)
import Geomancy.Vec3
import Apecs (Component(..), Map, Storage(..), Entity)
import Unsafe.Coerce
import Ghengin.Scene.Graph
import {-# SOURCE #-} Ghengin (Ghengin)

import qualified DearImGui as IM

data UIWindow w = forall a. UIWindow Text (Ghengin w a)

-- TODO: UI in the Scene Graph?
newEntityUI :: EntityConstraints w (UIWindow w)
            => Text -> Ghengin w a -> SceneGraph w Entity
newEntityUI text act = newEntity (UIWindow text act)

type UI w = Ghengin w Bool

data IOSelectRef a = IOSelectRef (IORef a) (IORef Int)
newIOSelectRef :: MonadIO m => a -> m (IOSelectRef a)
newIOSelectRef x = IOSelectRef <$> liftIO (newIORef x) <*> liftIO (newIORef 0)

readIOSelectRef :: MonadIO m => IOSelectRef a -> m a
readIOSelectRef (IOSelectRef r _) = liftIO $ readIORef r

instance HasGetter (IOSelectRef a) a where
  get = liftIO . readIOSelectRef

-- The component should also define how to update the scene


colorPicker :: Text -> IORef Vec3 -> UI w
colorPicker t ref = IM.colorPicker3 t (unsafeCoerce ref :: IORef IM.ImVec3) -- Unsafe coerce Vec3 to ImVec3. They have the same representation. Right?

sliderFloat :: Text -> IORef Float -> Float -> Float -> UI w
sliderFloat = IM.sliderFloat

sliderInt :: Text -> IORef Int -> Int -> Int -> UI w
sliderInt = IM.sliderInt

sliderVec3 :: Text -> IORef Vec3 -> Float -> Float -> UI w
sliderVec3 t ref f1 f2 = do
  v <- get ref
  withVec3 v $ \x y z -> do
    tmpR <- liftIO $ newIORef (x,y,z)
    b <- IM.sliderFloat3 t tmpR f1 f2
    (x',y',z') <- get tmpR
    ref $= vec3 x' y' z'
    pure b

dragFloat :: Text -> IORef Float -> Float -> Float -> UI w
dragFloat t ref f1 f2 = IM.dragFloat t ref 0.05 f1 f2

checkBox :: Text -> IORef Bool -> UI w
checkBox = IM.checkbox

    -- get ref >>= \case
    --   WithVec3 x y z -> do
    --     tmpRef <- liftIO $ newIORef (ImVec3 x y z)
    --     b <- IM.colorPicker3 t tmpRef -- Unsafe coerce Vec3 to ImVec3. They have the same representation. Right?
    --     ImVec3 x' y' z' <- get tmpRef
    --     ref $= vec3 x' y' z'
    --     pure b

withTree :: Text -> Ghengin w a -> Ghengin w ()
withTree t act = do
  b <- IM.treeNode t
  if b then do
    _ <- act
    IM.treePop
    pure ()
  else
    pure ()

button :: Text -> UI w
button = IM.button

withCombo :: Show a
          => Text      -- ^ Combo label
          -> IOSelectRef a   -- ^ Reference to current item
          -> NonEmpty a -- ^ List of possible items
          -> UI w
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


instance Component (UIWindow w) where
  type Storage (UIWindow w) = Map (UIWindow w)

-- Careful! The components cannot have the same Id otherwise they will behave
-- the same.

class UISettings a where

  -- | Every 'UISettings' instance has a draw function that draws the UI and
  -- reacts to the changes in it the imgui way. Some UISettings instances might
  -- simply define the UI layout, however, some might react to a button and e.g.
  -- update an existing instance. For that, they need the said instance in the
  -- body of the function. 'ReactivityInput' defines the additional data required
  -- by these UISettings to define how the game reacts to the UI.
  type ReactivityInput a

  -- | 'ReactivityOutput' is like 'ReactivityInput' but defines the data that
  -- the UI returns based on the UI events.
  type ReactivityOutput a

  -- | The world constraints to react to the changes
  type ReactivityConstraints a w :: Constraint
  type ReactivityConstraints _ _ = ()

  makeSettings   :: IO a

  -- | Makes the UI components for these UI settings. Returns true if the settings were modified.
  makeComponents :: ReactivityConstraints a w
                 => a -> ReactivityInput a -> Ghengin w (ReactivityOutput a)


