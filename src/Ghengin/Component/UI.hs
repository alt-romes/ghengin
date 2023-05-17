{-# LANGUAGE GADTs #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE LinearTypes #-}
module Ghengin.Component.UI where

import Prelude.Linear hiding (IO)
import qualified Prelude
import Data.Kind
import qualified Control.Monad as Base
import Control.Functor.Linear as Linear hiding (get)
import Control.Monad.IO.Class.Linear
import Data.List.NonEmpty (NonEmpty(..))
import Data.IORef (IORef)
import System.IO.Linear
import Data.StateVar
import Data.Text (Text, pack)
import Geomancy.Vec3
import Apecs.Linear (Component(..), Map, Storage(..), Entity)
import Unsafe.Coerce
import Ghengin.Scene.Graph
import {-# SOURCE #-} Ghengin (Ghengin)


import qualified DearImGui as IM


data UIWindow w = UIWindow Text (Ghengin w ())

-- TODO: UI in the Scene Graph?
newEntityUI :: EntityConstraints w (UIWindow w)
            => Text -> Ghengin w () -> SceneGraph w (Ur Entity)
newEntityUI text act = newEntity (UIWindow text act)

type UI w = Ghengin w (Ur Bool)

data IOSelectRef a = IOSelectRef (IORef a) (IORef Int)
newIOSelectRef :: MonadIO m => a -> m (Ur (IOSelectRef a))
newIOSelectRef x = liftIO $ Linear.do
  Ur x' <- newIORef x
  Ur y' <- newIORef 0
  pure $ Ur $ IOSelectRef x' y'

readIOSelectRef :: MonadIO m => IOSelectRef a -> m (Ur a)
readIOSelectRef (IOSelectRef r _) = liftIO $ readIORef r

-- instance HasGetter (IOSelectRef a) a where
--   get x = Linear.do
--     Ur y <- liftIO $ readIOSelectRef x
--     pure y

-- The component should also define how to update the scene


colorPicker :: Dupable w => Text -> IORef Vec3 -> UI w
colorPicker t ref = liftSystemIOU $ IM.colorPicker3 t (unsafeCoerce ref :: IORef IM.ImVec3) -- Unsafe coerce Vec3 to ImVec3. They have the same representation. Right?

sliderFloat :: Dupable w => Text -> IORef Float -> Float -> Float -> UI w
sliderFloat a b c d = liftSystemIOU $ IM.sliderFloat a b c d

sliderInt :: Dupable w => Text -> IORef Int -> Int -> Int -> UI w
sliderInt a b c d = liftSystemIOU $ IM.sliderInt a b c d

sliderVec3 :: Dupable w => Text -> IORef Vec3 -> Float -> Float -> UI w
sliderVec3 t ref f1 f2 = liftIO $ Linear.do
  Ur v <- readIORef ref
  withVec3 v $ \x y z -> Linear.do
    Ur tmpR <- newIORef (x,y,z)
    Ur b <- liftSystemIOU $ IM.sliderFloat3 t tmpR f1 f2
    Ur (x',y',z') <- readIORef tmpR
    writeIORef ref (vec3 x' y' z')
    pure (Ur b)

dragFloat :: Dupable w => Text -> IORef Float -> Float -> Float -> UI w
dragFloat t ref f1 f2 = liftSystemIOU $ IM.dragFloat t ref 0.05 f1 f2

checkBox :: Dupable w => Text -> IORef Bool -> UI w
checkBox a b = liftSystemIOU $ IM.checkbox a b

    -- get ref >>= \case
    --   WithVec3 x y z -> do
    --     tmpRef <- liftIO $ newIORef (ImVec3 x y z)
    --     b <- IM.colorPicker3 t tmpRef -- Unsafe coerce Vec3 to ImVec3. They have the same representation. Right?
    --     ImVec3 x' y' z' <- get tmpRef
    --     ref $= vec3 x' y' z'
    --     pure b

withTree :: Dupable w => Text -> Ghengin w () -> Ghengin w ()
withTree t act = Linear.do
  Ur b <- liftSystemIOU $ IM.treeNode t
  if b then Linear.do
    act
    liftSystemIO IM.treePop
    pure ()
  else
    pure ()

button :: Dupable w => Text -> UI w
button x = liftSystemIOU $ IM.button x

withCombo :: (Show a, Dupable w)
          => Text      -- ^ Combo label
          -> IOSelectRef a   -- ^ Reference to current item
          -> NonEmpty a -- ^ List of possible items
          -> UI w
withCombo t (IOSelectRef ref currIx) (opt:|opts) = Linear.do

  Ur currSelected <- liftIO $ readIORef currIx

  Ur b <- liftSystemIOU $ IM.beginCombo t (pack $ show $ (opt:opts) Prelude.!! currSelected)
  if b then liftSystemIO $ do

    bs <- Base.forM (Prelude.zip (opt:opts) [0..]) $ \(o, n) -> do
            currIx' <- get currIx
            let is_selected = currIx' == n
            b' <- IM.selectableWith (IM.defSelectableOptions{IM.selected=is_selected}) (pack $ show o)
            Base.when b' $ currIx $= n
            -- when is_selected (IM.setItemDefaultFocus)
            Prelude.pure b'
    currIx' <- get currIx
    ref $= ((opt:opts) Prelude.!! currIx')
    IM.endCombo
    Prelude.pure $ move $ or bs

  else
    pure (Ur False)


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

  makeSettings   :: IO (Ur a)

  -- | Makes the UI components for these UI settings. Returns true if the settings were modified.
  makeComponents :: ReactivityConstraints a w
                 => a -> ReactivityInput a -> Ghengin w (ReactivityOutput a)


