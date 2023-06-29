{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
module Ghengin.Input
  ( GLFW.Key(..)
  , ifPressed
  , ifReleased
  , getKey
  ) where

import Prelude.Linear
import Control.Functor.Linear as Linear
import Control.Monad.IO.Class.Linear as Linear
import qualified Graphics.UI.GLFW as GLFW
import Ghengin.Vulkan.Renderer.GLFW.Window
import Ghengin.Vulkan.Renderer.Kernel
import Ghengin
import qualified Unsafe.Linear as Unsafe

getKey :: Dupable w => GLFW.Key -> Ghengin w (Ur GLFW.KeyState)
getKey k = lift $ Linear.do
  Ur unsafe_w <- renderer $ Unsafe.toLinear \renv -> Linear.pure (Ur renv._vulkanWindow._window, renv)
  Linear.liftSystemIOU $ GLFW.getKey unsafe_w k

ifPressed :: Dupable w
          => GLFW.Key
          -> Ghengin w (Ur a) -- ^ Then
          -> Ghengin w (Ur a) -- ^ Else
          -> Ghengin w (Ur a) -- ^ Result
ifPressed k t e =
  getKey k Linear.>>= \case
    Ur GLFW.KeyState'Pressed -> t
    Ur _ -> e

ifReleased :: Dupable w
          => GLFW.Key
          -> Ghengin w (Ur a) -- ^ Then
          -> Ghengin w (Ur a) -- ^ Else
          -> Ghengin w (Ur a) -- ^ Result
ifReleased k t e =
  getKey k Linear.>>= \case
    Ur GLFW.KeyState'Released -> t
    Ur _ -> e

-- ifPressed :: GLFW.Key
--           -> Ghengin w a -- ^ Then
--           -> Ghengin w a -- ^ Else
--           -> Ghengin w a -- ^ Result
-- ifPressed k t e = do
--   getKey k >>= \case
--     GLFW.KeyState'Pressed -> t
--     _ -> e

-- | If it is not being pressed currently
-- ifReleased :: GLFW.Key
--           -> Ghengin w a -- ^ Then
--           -> Ghengin w a -- ^ Else
--           -> Ghengin w a -- ^ Result
-- ifReleased k t e = do
--   getKey k >>= \case
--     GLFW.KeyState'Released -> t
--     _ -> e

