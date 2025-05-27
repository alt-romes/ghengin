-- | Core utilities for handling user input
module Ghengin.Core.Input
  (
  -- * Higher level
  -- | Is a bit less flexible, but easy.
    CharStream, ScrollStream
  , registerCharStream, registerScrollStream
  , readCharInput, readScrollInput

  -- * Lower level
  , CharCallback, ScrollCallback, KeyCallback
  , setCharCallback, setScrollCallback, setKeyCallback
  ) where

import qualified Prelude
import Ghengin.Core.Prelude as Linear

import Control.Monad.STM
import Control.Concurrent.STM.TChan

import Ghengin.Core.Render
import Ghengin.Core

import qualified Unsafe.Linear as Unsafe
import qualified Graphics.UI.GLFW as GLFW

--------------------------------------------------------------------------------
-- Higher level, less flexible

newtype CharStream = CS (TChan Char)

-- | Register the 'CharCallback' as an action which produces to a 'CharStream'
-- that can be easily consumed with 'readCharInput'.
registerCharStream :: Core CharStream
registerCharStream = Linear.do
  Ur chan <- liftSystemIOU newTChanIO
  setCharCallback (Just (\c -> atomically $ writeTChan chan c))
  return (CS chan)

-- | Read a 'Char' user input -- returns Nothing if nothing was input rather
-- than blocking.
readCharInput :: CharStream -> Core (Ur (Maybe Char))
readCharInput (CS chan) = liftSystemIOU $ atomically $ tryReadTChan chan

newtype ScrollStream = SS (TChan (Double, Double))

-- | Register the 'ScrollCallback' as an action which produces to a 'ScrollStream'
-- that can be easily consumed with 'readScrollInput'.
registerScrollStream :: Core ScrollStream
registerScrollStream = Linear.do
  Ur chan <- liftSystemIOU newTChanIO
  setScrollCallback (Just (\x y -> atomically $ writeTChan chan (x,y)))
  return (SS chan)

-- | Returns True if the user scrolled, and nothing otherwise (rather than blocking).
-- See See <http://www.glfw.org/docs/3.3/input.html#scrolling Scroll Input>
readScrollInput :: ScrollStream -> Core (Ur (Maybe (Double, Double)))
readScrollInput (SS chan) = liftSystemIOU $ atomically $ tryReadTChan chan

--------------------------------------------------------------------------------
-- Lower level

type CharCallback = Char -> Prelude.IO ()

setCharCallback :: Maybe CharCallback -> Core ()
setCharCallback cb = liftCore $ withWindow $
  Unsafe.toLinear \w -> Linear.do
    liftSystemIO $ GLFW.setCharCallback w (const Prelude.<$> cb)
    return w

type ScrollCallback = Double -> Double -> Prelude.IO ()

setScrollCallback :: Maybe ScrollCallback -> Core ()
setScrollCallback cb = liftCore $ withWindow $
  Unsafe.toLinear \w -> Linear.do
    liftSystemIO $ GLFW.setScrollCallback w (const Prelude.<$> cb)
    return w

type KeyCallback = GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> Prelude.IO ()

setKeyCallback :: Maybe KeyCallback -> Core ()
setKeyCallback cb = liftCore $ withWindow $
  Unsafe.toLinear \w -> Linear.do
    liftSystemIO $ GLFW.setKeyCallback w (const Prelude.<$> cb)
    return w

