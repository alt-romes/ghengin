-- | Core utilities for handling user input
module Ghengin.Core.Input
  (
  -- * Higher level
  -- | Is a bit less flexible, but easy.
    CharStream, ScrollStream
  , registerCharStream, registerScrollStream
  , readCharInput, readScrollInput

  -- ** Mouse
  , MouseDrag(..), MouseDragStream
  , registerMouseDragStream, readMouseDrag

  -- * Lower level
  , CharCallback, ScrollCallback, KeyCallback
  , setCharCallback, setScrollCallback, setKeyCallback
  ) where

import qualified Prelude
import qualified Control.Monad as Base
import qualified Data.IORef as Base
import Ghengin.Core.Prelude as Linear

import Control.Monad.STM
import Control.Concurrent.STM.TChan

import Ghengin.Core.Render

import qualified Unsafe.Linear as Unsafe
import qualified Graphics.UI.GLFW as GLFW

--------------------------------------------------------------------------------
-- Higher level, less flexible

newtype CharStream = CS (TChan Char)

-- | Register the 'CharCallback' as an action which produces to a 'CharStream'
-- that can be easily consumed with 'readCharInput'.
registerCharStream :: Renderer (Ur CharStream)
registerCharStream = Linear.do
  Ur chan <- liftSystemIOU newTChanIO
  setCharCallback (Just (\c -> atomically $ writeTChan chan c))
  return (Ur (CS chan))

-- | Read a 'Char' user input -- returns Nothing if nothing was input rather
-- than blocking.
readCharInput :: CharStream -> Renderer (Ur (Maybe Char))
readCharInput (CS chan) = liftSystemIOU $ atomically $ tryReadTChan chan

newtype ScrollStream = SS (TChan (Double, Double))

-- | Register the 'ScrollCallback' as an action which produces to a 'ScrollStream'
-- that can be easily consumed with 'readScrollInput'.
registerScrollStream :: Renderer (Ur ScrollStream)
registerScrollStream = Linear.do
  Ur chan <- liftSystemIOU newTChanIO
  setScrollCallback (Just (\x y -> atomically $ writeTChan chan (x,y)))
  return (Ur (SS chan))

-- | Returns True if the user scrolled, and nothing otherwise (rather than blocking).
-- See See <http://www.glfw.org/docs/3.3/input.html#scrolling Scroll Input>
readScrollInput :: ScrollStream -> Renderer (Ur (Maybe (Double, Double)))
readScrollInput (SS chan) = liftSystemIOU $ atomically $ tryReadTChan chan

-- ** Mouse

data MouseDrag = MouseDrag
  { dragDeltaX :: Double
  , dragDeltaY :: Double
  }

newtype MouseDragStream = MDS (TChan MouseDrag)

-- | Register mouse button and cursor position callbacks to produce a 'MouseDragStream'
-- that can be easily consumed with 'readMouseDrag'.
--
-- Takes an action which can additionally disallow a dragging to start by returning @False@.
-- This is typically useful if you are using dear-imgui and want to prevent
-- dragging actions at the same time as interacting with the UI.
-- See planets-core for an example.
registerMouseDragStream :: Prelude.IO Bool -> Renderer (Ur MouseDragStream)
registerMouseDragStream dragIsAllowedIO = Linear.do
  Ur chan <- liftSystemIOU newTChanIO
  Ur stateRef <- liftSystemIOU $ Base.newIORef (False, 0, 0) -- (isDragging, lastX, lastY)
  
  -- Set cursor position callback
  setCursorPosCallback (Just $ \x y -> do
    (isDragging, lastX, lastY) <- Base.readIORef stateRef

    Base.when isDragging $ do
      let deltaX = x - lastX
          deltaY = y - lastY
      atomically $ writeTChan chan (MouseDrag deltaX deltaY)
    Base.writeIORef stateRef (isDragging, x, y)
    )
  
  -- Set mouse button callback
  setMouseButtonCallback (Just $ \btn state _mods -> do

    dragIsAllowed <- dragIsAllowedIO

    Base.when (case btn of GLFW.MouseButton'1 -> True; _ -> False) $ do
      (_, lastX, lastY) <- Base.readIORef stateRef
      case (dragIsAllowed, state) of
        (True, GLFW.MouseButtonState'Pressed) ->
          Base.writeIORef stateRef (True, lastX, lastY)
        (False, GLFW.MouseButtonState'Pressed) ->
          Prelude.pure () -- do Nothing
        (_, GLFW.MouseButtonState'Released) ->
          -- always allow releasing
          Base.writeIORef stateRef (False, lastX, lastY)
    )
  
  return (Ur (MDS chan))

-- | Read mouse drag deltas -- returns Nothing if no drag occurred rather than blocking.
readMouseDrag :: MouseDragStream -> Renderer (Ur (Maybe MouseDrag))
readMouseDrag (MDS chan) = liftSystemIOU $ atomically $ tryReadTChan chan

--------------------------------------------------------------------------------
-- Lower level

type CharCallback = Char -> Prelude.IO ()

setCharCallback :: Maybe CharCallback -> Renderer ()
setCharCallback cb = withWindow $
  Unsafe.toLinear \w -> Linear.do
    liftSystemIO $ GLFW.setCharCallback w (const Prelude.<$> cb)
    return w

type ScrollCallback = Double -> Double -> Prelude.IO ()

setScrollCallback :: Maybe ScrollCallback -> Renderer ()
setScrollCallback cb = withWindow $
  Unsafe.toLinear \w -> Linear.do
    liftSystemIO $ GLFW.setScrollCallback w (const Prelude.<$> cb)
    return w

type KeyCallback = GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> Prelude.IO ()

setKeyCallback :: Maybe KeyCallback -> Renderer ()
setKeyCallback cb = withWindow $
  Unsafe.toLinear \w -> Linear.do
    liftSystemIO $ GLFW.setKeyCallback w (const Prelude.<$> cb)
    return w

-- ** Mouse

type CursorPosCallback = Double -> Double -> Prelude.IO ()

setCursorPosCallback :: Maybe CursorPosCallback -> Renderer ()
setCursorPosCallback cb = withWindow $
  Unsafe.toLinear \w -> Linear.do
    liftSystemIO $ GLFW.setCursorPosCallback w (const Prelude.<$> cb)
    return w

type MouseButtonCallback = GLFW.MouseButton -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> Prelude.IO ()

setMouseButtonCallback :: Maybe MouseButtonCallback -> Renderer ()
setMouseButtonCallback cb = withWindow $
  Unsafe.toLinear \w -> Linear.do
    liftSystemIO $ GLFW.setMouseButtonCallback w (const Prelude.<$> cb)
    return w
