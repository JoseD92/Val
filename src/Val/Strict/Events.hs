--------------------------------------------------------------------------------
-- |
-- Module      :  Val.Events
-- Copyright   :  Copyright (c) 2017, Jose Daniel Duran Toro
-- License     :  BSD3
--
-- Maintainer  :  Jose Daniel Duran Toro <jose_daniel_d@hotmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Provides event for the ObjInput that uses GLUT. This module is specially useful if you implement objects using arrows and signal functions, is you are using makeSF you are better off importing EasyGLUT and Val.Data and constructing your conditions yourself.
--
--------------------------------------------------------------------------------
{-# LANGUAGE Arrows #-}

module Val.Strict.Events (
  --Key Events
  keyPress,
  keyDown,
  keyUp,
  keyReleased,

  --Mouse Fix Events
  mouseMoved,
  mouseMovedX,
  mouseMovedY,

  --Mouse Free Events
  mousePosition,
  mousePositionMoved,

  getObjOut,
  getObjects,
  inputEvent
) where

import EasyGLUT
import FRP.Yampa
import Control.Arrow
import Val.Strict.Data
import Val.Strict.IL
import qualified Data.Map.Strict as Map

-- | Generates an Event if the key have been press this frame.
keyPress :: Key -> ObjInput s a -> Event ()
keyPress k oi = maybePressed NoEvent (Event ()) $ Map.lookup k . keysGI . oiGameInput $ oi

-- | Generates an Event if the key is down.
keyDown :: Key -> ObjInput s a -> Event ()
keyDown k oi = maybeDown NoEvent (Event ()) $ Map.lookup k . keysGI . oiGameInput $ oi

-- | Generates an Event if the key is up.
keyUp :: Key -> ObjInput s a -> Event ()
keyUp k oi = maybeUp NoEvent (Event ()) $ Map.lookup k . keysGI . oiGameInput $ oi

-- | Generates an Event if the key have been released this frame.
keyReleased :: Key -> ObjInput s a -> Event ()
keyReleased k oi = maybeReleased NoEvent (Event ()) $ Map.lookup k . keysGI . oiGameInput $ oi


-- | Generates an Event, containing the x and y movement of the mouse, if the mouse if fixed to a position (see EasyGLUT) and have moved in this frame.
mouseMoved :: ObjInput s a -> Event (Int,Int)
mouseMoved ObjInput{oiGameInput=GameInput{mouseGI=FreeMouse _ _}} = NoEvent
mouseMoved ObjInput{oiGameInput=GameInput{mouseGI=FixMouse x y}} =
  if x /= 0 || y /= 0 then Event (fromIntegral x,fromIntegral y) else NoEvent

-- | Same as mouseMoved but only on the x axis.
mouseMovedX :: ObjInput s a -> Event Int
mouseMovedX ObjInput{oiGameInput=GameInput{mouseGI=FreeMouse _ _}} = NoEvent
mouseMovedX ObjInput{oiGameInput=GameInput{mouseGI=FixMouse x _}} =
  if x /= 0 then Event (fromIntegral x) else NoEvent

-- | Same as mouseMoved but only on the y axis.
mouseMovedY :: ObjInput s a -> Event Int
mouseMovedY ObjInput{oiGameInput=GameInput{mouseGI=FreeMouse _ _}} = NoEvent
mouseMovedY ObjInput{oiGameInput=GameInput{mouseGI=FixMouse _ y}} =
  if y /= 0 then Event (fromIntegral y) else NoEvent


-- | Generates and event when the mouse have free movement containing the actual mouse position.
mousePosition :: ObjInput s a -> Event (Int,Int)
mousePosition ObjInput{oiGameInput=GameInput{mouseGI=FixMouse _ _}} = NoEvent
mousePosition ObjInput{oiGameInput=GameInput{mouseGI=FreeMouse x y}} =
  Event (fromIntegral x,fromIntegral y)

-- | Generates and event when the mouse have free movement and is in a different position than last time, containing the actual mouse position.
mousePositionMoved :: (Int,Int) -> SF (ObjInput s a) (Event (Int,Int))
mousePositionMoved initialPosition = proc oi -> do
  rec
    mouseThisFrame <- hold initialPosition -< mousePosition oi
    mouseLastFrame <- hold initialPosition -< tag (mousePosition oi) mouseThisFrame
  returnA -< aux mouseThisFrame mouseLastFrame
  where
    aux (x1,y1) (x2,y2) = if x1 /= x2 || y1 /= y2 then Event (x1-x2,y1-y2) else NoEvent


-- | Returns the output state from last frame of the object with the given key if it existed last frame.
getObjOut :: ObjInput state eventType -> ILKey -> Maybe state
getObjOut oi key = lookupIL key $ oiPastFrame oi

-- | Retruns the objects from last frame along with its key.
getObjects :: ObjInput state eventType -> [(ILKey,state)]
getObjects = assocsIL . oiPastFrame

-- | Returns the event combinator.
inputEvent :: ObjInput s a -> Event a
inputEvent = oiEvents
