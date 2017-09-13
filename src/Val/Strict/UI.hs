module Val.Strict.UI (
  UIRead(..),
  UIActions(..),
  getData,
  execUIActions
) where

import FRP.Yampa
import EasyGL
import EasyGLUT hiding (FreeMouse)

data ScreenState = Windowed {
    heigth :: Double,
    width :: Double
  } | FullScreen {
    heigth :: Double,
    width :: Double
  }

data UIRead = UIRead {
    uiScreenSize :: (Int,Int),
    uiScreenPosition :: (Int,Int)
  }

getData :: GLUT UIRead
getData = do
  (sizex,sizey) <- getWindowSize
  (posx,posy) <- getWindowPosition
  return $ UIRead
    (fromIntegral sizex,fromIntegral sizey)
    (fromIntegral posx,fromIntegral posy)

data UIActions = FixMouseAt Int Int
  | FreeMouse
  | HideCursor
  | ShowCursor
  | SetScreenSize Int Int
  | SetScreenPosition Int Int

execUIActions :: UIActions -> GLUT ()
execUIActions (FixMouseAt x y) = fixMouseAt (fromIntegral x) (fromIntegral y)
execUIActions FreeMouse = freeMouse
execUIActions HideCursor = hideCursor
execUIActions ShowCursor = showCursor
execUIActions (SetScreenSize x y) = setWindowSize (fromIntegral x) (fromIntegral y)
execUIActions (SetScreenPosition x y) = setWindowPosition (fromIntegral x) (fromIntegral y)
