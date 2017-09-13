{-# LANGUAGE Arrows #-}

module Test.Controller (
  cam,
  controllerSF
) where

import Val.Strict hiding (yaw,picth)
import EasyGL
import qualified EasyGLUT as GLUT
import Data.Either
import FRP.Yampa
import Data.List (find)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Map as Map
import Test.Data
import Test.Behaviours
import System.Mem
import qualified System.Random as R

sumWithLimits max min a b
  | a + b > max = max
  | a + b < min = min
  | otherwise = a + b

sumWithLimitsSF :: (Num a,Ord a) => a -> a -> SF (a,a) a
sumWithLimitsSF max min = arr $ uncurry $ sumWithLimits max min

sumCiclic max a b
  | a + b > max = a + b - max
  | a + b < 0 = a + b + max
  | otherwise = a + b

sumCiclicSF :: (Num a,Ord a) => a -> SF (a,a) a
sumCiclicSF max = arr $ uncurry $ sumCiclic max

toAngle :: Floating a => a -> a
toAngle a = pi*a/180

sina :: Floating a => a -> a
sina = sin . toAngle

cosa :: Floating a => a -> a
cosa = cos . toAngle

updateCam :: Double
  -> Camera3D
  -> GL.GLdouble
  -> GL.GLdouble
  -> GL.GLdouble
  -> Time
  -> ObjInput GameState EventTypes
  -> Camera3D
updateCam mvSensitivity cam0 yaw pitch roll deltaT gi = cam2
  where
    eventid a b c = event a (const b) c
    pressUp = eventid (0,0,0) ((*(-1)) . sina $ yaw,0,(*(-1)) . cosa $ yaw) $ keyDown (GLUT.Char 'w') gi
    pressDown = eventid (0,0,0) (sina yaw,0,cosa yaw) $ keyDown (GLUT.Char 's') gi
    pressLeft = eventid (0,0,0) ((*(-1)) . sina . (+ 90) $ yaw,0,(*(-1)) . cosa . (+ 90) $ yaw) $ keyDown (GLUT.Char 'a') gi
    pressRight = eventid (0,0,0) ((*(-1)) . sina . subtract 90 $ yaw,0,(*(-1)) . cosa . subtract 90 $ yaw) $ keyDown (GLUT.Char 'd') gi
    pressSpace = eventid (0,0,0) (0,1,0) $ keyDown (GLUT.Char ' ') gi
    pressLControl = eventid (0,0,0) (0,-1,0) $ keyDown (GLUT.Char 'q') gi
    tripletSum (x1,y1,z1) (x2,y2,z2) = (x1+x2,y1+y2,z1+z2)
    (mvx,mvy,mvz) = foldl tripletSum (0,0,0) [pressUp,pressDown,pressLeft,pressRight,pressSpace,pressLControl]
    deltamov = deltaT*mvSensitivity
    cam1 = if (mvx,mvy,mvz) /= (0,0,0) then
      translateCamera (mvx*deltamov) (mvy*deltamov) (mvz*deltamov) cam0
      else cam0
    cam2 = setYawPitchRoll yaw pitch roll cam1

spawner :: R.RandomGen g => g -> SF () (Event [Object GameState EventTypes])
spawner g = switch aux aux2
  where
    aux = proc _ -> do
      e <- notYet -< Event g4
      returnA -< (Event [newCube],e)
    (initx,g1) = R.randomR (-20,20) g
    (initz,g2) = R.randomR (-20,20) g1
    (velx,g3) = R.randomR (-5,5) g2
    (velz,g4) = R.randomR (-5,5) g3
    newCube = cube (initx,initz) (velx,velz)
    aux2 g = switch (aux3 g) spawner
    aux3 g = proc _ -> do
      e <- after 2 g -< ()
      returnA -< (noEvent,e)

controllerSF :: R.RandomGen g => g -> Double -> Double -> Object GameState EventTypes
controllerSF g mvSen viewSen = proc oi -> do
  let (mousex,mousey) = event (0,0) id $ mouseMoved oi
      t = deltaTime oi
      (wx,wy) = uiScreenSize . uiState $ oi
      uiAction2 = event [] (const [FixMouseAt (div wx 2) (div wy 2),HideCursor]) $ keyDown (GLUT.Char 'z') oi
      uiAction = event uiAction2 (const [FreeMouse,ShowCursor]) $ keyDown (GLUT.Char 'x') oi
  rec
    yaw <- iPre 0 <<< sumCiclicSF 360 -< (yaw,realToFrac $ viewSen * fromIntegral mousex)
    pitch <- iPre 0 <<< sumWithLimitsSF 90 (-90) -< (pitch,realToFrac $ viewSen * fromIntegral mousey)
    roll <- iPre 0 -< roll
    cam <- iPre c -< updateCam mvSen cam yaw pitch roll t oi

  spawn <- spawner g -< ()
  rec
    num <- iPre 0 -< event num (const $ 1+num) spawn
  --let out = (newObjOutput $ Controller cam){ooUIReq=uiAction,ooSpawnReq=spawn}
  let out = (newObjOutput $ Controller cam){ooUIReq=uiAction,ooSpawnReq=spawn,ooWorldSpawn=event [] (const [print num]) spawn}
  returnA -< out
  where
    (Right c) = createCamera3D 0 0 10 0 0 0 30 (800/600) 0.3 200

cam :: SF (GameInput,IL GameState) Camera3D
cam = proc (_,il) -> do --makeCamSF (MyData c 0 0 0) (camfun 5 5)
    rec
      controllerFound <- iPre noEvent <<< controlerArr -< (controllerFound,il)
    returnA -< event c (camController . snd) controllerFound
  where
    (Right c) = createCamera3D 0 0 10 0 0 0 30 (800/600) 0.3 200
    isController Controller{} = True
    isController _ = False
    control il = find (isController . snd) $ assocsIL il
    controlE il = maybe noEvent Event (control il)
    controlerArr = arr $ \(e,il) -> event
      (controlE il)
      (\(key,_) -> maybe (controlE il) (\a-> Event (key,a)) (lookupIL key il) )
      e
