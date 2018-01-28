{-# LANGUAGE Arrows #-}

module Tron.Behaviours (
plane,walls,moto
) where

import           Data.Either
import           Data.List                 (find)
import qualified Data.Map                  as Map
import           Data.SG
import           Debug.Trace
import           EasyGL
import qualified EasyGLUT                  as GLUT
import           FRP.Yampa
import qualified Graphics.Rendering.OpenGL as GL
import           Tron.Data
import           Val.Strict                hiding (picth, yaw)

toRad :: Floating a => a -> a
toRad a = pi*a/180

sina :: Floating a => a -> a
sina = sin . toRad

cosa :: Floating a => a -> a
cosa = cos . toRad

plane :: GL.GLdouble -> Object GameState EventTypes
plane size = proc oi -> do
  --let t = realToFrac $ deltaTime oi
  --    e = mergeBy (+) (tag (keyDown (GLUT.Char 'o') oi) (-1)) (tag (keyDown (GLUT.Char 'p') oi) 1)
  --rec
  --  scale <- iPre 1 <<< arr (uncurry (+)) -< (scale,event 0 (*t) e)
  let ret = newObjOutput Null
      trans = Transform
        (GL.Vector3 0 0 0)
        (Quaternion 0 (GL.Vector3 0 1 0))
        size 1 size
      uni = do
        set "scalex" (100 :: GL.GLfloat)
        set "scaley" (100 :: GL.GLfloat)
  returnA -< ret{ooRenderer=Just("plane",trans,uni)}

walls size = map wall [
    Transform (GL.Vector3 1 1 0) (Quaternion 90 (GL.Vector3 0 1 0)) size (size/2) size
    , Transform (GL.Vector3 (-1) 1 0) (Quaternion 270 (GL.Vector3 0 1 0)) size (size/2) size
    , Transform (GL.Vector3 0 1 1) (Quaternion 0 (GL.Vector3 0 1 0)) size (size/2) size
    , Transform (GL.Vector3 0 1 (-1)) (Quaternion 180 (GL.Vector3 0 1 0)) size (size/2) size
  ]

wall :: Transform -> Object GameState EventTypes
wall t = proc oi -> do
  let ret = newObjOutput Null
      trans = t
      uni = do
        set "scalex" (100 :: GL.GLfloat)
        set "scaley" (50 :: GL.GLfloat)
  returnA -< ret{ooRenderer=Just("wall",trans,uni)}

moto :: GL.GLdouble -> Object GameState EventTypes
moto speed = proc oi -> do
  let steer = mergeBy (+)
        (tag (keyDown (GLUT.Char 'k') oi) (-1))
        (tag (keyDown (GLUT.Char 'l') oi) 1)
  rot <- integral <<< arr (*25) -< event 0 id steer
  trail <- repeatedly 0.1 () -< ()
  x <- integral -< speed * sina rot
  z <- integral -< speed * cosa rot
  (prevx,prevz) <- dHold (0,0) -< tag trail (x,z)
  let ret = newObjOutput (Moto $ makeRect x z rot)
      trans = Transform (GL.Vector3 x 0 z) (Quaternion (rot+90) (GL.Vector3 0 1 0)) 1 1 1
      uni = set "color" (GL.Vector4 1 0 0 0.2 :: GL.Vector4 GL.GLfloat)
      go = genTrail (motoLength/speed) 60 (GL.Vector3 1 0 0) (prevx,prevz) (x,z)
  returnA -< ret{
    ooRenderer=Just("moto",trans,uni),
    ooSpawnReq=tag trail [go],
    ooKillReq= tag (inputEvent oi) ()}
  where
    motoLength = 2.5
    makeRect x z rot = rotateShape (toRad $ rot*(-1)) $ Rectangle (Point2 (x,z)) (0.6,2.3)

genTrail :: Time
  -> Time
  -> GL.Vector3 GL.GLfloat
  -> (GL.GLdouble,GL.GLdouble)
  -> (GL.GLdouble,GL.GLdouble)
  -> Object GameState EventTypes
genTrail activationTime lifeTime (GL.Vector3 r g b) (x1,z1) (x2,z2) = proc _ -> do
  e <- notYet <<< edge <<< arr (> activationTime) <<< time -< ()
  returnA -< (newObjOutput Null){
    ooSpawnReq = tag e [trailT],
    ooKillReq= e,
    ooRenderer= Just("wall2",trans,uni)}
  where
    line@(Line2 _ (Rel2 _ tam)) = lineTo (Point2 (x1,z1)) (Point2 (x2,z2))
    p@(Point2 (x,z)) = alongLine 0.5 line
    angle = (toAngle $ getLineDir2 line)*180/pi
    quadt = Quaternion (angle * (-1)) (GL.Vector3 0 1 0)
    modifier = (realToFrac $ sqrt tam)/2
    trans = Transform (GL.Vector3 (x/modifier) 0 (z/modifier)) quadt modifier 1 modifier
    uni = set "color" (GL.Vector4 r g b 1)

    trailT = proc oi -> do
      kill <- after lifeTime () -< ()
      --gradiente <- arr (+1) <<< integral -< (-0.8)/lifeTime
      let ret = newObjOutput (Trail line)
      returnA -< ret{ooRenderer=Just("wall2",trans,uni),ooKillReq=kill}
