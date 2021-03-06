{-# LANGUAGE Arrows #-}

module Test.Behaviours (
plane,sphere
) where

import           Data.Either
import           Data.List                 (find)
import qualified Data.Map                  as Map
import           EasyGL
import qualified EasyGLUT                  as GLUT
import           FRP.Yampa
import qualified Graphics.Rendering.OpenGL as GL
import           System.Mem
import           Test.Data
import           Val.Strict                hiding (picth, yaw)

moveSF :: GL.GLdouble -> GL.GLdouble -> (GL.GLdouble,GL.GLdouble) -> SF () (GL.GLdouble,GL.GLdouble)
moveSF limMax limMin d@(dir,_) = if (dir > 0) then aux2 (> limMax) (< limMin) d else aux2 (< limMin) (> limMax) d
  where
    aux2 f1 f2 (dir,initx) = switch (aux f1 dir initx) (aux2 f2 f1)
    aux f dir initx = proc _ -> do
      xnew <- (+initx) ^<< integral -< dir
      e <- edge -< f xnew
      returnA -< ((xnew,dir),tag e (-dir,xnew))

moveXZSF :: GL.GLdouble -> GL.GLdouble -> (GL.GLdouble,GL.GLdouble) ->
  GL.GLdouble -> GL.GLdouble -> (GL.GLdouble,GL.GLdouble) ->
  SF () (GL.GLdouble,GL.GLdouble,GL.GLdouble,GL.GLdouble)
moveXZSF limMaxX limMinX initX limMaxZ limMinZ initZ = proc _ -> do
  (x,dirx) <- moveSF limMaxX limMinX initX -< ()
  (z,dirz) <- moveSF limMaxZ limMinZ initZ -< ()
  returnA -< (x,z,dirx,dirz)

getCollition :: SF (ObjInput GameState EventTypes) (Event EventTypes)
getCollition = proc oi -> do
  returnA -< event noEvent toEvent $ inputEvent oi
  where
    toEvent NoCollition = noEvent
    toEvent a           = Event a

bounce :: GL.GLdouble -> GL.GLdouble -> GL.GLdouble -> SF () GL.GLdouble
bounce gravity initPos velInit = aux initPos
  where
    aux pos = switch (aux2 pos) aux
    aux2 pos = proc () -> do
      rec
        vel <- (+velInit) ^<< integral -< gravity
        y <- (+pos) ^<< integral -< vel
        bounceEvent <- edge -< y < initPos && vel < 0
      returnA -< (y,tag bounceEvent pos)

sphere :: (GL.GLdouble,GL.GLdouble,GL.GLdouble)
  -> GL.GLdouble
  -> Object GameState EventTypes
sphere (initx,inity,initz) vely = proc gi -> do
  y <- bounce (-0.98) 0 1 -< ()

  let ret = newObjOutput $ Sphere 0 0 0 1
      trans = Transform
        (GL.Vector3 initx y initz)
        (Quaternion 0 (GL.Vector3 0 1 0))
        1 1 1
      uni :: Uniform ()
      uni = do
        set "use" (2 :: GL.GLint)
        setF "color" [1,1,1,1 :: GL.GLdouble]
  returnA -< ret{ooRenderer=Just("sphere",trans,uni)}

plane :: Object GameState EventTypes
plane = proc oi -> do
  rec
    r <- iPre 0 <<< impulseIntegral -< (0.1,if r > 1 then Event (-1) else noEvent)
    g <- iPre 0 <<< impulseIntegral -< (0.2,if g > 1 then Event (-1) else noEvent)
    b <- iPre 0 <<< impulseIntegral -< (0.3,if b > 1 then Event (-1) else noEvent)
  let ret = newObjOutput Null
      trans = Transform
        (GL.Vector3 0 (-1) 0)
        (Quaternion 0 (GL.Vector3 0 1 0))
        40 1 40
      uni = do
        set "use" (4 :: GL.GLint)
        setF "color" [r,g,b,1 :: GL.GLfloat]
  returnA -< ret{ooRenderer=Just("plane",trans,uni)}
