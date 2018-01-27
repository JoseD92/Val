{-# LANGUAGE Arrows #-}

module Test.Behaviours (
plane,cube,sphere
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

cube :: (GL.GLdouble,GL.GLdouble)
  -> (GL.GLdouble,GL.GLdouble)
  -> Object GameState EventTypes
cube (initx,initz) (velx,velz) = proc gi -> do
  collEvent <- getCollition -< gi
  rec
    rot <- impulseIntegral -< (180,tag e (-360))
    e <- iPre noEvent <<< edge -< rot > 360
    (x,z,dirx,dirz) <- drSwitch  (moveXZSF 40 (-40) (velx,initx) 40 (-40) (velz,initz)) -<
      ((),tag collEvent (moveXZSF 40 (-40) (-dirx,x) 40 (-40) (-dirz,z)))

  let ret = newObjOutput $ Cube (realToFrac x) 0 (realToFrac z) 1
      trans = Transform
        (GL.Vector3 x 0 z)
        (Quaternion rot (GL.Vector3 0 1 0))
        1 1 1
      die = event noEvent (\(Collition gs) -> if isSphere gs then Event () else noEvent ) collEvent
      uni :: Uniform ()
      uni = do
        set "use" (2 :: GL.GLint)
        setF "color" [1,1,1,1 :: GL.GLdouble]
  returnA -< ret{ooRenderer=Just("cube",trans,uni),ooKillReq=die}

sphere :: (GL.GLdouble,GL.GLdouble)
  -> (GL.GLdouble,GL.GLdouble)
  -> Object GameState EventTypes
sphere (initx,initz) (velx,velz) = proc gi -> do
  collEvent <- getCollition -< gi
  rec
    rot <- impulseIntegral -< (180,tag e (-360))
    e <- iPre noEvent <<< edge -< rot > 360
    (x,z,dirx,dirz) <- drSwitch  (moveXZSF 40 (-40) (velx,initx) 40 (-40) (velz,initz)) -<
      ((),tag collEvent (moveXZSF 40 (-40) (-dirx,x) 40 (-40) (-dirz,z)))

  let ret = newObjOutput $ Sphere (realToFrac x) 0 (realToFrac z) 1
      trans = Transform
        (GL.Vector3 x 0 z)
        (Quaternion rot (GL.Vector3 0 1 0))
        1 1 1
      die = event noEvent (\(Collition gs) -> if isCube gs then Event () else noEvent ) collEvent
      uni :: Uniform ()
      uni = do
        set "use" (2 :: GL.GLint)
        setF "color" [1,1,1,1 :: GL.GLdouble]
  returnA -< ret{ooRenderer=Just("sphere",trans,uni),ooKillReq=die}

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
