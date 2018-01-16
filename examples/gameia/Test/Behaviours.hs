{-# LANGUAGE Arrows #-}

module Test.Behaviours (
plane,player,seeker
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

import           AI.AIAlgo
import           AI.AIData
import           Graphics.Rendering.OpenGL (Vector2 (..), Vector3 (..))

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

move :: SF (ObjInput GameState e,Kinematic) Kinematic
move = proc (oi,k) -> do
  let
    t = deltaTime oi
    pressB = event 0 (const 1) $ keyDown (GLUT.Char 'b') oi
    pressH = event 0 (const 1) $ keyDown (GLUT.Char 'h') oi
    pressN = event 0 (const 1) $ keyDown (GLUT.Char 'n') oi
    pressM = event 0 (const 1) $ keyDown (GLUT.Char 'm') oi
    (Vector2 x z) = position k
    zmov = 5 * t * (pressM - pressB) + z
    xmov = 5 * t * (pressH - pressN) + x
  returnA -< k{position=Vector2 xmov zmov}

myuni = do
  set "use" (2 :: GL.GLint)
  setF "color" [1,1,1,1 :: GL.GLdouble]

seeker :: Double -> ILKey -> Object GameState e
seeker speed targetKey = proc oi -> do
  let
    target = getObjOut oi targetKey
    (die,targetKin) =
      maybe (Event (),undefined) (\t->(noEvent,kinematic t)) target
  dtime <- arr deltaTime -< oi
  rec
    steer <- arr (uncurry $ arrive 0.25 3) -< (kin,targetKin)
    kin <- iPre initKin <<< arr (\(t,s,k)->updateKinematic t s k) -< (dtime,steer,kin)
  returnA -< (newObjOutput $ Seeker kin){
    ooRenderer=Just("cone",toTansform 0.1 kin,myuni)}
    --ooWorldReq=[IOReq (print kin >> return undefined) (\e->undefined) True]}
  where
  initKin = Kinematic 0 0 0 (Just 10) 0


player :: (Double,Double,Double)
  -> Double
  -> Object GameState e
player (initx,inity,initz) vely = proc oi -> do
  rec
    playerKin <- iPre initKin <<< move -< (oi,playerKin)

  let ret = newObjOutput $ Player playerKin
      trans = toTansform inity playerKin
  returnA -< ret{ooRenderer=Just("sphere",trans,myuni)}
  where
    initKin = Kinematic (Vector2 initx initz) 0 (Vector2 0 0) Nothing 0

plane :: Object GameState e
plane = proc oi -> do
  rec
    r <- iPre 0 <<< impulseIntegral -< (0.1,if r > 1 then Event (-1) else noEvent)
    g <- iPre 0 <<< impulseIntegral -< (0.2,if g > 1 then Event (-1) else noEvent)
    b <- iPre 0 <<< impulseIntegral -< (0.3,if b > 1 then Event (-1) else noEvent)
  let ret = newObjOutput Null
      trans = Transform
        (GL.Vector3 0 (-1) 0)
        (Quaternion 0 (GL.Vector3 0 1 0))
        2 1 2
      uni = do
        set "use" (4 :: GL.GLint)
        setF "color" [r,g,b,1 :: GL.GLfloat]
  returnA -< ret{ooRenderer=Just("plane",trans,uni)}
