{-# LANGUAGE Arrows #-}

import           Data.Either
import           Data.List                 (find)
import qualified Data.Map                  as Map
import           EasyGL
import           EasyGLUT
import           FRP.Yampa                 hiding (RandomGen, randomR)
import qualified Graphics.Rendering.OpenGL as GL
import           System.IO                 (stderr)
import           System.Random
import qualified System.Random.TF          as TF
import           Val.Strict                hiding (yaw)

-- Se cargan los recursos a utilizar en el ejemplo
load :: IO ResourceMap
load = do
  myShader <- loadShadersFromFile
    ["./assets/3Dshaders/vertex.shader",
    "./assets/3Dshaders/ColorShader.shader"]
    [VertexShader,FragmentShader]
    (Just stderr)
  (Right mat) <- makeMaterial myShader []
  let plane = ("plane","./assets/plane.obj",mat)

  myShader <- loadShadersFromFile
    ["./assets/3Dshaders/vertex.shader",
    "./assets/3Dshaders/NormalShader.shader"]
    [VertexShader,FragmentShader]
    (Just stderr)
  (Right mat) <- makeMaterial myShader []
  let sphere = ("sphere","./assets/sphere.obj",mat)

  loadResouces [plane,sphere]

-- Informacion de los objetos.
data GameState = Null
  | Sphere {
    x    :: Double,
    y    :: Double,
    z    :: Double,
    size :: Double
  }

collition :: GameState -> GameState -> Bool
collition Sphere{x=x1,z=y1} Sphere{x=x2,z=y2} =
  ( (x1-x2)^2 + (y1-y2)^2 ) < 4
collition _ _ = False

data EventTypes = Collition GameState
  | NoCollition

instance MergeableEvent EventTypes where
  union NoCollition NoCollition = NoCollition
  union a NoCollition           = a
  union NoCollition a           = a
  union a b                     = a

-- Funcion que detecta colisiones.
collitionGen :: IL GameState -> IL (Event EventTypes)
collitionGen inObjs = mapILWithKey aux inObjs
  where
    assocs = assocsIL inObjs
    aux key obj = case valid of
      (x:_) -> Event $ Collition . snd $ x
      []    -> noEvent
      where
        valid = filter
          (\(key2,obj2) -> key /= key2 && collition obj obj2 )
          assocs

cam :: SF (GameInput,IL GameState) Camera3D
cam = proc _ -> do
    returnA -< c
    where
    (Right c) = createCamera3D 0 150 0 0 (-90) 0 30 (800/600) 0.3 200

plane :: Object GameState EventTypes
plane = proc _ -> do
  let ret = newObjOutput Null
      trans = Transform
        (GL.Vector3 0 (-1) 0)
        (Quaternion 0 (GL.Vector3 0 1 0))
        40 1 40
      uni = do
        set "color" $ GL.Color4 1 1 1 (1 :: GL.GLfloat)
  returnA -< ret{ooRenderer=Just("plane",trans,uni)}

moveSF :: GL.GLdouble
  -> GL.GLdouble
  -> (GL.GLdouble,GL.GLdouble)
  -> SF () (GL.GLdouble,GL.GLdouble)
moveSF limMax limMin d@(dir,_) =
  if (dir > 0) then aux2 (> limMax) (< limMin) d else aux2 (< limMin) (> limMax) d
  where
    aux2 f1 f2 (dir,initx) = switch (aux f1 dir initx) (aux2 f2 f1)
    aux f dir initx = proc _ -> do
      xnew <- (+initx) ^<< integral -< dir
      e <- edge -< f xnew
      returnA -< ((xnew,dir),tag e (-dir,xnew))

moveXZSF :: GL.GLdouble -> GL.GLdouble -> (GL.GLdouble,GL.GLdouble) ->
  GL.GLdouble -> GL.GLdouble -> (GL.GLdouble,GL.GLdouble) ->
  SF a (GL.GLdouble,GL.GLdouble,GL.GLdouble,GL.GLdouble)
moveXZSF limMaxX limMinX initX limMaxZ limMinZ initZ = proc _ -> do
  (x,dirx) <- moveSF limMaxX limMinX initX -< ()
  (z,dirz) <- moveSF limMaxZ limMinZ initZ -< ()
  returnA -< (x,z,dirx,dirz)

type Info = (GL.GLdouble,GL.GLdouble,GL.GLdouble,GL.GLdouble)

getCollition :: SF (ObjInput GameState EventTypes,Info) (Event Info)
getCollition = proc (oi,salida) -> do
  let myEvent = event noEvent toEvent $ inputEvent oi
  returnA -< tag myEvent salida
  where
    toEvent NoCollition = noEvent
    toEvent a           = Event a

sphere :: GL.GLdouble -> GL.GLdouble
  -> GL.GLdouble -> GL.GLdouble
  -> Object GameState EventTypes
sphere initx initz velx velz = proc gi -> do
  rec
    rot <- impulseIntegral -< (180,tag e (-360))
    e <- iPre noEvent <<< edge -< rot > 360
    (x,z,dirx,dirz) <- moveArr (initx,initz,velx,velz) -< gi

  let ret = newObjOutput $ Sphere (realToFrac x) 0 (realToFrac z) 1
      trans = Transform
        (GL.Vector3 x 0 z)
        (Quaternion rot (GL.Vector3 0 1 0))
        1 1 1
  returnA -< ret{ooRenderer=Just("sphere",trans,return ())}
  where
    moveArr (x,z,velx,velz) = dkSwitch
      (moveXZSF 40 (-40) (velx,x) 40 (-40) (velz,z))
      (getCollition >>> notYet)
      (\sf (x,z,velx,velz) -> moveArr (x,z,-velx,-velz) )

randomSphere :: RandomGen g => g -> (Object GameState EventTypes, g)
randomSphere g = (sphere initx initz velx velz,g4)
  where
    (initx,g1) = randomR (-39,39) g
    (initz,g2) = randomR (-39,39) g1
    (velx,g3) = randomR (-4,4) g2
    (velz,g4) = randomR (-4,4) g3

randomSphereList :: RandomGen g => g -> [Object GameState EventTypes]
randomSphereList g = obj:(randomSphereList g1)
	where
		(obj,g1) = randomSphere g

main :: IO ()
main = do
  gen <- TF.mkSeedTime >>= return . TF.seedTFGen
  let il = [plane]++(take 200 $ randomSphereList gen)
  initScenePar cam load [collitionGen] il
