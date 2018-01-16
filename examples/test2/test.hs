{-# LANGUAGE Arrows #-}

import Val.Strict hiding (yaw)
import EasyGL
import EasyGLUT
import Data.Either
import System.IO (stderr)
import FRP.Yampa
import Data.List (find)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Map as Map

import Test.Data
import Test.Controller
import Test.EventGenerators
import Test.Behaviours
import qualified System.Random.TF as TF

load :: IO ResourceMap
load = do
  myShader <- loadShadersFromFile ["./assets/3Dshaders/vertex.shader","./assets/3Dshaders/frag.shader"] [VertexShader,FragmentShader] $ Just stderr
  (Right mat) <- makeMaterial myShader []
  let cube = ("cube","./assets/cube.obj",mat)
      plane = ("plane","./assets/plane.obj",mat)
      sphere = ("sphere","./assets/sphere.obj",mat)
  loadResouces [cube,plane,sphere]


main :: IO ()
main = do
  gen <- fmap TF.seedTFGen TF.mkSeedTime
  let il = [plane,sphere (0,1,0) 10,controllerSF gen 5 (-0.1)]
  initScenePar cam load [collitionGen] il
