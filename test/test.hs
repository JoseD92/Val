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
import qualified System.Random as R

load :: IO ResourceMap
load = do
  myShader <- loadShadersFromFile ["./assets/3Dshaders/vertex.shader","./assets/3Dshaders/frag.shader"] [VertexShader,FragmentShader] $ Just stderr
  (Right mat) <- makeMaterial myShader []
  let cube = ("cube","./assets/cube.obj",mat)
      plane = ("plane","./assets/plane.obj",mat)
  loadResouces [cube,plane]


main :: IO ()
main = do
  gen <- R.newStdGen
  let il = insertIL plane $ insertIL (controllerSF gen 5 (-0.1)) emptyIL
  initScenePar cam load [collitionGen] il
