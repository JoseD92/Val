module Main where

import           EasyGL
import           EasyGLUT
import qualified Graphics.Rendering.OpenGL as GL
import           System.Environment
import           System.IO
import           Tron.Behaviours
import           Tron.Controller
import           Tron.Data
import           Val.Strict                hiding (yaw)

load :: IO ResourceMap
load = do
  myShader <- loadShadersFromFile
    ["./assets/3Dshaders/vertex.shader",
    "./assets/3Dshaders/TextureFrag.shader"]
    [VertexShader,FragmentShader]
    (Just stderr)
  (Right planeMat) <- makeMaterial myShader [("./assets/texture/tronFloor2.png","sampler01")]
  let plane = ("plane","./assets/plane.obj",planeMat)
  let wall = ("wall","./assets/wall.obj",planeMat)

  myShader <- loadShadersFromFile
    ["./assets/3Dshaders/vertex.shader",
    "./assets/3Dshaders/ColorShader.shader"]
    [VertexShader,FragmentShader]
    (Just stderr)
  (Right motoMat) <- makeMaterial myShader []
  let moto = ("moto","./assets/moto.obj",motoMat)
  let wall2 = ("wall2","./assets/wall.obj",motoMat)

  loadResouces [plane,wall,moto,wall2]

main :: IO ()
main = do
  l <- getArgs
  case l of
    ["double"] -> double
    ["single"] -> single

single :: IO ()
single = do
    let il = [moto p1env
          ,plane 80
          ,controllerSF (0,65,(-150)) (180,-30,0) 10 (-0.1)] -- ++ walls 80
    initScenePar cam load [eventGenerator] il
    where
      p1env = MotoEnv (-40) 0 0 90 (GL.Vector4 1 0 0 1) 5 (Char '2') (Char '1')

double :: IO ()
double = do
    let il = [moto p1env
          ,moto p2env
          ,plane 80
          ,controllerSF (0,80,(-200)) (180,-30,0) 10 (-0.1)] -- ++ walls 80
    initScenePar cam load [eventGenerator] il
    where
      p1env = MotoEnv (-40) 0 0 90 (GL.Vector4 1 0 0 1) 5 (Char '2') (Char '1')
      p2env = MotoEnv (40) 0 0 270 (GL.Vector4 0 1 0 1) 5 (Char 'l') (Char 'k')
