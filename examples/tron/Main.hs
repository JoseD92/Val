module Main where

import           EasyGL
import           EasyGLUT
import           System.Environment
import           System.IO
import           Tron.Behaviours
import           Tron.Controller
import           Tron.Data
import           Val.Strict         hiding (yaw)

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
    let il = [moto 5,plane 80,controllerSF 10 (-0.1)] ++ walls 80
    initScenePar cam load [eventGenerator] il
