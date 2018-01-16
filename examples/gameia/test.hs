{-# LANGUAGE Arrows #-}

import           Data.Either
import           Data.List                 (find)
import qualified Data.Map                  as Map
import           EasyGL
import           EasyGLUT
import           FRP.Yampa
import qualified Graphics.Rendering.OpenGL as GL
import           System.IO                 (stderr)
import           Val.Strict                hiding (yaw)

import qualified System.Random.TF          as TF
import           Test.Behaviours
import           Test.Controller
import           Test.Data

load :: IO ResourceMap
load = do
  myShader <- loadShadersFromFile ["./assets/3Dshaders/vertex.shader","./assets/3Dshaders/frag.shader"] [VertexShader,FragmentShader] $ Just stderr
  myShader2 <- loadShadersFromFile ["./assets/3Dshaders/vertex.shader","./assets/3Dshaders/fragLink.shader"] [VertexShader,FragmentShader] $ Just stderr
  (Right mat) <- makeMaterial myShader []
  (Right mat2) <- makeMaterial myShader2 [("./assets/Young_Link/YoungLink_grp1.png","sampler01")]
  let cone = ("cone","./assets/cone.obj",mat)
      plane = ("plane","./assets/plane.obj",mat)
      sphere = ("sphere","./assets/sphere.obj",mat)
  loadResouces [cone,plane,sphere]

data NullEvent = NullEvent
instance MergeableEvent NullEvent where
  union NullEvent NullEvent = NullEvent

nullGenerator :: IL a -> IL (Event NullEvent)
nullGenerator = const emptyIL

main :: IO ()
main = do
  gen <- fmap TF.seedTFGen TF.mkSeedTime
  let il = [plane,controllerSF 5 (-0.1),player (0,1,0) 2,seeker 1 2]
  initScenePar cam load [nullGenerator] il
