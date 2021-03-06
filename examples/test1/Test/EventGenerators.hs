module Test.EventGenerators (
collitionGen
) where
import Test.Data

import Val.Strict hiding (yaw)
import EasyGL
import EasyGLUT
import Data.Either
import FRP.Yampa
import Data.List (find)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Map as Map

collitionGen :: IL GameState -> IL (Event EventTypes)
collitionGen inObjs = mapILWithKey aux inObjs
  where
    assocs = assocsIL inObjs
    aux key obj = case valid of (x:_) -> Event $ Collition . snd $ x
                                [] -> noEvent
      where
        valid = filter (\(key2,obj2) -> key /= key2 && collition obj obj2 ) assocs
