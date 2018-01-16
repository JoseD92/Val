{-# LANGUAGE Arrows #-}

module Test.Data (
  GameState(..),
  EventTypes(..),
  collition,
  isCube,
  isSphere
) where

import Val.Strict hiding (yaw)
import EasyGL
import EasyGLUT
import Data.Either
import FRP.Yampa
import Data.List (find)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Map as Map
import Control.DeepSeq
import Control.Seq

data GameState = Null
  | Cube {
    x :: Double,
    y :: Double,
    z :: Double,
    size :: Double
  } | Sphere {
    x :: Double,
    y :: Double,
    z :: Double,
    size :: Double
  } | Controller {
    camController :: Camera3D
  }

isCube :: GameState -> Bool
isCube Cube{} = True
isCube _ = False

isSphere :: GameState -> Bool
isSphere Sphere{} = True
isSphere _ = False

instance Show GameState where
  show Cube{x=xin,y=yin,z=zin} = "Cube "++show xin++" "++show yin++" "++show zin
  show Sphere{x=xin,y=yin,z=zin} = "Sphere "++show xin++" "++show yin++" "++show zin
  show Null = "Null"
  show Controller{} = "Controller"

collition :: GameState -> GameState -> Bool
collition Cube{x=x1,z=y1} Cube{x=x2,z=y2} = ( (x1-x2)^2 + (y1-y2)^2 ) < 4
collition Cube{x=x1,z=y1} Sphere{x=x2,z=y2} = ( (x1-x2)^2 + (y1-y2)^2 ) < 4
collition Sphere{x=x1,z=y1} Cube{x=x2,z=y2} = ( (x1-x2)^2 + (y1-y2)^2 ) < 4
collition Sphere{x=x1,z=y1} Sphere{x=x2,z=y2} = ( (x1-x2)^2 + (y1-y2)^2 ) < 4
collition _ _ = False

data EventTypes = Collition GameState
  | NoCollition
  deriving (Show)

instance NFData GameState where
  rnf (Cube x y z size) = rnf x `seq` rnf y `seq` rnf z `seq` rnf size
  rnf (Sphere x y z size) = rnf x `seq` rnf y `seq` rnf z `seq` rnf size
  rnf (Controller _) = ()
  rnf Null = ()

instance NFData EventTypes where
  rnf (Collition x) = rnf x
  rnf NoCollition = ()

instance MergeableEvent EventTypes where
  union NoCollition NoCollition = NoCollition
  union a NoCollition = a
  union NoCollition a = a
  union a b = a
