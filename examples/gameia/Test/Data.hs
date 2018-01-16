{-# LANGUAGE Arrows #-}

module Test.Data (
  GameState(..),
  isPlayer
) where

import           Control.DeepSeq
import           Control.Seq
import           Data.Either
import           Data.List                 (find)
import qualified Data.Map                  as Map
import           EasyGL
import           EasyGLUT
import           FRP.Yampa
import qualified Graphics.Rendering.OpenGL as GL
import           Val.Strict                hiding (yaw)

import           AI.AIAlgo
import           AI.AIData

data GameState = Null
  | Player {
    kinematic :: Kinematic
  } | Seeker {
    kinematic :: Kinematic
  } | Controller {
    camController :: Camera3D
  }

isPlayer :: GameState -> Bool
isPlayer Player{} = True
isPlayer _        = False
