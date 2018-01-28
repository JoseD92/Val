{-# LANGUAGE Arrows          #-}
{-# LANGUAGE RecordWildCards #-}

module Tron.Data (
  GameState(..),
  EventTypes(..),
  collition,
  eventGenerator
) where

import           Control.DeepSeq
import           Control.Seq
import           Data.Either
import           Data.List                 (find)
import qualified Data.Map                  as Map
import           Data.Maybe                (isJust)
import           Data.SG
import           EasyGL
import           EasyGLUT
import           FRP.Yampa
import qualified Graphics.Rendering.OpenGL as GL
import           Val.Strict                hiding (yaw)

data GameState = Null
  | Controller {
    camController :: Camera3D
  } | Moto {
    motoCube :: Shape' GL.GLdouble,
    trans    :: (GL.GLdouble,GL.GLdouble,GL.GLdouble,GL.GLdouble)
  } | Trail {
    trailLine :: Line2' GL.GLdouble
  }

collition :: GameState -> GameState -> Bool
collition Null _ = False
collition _ Null = False
collition Controller{} _ = False
collition _ Controller{} = False
collition Moto{motoCube=c1} Moto{motoCube=c2} = c1 /= c2 && (isJust $ overlap c1 c2)
collition Trail{..} Moto{..} = maybe False (\(x,y)-> (x>0 && x<=1) || (y>0 && y<=1)) $ intersectLineShape trailLine motoCube
collition Moto{..} Trail{..} = maybe False (\(x,y)-> (x>0 && x<=1) || (y>0 && y<=1)) $ intersectLineShape trailLine motoCube
collition Trail{} Trail{} = False

eventGenerator :: IL GameState -> IL (Event EventTypes)
eventGenerator il = fmap genEvent il
  where
    genEvent m@Moto{} = if any (collition m) il then Event Collition else noEvent
    genEvent _ = noEvent

data EventTypes = Collition
  | NoCollition

instance NFData GameState where
  rnf (Controller _) = ()
  rnf Null           = ()
  rnf Moto{}         = ()
  rnf Trail{}        = ()

instance NFData EventTypes where
  rnf Collition   = ()
  rnf NoCollition = ()

instance MergeableEvent EventTypes where
  union NoCollition NoCollition = NoCollition
  union a NoCollition           = a
  union NoCollition a           = a
  union a b                     = a
