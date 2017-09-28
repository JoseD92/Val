module Val.Strict.Data (
  ResourceIdentifier,
  Resource,
  ResourceMap,

  MergeableEvent(..),
  GameInput(..),
  Object,
  ObjInput(..),
  IOReq(..),
  IOExec(..),
  ObjOutput(..),

  emptyGameInput,
  toExec,
  emptyObjInput,
  newObjOutput,

  -- From Transform
  Rotation(..),
  valRotate,
  Transform(..),
  useTransform,

  -- Modules
  module Val.Strict.IL
)
where

import qualified Graphics.Rendering.OpenGL as GL
import FRP.Yampa
import Data.Map.Strict hiding (fromList,union,unions,toList)
import qualified Data.Map.Strict as Map
import Control.DeepSeq
import Control.Seq
import EasyGL
import EasyGLUT
import Data.List (foldl1')
import Data.Foldable (toList)
import qualified EasyGLUT as GLUT
import Control.Exception
import Control.Concurrent.Async
import Val.Strict.UI
import Val.Strict.IL

--------------------------------------------------------------------------------
-- About resource generation.
--------------------------------------------------------------------------------

-- | A value that identifies a resource.
type ResourceIdentifier = String

-- | Info to load a Resource.
type Resource = (
    ResourceIdentifier, -- name of Resource
    String, -- Path to Obj to load.
    Material  -- material to render obj
  )

-- | A map from a ResourceIdentifier to a loaded mesh and material.
type ResourceMap = Map ResourceIdentifier (Entity,Material)

--------------------------------------------------------------------------------
-- Game stuff.
--------------------------------------------------------------------------------

-- | Describes the type of objects that are event containers and can combine the events.
class MergeableEvent a where
  union :: a -> a -> a

data GameInput = GameInput {
    keysGI :: Map Key KeyState,
    mouseGI :: MouseState,
    timeGI :: Time,
    uiGI :: UIRead
  }

emptyGameInput :: GameInput
emptyGameInput = GameInput Map.empty (GLUT.FreeMouse 0 0) 0 undefined

type Object outState eventType = SF (ObjInput outState eventType) (ObjOutput outState eventType)

data ObjInput state eventType = ObjInput {
  oiEvents :: Event eventType,
  oiGameInput :: GameInput,
  oiPastFrame :: IL state
}

instance (NFData eventType) => NFData (ObjInput state eventType) where
  rnf ObjInput{oiEvents=events} = rnf events -- the game input from the world will normally be normal form because it comes from IO.

data IOReq a = IOReq {
    ioReqIO :: IO a,
    ioReqError :: SomeException -> a,
    ioBloq :: Bool -- whether to wait or not the io. If True the result will be deliver next frame, otherwise, when result is available.
  }

data IOExec a = IOExec {
    ioExecIO :: Async a,
    ioExecError :: SomeException -> a,
    ioExecBloq :: Bool -- whether to wait or not the io. If True the result will be deliver next frame, otherwise, when result is available.
  }

toExec :: IOReq a -> IO (IOExec a)
toExec req = do
  a <- async $ ioReqIO req
  return IOExec{
      ioExecIO=a,
      ioExecError=ioReqError req,
      ioExecBloq=ioBloq req
    }

data ObjOutput state eventType = ObjOutput {
  ooObjState :: !state,
  ooRenderer :: Maybe (ResourceIdentifier,Transform,Uniform ()),
  ooKillReq :: Event (),
  ooSpawnReq :: Event [Object state eventType],
  ooWorldReq :: [IOReq eventType], -- request to perform IO
  ooWorldSpawn :: [IO ()], -- request to perform IO, result is not needed, action will be spawn in a new thread and system will forget about it.
  ooUIReq :: [UIActions]
}

emptyObjInput :: ObjInput s a
emptyObjInput = ObjInput undefined emptyGameInput emptyIL

newObjOutput :: a -> ObjOutput a b
newObjOutput state = ObjOutput{ooObjState=state,
  ooRenderer=Nothing,
  ooKillReq=noEvent,
  ooSpawnReq=noEvent,
  ooWorldReq=[],
  ooWorldSpawn=[],
  ooUIReq=[]}

-- Event generators:
-- broadcast - recorre, acumula en un solo map y une al final
-- pair
-- scan and shot, evalua todo por evento y luego decide a quien darselos.

--------------------------------------------------------------------------------
-- Object Transformation in OpenGL.
--------------------------------------------------------------------------------

-- | Rotation for a given object.
data Rotation = Euler {
      yaw :: GL.GLdouble,
      picth :: GL.GLdouble,
      roll :: GL.GLdouble
    }
  | Quaternion {
      angle :: GL.GLdouble,
      vector :: GL.Vector3 GL.GLdouble
    } deriving Show

-- | Resolves rotations.
valRotate :: Rotation -> IO ()
valRotate (Quaternion angle vector) = GL.rotate angle vector
valRotate (Euler yaw picth roll) = do
  GL.rotate roll (GL.Vector3 0 0 1 )
  GL.rotate picth (GL.Vector3 1 0 0 )
  GL.rotate yaw (GL.Vector3 0 1 0 )

-- | Transformations for a given object.
data Transform = Transform {
    translation :: GL.Vector3 GL.GLdouble,
    rotation :: Rotation,
    scaleX :: GL.GLdouble,
    scaleY :: GL.GLdouble,
    scaleZ :: GL.GLdouble
  } deriving Show

-- | Sets OpenGL matrix to the given transformation.
useTransform :: Transform -> IO ()
useTransform (Transform translation rotation scaleX scaleY scaleZ) = do
  GL.scale scaleX scaleY scaleZ
  --valRotate rotation
  GL.translate translation
  valRotate rotation
