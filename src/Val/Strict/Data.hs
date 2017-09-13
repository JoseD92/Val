module Val.Strict.Data (
  ResourceIdentifier,
  Resource,
  ResourceMap,

  EventIdentifier,
  GameInput(..),
  Object,
  ObjInput(..),
  IOReq(..),
  IOExec(..),
  ObjOutput(..),
  ILKey,
  IL(..),

  emptyGameInput,
  toExec,
  emptyObjInput,
  newObjOutput,
  emptyIL,
  lookupIL,
  insertIL,
  insertILWithKey,
  fromList,
  elemsIL,
  assocsIL,
  deleteIL,
  mapIL,
  mapILKeys,
  mapILWithKey,
  modifyIL,
  memberIL,

  -- From Transform
  Rotation(..),
  valRotate,
  Transform(..),
  useTransform
)
where

import qualified Graphics.Rendering.OpenGL as GL
import FRP.Yampa
import Data.Map.Strict hiding (fromList)
import qualified Data.Map.Strict as Map
import EasyGL
import EasyGLUT
import qualified EasyGLUT as GLUT
import Control.DeepSeq
import Control.Seq
import Control.Exception
import Control.Concurrent.Async
import Val.Strict.UI

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

type EventIdentifier = String

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
  oiEvents :: Map EventIdentifier eventType,
  oiGameInput :: GameInput
}

instance (NFData eventType) => NFData (ObjInput state eventType) where
  rnf ObjInput{oiEvents=events} = rnf events -- the game input from the world will normally be normal form because it comes from IO.

data IOReq a = IOReq {
    ioReqEventIdentifier :: EventIdentifier,
    ioReqIO :: IO a,
    ioReqError :: SomeException -> a,
    ioBloq :: Bool -- whether to wait or not the io. If True the result will be deliver next frame, otherwise, when result is available.
  }

data IOExec a = IOExec {
    ioExecEventIdentifier :: EventIdentifier,
    ioExecIO :: Async a,
    ioExecError :: SomeException -> a,
    ioExecBloq :: Bool -- whether to wait or not the io. If True the result will be deliver next frame, otherwise, when result is available.
  }

toExec :: IOReq a -> IO (IOExec a)
toExec req = do
  a <- async $ ioReqIO req
  return IOExec{
      ioExecEventIdentifier=ioReqEventIdentifier req,
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
emptyObjInput = ObjInput empty emptyGameInput

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
-- Container for objects.
--------------------------------------------------------------------------------

type ILKey = Integer
-- | Identity List, inspired by Yampa Arcade paper, contains objects that can be identifiable.
data IL a = IL {
    ilNext :: ILKey,
    ilAssocs :: Map ILKey a
  }

emptyIL :: IL a
emptyIL = IL 0 Map.empty

lookupIL :: ILKey -> IL a -> Maybe a
lookupIL key = Map.lookup key . ilAssocs

insertIL :: a -> IL a -> IL a
insertIL a il = IL (ilNext il + 1) (Map.insert (ilNext il) a (ilAssocs il))

insertILWithKey :: a -> ILKey -> IL a -> IL a
insertILWithKey a k il@IL{ilAssocs=m} = il{ilAssocs=Map.insert k a m}

fromList :: [a] -> IL a
fromList l = IL (fromIntegral $ length l) (Map.fromList $ zip [0..] l)

elemsIL :: IL a -> [a]
elemsIL = Map.elems . ilAssocs

assocsIL :: IL a -> [(ILKey,a)]
assocsIL = Map.assocs . ilAssocs

deleteIL :: ILKey -> IL a -> IL a
deleteIL k il = il{ilAssocs=Map.delete k $ ilAssocs il}

mapIL :: (a->b) -> IL a -> IL b
mapIL f il = il{ilAssocs=Map.map f $ ilAssocs il}

mapILKeys :: (ILKey->b) -> IL a -> IL b
mapILKeys f il = il{ilAssocs=Map.mapWithKey (\k _ -> f k) $ ilAssocs il}

mapILWithKey :: (ILKey -> a -> b) -> IL a -> IL b
mapILWithKey f il = il{ilAssocs=Map.mapWithKey f $ ilAssocs il}

-- | Modifies a single element with given function if it exist.
modifyIL :: ILKey -> (a -> a) -> IL a -> IL a
modifyIL key f il@IL{ilAssocs=m} = maybe il (\a -> il{ilAssocs=Map.insert key (f a) m} ) $ Map.lookup key m

memberIL :: ILKey -> IL a -> Bool
memberIL key IL{ilAssocs=m} = Map.member key m

instance Functor IL where
  fmap = mapIL

instance Foldable IL where
  foldMap f il = foldMap f (ilAssocs il)
  foldr f b il = Map.foldr f b (ilAssocs il)

instance Traversable IL where
  traverse f il = IL (ilNext il) <$> traverse f (ilAssocs il)

instance (NFData a) => NFData (IL a) where
  rnf (IL n assoc) = rnf n `seq` rnf assoc

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
