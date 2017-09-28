{-# LANGUAGE Arrows #-}
module Val.Strict.Scene (
initScene,
initScenePar
)
where

import           Val.Strict.Data
import qualified Val.Strict.IL as IL
import           Val.Strict.UI hiding (FreeMouse)
import           Val.Strict.Scene.Resources

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Parallel.Strategies
import           Data.IORef                  (IORef, newIORef, readIORef,
                                              writeIORef)
import           Data.Map.Strict             hiding (foldl, map, foldr, foldl', union, unions, null)
import qualified Data.Map.Strict             as Map
import Data.Foldable (foldl')
import           Data.Maybe
import           DeltaClock
import           EasyGL
import           EasyGLUT
import           FRP.Yampa
import qualified Graphics.Rendering.OpenGL   as GL
import qualified Graphics.UI.GLUT            as GLUT
import           System.Exit
import           System.Mem

-- | Reads outputs values of objects to generate a killOrSpawn event.
killOrSpawn :: (a,IL (ObjOutput s et)) -> Event (IL (Object s et) -> IL (Object s et))
killOrSpawn (_,oo) = foldl (mergeBy (.)) noEvent events
  where
    events = map toEvent $ assocsIL oo
    toEvent (k,v) = mergeBy (.) kill spawns
      where
        spawns = fmap (foldl (.) id . map insertIL) $ ooSpawnReq v
        kill = tag (ooKillReq v) (deleteIL k)

feedbackSF :: IsCamera c => SF (GameInput,IL s) c
  -> SF (GameInput,IL (ObjOutput s et),IL (Event et)) (IL (ObjOutput s et))
  -> SF (GameInput,IL (Event et)) (IL (ObjOutput s et),c)
feedbackSF camera sf = proc (gi,c) -> do
  rec
    b <- sf -< (gi,b,c)
  cam <- camera -< (gi,mapIL ooObjState b)
  returnA -< (b,cam)

unions :: (MergeableEvent a) => [Event a] -> Event a
unions = foldl' (mergeBy union) noEvent

--------------------------------------------------------------------------------
-- Game single scene.
--------------------------------------------------------------------------------

-- | Generates the input for each object based on world input and provided event generators.
route :: MergeableEvent et => [ IL s -> IL (Event et) ] -- event generators
  -> (GameInput, IL (ObjOutput s et), IL (Event et)) -- input from world and oo from last iteration
  -> IL sf -- objects to route
  -> IL (ObjInput s et,sf)
route eventGenerators (gi,ooil,worldEvents) objs = mapILWithKey aux objs
  where
    defaultObjInput = ObjInput noEvent gi states
    states = mapIL ooObjState ooil
    events = map (\f -> f states) eventGenerators
    aux key o = (defaultObjInput{oiEvents= unions e },o)
      where
        worldEvent = lookupIL key worldEvents
        thisEvent = map (lookupIL key) events
        e = catMaybes $ worldEvent:thisEvent

-- | Generates the input for each object based on world input and provided event generators.
routePar :: MergeableEvent et => [ IL s -> IL (Event et) ] -- event generators
  -> (GameInput, IL (ObjOutput s et), IL (Event et)) -- input from world and oo from last iteration
  -> IL sf -- objects to route
  -> IL (ObjInput s et,sf)
routePar eventGenerators (gi,ooil,worldEvents) objs =
  withStrategy (parTraversable (evalTuple2 rpar r0)) $ mapILWithKey aux objs
  where
    defaultObjInput = ObjInput noEvent gi states
    states = mapIL ooObjState ooil
    events = map (\f -> f states) eventGenerators
    aux key o = (defaultObjInput{oiEvents= unions e },o)
      where
        worldEvent = lookupIL key worldEvents
        thisEvent = map (lookupIL key) events
        e = catMaybes $ worldEvent:thisEvent

type SceneSF s et = [ IL s -> IL (Event et) ]
  -> IL (Object s et)
  -> SF (GameInput, IL (ObjOutput s et), IL (Event et)) (IL (ObjOutput s et))

-- | A val scene.
sceneSF :: MergeableEvent et => SceneSF s et
sceneSF eventGenerators objs = dpSwitch
  (route eventGenerators)
  objs
  (arr killOrSpawn >>> notYet)
  (\objects f -> sceneSF eventGenerators (f objects))

sceneSFPar :: MergeableEvent et => SceneSF s et
sceneSFPar eventGenerators objs = dpSwitch
  (routePar eventGenerators)
  objs
  (arr killOrSpawn >>> notYet)
  (\objects f -> sceneSFPar eventGenerators (f objects))

initScene :: (IsCamera c,MergeableEvent et) => SF (GameInput,IL s) c
  -> IO ResourceMap
  -> [ IL s -> IL (Event et) ]
  -> [Object s et]
  -> IO ()
initScene = initSceneAux sceneSF

initScenePar :: (IsCamera c,MergeableEvent et) => SF (GameInput,IL s) c
  -> IO ResourceMap
  -> [ IL s -> IL (Event et) ]
  -> [Object s et]
  -> IO ()
initScenePar = initSceneAux sceneSFPar

initSceneAux :: (IsCamera c,MergeableEvent et) => SceneSF s et
  -> SF (GameInput,IL s) c
  -> IO ResourceMap
  -> [ IL s -> IL (Event et) ]
  -> [Object s et]
  -> IO ()
initSceneAux sf camSF resources eventGenerators objsList = do
  let objs = IL.fromList objsList

  glfeedMVar <- newEmptyMVar
  glinputMVar <- newMVar $ GameInput Map.empty (FreeMouse 0 0) 0 (UIRead (800,600) (0,0))
  ioreqfeedMVar <- newEmptyMVar
  ioreqResponseMVar <- newEmptyMVar

  waitLoad <- newEmptyMVar -- Force Yampa to wait until resources have been loaded.

  _ <- forkIO $ ioReqThread ioreqfeedMVar ioreqResponseMVar []
  _ <- forkIO $ do
    rh <- reactInit
      (return (emptyGameInput,emptyIL))
      (reacFun [glfeedMVar,ioreqfeedMVar])
      (feedbackSF camSF (sf eventGenerators objs))

    _ <- takeMVar waitLoad
    clock <- initClock >>= newIORef
    forever $ do
      gameInput <- takeMVar glinputMVar
      events <- takeMVar ioreqResponseMVar
      time <- getDelta clock
      react rh (time,Just (gameInput{timeGI=time},events))

  glThread waitLoad glfeedMVar glinputMVar resources
  where
    reacFun l _ bool out = do
      mapConcurrently_ (`putMVar` out) l
      return bool


--------------------------------------------------------------------------------
-- Threads.
--------------------------------------------------------------------------------

-- | Thread that run io request from the objects output.
ioReqThread :: MergeableEvent et => MVar (IL (ObjOutput s et),c)
  -> MVar (IL (Event et))
  -> [(ILKey,IOExec et)]
  -> IO ()
ioReqThread inMvar outMvar l = do
  (e,l2) <- foldM eventAux (emptyIL,[]) l
  putMVar outMvar e
  (out,_) <- takeMVar inMvar
  l3 <- foldM newAsynAux l2 $ assocsIL out
  ioReqThread inMvar outMvar l3
  where
    newAsynAux l (key,a) = do
      mapM_ async $ ooWorldSpawn a
      let ioReqs = ooWorldReq a
      execs <- mapM toExec ioReqs
      let ll = zip (repeat key) execs
      return $ ll++l

-- | process a ioreq.
eventAux :: MergeableEvent et => (IL (Event et),[(ILKey,IOExec et)])
  -> (ILKey,IOExec et)
  -> IO (IL (Event et),[(ILKey,IOExec et)])
eventAux (il,l) (key,ioreq) =
  if ioExecBloq ioreq then do
    out <- waitCatch (ioExecIO ioreq)
    return $ either (\e -> (myinsert il key (ioExecError ioreq e),l) )
      (\et -> (myinsert il key et,l) )
      out
  else do
    mio <- poll (ioExecIO ioreq)
    case mio of
      Nothing -> return (il,(key,ioreq):l)
      Just e -> return $ either (\e -> (myinsert il key (ioExecError ioreq e),l) )
        (\et -> (myinsert il key et,l) )
        e
  where
    myinsert il k e = if memberIL k il then modifyIL k (mergeBy union $ Event e) il
      else insertILWithKey (Event e) k il

-- | A thread that render the output of each tick.
glThread :: (IsCamera c) => MVar Bool
  -> MVar (IL (ObjOutput s et),c)
  -> MVar GameInput
  -> IO ResourceMap
  -> IO ()
glThread waitLoad mvar gimvar resources = do
  initOpenGLEnvironment 800 600 ""
  rm <- resources
  performGC
  putMVar waitLoad True
  initGL $ aux rm
  where
    aux rm = do
      keys <- getKeysInfo
      mouse <- getMouseInfo
      ui <- getData
      (out,cam) <- liftIO $ takeMVar mvar
      liftIO $ putMVar gimvar $ GameInput keys mouse undefined ui
      mapM_ (mapM_ execUIActions . ooUIReq) out
      let assocs = assocsIL out
          renderComponents = foldl' (\l (_,oo) -> maybe l (:l) $ ooRenderer oo) [] assocs
      useCamera cam
      liftIO $ render rm renderComponents
