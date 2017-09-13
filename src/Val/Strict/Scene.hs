{-# LANGUAGE Arrows #-}
module Val.Strict.Scene (
initScene,
initScenePar
)
where

import           Val.Strict.Data
import           Val.Strict.Scene.Data
import           Val.Strict.UI
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
import           Data.Map.Strict             hiding (foldl, map, foldr, foldl')
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
  -> SF (GameInput,IL (ObjOutput s et),IL (Map EventIdentifier et)) (IL (ObjOutput s et))
  -> SF (GameInput,IL (Map EventIdentifier et)) (IL (ObjOutput s et),c)
feedbackSF camera sf = proc (gi,c) -> do
  rec
    b <- sf -< (gi,b,c)
  cam <- camera -< (gi,mapIL ooObjState b)
  returnA -< (b,cam)


-- | Main loop for OpenGL ones the SF have been initialized.
sceneLoop :: IORef [(ILKey,IOExec et)]
  -> IORef (GLUT ())
  -> IO Double
  -> ReactHandle (GameInput,IL (Map EventIdentifier et)) (IL (ObjOutput s et),c)
  -> GLUT ()
sceneLoop asyncRef uiRef clock rh = do
  keys <- getKeysInfo
  mouse <- getMouseInfo
  ui <- getData
  uiaction <- liftIO $ do
    (e,l) <- readIORef asyncRef >>= events
    writeIORef asyncRef l
    deltaTime <- clock
    b <- react rh (deltaTime,Just (GameInput keys mouse deltaTime ui,e))
    unless b GLUT.exit
    readIORef uiRef
  uiaction
  where
    events = foldM eventAux (emptyIL,[])

-- | process a ioreq.
eventAux :: (IL (Map EventIdentifier et),[(ILKey,IOExec et)])
  -> (ILKey,IOExec et)
  -> IO (IL (Map EventIdentifier et),[(ILKey,IOExec et)])
eventAux (il,l) (key,ioreq) =
  if ioExecBloq ioreq then do
    out <- waitCatch (ioExecIO ioreq)
    return $ either (\e -> (myinsert il key (ioExecEventIdentifier ioreq) (ioExecError ioreq e),l) )
      (\et -> (myinsert il key (ioExecEventIdentifier ioreq) et,l) )
      out
  else do
    mio <- poll (ioExecIO ioreq)
    case mio of
      Nothing -> return (il,(key,ioreq):l)
      Just e -> return $ either (\e -> (myinsert il key (ioExecEventIdentifier ioreq) (ioExecError ioreq e),l) )
        (\et -> (myinsert il key (ioExecEventIdentifier ioreq) et,l) )
        e
  where
    myinsert il k i e = if memberIL k il then modifyIL k (Map.insert i e) il
      else insertILWithKey (Map.singleton i e) k il

sceneRender :: IsCamera c => ResourceMap
  -> IORef [(ILKey,IOExec et)]
  -> IORef (GLUT ())
  -> ReactHandle (GameInput,IL (Map EventIdentifier et)) (IL (ObjOutput s et),c)
  -> Bool
  -> (IL (ObjOutput s et),c)
  -> IO Bool
sceneRender rm asyn uiRef rh b (out,cam) = do
  otherIO <- async $ mapConcurrently_ id [io1,io2]
  ioOpenGL -- this thread have the OpenGL context, cant sent it away.
  wait otherIO
  return b
  where
    assocs = assocsIL out
    renderComponents = foldl' (\l (_,oo) -> maybe l (:l) $ ooRenderer oo) [] assocs
    newAsyn ini = foldM newAsynAux ini assocs
    newAsynAux l (key,a) = do
      mapM_ async $ ooWorldSpawn a
      let ioReqs = ooWorldReq a
      execs <- mapM toExec ioReqs
      let ll = zip (repeat key) execs
      return $ ll++l
    io1 = readIORef asyn >>= newAsyn >>= writeIORef asyn
    io2 = writeIORef uiRef $ foldl'
      (\io oo -> foldl' (flip ((>>) . execUIActions)) (return ()) (ooUIReq oo) >> io)
      (return ())
      out
    ioOpenGL = useCamera cam >> render rm renderComponents

--------------------------------------------------------------------------------
-- Game single scene.
--------------------------------------------------------------------------------

-- | Generates the input for each object based on world input and provided event generators.
route :: [ IL s -> IL (Map EventIdentifier et) ] -- event generators
  -> (GameInput, IL (ObjOutput s et), IL (Map EventIdentifier et)) -- input from world and oo from last iteration
  -> IL sf -- objects to route
  -> IL (ObjInput s et,sf)
route eventGenerators (gi,ooil,worldEvents) = mapILWithKey aux
  where
    defaultObjInput = ObjInput Map.empty gi
    states = mapIL ooObjState ooil
    events = map (\f -> f states) eventGenerators
    aux key o = (defaultObjInput{oiEvents= unions e },o)
      where
        worldEvent = lookupIL key worldEvents
        thisEvent = map (lookupIL key) events
        e = catMaybes $ worldEvent:thisEvent

-- | A val scene.
sceneSF :: [ IL s -> IL (Map EventIdentifier et) ]
  -> IL (Object s et)
  -> SF (GameInput, IL (ObjOutput s et), IL (Map EventIdentifier et)) (IL (ObjOutput s et))
sceneSF eventGenerators objs = dpSwitch
  (route eventGenerators)
  objs
  (arr killOrSpawn >>> notYet)
  (\objects f -> sceneSF eventGenerators (f objects))

initScene :: IsCamera c => SF (GameInput,IL s) c
  -> IO ResourceMap
  -> [ IL s -> IL (Map EventIdentifier et) ]
  -> IL (Object s et)
  -> IO ()
initScene camSF resources eventGenerators objs = do
  initOpenGLEnvironment 800 600 ""
  rm <- resources
  ref <- newIORef []
  uiref <- newIORef $ return ()
  rh <- reactInit
    (return (emptyGameInput,emptyIL))
    (sceneRender rm ref uiref)
    (feedbackSF camSF (sceneSF eventGenerators objs))
  clock <- initClock >>= newIORef

  performGC
  initGL $ sceneLoop ref uiref (getDelta clock) rh

--------------------------------------------------------------------------------
-- Game single scene, run using parallel.
--------------------------------------------------------------------------------

-- | Same as route but with parallel computations.
routePar :: (NFData et) => [ IL s -> IL (Map EventIdentifier et) ]
  -> (GameInput, IL (ObjOutput s et), IL (Map EventIdentifier et))
  -> IL sf
  -> IL (ObjInput s et,sf)
routePar eventGenerators (gi,ooil,worldEvents) objs =
  withStrategy (parTraversable (evalTuple2 rpar r0)) $ mapILWithKey aux objs
  where
    defaultObjInput = ObjInput Map.empty gi
    states = mapIL ooObjState ooil
    -- events = withStrategy (parTraversable rpar) $ map (\f -> f states) eventGenerators
    events = map (\f -> f states) eventGenerators
    aux key o = (defaultObjInput{oiEvents= unions e },o)
      where
        worldEvent = lookupIL key worldEvents
        thisEvent = map (lookupIL key) events
        e = catMaybes $ worldEvent:thisEvent

-- | Same as sceneSF but with parallel computations.
sceneSFPar :: (NFData et) => [ IL s -> IL (Map EventIdentifier et) ]
  -> IL (Object s et)
  -> SF (GameInput, IL (ObjOutput s et), IL (Map EventIdentifier et)) (IL (ObjOutput s et))
sceneSFPar eventGenerators objs = dpSwitch
  (routePar eventGenerators)
  objs (arr killOrSpawn >>> notYet)
  (\objects f -> sceneSFPar eventGenerators (f objects))

initScenePar :: (NFData et,IsCamera c) => SF (GameInput,IL s) c
  -> IO ResourceMap
  -> [ IL s -> IL (Map EventIdentifier et) ]
  -> IL (Object s et)
  -> IO ()
initScenePar camSF resources eventGenerators objs = do
  initOpenGLEnvironment 800 600 ""
  rm <- resources
  ref <- newIORef []
  uiref <- newIORef $ return ()
  rh <- reactInit
    (return (emptyGameInput,emptyIL))
    (sceneRender rm ref uiref)
    (feedbackSF camSF (sceneSFPar eventGenerators objs))
  clock <- initClock >>= newIORef

  performGC
  initGL $ sceneLoop ref uiref (getDelta clock) rh
