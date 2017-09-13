{-# LANGUAGE Arrows #-}

module Val.Strict.Util (
  makeSF,
  makeCamSF,
  deltaTime,
  uiState
)
where

import           FRP.Yampa
import           Val.Strict.Data
import           Val.Strict.UI
import EasyGL

deltaTime :: ObjInput b c -> Time
deltaTime = timeGI . oiGameInput

uiState :: ObjInput b c -> UIRead
uiState = uiGI . oiGameInput

makeSF :: a -> (a -> ObjInput b c -> (a,ObjOutput b c)) -> Object b c
makeSF a0 f = proc oi -> do
  rec
    --(a1,oo) <- iPre (f a0 emptyObjInput) <<< arr (uncurry f) -< (a1,oi)
    --(a1,oo) <- iPre (a0,undefined) <<< arr (uncurry f) -< (a1,oi)
    (a2,oo) <- arr (uncurry f) -< (a1,oi)
    a1 <- iPre a0 -< a2
  returnA -< oo

makeCamSF :: IsCamera c => a
  -> (a -> Time -> (GameInput,IL b) -> (a,c))
  -> SF (GameInput,IL b) c
makeCamSF a0 f = proc input -> do
  rec
    t2 <- iPre 0 -< t
    t <- time -< ()
  let delta = t - t2
  rec
    (a1,cam) <- iPre (f a0 0 (emptyGameInput,emptyIL)) <<< arr (\(state,time,oi) -> f state time oi) -< (a1,delta,input)
  returnA -< cam
