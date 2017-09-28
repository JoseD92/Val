module Val.Strict (
  -- From UI
  UIRead(..),
  UIActions(..),

  -- From Scene
  initScene,
  initScenePar,

  -- From Util
  makeSF,
  makeCamSF,
  deltaTime,
  uiState,

  -- From Events
  keyPress,
  keyDown,
  keyUp,
  keyReleased,
  mouseMoved,
  mouseMovedX,
  mouseMovedY,
  mousePosition,
  mousePositionMoved,
  getObjOut,
  getObjects,
  inputEvent,

  -- From Data
  ResourceIdentifier,
  Resource,
  ResourceMap,

  MergeableEvent(..),
  GameInput(keysGI,mouseGI),
  Object,
  ObjInput,
  IOReq(..),
  ObjOutput(..),
  ILKey,
  IL,

  emptyGameInput,
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

  Rotation(..),
  Transform(..),

  -- From Resources.
  loadResouces
)
where

import Val.Strict.Scene
import Val.Strict.Util
import Val.Strict.Events
import Val.Strict.Data
import Val.Strict.UI
import Val.Strict.Scene.Resources
