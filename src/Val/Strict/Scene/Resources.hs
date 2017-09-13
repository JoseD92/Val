module Val.Strict.Scene.Resources (
loadResouces,
render
) where

import EasyGL

import Control.Concurrent.Async
import Data.Map.Strict
import qualified Data.Map.Strict as Map
import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.ByteString.Lazy   as BS
import Val.Strict.Data

loadResouce :: (ResourceIdentifier,[IndexedModel],Material) -> IO (ResourceIdentifier,(Entity,Material))
loadResouce (identifier,im,mat) = do
  ent <- indexedModel2Ent im
  return (identifier,(ent,mat))

loadResouce1 :: Resource -> IO (ResourceIdentifier,[IndexedModel],Material)
loadResouce1 (identifier,path,mat) = do
  im <- (toIndexedModel . readObj) <$> BS.readFile path
  return (identifier,im,mat)

loadResouces :: [Resource] -> IO ResourceMap
loadResouces resouces = do
  resouces0 <- mapConcurrently loadResouce1 resouces
  loadedResources <- mapM loadResouce resouces0
  return $ Map.fromList loadedResources

render :: ResourceMap -> [(ResourceIdentifier,Transform,Uniform ())] -> IO ()
render resouces = mapM_ aux
  where
    aux (identifier,transform,uni) = maybe
      (putStrLn $ "No resouce: "++identifier)
      (\(ent,mat) -> GL.preservingMatrix (useTransform transform >> drawWithMat mat ent uni))
      (Map.lookup identifier resouces)
