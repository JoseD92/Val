module Val.Strict.IL (
  ILKey,
  IL(..),
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
  memberIL
) where

import Data.Map.Strict hiding (fromList)
import qualified Data.Map.Strict as Map
import Control.DeepSeq
import Control.Seq

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
