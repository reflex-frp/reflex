{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
-- | 'Data.Map' with a better 'Monoid' instance
--
-- 'Data.Map' has @mappend = union@, which is left-biased.  AppendMap has
-- @mappend = unionWith mappend@ instead.
module Data.AppendMap where

import Prelude hiding (map)

import Control.Lens
import Data.Align
import Data.Coerce
import Data.Default
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup
import Data.Set (Set)
import Data.Typeable
import GHC.Generics (Generic)
import Reflex (FunctorMaybe (..))
import Reflex.Patch (Additive, Group (..))

newtype AppendMap k m = AppendMap { _unAppendMap :: Map k m }
  deriving (Eq, Ord, Show, Read, Typeable, Generic, Functor, Foldable, Traversable, Align)

type instance (Index (AppendMap k m)) = k
type instance (IxValue (AppendMap k m)) = m

instance Ord k => Ixed (AppendMap k m) where
  ix i = _Wrapped . ix i

instance Ord k => At (AppendMap k m) where
  at i = _Wrapped . at i

instance FunctorMaybe (AppendMap k) where
  fmapMaybe = mapMaybe

-- | Deletes a key, returning 'Nothing' if the result is empty.
nonEmptyDelete :: Ord k => k -> AppendMap k a -> Maybe (AppendMap k a)
nonEmptyDelete k vs =
  let deleted = delete k vs
  in if Data.AppendMap.null deleted
       then Nothing
       else Just deleted

mapMaybeNoNull :: (a -> Maybe b)
               -> AppendMap token a
               -> Maybe (AppendMap token b)
mapMaybeNoNull f as =
  let bs = fmapMaybe f as
  in if Data.AppendMap.null bs
       then Nothing
       else Just bs

instance FunctorWithIndex k (AppendMap k) where
  imap f = AppendMap . imap f . _unAppendMap
  imapped = _Wrapped . imapped

instance FoldableWithIndex k (AppendMap k) where
  ifolded = _Wrapped . ifolded

instance TraversableWithIndex k (AppendMap k) where
  itraverse = itraverseOf itraversed
  itraversed = _Wrapped . itraversed

instance (Ord k, Semigroup m) => Semigroup (AppendMap k m) where
  (AppendMap m0) <> (AppendMap m1) = AppendMap $ Map.unionWith (<>) m0 m1

instance (Ord k, Semigroup m) => Monoid (AppendMap k m) where
  mempty = empty
  mappend = (<>)

instance (Ord k, Group q) => Group (AppendMap k q) where
  negateG = map negateG

instance (Ord k, Additive q) => Additive (AppendMap k q)

(!) :: forall k a. Ord k => AppendMap k a -> k -> a
(!) = coerce ((Map.!) @k @a)
infixl 9 !

(\\) :: forall k a b. Ord k => AppendMap k a -> AppendMap k b -> AppendMap k a
(\\) = coerce ((Map.\\) @k @a @b)
infixl 9 \\

null :: forall k a. AppendMap k a -> Bool
null = coerce (Map.null @k @a)

size :: forall k a. AppendMap k a -> Int
size = coerce (Map.size @k @a)

member :: forall k a. Ord k => k -> AppendMap k a -> Bool
member = coerce (Map.member @k @a)

notMember :: forall k a. Ord k => k -> AppendMap k a -> Bool
notMember = coerce (Map.notMember @k @a)

lookup :: forall k a. Ord k => k -> AppendMap k a -> Maybe a
lookup = coerce (Map.lookup @k @a)

findWithDefault :: forall k a. Ord k => a -> k -> AppendMap k a -> a
findWithDefault = coerce (Map.findWithDefault @k @a)

lookupLT :: forall k a. Ord k => k -> AppendMap k a -> Maybe (k, a)
lookupLT = coerce (Map.lookupLT @k @a)

lookupGT :: forall k a. Ord k => k -> AppendMap k a -> Maybe (k, a)
lookupGT = coerce (Map.lookupGT @k @a)

lookupLE :: forall k a. Ord k => k -> AppendMap k a -> Maybe (k, a)
lookupLE = coerce (Map.lookupLE @k @a)

lookupGE :: forall k a. Ord k => k -> AppendMap k a -> Maybe (k, a)
lookupGE = coerce (Map.lookupGE @k @a)

empty :: forall k a. AppendMap k a
empty = coerce (Map.empty @k @a)

singleton :: forall k a. k -> a -> AppendMap k a
singleton = coerce (Map.singleton @k @a)

insert :: forall k a. Ord k => k -> a -> AppendMap k a -> AppendMap k a
insert = coerce (Map.insert @k @a)

insertWith :: Ord k => (a -> a -> a) -> k -> a -> AppendMap k a -> AppendMap k a
insertWith f = coerce (Map.insertWith f)

insertWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> AppendMap k a -> AppendMap k a
insertWithKey f = coerce (Map.insertWithKey f)

insertLookupWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> AppendMap k a -> (Maybe a, AppendMap k a)
insertLookupWithKey f = coerce (Map.insertLookupWithKey f)

delete :: forall k a. Ord k => k -> AppendMap k a -> AppendMap k a
delete = coerce (Map.delete @k @a)

adjust :: Ord k => (a -> a) -> k -> AppendMap k a -> AppendMap k a
adjust f = coerce (Map.adjust f)

adjustWithKey :: Ord k => (k -> a -> a) -> k -> AppendMap k a -> AppendMap k a
adjustWithKey f = coerce (Map.adjustWithKey f)

update :: Ord k => (a -> Maybe a) -> k -> AppendMap k a -> AppendMap k a
update f = coerce (Map.update f)

updateWithKey :: Ord k => (k -> a -> Maybe a) -> k -> AppendMap k a -> AppendMap k a
updateWithKey f = coerce (Map.updateWithKey f)

updateLookupWithKey :: Ord k => (k -> a -> Maybe a) -> k -> AppendMap k a -> (Maybe a, AppendMap k a)
updateLookupWithKey f = coerce (Map.updateLookupWithKey f)

alter :: Ord k => (Maybe a -> Maybe a) -> k -> AppendMap k a -> AppendMap k a
alter f = coerce (Map.alter f)

unionWith :: Ord k => (a -> a -> a) -> AppendMap k a -> AppendMap k a -> AppendMap k a
unionWith f = coerce (Map.unionWith f)

unionWithKey :: Ord k => (k -> a -> a -> a) -> AppendMap k a -> AppendMap k a -> AppendMap k a
unionWithKey f = coerce (Map.unionWithKey f)

unionsWith :: Ord k => (a -> a -> a) -> [AppendMap k a] -> AppendMap k a
unionsWith f = coerce (Map.unionsWith f)

difference :: Ord k => AppendMap k a -> AppendMap k b -> AppendMap k a
difference = (\\)

differenceWith :: Ord k => (a -> b -> Maybe a) -> AppendMap k a -> AppendMap k b -> AppendMap k a
differenceWith f = coerce (Map.differenceWith f)

differenceWithKey :: Ord k => (k -> a -> b -> Maybe a) -> AppendMap k a -> AppendMap k b -> AppendMap k a
differenceWithKey f = coerce (Map.differenceWithKey f)

intersectionWith :: Ord k => (a -> b -> c) -> AppendMap k a -> AppendMap k b -> AppendMap k c
intersectionWith f = coerce (Map.intersectionWith f)

intersectionWithKey :: Ord k => (k -> a -> b -> c) -> AppendMap k a -> AppendMap k b -> AppendMap k c
intersectionWithKey f = coerce (Map.intersectionWithKey f)

mergeWithKey :: Ord k => (k -> a -> b -> Maybe c) -> (AppendMap k a -> AppendMap k c) -> (AppendMap k b -> AppendMap k c) -> AppendMap k a -> AppendMap k b -> AppendMap k c
mergeWithKey f = coerce (Map.mergeWithKey f)

map :: (a -> b) -> AppendMap k a -> AppendMap k b
map = fmap

mapWithKey :: (k -> a -> b) -> AppendMap k a -> AppendMap k b
mapWithKey = imap

traverseWithKey :: Applicative t => (k -> a -> t b) -> AppendMap k a -> t (AppendMap k b)
traverseWithKey = itraverse

mapAccum :: (a -> b -> (a, c)) -> a -> AppendMap k b -> (a, AppendMap k c)
mapAccum f = coerce (Map.mapAccum f)

mapAccumWithKey :: (a -> k -> b -> (a, c)) -> a -> AppendMap k b -> (a, AppendMap k c)
mapAccumWithKey f = coerce (Map.mapAccumWithKey f)

mapAccumRWithKey :: (a -> k -> b -> (a, c)) -> a -> AppendMap k b -> (a, AppendMap k c)
mapAccumRWithKey f = coerce (Map.mapAccumRWithKey f)

mapKeys :: forall k1 k2 a. Ord k2 => (k1 -> k2) -> AppendMap k1 a -> AppendMap k2 a
mapKeys = coerce (Map.mapKeys @k2 @k1 @a)

mapKeysWith :: Ord k2 => (a -> a -> a) -> (k1 -> k2) -> AppendMap k1 a -> AppendMap k2 a
mapKeysWith f = coerce (Map.mapKeysWith f)

mapKeysMonotonic :: forall k1 k2 a. (k1 -> k2) -> AppendMap k1 a -> AppendMap k2 a
mapKeysMonotonic = coerce (Map.mapKeysMonotonic @k1 @k2 @a)

foldr :: (a -> b -> b) -> b -> AppendMap k a -> b
foldr f = coerce (Map.foldr f)

foldl :: (a -> b -> a) -> a -> AppendMap k b -> a
foldl f = coerce (Map.foldl f)

foldrWithKey :: (k -> a -> b -> b) -> b -> AppendMap k a -> b
foldrWithKey f = coerce (Map.foldrWithKey f)

foldlWithKey :: (a -> k -> b -> a) -> a -> AppendMap k b -> a
foldlWithKey f = coerce (Map.foldlWithKey f)

foldMapWithKey :: Monoid m => (k -> a -> m) -> AppendMap k a -> m
foldMapWithKey f = coerce (Map.foldMapWithKey f)

foldr' :: (a -> b -> b) -> b -> AppendMap k a -> b
foldr' f = coerce (Map.foldr' f)

foldl' :: (a -> b -> a) -> a -> AppendMap k b -> a
foldl' f = coerce (Map.foldl' f)

foldrWithKey' :: (k -> a -> b -> b) -> b -> AppendMap k a -> b
foldrWithKey' f = coerce (Map.foldrWithKey' f)

foldlWithKey' :: (a -> k -> b -> a) -> a -> AppendMap k b -> a
foldlWithKey' f = coerce (Map.foldlWithKey' f)

elems :: forall k a. AppendMap k a -> [a]
elems = coerce (Map.elems @k @a)

keys :: forall k a. AppendMap k a -> [k]
keys = coerce (Map.keys @k @a)

assocs :: forall k a. AppendMap k a -> [(k, a)]
assocs = coerce (Map.assocs @k @a)

keysSet :: forall k a. AppendMap k a -> Set k
keysSet = coerce (Map.keysSet @k @a)

fromSet :: (k -> a) -> Set k -> AppendMap k a
fromSet f = coerce (Map.fromSet f)

toList :: forall k a. AppendMap k a -> [(k, a)]
toList = coerce (Map.toList @k @a)

fromList :: forall k a. Ord k => [(k, a)] -> AppendMap k a
fromList = coerce (Map.fromList @k @a)

fromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> AppendMap k a
fromListWith f = coerce (Map.fromListWith f)

fromListWithKey :: Ord k => (k -> a -> a -> a) -> [(k, a)] -> AppendMap k a
fromListWithKey f = coerce (Map.fromListWithKey f)

toAscList :: forall k a. AppendMap k a -> [(k, a)]
toAscList = coerce (Map.toAscList @k @a)

toDescList :: forall k a. AppendMap k a -> [(k, a)]
toDescList = coerce (Map.toDescList @k @a)

fromAscList :: forall k a. Eq k => [(k, a)] -> AppendMap k a
fromAscList = coerce (Map.fromAscList @k @a)

fromAscListWith :: Eq k => (a -> a -> a) -> [(k, a)] -> AppendMap k a
fromAscListWith f = coerce (Map.fromAscListWith f)

fromAscListWithKey :: Eq k => (k -> a -> a -> a) -> [(k, a)] -> AppendMap k a
fromAscListWithKey f = coerce (Map.fromAscListWithKey f)

fromDistinctAscList :: forall k a. [(k, a)] -> AppendMap k a
fromDistinctAscList = coerce (Map.fromDistinctAscList @k @a)

filter :: (a -> Bool) -> AppendMap k a -> AppendMap k a
filter f = coerce (Map.filter f)

filterWithKey :: (k -> a -> Bool) -> AppendMap k a -> AppendMap k a
filterWithKey f = coerce (Map.filterWithKey f)

partition :: (a -> Bool) -> AppendMap k a -> (AppendMap k a, AppendMap k a)
partition f = coerce (Map.partition f)

partitionWithKey :: (k -> a -> Bool) -> AppendMap k a -> (AppendMap k a, AppendMap k a)
partitionWithKey f = coerce (Map.partitionWithKey f)

mapMaybe :: (a -> Maybe b) -> AppendMap k a -> AppendMap k b
mapMaybe f = coerce (Map.mapMaybe f)

mapMaybeWithKey :: (k -> a -> Maybe b) -> AppendMap k a -> AppendMap k b
mapMaybeWithKey f = coerce (Map.mapMaybeWithKey f)

mapEither :: (a -> Either b c) -> AppendMap k a -> (AppendMap k b, AppendMap k c)
mapEither f = coerce (Map.mapEither f)

mapEitherWithKey :: (k -> a -> Either b c) -> AppendMap k a -> (AppendMap k b, AppendMap k c)
mapEitherWithKey f = coerce (Map.mapEitherWithKey f)

split :: forall k a. Ord k => k -> AppendMap k a -> (AppendMap k a, AppendMap k a)
split = coerce (Map.split @k @a)

splitLookup :: forall k a. Ord k => k -> AppendMap k a -> (AppendMap k a, Maybe a, AppendMap k a)
splitLookup = coerce (Map.splitLookup @k @a)

splitRoot :: forall k a. AppendMap k a -> [AppendMap k a]
splitRoot = coerce (Map.splitRoot @k @a)

isSubmapOf :: forall k a. (Ord k, Eq a) => AppendMap k a -> AppendMap k a -> Bool
isSubmapOf = coerce (Map.isSubmapOf @k @a)

isSubmapOfBy :: Ord k => (a -> b -> Bool) -> AppendMap k a -> AppendMap k b -> Bool
isSubmapOfBy f = coerce (Map.isSubmapOfBy f)

isProperSubmapOf :: forall k a. (Ord k, Eq a) => AppendMap k a -> AppendMap k a -> Bool
isProperSubmapOf = coerce (Map.isProperSubmapOf @k @a)

isProperSubmapOfBy :: Ord k => (a -> b -> Bool) -> AppendMap k a -> AppendMap k b -> Bool
isProperSubmapOfBy f = coerce (Map.isProperSubmapOfBy f)

lookupIndex :: forall k a. Ord k => k -> AppendMap k a -> Maybe Int
lookupIndex = coerce (Map.lookupIndex @k @a)

findIndex :: forall k a. Ord k => k -> AppendMap k a -> Int
findIndex = coerce (Map.findIndex @k @a)

elemAt :: forall k a. Int -> AppendMap k a -> (k, a)
elemAt = coerce (Map.elemAt @k @a)

updateAt :: forall k a. (k -> a -> Maybe a) -> Int -> AppendMap k a -> AppendMap k a
updateAt = coerce (Map.updateAt @k @a)

deleteAt :: forall k a. Int -> AppendMap k a -> AppendMap k a
deleteAt = coerce (Map.deleteAt @k @a)

findMin :: forall k a. AppendMap k a -> (k, a)
findMin = coerce (Map.findMin @k @a)

findMax :: forall k a. AppendMap k a -> (k, a)
findMax = coerce (Map.findMax @k @a)

deleteMin :: forall k a. AppendMap k a -> AppendMap k a
deleteMin = coerce (Map.deleteMin @k @ a)

deleteMax :: forall k a. AppendMap k a -> AppendMap k a
deleteMax = coerce (Map.deleteMax @k @a)

deleteFindMin :: forall k a. AppendMap k a -> ((k, a), AppendMap k a)
deleteFindMin = coerce (Map.deleteFindMin @k @a)

deleteFindMax :: forall k a. AppendMap k a -> ((k, a), AppendMap k a)
deleteFindMax = coerce (Map.deleteFindMax @k @a)

updateMin :: (a -> Maybe a) -> AppendMap k a -> AppendMap k a
updateMin f = coerce (Map.updateMin f)

updateMax :: (a -> Maybe a) -> AppendMap k a -> AppendMap k a
updateMax f = coerce (Map.updateMax f)

updateMinWithKey :: (k -> a -> Maybe a) -> AppendMap k a -> AppendMap k a
updateMinWithKey f = coerce (Map.updateMinWithKey f)

updateMaxWithKey :: (k -> a -> Maybe a) -> AppendMap k a -> AppendMap k a
updateMaxWithKey f = coerce (Map.updateMaxWithKey f)

minView :: forall k a. AppendMap k a -> Maybe (a, AppendMap k a)
minView = coerce (Map.minView @k @a)

maxView :: forall k a. AppendMap k a -> Maybe (a, AppendMap k a)
maxView = coerce (Map.maxView @k @a)

minViewWithKey :: forall k a. AppendMap k a -> Maybe ((k, a), AppendMap k a)
minViewWithKey = coerce (Map.minViewWithKey @k @a)

maxViewWithKey :: forall k a. AppendMap k a -> Maybe ((k, a), AppendMap k a)
maxViewWithKey = coerce (Map.maxViewWithKey @k @a)

showTree :: forall k a. (Show k, Show a) => AppendMap k a -> String
showTree = coerce (Map.showTree @k @a)

showTreeWith :: (k -> a -> String) -> Bool -> Bool -> AppendMap k a -> String
showTreeWith f = coerce (Map.showTreeWith f)

valid :: forall k a. Ord k => AppendMap k a -> Bool
valid = coerce (Map.valid @k @a)

instance Default (AppendMap k a) where
  def = empty

instance AppendMap k' v' ~ t => Rewrapped (AppendMap k a) t

instance Wrapped (AppendMap k a) where
  type Unwrapped (AppendMap k a) = Map k a
  _Wrapped' = iso (\(AppendMap m) -> m) AppendMap

-- | Operator for creating a singleton 'Map'
(=:) :: k -> a -> AppendMap k a
k =: v = singleton k v
infixr 7 =:

