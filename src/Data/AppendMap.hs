{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | 'Data.Map' with a better 'Monoid' instance
--
-- 'Data.Map' has @mappend = union@, which is left-biased.  AppendMap has
-- @mappend = unionWith mappend@ instead.
module Data.AppendMap
  ( module Data.AppendMap
  , module Data.Map.Monoidal
  ) where

import Prelude hiding (map)

import Data.Align
import Data.Coerce
import Data.Default
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Monoidal
import Data.Semigroup
import Data.Set (Set)
import Reflex.Class (FunctorMaybe (..))
import Reflex.Patch (Additive, Group (..))

{-# DEPRECATED AppendMap "Use 'MonoidalMap' instead" #-}
type AppendMap = MonoidalMap

{-# DEPRECATED _unAppendMap "Use 'getMonoidalMap' instead" #-}
_unAppendMap :: MonoidalMap k v -> Map k v
_unAppendMap = getMonoidalMap

pattern AppendMap :: Map k v -> MonoidalMap k v
pattern AppendMap m = MonoidalMap m

deriving instance Ord k => Align (MonoidalMap k)

instance FunctorMaybe (MonoidalMap k) where
  fmapMaybe = mapMaybe

-- | Deletes a key, returning 'Nothing' if the result is empty.
nonEmptyDelete :: Ord k => k -> MonoidalMap k a -> Maybe (MonoidalMap k a)
nonEmptyDelete k vs =
  let deleted = delete k vs
  in if Data.AppendMap.null deleted
       then Nothing
       else Just deleted

mapMaybeNoNull :: (a -> Maybe b)
               -> MonoidalMap token a
               -> Maybe (MonoidalMap token b)
mapMaybeNoNull f as =
  let bs = fmapMaybe f as
  in if Data.AppendMap.null bs
       then Nothing
       else Just bs

#if !MIN_VERSION_monoidal_containers(0,3,1)
instance (Ord k, Semigroup v) => Semigroup (MonoidalMap k v) where
  MonoidalMap a <> MonoidalMap b = MonoidalMap $ Map.unionWith (<>) a b
#endif

instance (Ord k, Group q) => Group (MonoidalMap k q) where
  negateG = map negateG

instance (Ord k, Additive q) => Additive (MonoidalMap k q)

(!) :: forall k a. Ord k => MonoidalMap k a -> k -> a
(!) = coerce ((Map.!) :: Map k a -> k -> a)
infixl 9 !

(\\) :: forall k a b. Ord k => MonoidalMap k a -> MonoidalMap k b -> MonoidalMap k a
(\\) = coerce ((Map.\\) :: Map k a -> Map k b -> Map k a)
infixl 9 {-"-} \\ {-"-} --These comments prevent the C preprocssor from complaining

null :: forall k a. MonoidalMap k a -> Bool
null = coerce (Map.null :: Map k a -> Bool)

lookup :: forall k a. Ord k => k -> MonoidalMap k a -> Maybe a
lookup = coerce (Map.lookup :: k -> Map k a -> Maybe a)

lookupLT :: forall k a. Ord k => k -> MonoidalMap k a -> Maybe (k, a)
lookupLT = coerce (Map.lookupLT :: k -> Map k a -> Maybe (k,a))

lookupGT :: forall k a. Ord k => k -> MonoidalMap k a -> Maybe (k, a)
lookupGT = coerce (Map.lookupGT :: k -> Map k a -> Maybe (k,a))

lookupLE :: forall k a. Ord k => k -> MonoidalMap k a -> Maybe (k, a)
lookupLE = coerce (Map.lookupLE :: k -> Map k a -> Maybe (k,a))

lookupGE :: forall k a. Ord k => k -> MonoidalMap k a -> Maybe (k, a)
lookupGE = coerce (Map.lookupGE :: k -> Map k a -> Maybe (k,a))

empty :: forall k a. MonoidalMap k a
empty = coerce (Map.empty :: Map k a)

insert :: forall k a. Ord k => k -> a -> MonoidalMap k a -> MonoidalMap k a
insert = coerce (Map.insert :: k -> a -> Map k a -> Map k a)

insertWith :: forall k a. Ord k => (a -> a -> a) -> k -> a -> MonoidalMap k a -> MonoidalMap k a
insertWith = coerce (Map.insertWith :: (a -> a -> a) -> k -> a -> Map k a -> Map k a)

insertWithKey :: forall k a. Ord k => (k -> a -> a -> a) -> k -> a -> MonoidalMap k a -> MonoidalMap k a
insertWithKey = coerce (Map.insertWithKey :: (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a)

insertLookupWithKey :: forall k a. Ord k => (k -> a -> a -> a) -> k -> a -> MonoidalMap k a -> (Maybe a, MonoidalMap k a)
insertLookupWithKey = coerce (Map.insertLookupWithKey :: (k -> a -> a -> a) -> k -> a -> Map k a -> (Maybe a, Map k a))

delete :: forall k a. Ord k => k -> MonoidalMap k a -> MonoidalMap k a
delete = coerce (Map.delete :: k -> Map k a -> Map k a)

adjust :: forall k a. Ord k => (a -> a) -> k -> MonoidalMap k a -> MonoidalMap k a
adjust = coerce (Map.adjust :: (a -> a) -> k -> Map k a -> Map k a)

adjustWithKey :: forall k a. Ord k => (k -> a -> a) -> k -> MonoidalMap k a -> MonoidalMap k a
adjustWithKey = coerce (Map.adjustWithKey :: (k -> a -> a) -> k -> Map k a -> Map k a)

update :: forall k a. Ord k => (a -> Maybe a) -> k -> MonoidalMap k a -> MonoidalMap k a
update = coerce (Map.update :: (a -> Maybe a) -> k -> Map k a -> Map k a)

updateWithKey :: forall k a. Ord k => (k -> a -> Maybe a) -> k -> MonoidalMap k a -> MonoidalMap k a
updateWithKey = coerce (Map.updateWithKey :: (k -> a -> Maybe a) -> k -> Map k a -> Map k a)

updateLookupWithKey :: forall k a. Ord k => (k -> a -> Maybe a) -> k -> MonoidalMap k a -> (Maybe a, MonoidalMap k a)
updateLookupWithKey = coerce (Map.updateLookupWithKey :: (k -> a -> Maybe a) -> k -> Map k a -> (Maybe a, Map k a))

alter :: forall k a. Ord k => (Maybe a -> Maybe a) -> k -> MonoidalMap k a -> MonoidalMap k a
alter = coerce (Map.alter :: (Maybe a -> Maybe a) -> k -> Map k a -> Map k a)

unionWith :: forall k a. Ord k => (a -> a -> a) -> MonoidalMap k a -> MonoidalMap k a -> MonoidalMap k a
unionWith = coerce (Map.unionWith :: (a -> a -> a) -> Map k a -> Map k a -> Map k a)

unionWithKey :: forall k a. Ord k => (k -> a -> a -> a) -> MonoidalMap k a -> MonoidalMap k a -> MonoidalMap k a
unionWithKey = coerce (Map.unionWithKey :: (k -> a -> a -> a) -> Map k a -> Map k a -> Map k a)

unionsWith :: forall k a. Ord k => (a -> a -> a) -> [MonoidalMap k a] -> MonoidalMap k a
unionsWith = coerce (Map.unionsWith :: (a -> a -> a) -> [Map k a] -> Map k a)

difference :: forall k a b. Ord k => MonoidalMap k a -> MonoidalMap k b -> MonoidalMap k a
difference = (\\)

differenceWith :: forall k a b. Ord k => (a -> b -> Maybe a) -> MonoidalMap k a -> MonoidalMap k b -> MonoidalMap k a
differenceWith = coerce (Map.differenceWith :: (a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a)

differenceWithKey :: forall k a b. Ord k => (k -> a -> b -> Maybe a) -> MonoidalMap k a -> MonoidalMap k b -> MonoidalMap k a
differenceWithKey = coerce (Map.differenceWithKey :: (k -> a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a)

intersectionWith :: forall k a b c. Ord k => (a -> b -> c) -> MonoidalMap k a -> MonoidalMap k b -> MonoidalMap k c
intersectionWith = coerce (Map.intersectionWith :: (a -> b -> c) -> Map k a -> Map k b -> Map k c)

intersectionWithKey :: forall k a b c. Ord k => (k -> a -> b -> c) -> MonoidalMap k a -> MonoidalMap k b -> MonoidalMap k c
intersectionWithKey = coerce (Map.intersectionWithKey :: (k -> a -> b -> c) -> Map k a -> Map k b -> Map k c)

mergeWithKey :: forall k a b c. Ord k => (k -> a -> b -> Maybe c) -> (MonoidalMap k a -> MonoidalMap k c) -> (MonoidalMap k b -> MonoidalMap k c) -> MonoidalMap k a -> MonoidalMap k b -> MonoidalMap k c
mergeWithKey = coerce (Map.mergeWithKey :: (k -> a -> b -> Maybe c) -> (Map k a -> Map k c) -> (Map k b -> Map k c) -> Map k a -> Map k b -> Map k c)

map :: (a -> b) -> MonoidalMap k a -> MonoidalMap k b
map = fmap

mapWithKey :: (k -> a -> b) -> MonoidalMap k a -> MonoidalMap k b
mapWithKey f (MonoidalMap m) = MonoidalMap $ Map.mapWithKey f m

traverseWithKey :: Applicative t => (k -> a -> t b) -> MonoidalMap k a -> t (MonoidalMap k b)
traverseWithKey f (MonoidalMap m) = MonoidalMap <$> Map.traverseWithKey f m

mapAccum :: forall k a b c. (a -> b -> (a, c)) -> a -> MonoidalMap k b -> (a, MonoidalMap k c)
mapAccum = coerce (Map.mapAccum :: (a -> b -> (a, c)) -> a -> Map k b -> (a, Map k c))

mapAccumWithKey :: forall k a b c. (a -> k -> b -> (a, c)) -> a -> MonoidalMap k b -> (a, MonoidalMap k c)
mapAccumWithKey = coerce (Map.mapAccumWithKey :: (a -> k -> b -> (a, c)) -> a -> Map k b -> (a, Map k c))

mapAccumRWithKey :: forall k a b c. (a -> k -> b -> (a, c)) -> a -> MonoidalMap k b -> (a, MonoidalMap k c)
mapAccumRWithKey = coerce (Map.mapAccumRWithKey :: (a -> k -> b -> (a, c)) -> a -> Map k b -> (a, Map k c))

mapKeys :: forall k1 k2 a. Ord k2 => (k1 -> k2) -> MonoidalMap k1 a -> MonoidalMap k2 a
mapKeys = coerce (Map.mapKeys :: (k1 -> k2) -> Map k1 a -> Map k2 a)

mapKeysWith :: forall k1 k2 a. Ord k2 => (a -> a -> a) -> (k1 -> k2) -> MonoidalMap k1 a -> MonoidalMap k2 a
mapKeysWith = coerce (Map.mapKeysWith :: (a -> a -> a) -> (k1 -> k2) -> Map k1 a -> Map k2 a)

mapKeysMonotonic :: forall k1 k2 a. (k1 -> k2) -> MonoidalMap k1 a -> MonoidalMap k2 a
mapKeysMonotonic = coerce (Map.mapKeysMonotonic :: (k1 -> k2) -> Map k1 a -> Map k2 a)

foldr :: forall k a b. (a -> b -> b) -> b -> MonoidalMap k a -> b
foldr = coerce (Map.foldr :: (a -> b -> b) -> b -> Map k a -> b)

foldl :: forall k a b. (a -> b -> a) -> a -> MonoidalMap k b -> a
foldl = coerce (Map.foldl :: (a -> b -> a) -> a -> Map k b -> a)

foldrWithKey :: forall k a b. (k -> a -> b -> b) -> b -> MonoidalMap k a -> b
foldrWithKey = coerce (Map.foldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b)

foldlWithKey :: forall k a b. (a -> k -> b -> a) -> a -> MonoidalMap k b -> a
foldlWithKey = coerce (Map.foldlWithKey :: (a -> k -> b -> a) -> a -> Map k b -> a)

foldMapWithKey :: forall k a m. Monoid m => (k -> a -> m) -> MonoidalMap k a -> m
foldMapWithKey = coerce (Map.foldMapWithKey :: (k -> a -> m) -> Map k a -> m)

foldr' :: forall k a b. (a -> b -> b) -> b -> MonoidalMap k a -> b
foldr' = coerce (Map.foldr' :: (a -> b -> b) -> b -> Map k a -> b)

foldl' :: forall k a b. (a -> b -> a) -> a -> MonoidalMap k b -> a
foldl' = coerce (Map.foldl' :: (a -> b -> a) -> a -> Map k b -> a)

foldrWithKey' :: forall k a b. (k -> a -> b -> b) -> b -> MonoidalMap k a -> b
foldrWithKey' = coerce (Map.foldrWithKey' :: (k -> a -> b -> b) -> b -> Map k a -> b)

foldlWithKey' :: forall k a b. (a -> k -> b -> a) -> a -> MonoidalMap k b -> a
foldlWithKey' = coerce (Map.foldlWithKey' :: (a -> k -> b -> a) -> a -> Map k b -> a)

keysSet :: forall k a. MonoidalMap k a -> Set k
keysSet = coerce (Map.keysSet :: Map k a -> Set k)

fromSet :: forall k a. (k -> a) -> Set k -> MonoidalMap k a
fromSet = coerce (Map.fromSet :: (k -> a) -> Set k -> Map k a)

toList :: forall k a. MonoidalMap k a -> [(k, a)]
toList = coerce (Map.toList :: Map k a -> [(k, a)])

fromList :: forall k a. Ord k => [(k, a)] -> MonoidalMap k a
fromList = coerce (Map.fromList :: [(k, a)] -> Map k a)

fromListWith :: forall k a. Ord k => (a -> a -> a) -> [(k, a)] -> MonoidalMap k a
fromListWith = coerce (Map.fromListWith :: (a -> a -> a) -> [(k, a)] -> Map k a)

fromListWithKey :: forall k a. Ord k => (k -> a -> a -> a) -> [(k, a)] -> MonoidalMap k a
fromListWithKey = coerce (Map.fromListWithKey :: (k -> a -> a -> a) -> [(k, a)] -> Map k a)

toAscList :: forall k a. MonoidalMap k a -> [(k, a)]
toAscList = coerce (Map.toAscList :: Map k a -> [(k, a)])

toDescList :: forall k a. MonoidalMap k a -> [(k, a)]
toDescList = coerce (Map.toDescList :: Map k a -> [(k, a)])

fromAscList :: forall k a. Eq k => [(k, a)] -> MonoidalMap k a
fromAscList = coerce (Map.fromAscList :: [(k, a)] -> Map k a)

fromAscListWith :: forall k a. Eq k => (a -> a -> a) -> [(k, a)] -> MonoidalMap k a
fromAscListWith = coerce (Map.fromAscListWith :: (a -> a -> a) -> [(k, a)] -> Map k a)

fromAscListWithKey :: forall k a. Eq k => (k -> a -> a -> a) -> [(k, a)] -> MonoidalMap k a
fromAscListWithKey = coerce (Map.fromAscListWithKey :: (k -> a -> a -> a) -> [(k, a)] -> Map k a)

fromDistinctAscList :: forall k a. [(k, a)] -> MonoidalMap k a
fromDistinctAscList = coerce (Map.fromDistinctAscList :: [(k, a)] -> Map k a)

filter :: forall k a. (a -> Bool) -> MonoidalMap k a -> MonoidalMap k a
filter = coerce (Map.filter :: (a -> Bool) -> Map k a -> Map k a)

filterWithKey :: forall k a. (k -> a -> Bool) -> MonoidalMap k a -> MonoidalMap k a
filterWithKey = coerce (Map.filterWithKey :: (k -> a -> Bool) -> Map k a -> Map k a)

partition :: forall k a. (a -> Bool) -> MonoidalMap k a -> (MonoidalMap k a, MonoidalMap k a)
partition = coerce (Map.partition :: (a -> Bool) -> Map k a -> (Map k a, Map k a))

partitionWithKey :: forall k a. (k -> a -> Bool) -> MonoidalMap k a -> (MonoidalMap k a, MonoidalMap k a)
partitionWithKey = coerce (Map.partitionWithKey :: (k -> a -> Bool) -> Map k a -> (Map k a, Map k a))

mapMaybe :: forall k a b. (a -> Maybe b) -> MonoidalMap k a -> MonoidalMap k b
mapMaybe = coerce (Map.mapMaybe :: (a -> Maybe b) -> Map k a -> Map k b)

mapMaybeWithKey :: forall k a b. (k -> a -> Maybe b) -> MonoidalMap k a -> MonoidalMap k b
mapMaybeWithKey = coerce (Map.mapMaybeWithKey :: (k -> a -> Maybe b) -> Map k a -> Map k b)

mapEither :: forall k a b c. (a -> Either b c) -> MonoidalMap k a -> (MonoidalMap k b, MonoidalMap k c)
mapEither = coerce (Map.mapEither :: (a -> Either b c) -> Map k a -> (Map k b, Map k c))

mapEitherWithKey :: forall k a b c. (k -> a -> Either b c) -> MonoidalMap k a -> (MonoidalMap k b, MonoidalMap k c)
mapEitherWithKey = coerce (Map.mapEitherWithKey :: (k -> a -> Either b c) -> Map k a -> (Map k b, Map k c))

split :: forall k a. Ord k => k -> MonoidalMap k a -> (MonoidalMap k a, MonoidalMap k a)
split = coerce (Map.split :: k -> Map k a -> (Map k a, Map k a))

splitLookup :: forall k a. Ord k => k -> MonoidalMap k a -> (MonoidalMap k a, Maybe a, MonoidalMap k a)
splitLookup = coerce (Map.splitLookup :: k -> Map k a -> (Map k a, Maybe a, Map k a))

splitRoot :: forall k a. MonoidalMap k a -> [MonoidalMap k a]
splitRoot = coerce (Map.splitRoot :: Map k a -> [Map k a])

isSubmapOf :: forall k a. (Ord k, Eq a) => MonoidalMap k a -> MonoidalMap k a -> Bool
isSubmapOf = coerce (Map.isSubmapOf :: Map k a -> Map k a -> Bool)

isSubmapOfBy :: forall k a b. Ord k => (a -> b -> Bool) -> MonoidalMap k a -> MonoidalMap k b -> Bool
isSubmapOfBy = coerce (Map.isSubmapOfBy :: (a -> b -> Bool) -> Map k a -> Map k b -> Bool)

isProperSubmapOf :: forall k a. (Ord k, Eq a) => MonoidalMap k a -> MonoidalMap k a -> Bool
isProperSubmapOf = coerce (Map.isProperSubmapOf :: Map k a -> Map k a -> Bool)

isProperSubmapOfBy :: forall k a b. Ord k => (a -> b -> Bool) -> MonoidalMap k a -> MonoidalMap k b -> Bool
isProperSubmapOfBy = coerce (Map.isProperSubmapOfBy :: (a -> b -> Bool) -> Map k a -> Map k b -> Bool)

lookupIndex :: forall k a. Ord k => k -> MonoidalMap k a -> Maybe Int
lookupIndex = coerce (Map.lookupIndex :: k -> Map k a -> Maybe Int)

findIndex :: forall k a. Ord k => k -> MonoidalMap k a -> Int
findIndex = coerce (Map.findIndex :: k -> Map k a -> Int)

elemAt :: forall k a. Int -> MonoidalMap k a -> (k, a)
elemAt = coerce (Map.elemAt :: Int -> Map k a -> (k, a))

updateAt :: forall k a. (k -> a -> Maybe a) -> Int -> MonoidalMap k a -> MonoidalMap k a
updateAt = coerce (Map.updateAt :: (k -> a -> Maybe a) -> Int -> Map k a -> Map k a)

deleteAt :: forall k a. Int -> MonoidalMap k a -> MonoidalMap k a
deleteAt = coerce (Map.deleteAt :: Int -> Map k a -> Map k a)

findMin :: forall k a. MonoidalMap k a -> (k, a)
findMin = coerce (Map.findMin :: Map k a -> (k, a))

findMax :: forall k a. MonoidalMap k a -> (k, a)
findMax = coerce (Map.findMax :: Map k a -> (k, a))

deleteMin :: forall k a. MonoidalMap k a -> MonoidalMap k a
deleteMin = coerce (Map.deleteMin :: Map k a -> Map k a)

deleteMax :: forall k a. MonoidalMap k a -> MonoidalMap k a
deleteMax = coerce (Map.deleteMax :: Map k a -> Map k a)

deleteFindMin :: forall k a. MonoidalMap k a -> ((k, a), MonoidalMap k a)
deleteFindMin = coerce (Map.deleteFindMin :: Map k a -> ((k, a), Map k a))

deleteFindMax :: forall k a. MonoidalMap k a -> ((k, a), MonoidalMap k a)
deleteFindMax = coerce (Map.deleteFindMax :: Map k a -> ((k, a), Map k a))

updateMin :: forall k a. (a -> Maybe a) -> MonoidalMap k a -> MonoidalMap k a
updateMin = coerce (Map.updateMin :: (a -> Maybe a) -> Map k a -> Map k a)

updateMax :: forall k a. (a -> Maybe a) -> MonoidalMap k a -> MonoidalMap k a
updateMax = coerce (Map.updateMax :: (a -> Maybe a) -> Map k a -> Map k a)

updateMinWithKey :: forall k a. (k -> a -> Maybe a) -> MonoidalMap k a -> MonoidalMap k a
updateMinWithKey = coerce (Map.updateMinWithKey :: (k -> a -> Maybe a) -> Map k a -> Map k a)

updateMaxWithKey :: forall k a. (k -> a -> Maybe a) -> MonoidalMap k a -> MonoidalMap k a
updateMaxWithKey = coerce (Map.updateMaxWithKey :: (k -> a -> Maybe a) -> Map k a -> Map k a)

minView :: forall k a. MonoidalMap k a -> Maybe (a, MonoidalMap k a)
minView = coerce (Map.minView :: Map k a -> Maybe (a, Map k a))

maxView :: forall k a. MonoidalMap k a -> Maybe (a, MonoidalMap k a)
maxView = coerce (Map.maxView :: Map k a -> Maybe (a, Map k a))

minViewWithKey :: forall k a. MonoidalMap k a -> Maybe ((k, a), MonoidalMap k a)
minViewWithKey = coerce (Map.minViewWithKey :: Map k a -> Maybe ((k, a), Map k a))

maxViewWithKey :: forall k a. MonoidalMap k a -> Maybe ((k, a), MonoidalMap k a)
maxViewWithKey = coerce (Map.maxViewWithKey :: Map k a -> Maybe ((k, a), Map k a))

showTree :: forall k a. (Show k, Show a) => MonoidalMap k a -> String
showTree = coerce (Map.showTree :: Map k a -> String)

showTreeWith :: forall k a. (k -> a -> String) -> Bool -> Bool -> MonoidalMap k a -> String
showTreeWith = coerce (Map.showTreeWith :: (k -> a -> String) -> Bool -> Bool -> Map k a -> String)

valid :: forall k a. Ord k => MonoidalMap k a -> Bool
valid = coerce (Map.valid :: Map k a -> Bool)

instance Default (MonoidalMap k a) where
  def = empty
