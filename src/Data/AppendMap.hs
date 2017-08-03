{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
(!) = coerce ((Map.!) :: Map k a -> k -> a)
infixl 9 !

(\\) :: forall k a b. Ord k => AppendMap k a -> AppendMap k b -> AppendMap k a
(\\) = coerce ((Map.\\) :: Map k a -> Map k b -> Map k a)
infixl 9 \\

null :: forall k a. AppendMap k a -> Bool
null = coerce (Map.null :: Map k a -> Bool)

size :: forall k a. AppendMap k a -> Int
size = coerce (Map.size :: Map k a -> Int)

member :: forall k a. Ord k => k -> AppendMap k a -> Bool
member = coerce (Map.member :: k -> Map k a -> Bool)

notMember :: forall k a. Ord k => k -> AppendMap k a -> Bool
notMember = coerce (Map.notMember :: k -> Map k a -> Bool)

lookup :: forall k a. Ord k => k -> AppendMap k a -> Maybe a
lookup = coerce (Map.lookup :: k -> Map k a -> Maybe a)

findWithDefault :: forall k a. Ord k => a -> k -> AppendMap k a -> a
findWithDefault = coerce (Map.findWithDefault :: a -> k -> Map k a -> a)

lookupLT :: forall k a. Ord k => k -> AppendMap k a -> Maybe (k, a)
lookupLT = coerce (Map.lookupLT :: k -> Map k a -> Maybe (k,a))

lookupGT :: forall k a. Ord k => k -> AppendMap k a -> Maybe (k, a)
lookupGT = coerce (Map.lookupGT :: k -> Map k a -> Maybe (k,a))

lookupLE :: forall k a. Ord k => k -> AppendMap k a -> Maybe (k, a)
lookupLE = coerce (Map.lookupLE :: k -> Map k a -> Maybe (k,a))

lookupGE :: forall k a. Ord k => k -> AppendMap k a -> Maybe (k, a)
lookupGE = coerce (Map.lookupGE :: k -> Map k a -> Maybe (k,a))

empty :: forall k a. AppendMap k a
empty = coerce (Map.empty :: Map k a)

singleton :: forall k a. k -> a -> AppendMap k a
singleton = coerce (Map.singleton :: k -> a -> Map k a)

insert :: forall k a. Ord k => k -> a -> AppendMap k a -> AppendMap k a
insert = coerce (Map.insert :: k -> a -> Map k a -> Map k a)

insertWith :: forall k a. Ord k => (a -> a -> a) -> k -> a -> AppendMap k a -> AppendMap k a
insertWith = coerce (Map.insertWith :: (a -> a -> a) -> k -> a -> Map k a -> Map k a)

insertWithKey :: forall k a. Ord k => (k -> a -> a -> a) -> k -> a -> AppendMap k a -> AppendMap k a
insertWithKey = coerce (Map.insertWithKey :: (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a)

insertLookupWithKey :: forall k a. Ord k => (k -> a -> a -> a) -> k -> a -> AppendMap k a -> (Maybe a, AppendMap k a)
insertLookupWithKey = coerce (Map.insertLookupWithKey :: (k -> a -> a -> a) -> k -> a -> Map k a -> (Maybe a, Map k a))

delete :: forall k a. Ord k => k -> AppendMap k a -> AppendMap k a
delete = coerce (Map.delete :: k -> Map k a -> Map k a)

adjust :: forall k a. Ord k => (a -> a) -> k -> AppendMap k a -> AppendMap k a
adjust = coerce (Map.adjust :: (a -> a) -> k -> Map k a -> Map k a)

adjustWithKey :: forall k a. Ord k => (k -> a -> a) -> k -> AppendMap k a -> AppendMap k a
adjustWithKey = coerce (Map.adjustWithKey :: (k -> a -> a) -> k -> Map k a -> Map k a)

update :: forall k a. Ord k => (a -> Maybe a) -> k -> AppendMap k a -> AppendMap k a
update = coerce (Map.update :: (a -> Maybe a) -> k -> Map k a -> Map k a)

updateWithKey :: forall k a. Ord k => (k -> a -> Maybe a) -> k -> AppendMap k a -> AppendMap k a
updateWithKey = coerce (Map.updateWithKey :: (k -> a -> Maybe a) -> k -> Map k a -> Map k a)

updateLookupWithKey :: forall k a. Ord k => (k -> a -> Maybe a) -> k -> AppendMap k a -> (Maybe a, AppendMap k a)
updateLookupWithKey = coerce (Map.updateLookupWithKey :: (k -> a -> Maybe a) -> k -> Map k a -> (Maybe a, Map k a))

alter :: forall k a. Ord k => (Maybe a -> Maybe a) -> k -> AppendMap k a -> AppendMap k a
alter = coerce (Map.alter :: (Maybe a -> Maybe a) -> k -> Map k a -> Map k a)

unionWith :: forall k a. Ord k => (a -> a -> a) -> AppendMap k a -> AppendMap k a -> AppendMap k a
unionWith = coerce (Map.unionWith :: (a -> a -> a) -> Map k a -> Map k a -> Map k a)

unionWithKey :: forall k a. Ord k => (k -> a -> a -> a) -> AppendMap k a -> AppendMap k a -> AppendMap k a
unionWithKey = coerce (Map.unionWithKey :: (k -> a -> a -> a) -> Map k a -> Map k a -> Map k a)

unionsWith :: forall k a. Ord k => (a -> a -> a) -> [AppendMap k a] -> AppendMap k a
unionsWith = coerce (Map.unionsWith :: (a -> a -> a) -> [Map k a] -> Map k a)

difference :: forall k a b. Ord k => AppendMap k a -> AppendMap k b -> AppendMap k a
difference = (\\)

differenceWith :: forall k a b. Ord k => (a -> b -> Maybe a) -> AppendMap k a -> AppendMap k b -> AppendMap k a
differenceWith = coerce (Map.differenceWith :: (a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a)

differenceWithKey :: forall k a b. Ord k => (k -> a -> b -> Maybe a) -> AppendMap k a -> AppendMap k b -> AppendMap k a
differenceWithKey = coerce (Map.differenceWithKey :: (k -> a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a)

intersectionWith :: forall k a b c. Ord k => (a -> b -> c) -> AppendMap k a -> AppendMap k b -> AppendMap k c
intersectionWith = coerce (Map.intersectionWith :: (a -> b -> c) -> Map k a -> Map k b -> Map k c)

intersectionWithKey :: forall k a b c. Ord k => (k -> a -> b -> c) -> AppendMap k a -> AppendMap k b -> AppendMap k c
intersectionWithKey = coerce (Map.intersectionWithKey :: (k -> a -> b -> c) -> Map k a -> Map k b -> Map k c)

mergeWithKey :: forall k a b c. Ord k => (k -> a -> b -> Maybe c) -> (AppendMap k a -> AppendMap k c) -> (AppendMap k b -> AppendMap k c) -> AppendMap k a -> AppendMap k b -> AppendMap k c
mergeWithKey = coerce (Map.mergeWithKey :: (k -> a -> b -> Maybe c) -> (Map k a -> Map k c) -> (Map k b -> Map k c) -> Map k a -> Map k b -> Map k c)

map :: (a -> b) -> AppendMap k a -> AppendMap k b
map = fmap

mapWithKey :: (k -> a -> b) -> AppendMap k a -> AppendMap k b
mapWithKey = imap

traverseWithKey :: Applicative t => (k -> a -> t b) -> AppendMap k a -> t (AppendMap k b)
traverseWithKey = itraverse

mapAccum :: forall k a b c. (a -> b -> (a, c)) -> a -> AppendMap k b -> (a, AppendMap k c)
mapAccum = coerce (Map.mapAccum :: (a -> b -> (a, c)) -> a -> Map k b -> (a, Map k c))

mapAccumWithKey :: forall k a b c. (a -> k -> b -> (a, c)) -> a -> AppendMap k b -> (a, AppendMap k c)
mapAccumWithKey = coerce (Map.mapAccumWithKey :: (a -> k -> b -> (a, c)) -> a -> Map k b -> (a, Map k c))

mapAccumRWithKey :: forall k a b c. (a -> k -> b -> (a, c)) -> a -> AppendMap k b -> (a, AppendMap k c)
mapAccumRWithKey = coerce (Map.mapAccumRWithKey :: (a -> k -> b -> (a, c)) -> a -> Map k b -> (a, Map k c))

mapKeys :: forall k1 k2 a. Ord k2 => (k1 -> k2) -> AppendMap k1 a -> AppendMap k2 a
mapKeys = coerce (Map.mapKeys :: (k1 -> k2) -> Map k1 a -> Map k2 a)

mapKeysWith :: forall k1 k2 a. Ord k2 => (a -> a -> a) -> (k1 -> k2) -> AppendMap k1 a -> AppendMap k2 a
mapKeysWith = coerce (Map.mapKeysWith :: (a -> a -> a) -> (k1 -> k2) -> Map k1 a -> Map k2 a)

mapKeysMonotonic :: forall k1 k2 a. (k1 -> k2) -> AppendMap k1 a -> AppendMap k2 a
mapKeysMonotonic = coerce (Map.mapKeysMonotonic :: (k1 -> k2) -> Map k1 a -> Map k2 a)

foldr :: forall k a b. (a -> b -> b) -> b -> AppendMap k a -> b
foldr = coerce (Map.foldr :: (a -> b -> b) -> b -> Map k a -> b)

foldl :: forall k a b. (a -> b -> a) -> a -> AppendMap k b -> a
foldl = coerce (Map.foldl :: (a -> b -> a) -> a -> Map k b -> a)

foldrWithKey :: forall k a b. (k -> a -> b -> b) -> b -> AppendMap k a -> b
foldrWithKey = coerce (Map.foldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b)

foldlWithKey :: forall k a b. (a -> k -> b -> a) -> a -> AppendMap k b -> a
foldlWithKey = coerce (Map.foldlWithKey :: (a -> k -> b -> a) -> a -> Map k b -> a)

foldMapWithKey :: forall k a m. Monoid m => (k -> a -> m) -> AppendMap k a -> m
foldMapWithKey = coerce (Map.foldMapWithKey :: Monoid m => (k -> a -> m) -> Map k a -> m)

foldr' :: forall k a b. (a -> b -> b) -> b -> AppendMap k a -> b
foldr' = coerce (Map.foldr' :: (a -> b -> b) -> b -> Map k a -> b)

foldl' :: forall k a b. (a -> b -> a) -> a -> AppendMap k b -> a
foldl' = coerce (Map.foldl' :: (a -> b -> a) -> a -> Map k b -> a)

foldrWithKey' :: forall k a b. (k -> a -> b -> b) -> b -> AppendMap k a -> b
foldrWithKey' = coerce (Map.foldrWithKey' :: (k -> a -> b -> b) -> b -> Map k a -> b)

foldlWithKey' :: forall k a b. (a -> k -> b -> a) -> a -> AppendMap k b -> a
foldlWithKey' = coerce (Map.foldlWithKey' :: (a -> k -> b -> a) -> a -> Map k b -> a)

elems :: forall k a. AppendMap k a -> [a]
elems = coerce (Map.elems :: Map k a -> [a])

keys :: forall k a. AppendMap k a -> [k]
keys = coerce (Map.keys :: Map k a -> [k])

assocs :: forall k a. AppendMap k a -> [(k, a)]
assocs = coerce (Map.assocs :: Map k a -> [(k, a)])

keysSet :: forall k a. AppendMap k a -> Set k
keysSet = coerce (Map.keysSet :: Map k a -> Set k)

fromSet :: forall k a. (k -> a) -> Set k -> AppendMap k a
fromSet = coerce (Map.fromSet :: (k -> a) -> Set k -> Map k a)

toList :: forall k a. AppendMap k a -> [(k, a)]
toList = coerce (Map.toList :: Map k a -> [(k, a)])

fromList :: forall k a. Ord k => [(k, a)] -> AppendMap k a
fromList = coerce (Map.fromList :: [(k, a)] -> Map k a)

fromListWith :: forall k a. Ord k => (a -> a -> a) -> [(k, a)] -> AppendMap k a
fromListWith = coerce (Map.fromListWith :: (a -> a -> a) -> [(k, a)] -> Map k a)

fromListWithKey :: forall k a. Ord k => (k -> a -> a -> a) -> [(k, a)] -> AppendMap k a
fromListWithKey = coerce (Map.fromListWithKey :: (k -> a -> a -> a) -> [(k, a)] -> Map k a)

toAscList :: forall k a. AppendMap k a -> [(k, a)]
toAscList = coerce (Map.toAscList :: Map k a -> [(k, a)])

toDescList :: forall k a. AppendMap k a -> [(k, a)]
toDescList = coerce (Map.toDescList :: Map k a -> [(k, a)])

fromAscList :: forall k a. Eq k => [(k, a)] -> AppendMap k a
fromAscList = coerce (Map.fromAscList :: [(k, a)] -> Map k a)

fromAscListWith :: forall k a. Eq k => (a -> a -> a) -> [(k, a)] -> AppendMap k a
fromAscListWith = coerce (Map.fromAscListWith :: (a -> a -> a) -> [(k, a)] -> Map k a)

fromAscListWithKey :: forall k a. Eq k => (k -> a -> a -> a) -> [(k, a)] -> AppendMap k a
fromAscListWithKey = coerce (Map.fromAscListWithKey :: (k -> a -> a -> a) -> [(k, a)] -> Map k a)

fromDistinctAscList :: forall k a. [(k, a)] -> AppendMap k a
fromDistinctAscList = coerce (Map.fromDistinctAscList :: [(k, a)] -> Map k a)

filter :: forall k a. (a -> Bool) -> AppendMap k a -> AppendMap k a
filter = coerce (Map.filter :: (a -> Bool) -> Map k a -> Map k a)

filterWithKey :: forall k a. (k -> a -> Bool) -> AppendMap k a -> AppendMap k a
filterWithKey = coerce (Map.filterWithKey :: (k -> a -> Bool) -> Map k a -> Map k a)

partition :: forall k a. (a -> Bool) -> AppendMap k a -> (AppendMap k a, AppendMap k a)
partition = coerce (Map.partition :: (a -> Bool) -> Map k a -> (Map k a, Map k a))

partitionWithKey :: forall k a. (k -> a -> Bool) -> AppendMap k a -> (AppendMap k a, AppendMap k a)
partitionWithKey = coerce (Map.partitionWithKey :: (k -> a -> Bool) -> Map k a -> (Map k a, Map k a))

mapMaybe :: forall k a b. (a -> Maybe b) -> AppendMap k a -> AppendMap k b
mapMaybe = coerce (Map.mapMaybe :: (a -> Maybe b) -> Map k a -> Map k b)

mapMaybeWithKey :: forall k a b. (k -> a -> Maybe b) -> AppendMap k a -> AppendMap k b
mapMaybeWithKey = coerce (Map.mapMaybeWithKey :: (k -> a -> Maybe b) -> Map k a -> Map k b)

mapEither :: forall k a b c. (a -> Either b c) -> AppendMap k a -> (AppendMap k b, AppendMap k c)
mapEither = coerce (Map.mapEither :: (a -> Either b c) -> Map k a -> (Map k b, Map k c))

mapEitherWithKey :: forall k a b c. (k -> a -> Either b c) -> AppendMap k a -> (AppendMap k b, AppendMap k c)
mapEitherWithKey = coerce (Map.mapEitherWithKey :: (k -> a -> Either b c) -> Map k a -> (Map k b, Map k c))

split :: forall k a. Ord k => k -> AppendMap k a -> (AppendMap k a, AppendMap k a)
split = coerce (Map.split :: k -> Map k a -> (Map k a, Map k a))

splitLookup :: forall k a. Ord k => k -> AppendMap k a -> (AppendMap k a, Maybe a, AppendMap k a)
splitLookup = coerce (Map.splitLookup :: k -> Map k a -> (Map k a, Maybe a, Map k a))

splitRoot :: forall k a. AppendMap k a -> [AppendMap k a]
splitRoot = coerce (Map.splitRoot :: Map k a -> [Map k a])

isSubmapOf :: forall k a. (Ord k, Eq a) => AppendMap k a -> AppendMap k a -> Bool
isSubmapOf = coerce (Map.isSubmapOf :: Map k a -> Map k a -> Bool)

isSubmapOfBy :: forall k a b. Ord k => (a -> b -> Bool) -> AppendMap k a -> AppendMap k b -> Bool
isSubmapOfBy = coerce (Map.isSubmapOfBy :: (a -> b -> Bool) -> Map k a -> Map k b -> Bool)

isProperSubmapOf :: forall k a. (Ord k, Eq a) => AppendMap k a -> AppendMap k a -> Bool
isProperSubmapOf = coerce (Map.isProperSubmapOf :: Map k a -> Map k a -> Bool)

isProperSubmapOfBy :: forall k a b. Ord k => (a -> b -> Bool) -> AppendMap k a -> AppendMap k b -> Bool
isProperSubmapOfBy = coerce (Map.isProperSubmapOfBy :: (a -> b -> Bool) -> Map k a -> Map k b -> Bool)

lookupIndex :: forall k a. Ord k => k -> AppendMap k a -> Maybe Int
lookupIndex = coerce (Map.lookupIndex :: k -> Map k a -> Maybe Int)

findIndex :: forall k a. Ord k => k -> AppendMap k a -> Int
findIndex = coerce (Map.findIndex :: k -> Map k a -> Int)

elemAt :: forall k a. Int -> AppendMap k a -> (k, a)
elemAt = coerce (Map.elemAt :: Int -> Map k a -> (k, a))

updateAt :: forall k a. (k -> a -> Maybe a) -> Int -> AppendMap k a -> AppendMap k a
updateAt = coerce (Map.updateAt :: (k -> a -> Maybe a) -> Int -> Map k a -> Map k a)

deleteAt :: forall k a. Int -> AppendMap k a -> AppendMap k a
deleteAt = coerce (Map.deleteAt :: Int -> Map k a -> Map k a)

findMin :: forall k a. AppendMap k a -> (k, a)
findMin = coerce (Map.findMin :: Map k a -> (k, a))

findMax :: forall k a. AppendMap k a -> (k, a)
findMax = coerce (Map.findMax :: Map k a -> (k, a))

deleteMin :: forall k a. AppendMap k a -> AppendMap k a
deleteMin = coerce (Map.deleteMin :: Map k a -> Map k a)

deleteMax :: forall k a. AppendMap k a -> AppendMap k a
deleteMax = coerce (Map.deleteMax :: Map k a -> Map k a)

deleteFindMin :: forall k a. AppendMap k a -> ((k, a), AppendMap k a)
deleteFindMin = coerce (Map.deleteFindMin :: Map k a -> ((k, a), Map k a))

deleteFindMax :: forall k a. AppendMap k a -> ((k, a), AppendMap k a)
deleteFindMax = coerce (Map.deleteFindMax :: Map k a -> ((k, a), Map k a))

updateMin :: forall k a. (a -> Maybe a) -> AppendMap k a -> AppendMap k a
updateMin = coerce (Map.updateMin :: (a -> Maybe a) -> Map k a -> Map k a)

updateMax :: forall k a. (a -> Maybe a) -> AppendMap k a -> AppendMap k a
updateMax = coerce (Map.updateMax :: (a -> Maybe a) -> Map k a -> Map k a)

updateMinWithKey :: forall k a. (k -> a -> Maybe a) -> AppendMap k a -> AppendMap k a
updateMinWithKey = coerce (Map.updateMinWithKey :: (k -> a -> Maybe a) -> Map k a -> Map k a)

updateMaxWithKey :: forall k a. (k -> a -> Maybe a) -> AppendMap k a -> AppendMap k a
updateMaxWithKey = coerce (Map.updateMaxWithKey :: (k -> a -> Maybe a) -> Map k a -> Map k a)

minView :: forall k a. AppendMap k a -> Maybe (a, AppendMap k a)
minView = coerce (Map.minView :: Map k a -> Maybe (a, Map k a))

maxView :: forall k a. AppendMap k a -> Maybe (a, AppendMap k a)
maxView = coerce (Map.maxView :: Map k a -> Maybe (a, Map k a))

minViewWithKey :: forall k a. AppendMap k a -> Maybe ((k, a), AppendMap k a)
minViewWithKey = coerce (Map.minViewWithKey :: Map k a -> Maybe ((k, a), Map k a))

maxViewWithKey :: forall k a. AppendMap k a -> Maybe ((k, a), AppendMap k a)
maxViewWithKey = coerce (Map.maxViewWithKey :: Map k a -> Maybe ((k, a), Map k a))

showTree :: forall k a. (Show k, Show a) => AppendMap k a -> String
showTree = coerce (Map.showTree :: (Show k, Show a) => Map k a -> String)

showTreeWith :: forall k a. (k -> a -> String) -> Bool -> Bool -> AppendMap k a -> String
showTreeWith = coerce (Map.showTreeWith :: (k -> a -> String) -> Bool -> Bool -> Map k a -> String)

valid :: forall k a. Ord k => AppendMap k a -> Bool
valid = coerce (Map.valid :: Ord k => Map k a -> Bool)

instance Default (AppendMap k a) where
  def = empty

instance AppendMap k' v' ~ t => Rewrapped (AppendMap k a) t

instance Wrapped (AppendMap k a) where
  type Unwrapped (AppendMap k a) = Map k a
  _Wrapped' = iso (\(AppendMap m) -> m) AppendMap
