{-# LANGUAGE LambdaCase #-}
module Data.Map.Misc
  (
  -- * Working with Maps
    diffMapNoEq
  , diffMap
  , applyMap
  , mapPartitionEithers
  , applyMapKeysSet
  ) where

import Data.Align
import Data.Either (isLeft)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.These

diffMapNoEq :: (Ord k) => Map k v -> Map k v -> Map k (Maybe v)
diffMapNoEq olds news = flip Map.mapMaybe (align olds news) $ \case
  This _ -> Just Nothing
  These _ new -> Just $ Just new
  That new -> Just $ Just new

diffMap :: (Ord k, Eq v) => Map k v -> Map k v -> Map k (Maybe v)
diffMap olds news = flip Map.mapMaybe (align olds news) $ \case
  This _ -> Just Nothing
  These old new
    | old == new -> Nothing
    | otherwise -> Just $ Just new
  That new -> Just $ Just new

applyMap :: Ord k => Map k (Maybe v) -> Map k v -> Map k v
applyMap patch old = insertions `Map.union` (old `Map.difference` deletions)
  where (deletions, insertions) = mapPartitionEithers $ maybeToEither <$> patch
        maybeToEither = \case
          Nothing -> Left ()
          Just r -> Right r

mapPartitionEithers :: Map k (Either a b) -> (Map k a, Map k b)
mapPartitionEithers m = (fromLeft <$> ls, fromRight <$> rs)
  where (ls, rs) = Map.partition isLeft m
        fromLeft (Left l) = l
        fromLeft _ = error "mapPartitionEithers: fromLeft received a Right value; this should be impossible"
        fromRight (Right r) = r
        fromRight _ = error "mapPartitionEithers: fromRight received a Left value; this should be impossible"

-- | Apply a map patch to a set
-- > applyMapKeysSet patch (Map.keysSet m) == Map.keysSet (applyMap patch m)
applyMapKeysSet :: Ord k => Map k (Maybe v) -> Set k -> Set k
applyMapKeysSet patch old = Map.keysSet insertions `Set.union` (old `Set.difference` Map.keysSet deletions)
  where (insertions, deletions) = Map.partition isJust patch

