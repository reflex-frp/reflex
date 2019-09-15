{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.TagMap
  ( TagMap
  , unTagMap
  , fromDMap
  , toDMap
  , insert
  ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum (..))
import Data.Unique.Tag.Local

import GHC.Exts (Any)
import Unsafe.Coerce

-- | Like DMap, but with 'Data.Unique.Tag.Tag' as the keys.  Implemented using 'Data.IntMap.IntMap' under the hood.
newtype TagMap x (v :: k -> *) = TagMap { unTagMap :: IntMap Any }

fromDMap :: forall k x (v :: k -> *). DMap (Tag x) v -> TagMap x v
fromDMap = TagMap . IntMap.fromDistinctAscList . fmap (\((k :: Tag x (a :: k)) :=> v) -> (tagId k, (unsafeCoerce :: v a -> Any) v)) . DMap.toAscList

toDMap :: forall x v. TagMap x v -> DMap (Tag x) v
toDMap = DMap.fromDistinctAscList . fmap (\(k, v) -> (unsafeTagFromId k :=> (unsafeCoerce :: Any -> v a) v)) . IntMap.toAscList . unTagMap

insert :: forall x a v. Tag x a -> v a -> TagMap x v -> TagMap x v
insert k v = TagMap . IntMap.insert (tagId k) ((unsafeCoerce :: v a -> Any) v) . unTagMap
