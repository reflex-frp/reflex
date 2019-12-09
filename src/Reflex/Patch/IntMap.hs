{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
-- | Module containing 'PatchIntMap', a 'Patch' for 'IntMap' which allows for
-- insert/update or delete of associations.
module Reflex.Patch.IntMap where

import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import Data.Maybe
import Data.Semigroup
import Reflex.Patch.Class
import Data.IntMap.Merge.Lazy
import Control.Applicative

-- | 'Patch' for 'IntMap' which represents insertion or deletion of keys in the mapping.
-- Internally represented by 'IntMap (Maybe a)', where @Just@ means insert/update
-- and @Nothing@ means delete.
newtype PatchIntMap a = PatchIntMap (IntMap (Maybe a)) deriving (Functor, Foldable, Traversable, Monoid)

-- | Apply the insertions or deletions to a given 'IntMap'.
instance Patch (PatchIntMap a) where
  type PatchTarget (PatchIntMap a) = IntMap a
  apply (PatchIntMap p) old
    | IntMap.null p
    = Nothing
    | otherwise
    = Just $! merge
        (mapMaybeMissing $ \_k mv -> mv)
        preserveMissing
        (zipWithMaybeMatched (\_k mv v -> mv <|> Just v)) p old

-- | @a <> b@ will apply the changes of @b@ and then apply the changes of @a@.
-- If the same key is modified by both patches, the one on the left will take
-- precedence.
instance Semigroup (PatchIntMap v) where
  PatchIntMap a <> PatchIntMap b = PatchIntMap $ a `mappend` b --TODO: Add a semigroup instance for Map
  -- PatchMap is idempotent, so stimes n is id for every n
  stimes = stimesIdempotentMonoid

-- | Map a function @Int -> a -> b@ over all @a@s in the given @'PatchIntMap' a@
-- (that is, all inserts/updates), producing a @PatchIntMap b@.
mapIntMapPatchWithKey :: (Int -> a -> b) -> PatchIntMap a -> PatchIntMap b
mapIntMapPatchWithKey f (PatchIntMap m) = PatchIntMap $ IntMap.mapWithKey (\ k mv -> f k <$> mv) m

-- | Map an effectful function @Int -> a -> f b@ over all @a@s in the given @'PatchIntMap' a@
-- (that is, all inserts/updates), producing a @f (PatchIntMap b)@.
traverseIntMapPatchWithKey :: Applicative f => (Int -> a -> f b) -> PatchIntMap a -> f (PatchIntMap b)
traverseIntMapPatchWithKey f (PatchIntMap m) = PatchIntMap <$> IntMap.traverseWithKey (\k mv -> traverse (f k) mv) m

-- | Extract all @a@s inserted/updated by the given @'PatchIntMap' a@.
patchIntMapNewElements :: PatchIntMap a -> [a]
patchIntMapNewElements (PatchIntMap m) = catMaybes $ IntMap.elems m

-- | Convert the given @'PatchIntMap' a@ into an @'IntMap' a@ with all
-- the inserts/updates in the given patch.
patchIntMapNewElementsMap :: PatchIntMap a -> IntMap a
patchIntMapNewElementsMap (PatchIntMap m) = IntMap.mapMaybe id m

-- | Subset the given @'IntMap' a@ to contain only the keys that would be
-- deleted by the given @'PatchIntMap' a@.
getDeletions :: PatchIntMap v -> IntMap v' -> IntMap v'
getDeletions (PatchIntMap m) v = IntMap.intersection v m
