{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
-- | 'Patch'es on 'Map' that consist only of insertions (including overwrites)
-- and deletions
module Reflex.Patch.Map where

import Reflex.Patch.Class

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Semigroup
import Data.Map.Merge.Lazy
import Control.Applicative

-- | A set of changes to a 'Map'.  Any element may be inserted/updated or
-- deleted.  Insertions are represented as values wrapped in 'Just', while
-- deletions are represented as 'Nothing's
newtype PatchMap k v = PatchMap { unPatchMap :: Map k (Maybe v) }
  deriving (Show, Read, Eq, Ord)

-- | Apply the insertions or deletions to a given 'Map'.
instance Ord k => Patch (PatchMap k v) where
  type PatchTarget (PatchMap k v) = Map k v
  {-# INLINABLE apply #-}
  apply (PatchMap p) old
    | Map.null p = Nothing
    | otherwise
    -- TODO: Can we return Nothing sometimes? This requires checking whether
    -- the old and new values are the same. If those are Events or similar,
    -- we're not likely to do so reliably. For other purposes, we can try
    -- pointer equality, but that will only be semi-reliable if both values
    -- have been forced.
    = Just $! --Note: the strict application here is critical to ensuring that incremental merges don't hold onto all their prerequisite events forever; can we make this more robust?
        merge
          (mapMaybeMissing $ \_k mv -> mv)
          preserveMissing
          (zipWithMaybeMatched (\_k mv v -> mv <|> Just v)) p old


-- | @a <> b@ will apply the changes of @b@ and then apply the changes of @a@.
-- If the same key is modified by both patches, the one on the left will take
-- precedence.
instance Ord k => Semigroup (PatchMap k v) where
  PatchMap a <> PatchMap b = PatchMap $ a `mappend` b --TODO: Add a semigroup instance for Map
  -- PatchMap is idempotent, so stimes n is id for every n
#if MIN_VERSION_semigroups(0,17,0)
  stimes = stimesIdempotentMonoid
#else
  times1p n x = case compare n 0 of
    LT -> error "stimesIdempotentMonoid: negative multiplier"
    EQ -> mempty
    GT -> x
#endif

-- | The empty 'PatchMap' contains no insertions or deletions
instance Ord k => Monoid (PatchMap k v) where
  mempty = PatchMap mempty
  mappend = (<>)

-- | 'fmap'ping a 'PatchMap' will alter all of the values it will insert.
-- Deletions are unaffected.
instance Functor (PatchMap k) where
  fmap f = PatchMap . fmap (fmap f) . unPatchMap

-- | Returns all the new elements that will be added to the 'Map'
patchMapNewElements :: PatchMap k v -> [v]
patchMapNewElements (PatchMap p) = catMaybes $ Map.elems p

-- | Returns all the new elements that will be added to the 'Map'
patchMapNewElementsMap :: PatchMap k v -> Map k v
patchMapNewElementsMap (PatchMap p) = Map.mapMaybe id p
