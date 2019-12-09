{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveTraversable #-}
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
    = changedToMaybe $
        mergeA
          (traverseMaybeMissing $ \_k mv ->
             case mv of
               Nothing -> Unchanged Nothing
               Just _ -> Changed mv)
          preserveMissing
          -- We could try to detect an update here that does nothing, but that
          -- will be quite unreliable for a map of Events or similar; it may
          -- not be worth the trouble.
          (zipWithMaybeAMatched (\_k mv v -> Changed $! mv <|> Just v)) p old

changedToMaybe :: Changed a -> Maybe a
changedToMaybe (Unchanged _) = Nothing
changedToMaybe (Changed a) = Just a

data Changed a
  = Unchanged a
  | Changed a
  deriving (Functor)

instance Applicative Changed where
  pure = Unchanged
  liftA2 f (Changed x) (Changed y) = Changed (f x y)
  liftA2 f (Unchanged x) (Changed y) = Changed (f x y)
  liftA2 f (Changed x) (Unchanged y) = Changed (f x y)
  liftA2 f (Unchanged x) (Unchanged y) = Unchanged (f x y)

-- | @a <> b@ will apply the changes of @b@ and then apply the changes of @a@.
-- If the same key is modified by both patches, the one on the left will take
-- precedence.
instance Ord k => Semigroup (PatchMap k v) where
  PatchMap a <> PatchMap b = PatchMap $ a `mappend` b --TODO: Add a semigroup instance for Map
  -- PatchMap is idempotent, so stimes n is id for every n
  stimes = stimesIdempotentMonoid

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
