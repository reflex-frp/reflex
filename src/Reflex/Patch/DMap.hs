{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
-- | 'Patch'es on 'DMap' that consist only of insertions (or overwrites) and deletions.
module Reflex.Patch.DMap where

import Reflex.Patch.Class
import Reflex.Patch.IntMap
import Reflex.Patch.Map

import Data.Dependent.Map (DMap, DSum (..), GCompare (..))
import qualified Data.Dependent.Map as DMap
import Data.Functor.Constant
import Data.Functor.Misc
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Semigroup (Semigroup (..))
import Data.Some (Some)

-- | A set of changes to a 'DMap'.  Any element may be inserted/updated or deleted.
-- Insertions are represented as @'ComposeMaybe' (Just value)@,
-- while deletions are represented as @'ComposeMaybe' Nothing@.
newtype PatchDMap k v = PatchDMap { unPatchDMap :: DMap k (ComposeMaybe v) }

deriving instance GCompare k => Semigroup (PatchDMap k v)

deriving instance GCompare k => Monoid (PatchDMap k v)

-- | Apply the insertions or deletions to a given 'DMap'.
instance GCompare k => Patch (PatchDMap k v) where
  type PatchTarget (PatchDMap k v) = DMap k v
  apply (PatchDMap diff) old = Just $! insertions `DMap.union` (old `DMap.difference` deletions) --TODO: return Nothing sometimes --Note: the strict application here is critical to ensuring that incremental merges don't hold onto all their prerequisite events forever; can we make this more robust?
    where insertions = DMap.mapMaybeWithKey (const $ getComposeMaybe) diff
          deletions = DMap.mapMaybeWithKey (const $ nothingToJust . getComposeMaybe) diff
          nothingToJust = \case
            Nothing -> Just $ Constant ()
            Just _ -> Nothing

-- | Map a function @v a -> v' a@ over any inserts/updates in the given
-- @'PatchDMap' k v@ to produce a @'PatchDMap' k v'@.
mapPatchDMap :: (forall a. v a -> v' a) -> PatchDMap k v -> PatchDMap k v'
mapPatchDMap f (PatchDMap p) = PatchDMap $ DMap.map (ComposeMaybe . fmap f . getComposeMaybe) p

-- | Map an effectful function @v a -> f (v' a)@ over any inserts/updates in the given
-- @'PatchDMap' k v@ to produce a @'PatchDMap' k v'@.
traversePatchDMap :: Applicative f => (forall a. v a -> f (v' a)) -> PatchDMap k v -> f (PatchDMap k v')
traversePatchDMap f = traversePatchDMapWithKey $ const f

-- | Map an effectful function @k a -> v a -> f (v' a)@ over any inserts/updates
-- in the given @'PatchDMap' k v@ to produce a @'PatchDMap' k v'@.
traversePatchDMapWithKey :: Applicative m => (forall a. k a -> v a -> m (v' a)) -> PatchDMap k v -> m (PatchDMap k v')
traversePatchDMapWithKey f (PatchDMap p) = PatchDMap <$> DMap.traverseWithKey (\k (ComposeMaybe v) -> ComposeMaybe <$> traverse (f k) v) p

-- | Weaken a @'PatchDMap' k v@ to a @'PatchMap' (Some k) v'@ using a function
-- @v a -> v'@ to weaken each value contained in the patch.
weakenPatchDMapWith :: (forall a. v a -> v') -> PatchDMap k v -> PatchMap (Some k) v'
weakenPatchDMapWith f (PatchDMap p) = PatchMap $ weakenDMapWith (fmap f . getComposeMaybe) p

-- | Convert a weak @'PatchDMap' ('Const2' k a) v@ where the @a@ is known by way of
-- the @Const2@ into a @'PatchMap' k v'@ using a rank 1 function @v a -> v'@.
patchDMapToPatchMapWith :: (v a -> v') -> PatchDMap (Const2 k a) v -> PatchMap k v'
patchDMapToPatchMapWith f (PatchDMap p) = PatchMap $ dmapToMapWith (fmap f . getComposeMaybe) p

-- | Convert a @'PatchMap' k v@ into a @'PatchDMap' ('Const2' k a) v'@ using a function @v -> v' a@.
const2PatchDMapWith :: forall k v v' a. (v -> v' a) -> PatchMap k v -> PatchDMap (Const2 k a) v'
const2PatchDMapWith f (PatchMap p) = PatchDMap $ DMap.fromDistinctAscList $ g <$> Map.toAscList p
  where g :: (k, Maybe v) -> DSum (Const2 k a) (ComposeMaybe v')
        g (k, e) = Const2 k :=> ComposeMaybe (f <$> e)

-- | Convert a @'PatchIntMap' v@ into a @'PatchDMap' ('Const2' Int a) v'@ using a function @v -> v' a@.
const2IntPatchDMapWith :: forall v f a. (v -> f a) -> PatchIntMap v -> PatchDMap (Const2 IntMap.Key a) f
const2IntPatchDMapWith f (PatchIntMap p) = PatchDMap $ DMap.fromDistinctAscList $ g <$> IntMap.toAscList p
  where g :: (IntMap.Key, Maybe v) -> DSum (Const2 IntMap.Key a) (ComposeMaybe f)
        g (k, e) = Const2 k :=> ComposeMaybe (f <$> e)

-- | Get the values that will be replaced or deleted if the given patch is applied to the given 'DMap'.
getDeletions :: GCompare k => PatchDMap k v -> DMap k v' -> DMap k v'
getDeletions (PatchDMap p) m = DMap.intersectionWithKey (\_ v _ -> v) m p
