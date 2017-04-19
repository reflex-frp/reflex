{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Reflex.Patch.DMap where

import Reflex.Patch.Class
import Reflex.Patch.Map

import Data.Dependent.Map (DMap, DSum (..), GCompare (..))
import qualified Data.Dependent.Map as DMap
import Data.Functor.Constant
import Data.Functor.Misc
import qualified Data.Map as Map
import Data.Semigroup (Semigroup (..))
import Data.Some (Some)

-- | A set of changes to a 'DMap'.  Any element may be inserted/updated or
-- deleted.
newtype PatchDMap k v = PatchDMap { unPatchDMap :: DMap k (ComposeMaybe v) }

deriving instance GCompare k => Semigroup (PatchDMap k v)

deriving instance GCompare k => Monoid (PatchDMap k v)

instance GCompare k => Patch (PatchDMap k v) where
  type PatchTarget (PatchDMap k v) = DMap k v
  apply (PatchDMap diff) old = Just $! insertions `DMap.union` (old `DMap.difference` deletions) --TODO: return Nothing sometimes --Note: the strict application here is critical to ensuring that incremental merges don't hold onto all their prerequisite events forever; can we make this more robust?
    where insertions = DMap.mapMaybeWithKey (const $ getComposeMaybe) diff
          deletions = DMap.mapMaybeWithKey (const $ nothingToJust . getComposeMaybe) diff
          nothingToJust = \case
            Nothing -> Just $ Constant ()
            Just _ -> Nothing

mapPatchDMap :: (forall a. v a -> v' a) -> PatchDMap k v -> PatchDMap k v'
mapPatchDMap f (PatchDMap p) = PatchDMap $ DMap.map (ComposeMaybe . fmap f . getComposeMaybe) p

traversePatchDMap :: Applicative m => (forall a. v a -> m (v' a)) -> PatchDMap k v -> m (PatchDMap k v')
traversePatchDMap f = traversePatchDMapWithKey $ const f

traversePatchDMapWithKey :: Applicative m => (forall a. k a -> v a -> m (v' a)) -> PatchDMap k v -> m (PatchDMap k v')
traversePatchDMapWithKey f (PatchDMap p) = PatchDMap <$> DMap.traverseWithKey (\k (ComposeMaybe v) -> ComposeMaybe <$> traverse (f k) v) p

weakenPatchDMapWith :: (forall a. v a -> v') -> PatchDMap k v -> PatchMap (Some k) v'
weakenPatchDMapWith f (PatchDMap p) = PatchMap $ weakenDMapWith (fmap f . getComposeMaybe) p

patchDMapToPatchMapWith :: (f v -> v') -> PatchDMap (Const2 k v) f -> PatchMap k v'
patchDMapToPatchMapWith f (PatchDMap p) = PatchMap $ dmapToMapWith (fmap f . getComposeMaybe) p

const2PatchDMapWith :: forall k v f a. (v -> f a) -> PatchMap k v -> PatchDMap (Const2 k a) f
const2PatchDMapWith f (PatchMap p) = PatchDMap $ DMap.fromDistinctAscList $ g <$> Map.toAscList p
  where g :: (k, Maybe v) -> DSum (Const2 k a) (ComposeMaybe f)
        g (k, e) = Const2 k :=> ComposeMaybe (f <$> e)

-- | Get the values that will be deleted if the given patch is applied to the
-- given 'DMap'.  Includes values that will be replaced.
getDeletions :: GCompare k => PatchDMap k v -> DMap k v' -> DMap k v'
getDeletions (PatchDMap p) m = DMap.intersectionWithKey (\_ v _ -> v) m p
