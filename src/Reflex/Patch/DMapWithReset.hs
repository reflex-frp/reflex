{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wall #-}

-- | 'Patch'es on 'DMap' that consist only of insertions (or overwrites) and deletions.
module Reflex.Patch.DMapWithReset where

import Reflex.Patch.Class

import Data.Dependent.Map (DMap, GCompare (..))
import qualified Data.Dependent.Map as DMap
import Data.Semigroup (Semigroup (..))
import Data.Constraint.Extras

-- | A set of changes to a 'DMap'.  Any element may be inserted/updated or deleted.
-- Insertions are represented as @'ComposeMaybe' (Just value)@,
-- while deletions are represented as @'ComposeMaybe' Nothing@.
newtype PatchDMapWithReset k p = PatchDMapWithReset { unPatchDMapWithReset :: DMap k (By p) }

-- | Holds the information about each key: where its new value should come from,
-- and where its old value should go to
data By p a
  = By_Insert (PatchTarget (p a)) -- ^ Insert the given value here
  | By_Delete -- ^ Delete the existing value, if any, from here
  | By_Patch (p a) -- ^ Patch the value here with the given patch

instance (Semigroup (p a), Patch (p a)) => Semigroup (By p a) where
  x@(By_Insert _) <> _ = x
  By_Delete <> _ = By_Delete
  By_Patch x <> By_Insert y = By_Insert (applyAlways x y)
  By_Patch x <> By_Patch y = By_Patch (x <> y)
  By_Patch _ <> By_Delete = By_Delete

instance (Semigroup (p a), Monoid (p a), Patch (p a)) => Monoid (By p a) where
  mappend = (<>)
  mempty = By_Patch mempty

instance
    ( GCompare k
    , Has' Semigroup k p
    , Has' Patch k p
    )
    => Semigroup (PatchDMapWithReset k p) where
  PatchDMapWithReset xs <> PatchDMapWithReset ys = PatchDMapWithReset $ DMap.unionWithKey
    (\k -> has' @Patch @p k
         $ has' @Semigroup @p k
         $ (<>)) xs ys

instance
    ( GCompare k
    , Has' Semigroup k p
    , Has' Patch k p
    )
    => Monoid (PatchDMapWithReset k p) where
  mappend = (<>)
  mempty = PatchDMapWithReset DMap.empty

class (Patch (p a), PatchTarget (p a) ~ Patches1LocallyTarget p a) => Patches1Locally p a where
  type Patches1LocallyTarget p :: k -> *

data These1 f g x
  = This1 (f x)
  | That1 (g x)
  | These1 (f x) (g x)

mergeWithKey
  :: forall k v1 v2 v.
  (GCompare k)
  => (forall x. k x -> v1 x -> Maybe (v x))
  -> (forall x. k x -> v2 x -> Maybe (v x))
  -> (forall x. k x -> v1 x -> v2 x -> Maybe (v x))
  -> DMap k v1 -> DMap k v2 -> DMap k v
mergeWithKey f g fg = \xs ys -> DMap.mapMaybeWithKey onlyThat $ DMap.unionWithKey doIt (DMap.map This1 xs) (DMap.map That1 ys)
  where
    doIt _ (This1 xs) (That1 ys) = These1 xs ys
    doIt _ _ _ = error "mergeWithKey misalligned keys"

    onlyThat :: forall x. k x -> These1 v1 v2 x -> Maybe (v x)
    onlyThat k = \case
      This1 xs -> f k xs
      That1 ys -> g k ys
      These1 xs ys -> fg k xs ys
{-# INLINE mergeWithKey #-}

-- | Apply the insertions or deletions to a given 'DMap'.
instance (GCompare k, Has (Patches1Locally p) k) => Patch (PatchDMapWithReset k p) where

  type PatchTarget (PatchDMapWithReset k p) = DMap k (Patches1LocallyTarget p)

  apply = go
    where
      go :: PatchDMapWithReset k p -> DMap k (Patches1LocallyTarget p) -> Maybe (DMap k (Patches1LocallyTarget p))
      go (PatchDMapWithReset diff) old = Just $! mergeWithKey (\_ -> Just) inserts updates old diff
        where
          updates :: forall x. k x -> Patches1LocallyTarget p x -> By p x -> Maybe (Patches1LocallyTarget p x)
          updates k ys = has @(Patches1Locally p) k $ \case
            By_Insert x -> Just x
            By_Delete -> Nothing
            By_Patch x -> Just $ applyAlways x ys

          inserts :: forall x. k x -> By p x -> Maybe (Patches1LocallyTarget p x)
          inserts k = has @(Patches1Locally p) k $ \case
            By_Insert x -> Just x
            By_Delete -> Nothing
            By_Patch _ -> Nothing

deriving instance (Patch (p a), Eq (p a), Eq (PatchTarget (p a))) => Eq (By p a)
deriving instance (Patch (p a), Show (p a), Show (PatchTarget (p a))) => Show (By p a)
deriving instance (Eq (DMap k (By p))) => Eq (PatchDMapWithReset k p)
deriving instance (Show (DMap k (By p))) => Show (PatchDMapWithReset k p)
