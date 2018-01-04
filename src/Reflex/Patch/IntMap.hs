{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Reflex.Patch.IntMap where

import Prelude hiding (lookup)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe
import Data.Semigroup
import Reflex.Patch.Class

newtype PatchIntMap a = PatchIntMap (IntMap (Maybe a)) deriving (Functor, Foldable, Traversable, Monoid)

instance Patch (PatchIntMap a) where
  type PatchTarget (PatchIntMap a) = IntMap a
  apply (PatchIntMap p) v = if IntMap.null p then Nothing else Just $
    let removes = IntMap.filter isNothing p
        adds = IntMap.mapMaybe id p
    in IntMap.union adds $ v `IntMap.difference` removes

-- | @a <> b@ will apply the changes of @b@ and then apply the changes of @a@.
-- If the same key is modified by both patches, the one on the left will take
-- precedence.
instance Semigroup (PatchIntMap v) where
  PatchIntMap a <> PatchIntMap b = PatchIntMap $ a `mappend` b --TODO: Add a semigroup instance for Map
  -- PatchMap is idempotent, so stimes n is id for every n
#if MIN_VERSION_semigroups(0,17,0)
  stimes = stimesIdempotentMonoid
#else
  times1p n x = case compare n 0 of
    LT -> error "stimesIdempotentMonoid: negative multiplier"
    EQ -> mempty
    GT -> x
#endif

traverseIntMapPatchWithKey :: Applicative t => (Int -> a -> t b) -> PatchIntMap a -> t (PatchIntMap b)
traverseIntMapPatchWithKey f (PatchIntMap m) = PatchIntMap <$> IntMap.traverseWithKey (\k mv -> traverse (f k) mv) m

patchIntMapNewElements :: PatchIntMap a -> [a]
patchIntMapNewElements (PatchIntMap m) = catMaybes $ IntMap.elems m

patchIntMapNewElementsMap :: PatchIntMap a -> IntMap a
patchIntMapNewElementsMap (PatchIntMap m) = IntMap.mapMaybe id m

getDeletions :: PatchIntMap v -> IntMap v' -> IntMap v'
getDeletions (PatchIntMap m) v = IntMap.intersection v m
