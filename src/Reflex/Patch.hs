-- | This module defines the 'Patch' class, which is used by Reflex to manage
-- changes to 'Reflex.Class.Incremental' values.
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Reflex.Patch
  ( Patch (..)
  , PatchDMap (..)
  , mapPatchDMap
  , traversePatchDMap
  , traversePatchDMapWithKey
  , weakenPatchDMapWith
  , patchDMapToPatchMapWith
  , const2PatchDMapWith
  , ComposeMaybe (..)
  , PatchMap (..)
  , patchMapNewElements
  , Group (..)
  , Additive
  , AdditivePatch (..)
  , PatchMapWithMove
  , unPatchMapWithMove
  , unsafePatchMapWithMove
  , patchMapWithMoveNewElements
  , MapEdit (..)
  , mapEditMoved
  , mapEditSetMoved
  , PatchDMapWithMove
  , unPatchDMapWithMove
  , unsafePatchDMapWithMove
  , patchThatSortsMapWith
  , patchThatChangesAndSortsMapWith
  , mapPatchDMapWithMove
  , traversePatchDMapWithMove
  , traversePatchDMapWithMoveWithKey
  , weakenPatchDMapWithMoveWith
  , patchDMapWithMoveToPatchMapWithMoveWith
  , const2PatchDMapWithMoveWith
  , DMapEdit (..)
  , dmapEditMoved
    -- * Test functions
  , validPatchMapWithMove
  , knownValidPatchMapWithMove
  , knownInvalidPatchMapWithMove
  ) where

import Control.Arrow
import Control.Monad.Identity
import Control.Monad.State
import Data.Dependent.Map (DMap, DSum (..), GCompare (..))
import qualified Data.Dependent.Map as DMap
import Data.Foldable
import Data.Function
import Data.Functor.Constant
import Data.Functor.Misc
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Semigroup (Semigroup (..), stimesIdempotentMonoid, (<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Some (Some)
import qualified Data.Some as Some
import Data.Tuple

-- | A 'Patch' type represents a kind of change made to a datastructure.
class Patch p where
  type PatchTarget p :: *
  -- | Apply the patch @p a@ to the value @a@.  If no change is needed, return
  -- 'Nothing'.
  apply :: p -> PatchTarget p -> Maybe (PatchTarget p)

-- | We can't use @Compose Maybe@ instead of 'ComposeMaybe', because that would
-- make the 'f' parameter have a nominal type role.  We need f to be
-- representational so that we can use safe 'coerce'.
newtype ComposeMaybe f a = ComposeMaybe { getComposeMaybe :: Maybe (f a) }

deriving instance Functor f => Functor (ComposeMaybe f)

instance Patch (Identity a) where
  type PatchTarget (Identity a) = a
  apply (Identity a) _ = Just a

-- | A set of changes to a 'DMap'.  Any element may be inserted/updated or
-- deleted.
newtype PatchDMap k v = PatchDMap (DMap k (ComposeMaybe v))

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

-- | A set of changes to a 'Map'.  Any element may be inserted/updated or
-- deleted.
newtype PatchMap k v = PatchMap { unPatchMap :: Map k (Maybe v) }

instance Ord k => Patch (PatchMap k v) where
  type PatchTarget (PatchMap k v) = Map k v
  apply (PatchMap p) old = Just $! insertions `Map.union` (old `Map.difference` deletions) --TODO: return Nothing sometimes --Note: the strict application here is critical to ensuring that incremental merges don't hold onto all their prerequisite events forever; can we make this more robust?
    where insertions = Map.mapMaybeWithKey (const id) p
          deletions = Map.mapMaybeWithKey (const nothingToJust) p
          nothingToJust = \case
            Nothing -> Just ()
            Just _ -> Nothing

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

instance Ord k => Monoid (PatchMap k v) where
  mempty = PatchMap mempty
  mappend = (<>)

instance Functor (PatchMap k) where
  fmap f = PatchMap . fmap (fmap f) . unPatchMap

-- | Returns all the new elements that will be added to the 'Map'
patchMapNewElements :: PatchMap k v -> [v]
patchMapNewElements (PatchMap p) = catMaybes $ Map.elems p

---- Patches based on commutative groups

-- | A 'Group' is a 'Monoid' where every element has an inverse.
class (Semigroup q, Monoid q) => Group q where
  negateG :: q -> q
  (~~) :: q -> q -> q
  r ~~ s = r <> negateG s

-- | An 'Additive' 'Semigroup' is one where (<>) is commutative
class Semigroup q => Additive q where

-- | The elements of an 'Additive' 'Semigroup' can be considered as patches of their own type.
newtype AdditivePatch p = AdditivePatch { unAdditivePatch :: p }

instance Additive p => Patch (AdditivePatch p) where
  type PatchTarget (AdditivePatch p) = p
  apply (AdditivePatch p) q = Just $ p <> q

data MapEdit k v
   = MapEdit_Insert Bool v -- ^ Insert the given value here
   | MapEdit_Delete Bool -- ^ Delete the existing value, if any, from here
   | MapEdit_Move Bool k -- ^ Move the value here from the given key
   deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

mapEditMoved :: MapEdit k v -> Bool
mapEditMoved = \case
  MapEdit_Insert moved _ -> moved
  MapEdit_Delete moved -> moved
  MapEdit_Move moved _ -> moved

mapEditSetMoved :: Bool -> MapEdit k v -> MapEdit k v
mapEditSetMoved moved = \case
  MapEdit_Insert _ v -> MapEdit_Insert moved v
  MapEdit_Delete _ -> MapEdit_Delete moved
  MapEdit_Move _ k -> MapEdit_Move moved k

-- | Patch a DMap with additions, deletions, and moves.
-- Invariants:
--   * The same source key may not be moved to two destination keys.
--   * Any key that is the source of a Move must also be edited
--     (otherwise, the Move would duplicate it)
data PatchMapWithMove k v = PatchMapWithMove (Map k (MapEdit k v)) deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

unPatchMapWithMove :: PatchMapWithMove k v -> Map k (MapEdit k v)
unPatchMapWithMove (PatchMapWithMove p) = p

-- | Warning: when using this function, you must ensure that the invariants of
-- 'PatchMapWithMove' are preserved; they will not be checked.
unsafePatchMapWithMove :: Map k (MapEdit k v) -> PatchMapWithMove k v
unsafePatchMapWithMove = PatchMapWithMove

instance Ord k => Patch (PatchMapWithMove k v) where
  type PatchTarget (PatchMapWithMove k v) = Map k v
  apply (PatchMapWithMove p) old = Just $! insertions `Map.union` (old `Map.difference` deletions) --TODO: return Nothing sometimes --Note: the strict application here is critical to ensuring that incremental merges don't hold onto all their prerequisite events forever; can we make this more robust?
    where insertions = flip Map.mapMaybeWithKey p $ const $ \case
            MapEdit_Insert _ v -> Just v
            MapEdit_Move _ k -> Map.lookup k old
            MapEdit_Delete _ -> Nothing
          deletions = flip Map.mapMaybeWithKey p $ const $ \case
            MapEdit_Delete _ -> Just ()
            _ -> Nothing

-- | Returns all the new elements that will be added to the 'Map'
patchMapWithMoveNewElements :: PatchMapWithMove k v -> [v]
patchMapWithMoveNewElements (PatchMapWithMove p) = catMaybes $ flip fmap (Map.elems p) $ \case
  MapEdit_Insert _ v -> Just v
  MapEdit_Move _ _ -> Nothing
  MapEdit_Delete _ -> Nothing

validPatchMapWithMove :: Ord k => PatchMapWithMove k v -> Bool
validPatchMapWithMove (PatchMapWithMove p) =
  let moveSources = catMaybes $ flip fmap (Map.elems p) $ \case
        MapEdit_Move _ k -> Just k
        _ -> Nothing
  in and
  [ -- All Moves must have unique source keys - a single source key can't be moved to two destinations
    all (==1) $ Map.fromListWith (+) $ fmap (\k -> (k, 1 :: Integer )) moveSources
  , -- All Move source keys must also be patched - since the value's being moved elsewhere, the original must either be Deleted or replaced (with Insert or Move)
    all (isJust . flip Map.lookup p) moveSources
  ]

knownValidPatchMapWithMove :: [PatchMapWithMove Int Int]
knownValidPatchMapWithMove =
  [ PatchMapWithMove mempty
  , PatchMapWithMove $ Map.fromList
    [ (1, MapEdit_Move True 2)
    , (2, MapEdit_Move True 1)
    ]
  ]

knownInvalidPatchMapWithMove :: [PatchMapWithMove Int Int]
knownInvalidPatchMapWithMove =
  [ -- Invalid because we move 2 to 1, but don't specify what to do with 2
    PatchMapWithMove $ Map.singleton 1 $ MapEdit_Move False 2
  , -- Invalid because we have multiple moves from 3
    PatchMapWithMove $ Map.fromList
    [ (1, MapEdit_Move False 3)
    , (2, MapEdit_Move False 3)
    , (3, MapEdit_Delete True)
    ]
  ]

patchThatSortsMapWith :: Ord k => (v -> v -> Ordering) -> Map k v -> PatchMapWithMove k v
patchThatSortsMapWith cmp m = PatchMapWithMove $ Map.fromList $ catMaybes $ zipWith g l $ sortBy (cmp `on` snd) l
  where l = Map.toList m
        g (to, _) (from, _) = if to == from
          then Nothing
          else Just (to, MapEdit_Move True from)

patchThatChangesAndSortsMapWith :: (Ord k, Ord v) => (v -> v -> Ordering) -> Map k v -> Map k v -> PatchMapWithMove k v
patchThatChangesAndSortsMapWith cmp oldByIndex newByIndexUnsorted = patch
  where oldByValue = Map.fromListWith Set.union $ swap . first Set.singleton <$> Map.toList oldByIndex
        newList = Map.toList newByIndexUnsorted
        newByIndex = Map.fromList $ zip (fst <$> newList) $ sortBy cmp $ snd <$> newList
        (insertsAndMoves, unusedValuesByValue) = flip runState oldByValue $ do
          let f k v = do
                remainingValues <- get
                let putRemainingKeys remainingKeys = put $ if Set.null remainingKeys
                      then Map.delete v remainingValues
                      else Map.insert v remainingKeys remainingValues
                case Map.lookup v remainingValues of
                  Nothing -> return $ MapEdit_Insert True v -- There's no existing value we can take
                  Just fromKs ->
                    if k `Set.member` fromKs
                    then do
                      putRemainingKeys $ Set.delete k fromKs
                      return $ MapEdit_Move True k -- There's an existing value, and it's here, so no patch necessary
                    else do
                      Just (fromK, remainingKeys) <- return $ Set.minView fromKs -- There's an existing value, but it's not here; move it here
                      putRemainingKeys remainingKeys
                      return $ MapEdit_Move True fromK
          Map.traverseWithKey f newByIndex
        unusedOldKeys = fold unusedValuesByValue
        pointlessMove k = \case
          MapEdit_Move _ k' | k == k' -> True
          _ -> False
        keyWasMoved k = k `Map.member` oldByIndex && not (k `Set.member` unusedOldKeys)
        patch = unsafePatchMapWithMove $ Map.filterWithKey (\k v -> not $ pointlessMove k v) $ Map.mergeWithKey (\k a _ -> Just $ mapEditSetMoved (keyWasMoved k) a) (Map.mapWithKey $ \k -> mapEditSetMoved $ keyWasMoved k) (Map.mapWithKey $ \k _ -> MapEdit_Delete $ keyWasMoved k) insertsAndMoves oldByIndex

data DMapEdit (k :: a -> *) (v :: a -> *) :: a -> * where
  -- | Insert the given value here
  DMapEdit_Insert :: Bool -> v a -> DMapEdit k v a
  -- | Delete the existing value, if any, from here
  DMapEdit_Delete :: Bool -> DMapEdit k v a
  -- | Move the value here from the given key
  DMapEdit_Move :: Bool -> k a -> DMapEdit k v a

dmapEditMoved :: DMapEdit k v a -> Bool
dmapEditMoved = \case
  DMapEdit_Insert moved _ -> moved
  DMapEdit_Delete moved -> moved
  DMapEdit_Move moved _ -> moved

-- | Like 'PatchMapWithMove', but for 'DMap'.
newtype PatchDMapWithMove k v = PatchDMapWithMove (DMap k (DMapEdit k v))

unPatchDMapWithMove :: PatchDMapWithMove k v -> DMap k (DMapEdit k v)
unPatchDMapWithMove (PatchDMapWithMove p) = p

-- | Warning: when using this function, you must ensure that the invariants of
-- 'PatchDMapWithMove' are preserved; they will not be checked.
unsafePatchDMapWithMove :: DMap k (DMapEdit k v) -> PatchDMapWithMove k v
unsafePatchDMapWithMove = PatchDMapWithMove

mapPatchDMapWithMove :: forall k v v'. (forall a. v a -> v' a) -> PatchDMapWithMove k v -> PatchDMapWithMove k v'
mapPatchDMapWithMove f (PatchDMapWithMove p) = PatchDMapWithMove $ DMap.map g p
  where g :: forall a. DMapEdit k v a -> DMapEdit k v' a
        g = \case
          DMapEdit_Insert moved v -> DMapEdit_Insert moved $ f v
          DMapEdit_Delete moved -> DMapEdit_Delete moved
          DMapEdit_Move moved k -> DMapEdit_Move moved k

traversePatchDMapWithMove :: forall m k v v'. Applicative m => (forall a. v a -> m (v' a)) -> PatchDMapWithMove k v -> m (PatchDMapWithMove k v')
traversePatchDMapWithMove f = traversePatchDMapWithMoveWithKey $ const f

traversePatchDMapWithMoveWithKey :: forall m k v v'. Applicative m => (forall a. k a -> v a -> m (v' a)) -> PatchDMapWithMove k v -> m (PatchDMapWithMove k v')
traversePatchDMapWithMoveWithKey f (PatchDMapWithMove p) = PatchDMapWithMove <$> DMap.traverseWithKey g p
  where g :: forall a. k a -> DMapEdit k v a -> m (DMapEdit k v' a)
        g k = \case
          DMapEdit_Insert moved v -> DMapEdit_Insert moved <$> f k v
          DMapEdit_Delete moved -> pure $ DMapEdit_Delete moved
          DMapEdit_Move moved fromKey -> pure $ DMapEdit_Move moved fromKey

weakenPatchDMapWithMoveWith :: forall k v v'. (forall a. v a -> v') -> PatchDMapWithMove k v -> PatchMapWithMove (Some k) v'
weakenPatchDMapWithMoveWith f (PatchDMapWithMove p) = PatchMapWithMove $ weakenDMapWith g p
  where g :: forall a. DMapEdit k v a -> MapEdit (Some k) v'
        g = \case
          DMapEdit_Insert moved v -> MapEdit_Insert moved $ f v
          DMapEdit_Delete moved -> MapEdit_Delete moved
          DMapEdit_Move moved k -> MapEdit_Move moved $ Some.This k

patchDMapWithMoveToPatchMapWithMoveWith :: forall k f v v'. (f v -> v') -> PatchDMapWithMove (Const2 k v) f -> PatchMapWithMove k v'
patchDMapWithMoveToPatchMapWithMoveWith f (PatchDMapWithMove p) = PatchMapWithMove $ dmapToMapWith g p
  where g :: DMapEdit (Const2 k v) f v -> MapEdit k v'
        g = \case
          DMapEdit_Insert moved v -> MapEdit_Insert moved $ f v
          DMapEdit_Delete moved -> MapEdit_Delete moved
          DMapEdit_Move moved (Const2 k) -> MapEdit_Move moved k

const2PatchDMapWithMoveWith :: forall k v f a. (v -> f a) -> PatchMapWithMove k v -> PatchDMapWithMove (Const2 k a) f
const2PatchDMapWithMoveWith f (PatchMapWithMove p) = PatchDMapWithMove $ DMap.fromDistinctAscList $ g <$> Map.toAscList p
  where g :: (k, MapEdit k v) -> DSum (Const2 k a) (DMapEdit (Const2 k a) f)
        g (k, e) = Const2 k :=> case e of
          MapEdit_Insert moved v -> DMapEdit_Insert moved $ f v
          MapEdit_Delete moved -> DMapEdit_Delete moved
          MapEdit_Move moved fromKey -> DMapEdit_Move moved $ Const2 fromKey

instance GCompare k => Patch (PatchDMapWithMove k v) where
  type PatchTarget (PatchDMapWithMove k v) = DMap k v
  apply (PatchDMapWithMove p) old = Just $! insertions `DMap.union` (old `DMap.difference` deletions) --TODO: return Nothing sometimes --Note: the strict application here is critical to ensuring that incremental merges don't hold onto all their prerequisite events forever; can we make this more robust?
    where insertions = DMap.mapMaybeWithKey insertFunc p
          insertFunc :: forall a. k a -> DMapEdit k v a -> Maybe (v a)
          insertFunc _ = \case
            DMapEdit_Insert _ v -> Just v
            DMapEdit_Move _ k -> DMap.lookup k old
            DMapEdit_Delete _ -> Nothing
          deletions = DMap.mapMaybeWithKey deleteFunc p
          deleteFunc :: forall a. k a -> DMapEdit k v a -> Maybe (Constant () a)
          deleteFunc _ = \case
            DMapEdit_Delete _ -> Just $ Constant ()
            _ -> Nothing
