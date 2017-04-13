-- | This module defines the 'Patch' class, which is used by Reflex to manage
-- changes to 'Reflex.Class.Incremental' values.
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Reflex.Patch
{-  ( Patch (..)
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
--  , unPatchDMapWithMove
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
  ) -} where

import Control.Arrow
import Control.Monad.Identity
import Control.Monad.State
import Data.Dependent.Map (DMap, DSum (..), GCompare (..))
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (ShowTag (..), ReadTag (..), EqTag (..), OrdTag (..))
import Data.Foldable
import Data.Function
import Data.Functor.Constant
import Data.Functor.Misc
import Data.GADT.Compare (GEq (..))
import Data.GADT.Show (GShow (..))
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Semigroup (Semigroup (..), stimesIdempotentMonoid, (<>))
import qualified Data.Set as Set
import Data.Some (Some)
import qualified Data.Some as Some
import Data.Tuple
import Data.Typeable (Proxy (..))

-- | A 'Patch' type represents a kind of change made to a datastructure.
class Patch p where
  type PatchTarget p :: *
  -- | Apply the patch @p a@ to the value @a@.  If no change is needed, return
  -- 'Nothing'.
  apply :: p -> PatchTarget p -> Maybe (PatchTarget p)

applyAlways :: Patch p => p -> PatchTarget p -> PatchTarget p
applyAlways p t = fromMaybe t $ apply p t

-- | We can't use @Compose Maybe@ instead of 'ComposeMaybe', because that would
-- make the 'f' parameter have a nominal type role.  We need f to be
-- representational so that we can use safe 'coerce'.
newtype ComposeMaybe f a = ComposeMaybe { getComposeMaybe :: Maybe (f a) } deriving (Show, Eq)

instance EqTag f g => EqTag f (ComposeMaybe g) where
  eqTagToEq f p r = eqTagToEq f (Proxy :: Proxy g) r

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
   = MapEdit_Insert (Maybe k) v -- ^ Insert the given value here
   | MapEdit_Delete (Maybe k) -- ^ Delete the existing value, if any, from here
   | MapEdit_Move (Maybe k) k -- ^ Move the value here from the given key
   deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

mapEditMoved :: MapEdit k v -> Bool
mapEditMoved = isJust . \case
  MapEdit_Insert moved _ -> moved
  MapEdit_Delete moved -> moved
  MapEdit_Move moved _ -> moved

mapEditSetMoved :: Maybe k -> MapEdit k v -> MapEdit k v
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
    [ (1, MapEdit_Move (Just 1) 2)
    , (2, MapEdit_Move (Just 2) 1)
    ]
  ]

knownInvalidPatchMapWithMove :: [PatchMapWithMove Int Int]
knownInvalidPatchMapWithMove =
  [ -- Invalid because we move 2 to 1, but don't specify what to do with 2
    PatchMapWithMove $ Map.singleton 1 $ MapEdit_Move Nothing 2
  , -- Invalid because we have multiple moves from 3
    PatchMapWithMove $ Map.fromList
    [ (1, MapEdit_Move Nothing 3)
    , (2, MapEdit_Move Nothing 3)
    , (3, MapEdit_Delete (Just 1))
    ]
  ]

patchThatSortsMapWith :: Ord k => (v -> v -> Ordering) -> Map k v -> PatchMapWithMove k v
patchThatSortsMapWith cmp m = PatchMapWithMove $ Map.fromList $ catMaybes $ zipWith g unsorted sorted
  where unsorted = Map.toList m
        sorted = sortBy (cmp `on` snd) unsorted
        f (to, _) (from, _) = if to == from then Nothing else
          Just (from, to)
        reverseMapping = Map.fromList $ catMaybes $ zipWith f unsorted sorted
        g (to, _) (from, _) = if to == from then Nothing else
          let Just movingTo = Map.lookup from reverseMapping
          in Just (to, MapEdit_Move (Just movingTo) from)

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
                  Nothing -> return $ MapEdit_Insert (Just undefined) v -- There's no existing value we can take
                  Just fromKs ->
                    if k `Set.member` fromKs
                    then do
                      putRemainingKeys $ Set.delete k fromKs
                      return $ MapEdit_Move (Just undefined) k -- There's an existing value, and it's here, so no patch necessary
                    else do
                      Just (fromK, remainingKeys) <- return $ Set.minView fromKs -- There's an existing value, but it's not here; move it here
                      putRemainingKeys remainingKeys
                      return $ MapEdit_Move (Just undefined) fromK
          Map.traverseWithKey f newByIndex
        unusedOldKeys = fold unusedValuesByValue
        pointlessMove k = \case
          MapEdit_Move _ k' | k == k' -> True
          _ -> False
        keyWasMoved k = if k `Map.member` oldByIndex && not (k `Set.member` unusedOldKeys)
          then Just undefined
          else Nothing
        patch = unsafePatchMapWithMove $ Map.filterWithKey (\k v -> not $ pointlessMove k v) $ Map.mergeWithKey (\k a _ -> Just $ mapEditSetMoved (keyWasMoved k) a) (Map.mapWithKey $ \k -> mapEditSetMoved $ keyWasMoved k) (Map.mapWithKey $ \k _ -> MapEdit_Delete $ keyWasMoved k) insertsAndMoves oldByIndex

data DMapEdit (k :: a -> *) (v :: a -> *) :: a -> * where
  -- | Insert the given value here
  DMapEdit_Insert :: v a -> DMapEdit k v a
  -- | Delete the existing value, if any, from here
  DMapEdit_Delete :: DMapEdit k v a
  -- | Move the value here from the given key
  DMapEdit_Move :: k a -> DMapEdit k v a
  deriving (Show, Read, Eq, Ord)

instance {-# INCOHERENT #-} (GShow k, ShowTag k v) => ShowTag k (DMapEdit k v) where
  showTagToShow k _ r = gshowToShow k $ showTagToShow k (Proxy :: Proxy v) r

instance {-# INCOHERENT #-} GShow k => ShowTag k (ComposeMaybe k) where
  showTagToShow k _ r = gshowToShow k (showTagToShow k (Proxy :: Proxy k) r)

instance (GEq k, EqTag k v) => EqTag k (DMapEdit k v) where
  eqTagToEq k p r = eqTagToEq k (Proxy :: Proxy v) (geqToEq k r)

{-
dmapEditMoved :: DMapEdit k v a -> Bool
dmapEditMoved = isJust . dmapEditMovedTo

dmapEditMovedTo :: DMapEdit k v a -> Maybe (k a)
dmapEditMovedTo = \case
  DMapEdit_Insert moved _ -> moved
  DMapEdit_Delete moved -> moved
  DMapEdit_Move moved _ -> moved
-}

-- | Like 'PatchMapWithMove', but for 'DMap'.
data PatchDMapWithMove k v = PatchDMapWithMove (DMap k (DMapEdit k v)) (DMap k (ComposeMaybe k))
  deriving (Show)

validPatchDMapWithMove :: forall k v. (GCompare k, EqTag k (ComposeMaybe k)) => PatchDMapWithMove k v -> Bool
validPatchDMapWithMove (PatchDMapWithMove dst src) = src == srcFromDst
  where
    srcFromDst :: DMap k (ComposeMaybe k)
    srcFromDst = DMap.fromList $ flip fmap (DMap.toList dst) $ \(to :=> edit) -> case edit of
      DMapEdit_Move from -> from :=> ComposeMaybe (Just to)
      _ -> to :=> ComposeMaybe Nothing

instance (Eq k, Eq v) => EqTag (Const2 k v) Identity where
  eqTagToEq (Const2 _) _ = id

instance (Eq k, Eq v) => EqTag (Const2 k v) (Const2 k v) where
  eqTagToEq (Const2 _) _ = id

moveDMapKey :: GCompare k => k a -> k a -> PatchDMapWithMove k v
moveDMapKey src dst = case src `geq` dst of
  Nothing -> PatchDMapWithMove (DMap.fromList [dst :=> DMapEdit_Move src, src :=> DMapEdit_Delete]) (DMap.fromList [src :=> ComposeMaybe (Just dst), dst :=> ComposeMaybe Nothing])
  Just _ -> mempty

deleteDMapKey :: k a -> PatchDMapWithMove k v
deleteDMapKey k = PatchDMapWithMove (DMap.singleton k DMapEdit_Delete) $ DMap.singleton k $ ComposeMaybe Nothing

{-
k1, k2 :: Const2 Int () ()
k1 = Const2 1
k2 = Const2 2
p1, p2 :: PatchDMapWithMove (Const2 Int ()) Identity
p1 = moveDMapKey k1 k2
p2 = moveDMapKey k2 k1
p12 = p1 <> p2
p21 = p2 <> p1
p12Slow = p1 `mappendPatchDMapWithMoveSlow` p2
p21Slow = p2 `mappendPatchDMapWithMoveSlow` p1

testPatchDMapWithMove = do
  print p1
  print p2
  print $ p12 == deleteDMapKey k1
  print $ p21 == deleteDMapKey k2
  print $ p12Slow == deleteDMapKey k1
  print $ p21Slow == deleteDMapKey k2

dst (PatchDMapWithMove x _) = x
src (PatchDMapWithMove _ x) = x
-}

instance (EqTag k k, EqTag k v, EqTag k (ComposeMaybe k)) => Eq (PatchDMapWithMove k v) where
  PatchDMapWithMove dstA srcA == PatchDMapWithMove dstB srcB = dstA == dstB && srcA == srcB

data Pair1 f g a = Pair1 (f a) (g a)

instance GCompare k => Semigroup (PatchDMapWithMove k v) where
  (<>) = mappend

instance GCompare k => Monoid (PatchDMapWithMove k v) where
  mempty = PatchDMapWithMove mempty mempty
  PatchDMapWithMove dstAfter srcAfter `mappend` PatchDMapWithMove dstBefore srcBefore = PatchDMapWithMove dst src
    where
      connections = DMap.toList $ DMap.intersectionWithKey (const Pair1) srcAfter dstBefore
      f :: DSum k (Pair1 (ComposeMaybe k) (DMapEdit k v)) -> Maybe (DSum k (ComposeMaybe (DMapEdit k v)))
      f (k :=> Pair1 (ComposeMaybe mToAfter) editBefore) = case mToAfter of
        Nothing -> Nothing -- Since the result of this connection was simply discarded, we don't need to change dst
        Just toAfter -> Just $ (toAfter :=>) $ ComposeMaybe $ case editBefore of
          DMapEdit_Move fromBefore -> if isJust $ fromBefore `geq` toAfter
            then Nothing -- The two moves have canceled out, so eliminate the destination entry
            else Just editBefore
          nonMove -> Just editBefore
      dstPatch = PatchDMap $ DMap.fromList $ catMaybes $ f <$> connections
      dst = applyAlways dstPatch $ DMap.union dstAfter dstBefore
      g :: DSum k (Pair1 (ComposeMaybe k) (DMapEdit k v)) -> Maybe (DSum k (ComposeMaybe (ComposeMaybe k)))
      g (k :=> Pair1 (ComposeMaybe mToAfter) editBefore) = case editBefore of
        DMapEdit_Move fromBefore -> Just $ (fromBefore :=>) $ ComposeMaybe $ case mToAfter of
          Nothing -> Just $ ComposeMaybe Nothing -- The item is destroyed in the second patch, so indicate that it is destroyed in the source map
          Just toAfter -> if isJust $ toAfter `geq` fromBefore
            then Nothing -- The two moves have canceled out, so eliminate the source entry
            else Just $ ComposeMaybe mToAfter
        _ -> Nothing --TODO: Is this right?
      srcPatch = PatchDMap $ DMap.fromList $ catMaybes $ g <$> connections
      src = applyAlways srcPatch $ DMap.union srcBefore srcAfter

mappendPatchDMapWithMoveSlow :: forall k v. (ShowTag k v, GCompare k) => PatchDMapWithMove k v -> PatchDMapWithMove k v -> PatchDMapWithMove k v
PatchDMapWithMove dstAfter srcAfter `mappendPatchDMapWithMoveSlow` PatchDMapWithMove dstBefore srcBefore = PatchDMapWithMove dst src
  where
    getDstAction k m = fromMaybe (DMapEdit_Move k) $ DMap.lookup k m -- Any key that isn't present is treated as that key moving to itself
    removeRedundantDst toKey (DMapEdit_Move fromKey) | isJust (toKey `geq` fromKey) = Nothing
    removeRedundantDst _ a = Just a
    f :: forall a. k a -> DMapEdit k v a -> Maybe (DMapEdit k v a)
    f toKey _ = removeRedundantDst toKey $ case getDstAction toKey dstAfter of
      DMapEdit_Move fromKey -> getDstAction fromKey dstBefore
      nonMove -> nonMove
    dst = DMap.mapMaybeWithKey f $ DMap.union dstAfter dstBefore
    getSrcAction k m = fromMaybe (ComposeMaybe $ Just k) $ DMap.lookup k m
    removeRedundantSrc fromKey (ComposeMaybe (Just toKey)) | isJust (fromKey `geq` toKey) = Nothing
    removeRedundantSrc _ a = Just a
    g :: forall a. k a -> ComposeMaybe k a -> Maybe (ComposeMaybe k a)
    g fromKey _ = removeRedundantSrc fromKey $ case getSrcAction fromKey srcBefore of
      ComposeMaybe Nothing -> ComposeMaybe Nothing
      ComposeMaybe (Just toKeyBefore) -> getSrcAction toKeyBefore srcAfter
    src = DMap.mapMaybeWithKey g $ DMap.union srcAfter srcBefore

{-
unPatchDMapWithMove :: PatchDMapWithMove k v -> DMap k (DMapEdit k v)
unPatchDMapWithMove (PatchDMapWithMove p) = p

-- | Warning: when using this function, you must ensure that the invariants of
-- 'PatchDMapWithMove' are preserved; they will not be checked.
unsafePatchDMapWithMove :: DMap k (DMapEdit k v) -> PatchDMapWithMove k v
unsafePatchDMapWithMove = PatchDMapWithMove
-}

{-
instance GCompare k => Monoid (PatchDMapWithMove k v) where
  mempty = PatchDMapWithMove mempty
  PatchDMapWithMove a `mappend` PatchDMapWithMove b = PatchDMapWithMove $ DMap.unionWithKey f a b
    where f :: forall a. k a -> DMapEdit k v a -> DMapEdit k v a -> DMapEdit k v a
          f _ ea eb = case (ea, eb) of
            (DMapEdit_Delete _, DMapEdit_Delete moved) -> DMapEdit_Delete $ do
              dst <- moved
              case DMap.lookup dst a of
                Nothing -> return dst
                Just dstChange -> dmapEditMovedTo dstChange
            (DMapEdit_Move _ from, DMapEdit_Delete moved) -> DMapEdit_Move moved from
            (DMapEdit_Insert _ v, DMapEdit_Delete moved) -> DMapEdit_Insert moved v
            (DMapEdit_Delete _, DMapEdit_Insert moved v) -> DMapEdit_Delete moved
            (DMapEdit_Move _ from, DMapEdit_Insert moved v) -> DMapEdit_Move moved from
            (DMapEdit_Insert _ newV, DMapEdit_Insert moved oldV) -> DMapEdit_Insert moved newV
            (DMapEdit_Delete _, DMapEdit_Move moved k) -> DMapEdit_Delete moved
-}

{-

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
          DMapEdit_Insert moved v -> MapEdit_Insert (Some.This <$> moved) $ f v
          DMapEdit_Delete moved -> MapEdit_Delete $ Some.This <$> moved
          DMapEdit_Move moved k -> MapEdit_Move (Some.This <$> moved) $ Some.This k

patchDMapWithMoveToPatchMapWithMoveWith :: forall k f v v'. (f v -> v') -> PatchDMapWithMove (Const2 k v) f -> PatchMapWithMove k v'
patchDMapWithMoveToPatchMapWithMoveWith f (PatchDMapWithMove p) = PatchMapWithMove $ dmapToMapWith g p
  where g :: DMapEdit (Const2 k v) f v -> MapEdit k v'
        g = \case
          DMapEdit_Insert moved v -> MapEdit_Insert (unConst2 <$> moved) $ f v
          DMapEdit_Delete moved -> MapEdit_Delete $ unConst2 <$> moved
          DMapEdit_Move moved (Const2 k) -> MapEdit_Move (unConst2 <$> moved) k

const2PatchDMapWithMoveWith :: forall k v f a. (v -> f a) -> PatchMapWithMove k v -> PatchDMapWithMove (Const2 k a) f
const2PatchDMapWithMoveWith f (PatchMapWithMove p) = PatchDMapWithMove $ DMap.fromDistinctAscList $ g <$> Map.toAscList p
  where g :: (k, MapEdit k v) -> DSum (Const2 k a) (DMapEdit (Const2 k a) f)
        g (k, e) = Const2 k :=> case e of
          MapEdit_Insert moved v -> DMapEdit_Insert (Const2 <$> moved) $ f v
          MapEdit_Delete moved -> DMapEdit_Delete $ Const2 <$> moved
          MapEdit_Move moved fromKey -> DMapEdit_Move (Const2 <$> moved) $ Const2 fromKey

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
-}
