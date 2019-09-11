{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- | 'Patch'es on 'Map' that can insert, delete, and move values from one key to
-- another
module Reflex.Patch.MapWithMove2 where

import Reflex.Patch.Class

import Control.Arrow
import Control.Monad.State
import Data.Foldable
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Semigroup
import qualified Data.Set as Set
import Data.These
import Data.Tuple

-- | Patch a DMap with additions, deletions, and moves.  Invariant: If key @k1@
-- is coming from @From_Move k2@, then key @k2@ should be going to @Just k1@,
-- and vice versa.  There should never be any unpaired From/To keys.
newtype PatchMapWithMove2 k p = PatchMapWithMove2 (Map k (NodeInfo k p))
deriving instance (Show k, Show p, Show (PatchTarget p)) => Show (PatchMapWithMove2 k p)
deriving instance (Eq k, Eq p, Eq (PatchTarget p)) => Eq (PatchMapWithMove2 k p)
deriving instance (Ord k, Ord p, Ord (PatchTarget p)) => Ord (PatchMapWithMove2 k p)

-- | Holds the information about each key: where its new value should come from,
-- and where its old value should go to
data NodeInfo k p = NodeInfo
  { _nodeInfo_from :: !(From k p)
    -- ^ Where do we get the new value for this key?
  , _nodeInfo_to :: !(To k)
    -- ^ If the old value is being kept (i.e. moved rather than deleted or
    -- replaced), where is it going?
  }
deriving instance (Show k, Show p, Show (PatchTarget p)) => Show (NodeInfo k p)
deriving instance (Read k, Read p, Read (PatchTarget p)) => Read (NodeInfo k p)
deriving instance (Eq k, Eq p, Eq (PatchTarget p)) => Eq (NodeInfo k p)
deriving instance (Ord k, Ord p, Ord (PatchTarget p)) => Ord (NodeInfo k p)

-- | Create a 'PatchMapWithMove2', validating it
patchMapWithMove2 :: Ord k => Map k (NodeInfo k p) -> Maybe (PatchMapWithMove2 k p)
patchMapWithMove2 m = if valid then Just $ PatchMapWithMove2 m else Nothing
  where valid = forwardLinks == backwardLinks
        forwardLinks = Map.mapMaybe _nodeInfo_to m
        backwardLinks = Map.fromList $ catMaybes $ flip fmap (Map.toList m) $ \(to, p) ->
          case _nodeInfo_from p of
            From_Move from _ -> Just (from, to)
            _ -> Nothing

-- | Create a 'PatchMapWithMove2' that inserts everything in the given 'Map'
patchMapWithMove2InsertAll :: Map k (PatchTarget p) -> PatchMapWithMove2 k p
patchMapWithMove2InsertAll m = PatchMapWithMove2 $ flip fmap m $ \v -> NodeInfo
  { _nodeInfo_from = From_Insert v
  , _nodeInfo_to = Nothing
  }

-- | Extract the internal representation of the 'PatchMapWithMove2'
unPatchMapWithMove2 :: PatchMapWithMove2 k p -> Map k (NodeInfo k p)
unPatchMapWithMove2 (PatchMapWithMove2 p) = p

-- | Warning: when using this function, you must ensure that the invariants of
-- 'PatchMapWithMove2' are preserved; they will not be checked.
unsafePatchMapWithMove2 :: Map k (NodeInfo k p) -> PatchMapWithMove2 k p
unsafePatchMapWithMove2 = PatchMapWithMove2

-- | Describe how a key's new value should be produced
data From k p
   = From_Insert (PatchTarget p) -- ^ Insert the given value here
   | From_Delete -- ^ Delete the existing value, if any, from here
   | From_Move !k !p -- ^ Move the value here from the given key, and apply the given patch

deriving instance (Show k, Show p, Show (PatchTarget p)) => Show (From k p)
deriving instance (Read k, Read p, Read (PatchTarget p)) => Read (From k p)
deriving instance (Eq k, Eq p, Eq (PatchTarget p)) => Eq (From k p)
deriving instance (Ord k, Ord p, Ord (PatchTarget p)) => Ord (From k p)

-- | Describe where a key's old value will go.  If this is 'Just', that means
-- the key's old value will be moved to the given other key; if it is 'Nothing',
-- that means it will be deleted.
type To = Maybe

-- | Apply the insertions, deletions, and moves to a given 'Map'
instance (Ord k, Patch p) => Patch (PatchMapWithMove2 k p) where
  type PatchTarget (PatchMapWithMove2 k p) = Map k (PatchTarget p)
  apply (PatchMapWithMove2 m) old = Just $! insertions `Map.union` (old `Map.difference` deletions) --TODO: return Nothing sometimes --Note: the strict application here is critical to ensuring that incremental merges don't hold onto all their prerequisite events forever; can we make this more robust?
    where insertions = flip Map.mapMaybeWithKey m $ \_ ni -> case _nodeInfo_from ni of
            From_Insert v -> Just v
            From_Move k p -> applyAlways p <$> Map.lookup k old
            From_Delete -> Nothing
          deletions = flip Map.mapMaybeWithKey m $ \_ ni -> case _nodeInfo_from ni of
            From_Delete -> Just ()
            _ -> Nothing

-- | Returns all the new elements that will be added to the 'Map'
patchMapWithMove2NewElements :: PatchMapWithMove2 k p -> [PatchTarget p]
patchMapWithMove2NewElements = Map.elems . patchMapWithMove2NewElementsMap

patchMapWithMove2NewElementsMap :: PatchMapWithMove2 k p -> Map k (PatchTarget p)
patchMapWithMove2NewElementsMap (PatchMapWithMove2 p) = Map.mapMaybe f p
  where f ni = case _nodeInfo_from ni of
          From_Insert v -> Just v
          From_Move _ _ -> Nothing
          From_Delete -> Nothing

-- | Create a 'PatchMapWithMove2' that, if applied to the given 'Map', will sort
-- its values using the given ordering function.  The set keys of the 'Map' is
-- not changed.
patchThatSortsMapWith :: (Ord k, Monoid p) => (PatchTarget p -> PatchTarget p -> Ordering) -> Map k (PatchTarget p) -> PatchMapWithMove2 k p
patchThatSortsMapWith cmp m = PatchMapWithMove2 $ Map.fromList $ catMaybes $ zipWith g unsorted sorted
  where unsorted = Map.toList m
        sorted = sortBy (cmp `on` snd) unsorted
        f (to, _) (from, _) = if to == from then Nothing else
          Just (from, to)
        reverseMapping = Map.fromList $ catMaybes $ zipWith f unsorted sorted
        g (to, _) (from, _) = if to == from then Nothing else
          let Just movingTo = Map.lookup from reverseMapping
          in Just (to, NodeInfo (From_Move from mempty) $ Just movingTo)

-- | Create a 'PatchMapWithMove2' that, if applied to the first 'Map' provided,
-- will produce a 'Map' with the same values as the second 'Map' but with the
-- values sorted with the given ordering function.
patchThatChangesAndSortsMapWith :: forall k p. (Ord k, Ord (PatchTarget p), Monoid p) => (PatchTarget p -> PatchTarget p -> Ordering) -> Map k (PatchTarget p) -> Map k (PatchTarget p) -> PatchMapWithMove2 k p
patchThatChangesAndSortsMapWith cmp oldByIndex newByIndexUnsorted = patchThatChangesMap oldByIndex newByIndex
  where newList = Map.toList newByIndexUnsorted
        newByIndex = Map.fromList $ zip (fst <$> newList) $ sortBy cmp $ snd <$> newList

-- | Create a 'PatchMapWithMove2' that, if applied to the first 'Map' provided,
-- will produce the second 'Map'.
patchThatChangesMap :: (Ord k, Ord (PatchTarget p), Monoid p) => Map k (PatchTarget p) -> Map k (PatchTarget p) -> PatchMapWithMove2 k p
patchThatChangesMap oldByIndex newByIndex = patch
  where oldByValue = Map.fromListWith Set.union $ swap . first Set.singleton <$> Map.toList oldByIndex
        (insertsAndMoves, unusedValuesByValue) = flip runState oldByValue $ do
          let f k v = do
                remainingValues <- get
                let putRemainingKeys remainingKeys = put $ if Set.null remainingKeys
                      then Map.delete v remainingValues
                      else Map.insert v remainingKeys remainingValues
                case Map.lookup v remainingValues of
                  Nothing -> return $ NodeInfo (From_Insert v) $ Just undefined -- There's no existing value we can take
                  Just fromKs ->
                    if k `Set.member` fromKs
                    then do
                      putRemainingKeys $ Set.delete k fromKs
                      return $ NodeInfo (From_Move k mempty) $ Just undefined -- There's an existing value, and it's here, so no patch necessary
                    else do
                      (fromK, remainingKeys) <- return . fromJust $ Set.minView fromKs -- There's an existing value, but it's not here; move it here
                      putRemainingKeys remainingKeys
                      return $ NodeInfo (From_Move fromK mempty) $ Just undefined
          Map.traverseWithKey f newByIndex
        unusedOldKeys = fold unusedValuesByValue
        pointlessMove k = \case
          From_Move k' _ | k == k' -> True
          _ -> False
        keyWasMoved k = if k `Map.member` oldByIndex && not (k `Set.member` unusedOldKeys)
          then Just undefined
          else Nothing
        patch = unsafePatchMapWithMove2 $ Map.filterWithKey (\k -> not . pointlessMove k . _nodeInfo_from) $ Map.mergeWithKey (\k a _ -> Just $ nodeInfoSetTo (keyWasMoved k) a) (Map.mapWithKey $ \k -> nodeInfoSetTo $ keyWasMoved k) (Map.mapWithKey $ \k _ -> NodeInfo From_Delete $ keyWasMoved k) insertsAndMoves oldByIndex

-- | Change the 'From' value of a 'NodeInfo'
nodeInfoMapFrom :: (From k v -> From k v) -> NodeInfo k v -> NodeInfo k v
nodeInfoMapFrom f ni = ni { _nodeInfo_from = f $ _nodeInfo_from ni }

-- | Change the 'From' value of a 'NodeInfo', using a 'Functor' (or
-- 'Applicative', 'Monad', etc.) action to get the new value
nodeInfoMapMFrom :: Functor f => (From k v -> f (From k v)) -> NodeInfo k v -> f (NodeInfo k v)
nodeInfoMapMFrom f ni = fmap (\result -> ni { _nodeInfo_from = result }) $ f $ _nodeInfo_from ni

-- | Set the 'To' field of a 'NodeInfo'
nodeInfoSetTo :: To k -> NodeInfo k v -> NodeInfo k v
nodeInfoSetTo to ni = ni { _nodeInfo_to = to }

-- |Helper data structure used for composing patches using the monoid instance.
data Fixup k v
   = Fixup_Delete
   | Fixup_Update (These (From k v) (To k))

-- |Compose patches having the same effect as applying the patches in turn: @'applyAlways' (p <> q) == 'applyAlways' p . 'applyAlways' q@
instance (Ord k, Monoid p, Eq p, Patch p) => Semigroup (PatchMapWithMove2 k p) where
  PatchMapWithMove2 ma <> PatchMapWithMove2 mb = PatchMapWithMove2 m
    where
      connections = Map.toList $ Map.intersectionWithKey (\_ a b -> (_nodeInfo_to a, _nodeInfo_from b)) ma mb
      h :: (k, (Maybe k, From k p)) -> [(k, Fixup k p)]
      h (_, (mToAfter, editBefore)) = case (mToAfter, editBefore) of
        (Just toAfter, From_Move fromBefore p)
          | fromBefore == toAfter && p == mempty
            -> [(toAfter, Fixup_Delete)]
          | otherwise
            -> [ (toAfter, Fixup_Update (This editBefore))
               , (fromBefore, Fixup_Update (That mToAfter))
               ]
        (Nothing, From_Move fromBefore _) -> [(fromBefore, Fixup_Update (That mToAfter))] -- The item is destroyed in the second patch, so indicate that it is destroyed in the source map
        (Just toAfter, _) -> [(toAfter, Fixup_Update (This editBefore))]
        (Nothing, _) -> []
      mergeFixups _ Fixup_Delete Fixup_Delete = Fixup_Delete
      mergeFixups _ (Fixup_Update a) (Fixup_Update b)
        | This x <- a, That y <- b
        = Fixup_Update $ These x y
        | That y <- a, This x <- b
        = Fixup_Update $ These x y
      mergeFixups _ _ _ = error "PatchMapWithMove2: incompatible fixups"
      fixups = Map.fromListWithKey mergeFixups $ concatMap h connections
      combineNodeInfos _ nia nib = NodeInfo
        { _nodeInfo_from = _nodeInfo_from nia
        , _nodeInfo_to = _nodeInfo_to nib
        }
      applyFixup _ ni = \case
        Fixup_Delete -> Nothing
        Fixup_Update u -> Just $ NodeInfo
          { _nodeInfo_from = case _nodeInfo_from ni of
              f@(From_Move _ p') -> case getHere u of -- The `from` fixup comes from the "old" patch
                Nothing -> f -- If there's no `from` fixup, just use the "new" `from`
                Just (From_Insert v) -> From_Insert $ applyAlways p' v
                Just From_Delete -> From_Delete
                Just (From_Move oldKey p) -> From_Move oldKey $ p' <> p
              _ -> error "PatchMapWithMove2: fixup for non-move From"
          , _nodeInfo_to = fromMaybe (_nodeInfo_to ni) $ getThere u
          }
      m = Map.differenceWithKey applyFixup (Map.unionWithKey combineNodeInfos ma mb) fixups
      getHere :: These a b -> Maybe a
      getHere = \case
        This a -> Just a
        These a _ -> Just a
        That _ -> Nothing
      getThere :: These a b -> Maybe b
      getThere = \case
        This _ -> Nothing
        These _ b -> Just b
        That b -> Just b

--TODO: Figure out how to implement this in terms of PatchDMapWithMove rather than duplicating it here
-- |Compose patches having the same effect as applying the patches in turn: @'applyAlways' (p <> q) == 'applyAlways' p . 'applyAlways' q@
instance (Ord k, Monoid p, Eq p, Patch p) => Monoid (PatchMapWithMove2 k p) where
  mempty = PatchMapWithMove2 mempty
  mappend = (<>)
