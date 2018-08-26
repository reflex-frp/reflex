{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-- | 'Patch'es on 'Map' that can insert, delete, and move values from one key to
-- another
module Reflex.Patch.MapWithMove where

import Reflex.Patch.Class

import Control.Arrow
import Control.Monad.State
import Data.Foldable
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Tuple

-- | Patch a DMap with additions, deletions, and moves.  Invariant: If key @k1@
-- is coming from @From_Move k2@, then key @k2@ should be going to @Just k1@,
-- and vice versa.  There should never be any unpaired From/To keys.
newtype PatchMapWithMove k v = PatchMapWithMove (Map k (NodeInfo k v)) deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- | Holds the information about each key: where its new value should come from,
-- and where its old value should go to
data NodeInfo k v = NodeInfo
  { _nodeInfo_from :: !(From k v)
    -- ^ Where do we get the new value for this key?
  , _nodeInfo_to :: !(To k)
    -- ^ If the old value is being kept (i.e. moved rather than deleted or
    -- replaced), where is it going?
  }
  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

-- | Create a 'PatchMapWithMove', validating it
patchMapWithMove :: Ord k => Map k (NodeInfo k v) -> Maybe (PatchMapWithMove k v)
patchMapWithMove m = if valid then Just $ PatchMapWithMove m else Nothing
  where valid = forwardLinks == backwardLinks
        forwardLinks = Map.mapMaybe _nodeInfo_to m
        backwardLinks = Map.fromList $ catMaybes $ flip fmap (Map.toList m) $ \(to, v) ->
          case _nodeInfo_from v of
            From_Move from -> Just (from, to)
            _ -> Nothing

-- | Create a 'PatchMapWithMove' that inserts everything in the given 'Map'
patchMapWithMoveInsertAll :: Map k v -> PatchMapWithMove k v
patchMapWithMoveInsertAll m = PatchMapWithMove $ flip fmap m $ \v -> NodeInfo
  { _nodeInfo_from = From_Insert v
  , _nodeInfo_to = Nothing
  }

-- | Extract the internal representation of the 'PatchMapWithMove'
unPatchMapWithMove :: PatchMapWithMove k v -> Map k (NodeInfo k v)
unPatchMapWithMove (PatchMapWithMove p) = p

-- | Warning: when using this function, you must ensure that the invariants of
-- 'PatchMapWithMove' are preserved; they will not be checked.
unsafePatchMapWithMove :: Map k (NodeInfo k v) -> PatchMapWithMove k v
unsafePatchMapWithMove = PatchMapWithMove

-- | Describe how a key's new value should be produced
data From k v
   = From_Insert v -- ^ Insert the given value here
   | From_Delete -- ^ Delete the existing value, if any, from here
   | From_Move !k -- ^ Move the value here from the given key
   deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

-- | Describe where a key's old value will go.  If this is 'Just', that means
-- the key's old value will be moved to the given other key; if it is 'Nothing',
-- that means it will be deleted.
type To = Maybe

-- | Apply the insertions, deletions, and moves to a given 'Map'
instance Ord k => Patch (PatchMapWithMove k v) where
  type PatchTarget (PatchMapWithMove k v) = Map k v
  apply (PatchMapWithMove p) old = Just $! insertions `Map.union` (old `Map.difference` deletions) --TODO: return Nothing sometimes --Note: the strict application here is critical to ensuring that incremental merges don't hold onto all their prerequisite events forever; can we make this more robust?
    where insertions = flip Map.mapMaybeWithKey p $ \_ ni -> case _nodeInfo_from ni of
            From_Insert v -> Just v
            From_Move k -> Map.lookup k old
            From_Delete -> Nothing
          deletions = flip Map.mapMaybeWithKey p $ \_ ni -> case _nodeInfo_from ni of
            From_Delete -> Just ()
            _ -> Nothing

-- | Returns all the new elements that will be added to the 'Map'
patchMapWithMoveNewElements :: PatchMapWithMove k v -> [v]
patchMapWithMoveNewElements = Map.elems . patchMapWithMoveNewElementsMap

patchMapWithMoveNewElementsMap :: PatchMapWithMove k v -> Map k v
patchMapWithMoveNewElementsMap (PatchMapWithMove p) = Map.mapMaybe f p
  where f ni = case _nodeInfo_from ni of
          From_Insert v -> Just v
          From_Move _ -> Nothing
          From_Delete -> Nothing

-- | Create a 'PatchMapWithMove' that, if applied to the given 'Map', will sort
-- its values using the given ordering function.  The set keys of the 'Map' is
-- not changed.
patchThatSortsMapWith :: Ord k => (v -> v -> Ordering) -> Map k v -> PatchMapWithMove k v
patchThatSortsMapWith cmp m = PatchMapWithMove $ Map.fromList $ catMaybes $ zipWith g unsorted sorted
  where unsorted = Map.toList m
        sorted = sortBy (cmp `on` snd) unsorted
        f (to, _) (from, _) = if to == from then Nothing else
          Just (from, to)
        reverseMapping = Map.fromList $ catMaybes $ zipWith f unsorted sorted
        g (to, _) (from, _) = if to == from then Nothing else
          let Just movingTo = Map.lookup from reverseMapping
          in Just (to, NodeInfo (From_Move from) $ Just movingTo)

-- | Create a 'PatchMapWithMove' that, if applied to the first 'Map' provided,
-- will produce a 'Map' with the same values as the second 'Map' but with the
-- values sorted with the given ordering function.
patchThatChangesAndSortsMapWith :: forall k v. (Ord k, Ord v) => (v -> v -> Ordering) -> Map k v -> Map k v -> PatchMapWithMove k v
patchThatChangesAndSortsMapWith cmp oldByIndex newByIndexUnsorted = patch
  where oldByValue = Map.fromListWith Set.union $ swap . first Set.singleton <$> Map.toList oldByIndex
        newList = Map.toList newByIndexUnsorted
        newByIndex = Map.fromList $ zip (fst <$> newList) $ sortBy cmp $ snd <$> newList
        insertsAndMoves :: Map k (NodeInfo k v)
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
                      return $ NodeInfo (From_Move k) $ Just undefined -- There's an existing value, and it's here, so no patch necessary
                    else do
                      Just (fromK, remainingKeys) <- return $ Set.minView fromKs -- There's an existing value, but it's not here; move it here
                      putRemainingKeys remainingKeys
                      return $ NodeInfo (From_Move fromK) $ Just undefined
          Map.traverseWithKey f newByIndex
        unusedOldKeys = fold unusedValuesByValue
        pointlessMove k = \case
          From_Move k' | k == k' -> True
          _ -> False
        keyWasMoved k = if k `Map.member` oldByIndex && not (k `Set.member` unusedOldKeys)
          then Just undefined
          else Nothing
        patch = unsafePatchMapWithMove $ Map.filterWithKey (\k -> not . pointlessMove k . _nodeInfo_from) $ Map.mergeWithKey (\k a _ -> Just $ nodeInfoSetTo (keyWasMoved k) a) (Map.mapWithKey $ \k -> nodeInfoSetTo $ keyWasMoved k) (Map.mapWithKey $ \k _ -> NodeInfo From_Delete $ keyWasMoved k) insertsAndMoves oldByIndex

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
