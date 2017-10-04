{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
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

-- | Patch a DMap with additions, deletions, and moves.
-- Invariants:
--   * The same source key may not be moved to two destination keys.
--   * Any key that is the source of a Move must also be edited
--     (otherwise, the Move would duplicate it)
newtype PatchMapWithMove k v = PatchMapWithMove (Map k (NodeInfo k v)) deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data NodeInfo k v = NodeInfo
  { _nodeInfo_from :: !(From k v)
  , _nodeInfo_to :: !(To k)
  }
  deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

unPatchMapWithMove :: PatchMapWithMove k v -> Map k (NodeInfo k v)
unPatchMapWithMove (PatchMapWithMove p) = p

-- | Warning: when using this function, you must ensure that the invariants of
-- 'PatchMapWithMove' are preserved; they will not be checked.
unsafePatchMapWithMove :: Map k (NodeInfo k v) -> PatchMapWithMove k v
unsafePatchMapWithMove = PatchMapWithMove

data From k v
   = From_Insert v -- ^ Insert the given value here
   | From_Delete -- ^ Delete the existing value, if any, from here
   | From_Move !k -- ^ Move the value here from the given key
   deriving (Show, Read, Eq, Ord, Functor, Foldable, Traversable)

type To = Maybe

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

validPatchMapWithMove :: Ord k => PatchMapWithMove k v -> Bool
validPatchMapWithMove (PatchMapWithMove p) =
  let f ni = case _nodeInfo_from ni of
        From_Move k -> Just k
        _ -> Nothing
      moveSources = catMaybes $ f <$> Map.elems p
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
    [ (1, NodeInfo (From_Move 2) $ Just 1)
    , (2, NodeInfo (From_Move 1) $ Just 2)
    ]
  ]

knownInvalidPatchMapWithMove :: [PatchMapWithMove Int Int]
knownInvalidPatchMapWithMove =
  [ -- Invalid because we move 2 to 1, but don't specify what to do with 2
    PatchMapWithMove $ Map.singleton 1 $ NodeInfo (From_Move 2) Nothing
  , -- Invalid because we have multiple moves from 3
    PatchMapWithMove $ Map.fromList
    [ (1, NodeInfo (From_Move 3) Nothing)
    , (2, NodeInfo (From_Move 3) Nothing)
    , (3, NodeInfo From_Delete (Just 1))
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
          in Just (to, NodeInfo (From_Move from) $ Just movingTo)

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

nodeInfoMapFrom :: (From k v -> From k v) -> NodeInfo k v -> NodeInfo k v
nodeInfoMapFrom f ni = ni { _nodeInfo_from = f $ _nodeInfo_from ni }

nodeInfoMapMFrom :: Functor f => (From k v -> f (From k v)) -> NodeInfo k v -> f (NodeInfo k v)
nodeInfoMapMFrom f ni = fmap (\result -> ni { _nodeInfo_from = result }) $ f $ _nodeInfo_from ni

nodeInfoSetTo :: To k -> NodeInfo k v -> NodeInfo k v
nodeInfoSetTo to ni = ni { _nodeInfo_to = to }
