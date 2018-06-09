{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |Module containing @'PatchDMapWithMove' k v@ and associated functions, which represents a 'Patch' to a @'DMap' k v@ which can insert, update, delete, and
-- move values between keys.
module Reflex.Patch.DMapWithMove where

import Reflex.Patch.Class
import Reflex.Patch.MapWithMove (PatchMapWithMove (..))
import qualified Reflex.Patch.MapWithMove as MapWithMove

import Data.Dependent.Map (DMap, DSum (..), GCompare (..))
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (EqTag (..))
import Data.Functor.Constant
import Data.Functor.Misc
import Data.Functor.Product
import Data.GADT.Compare (GEq (..))
import Data.GADT.Show (GShow, gshow)
import qualified Data.Map as Map
import Data.Maybe
import Data.Semigroup (Semigroup (..), (<>))
import Data.Some (Some)
import qualified Data.Some as Some
import Data.These

#ifdef EXPERIMENTAL_DEPENDENT_SUM_INSTANCES
import Data.Dependent.Sum (ShowTag (..))
import Data.GADT.Show (GShow (..))
import Data.Typeable (Proxy (..))
#endif

-- | Like 'PatchMapWithMove', but for 'DMap'. Each key carries a 'NodeInfo' which describes how it will be changed by the patch and connects move sources and
-- destinations.
--
-- Invariants:
--
--     * A key should not move to itself.
--     * A move should always be represented with both the destination key (as a 'From_Move') and the source key (as a @'ComposeMaybe' ('Just' destination)@)
newtype PatchDMapWithMove k v = PatchDMapWithMove (DMap k (NodeInfo k v))
#ifdef EXPERIMENTAL_DEPENDENT_SUM_INSTANCES
  deriving (Show)
#endif

-- |Structure which represents what changes apply to a particular key. @_nodeInfo_from@ specifies what happens to this key, and in particular what other key
-- the current key is moving from, while @_nodeInfo_to@ specifies what key the current key is moving to if involved in a move.
data NodeInfo k v a = NodeInfo
  { _nodeInfo_from :: !(From k v a)
  -- ^Change applying to the current key, be it an insert, move, or delete.
  , _nodeInfo_to :: !(To k a)
  -- ^Where this key is moving to, if involved in a move. Should only be @ComposeMaybe (Just k)@ when there is a corresponding 'From_Move'.
  }
  deriving (Show)

#ifdef EXPERIMENTAL_DEPENDENT_SUM_INSTANCES
instance {-# INCOHERENT #-} (GShow k, ShowTag k v) => ShowTag k (NodeInfo k v) where
  showTagToShow k _ r = gshowToShow k $ showTagToShow k (Proxy :: Proxy v) r

instance {-# INCOHERENT #-} (GEq k, EqTag k v) => EqTag k (From k v) where
  eqTagToEq k _ r = eqTagToEq k (Proxy :: Proxy v) (geqToEq k r)
#endif

-- |Structure describing a particular change to a key, be it inserting a new key (@From_Insert@), updating an existing key (@From_Insert@ again), deleting a
-- key (@From_Delete@), or moving a key (@From_Move@).
data From (k :: a -> *) (v :: a -> *) :: a -> * where
  From_Insert :: v a -> From k v a
  -- ^Insert a new or update an existing key with the given value @v a@
  From_Delete :: From k v a
  -- ^Delete the existing key
  From_Move :: !(k a) -> From k v a
  -- ^Move the value from the given key @k a@ to this key. The source key should also have an entry in the patch giving the current key as @_nodeInfo_to@,
  -- usually but not necessarily with @From_Delete@.
  deriving (Show, Read, Eq, Ord)

-- |Type alias for the "to" part of a 'NodeInfo'. @'ComposeMaybe' ('Just' k)@ means the key is moving to another key, @ComposeMaybe Nothing@ for any other
-- operation.
type To = ComposeMaybe

-- |Test whether a 'PatchDMapWithMove' satisfies its invariants.
validPatchDMapWithMove :: forall k v. (GCompare k, GEq k, GShow k) => DMap k (NodeInfo k v) -> Bool
validPatchDMapWithMove = not . null . validationErrorsForPatchDMapWithMove

-- |Enumerate what reasons a 'PatchDMapWithMove' doesn't satisfy its invariants, returning @[]@ if it's valid.
validationErrorsForPatchDMapWithMove :: forall k v. (GCompare k, GEq k, GShow k) => DMap k (NodeInfo k v) -> [String]
validationErrorsForPatchDMapWithMove m =
  noSelfMoves `mappend` movesBalanced
  where
    noSelfMoves = catMaybes . map selfMove . DMap.toAscList $ m
    selfMove (dst :=> NodeInfo (From_Move src) _)           | Just _ <- dst `geq` src = Just $ "self move of key " <> gshow src <> " at destination side"
    selfMove (src :=> NodeInfo _ (ComposeMaybe (Just dst))) | Just _ <- src `geq` dst = Just $ "self move of key " <> gshow dst <> " at source side"
    selfMove _ = Nothing
    movesBalanced = catMaybes . map unbalancedMove . DMap.toAscList $ m
    unbalancedMove (dst :=> NodeInfo (From_Move src) _) =
      case DMap.lookup src m of
        Nothing -> Just $ "unbalanced move at destination key " <> gshow dst <> " supposedly from " <> gshow src <> " but source key is not in the patch"
        Just (NodeInfo _ (ComposeMaybe (Just dst'))) ->
          if isNothing (dst' `geq` dst)
            then Just $ "unbalanced move at destination key " <> gshow dst <> " from " <> gshow src <> " is going to " <> gshow dst' <> " instead"
            else Nothing
        _ ->
          Just $ "unbalanced move at destination key " <> gshow dst <> " supposedly from " <> gshow src <> " but source key has no move to key"
    unbalancedMove (src :=> NodeInfo _ (ComposeMaybe (Just dst))) =
      case DMap.lookup dst m of
        Nothing -> Just $ " unbalanced move at source key " <> gshow src <> " supposedly going to " <> gshow dst <> " but destination key is not in the patch"
        Just (NodeInfo (From_Move src') _) ->
          if isNothing (src' `geq` src)
            then Just $ "unbalanced move at source key " <> gshow src <> " to " <> gshow dst <> " is coming from " <> gshow src' <> " instead"
            else Nothing

        _ ->
          Just $ "unbalanced move at source key " <> gshow src <> " supposedly going to " <> gshow dst <> " but destination key is not moving"
    unbalancedMove _ = Nothing

instance EqTag k (NodeInfo k v) => Eq (PatchDMapWithMove k v) where
  PatchDMapWithMove a == PatchDMapWithMove b = a == b

-- |Higher kinded 2-tuple, identical to @Data.Functor.Product@ from base ≥ 4.9
data Pair1 f g a = Pair1 (f a) (g a)

-- |Compose patches having the same effect as applying the patches in turn: @'applyAlways' (p <> q) == 'applyAlways' p . 'applyAlways' q@
instance GCompare k => Semigroup (PatchDMapWithMove k v) where
  (<>) = mappend

-- |Helper data structure used for composing patches using the monoid instance.
data Fixup k v a
   = Fixup_Delete
   | Fixup_Update (These (From k v a) (To k a))

-- |Compose patches having the same effect as applying the patches in turn: @'applyAlways' (p <> q) == 'applyAlways' p . 'applyAlways' q@
instance GCompare k => Monoid (PatchDMapWithMove k v) where
  mempty = PatchDMapWithMove mempty
  PatchDMapWithMove ma `mappend` PatchDMapWithMove mb = PatchDMapWithMove m
    where
      connections = DMap.toList $ DMap.intersectionWithKey (\_ a b -> Pair1 (_nodeInfo_to a) (_nodeInfo_from b)) ma mb
      h :: DSum k (Pair1 (ComposeMaybe k) (From k v)) -> [DSum k (Fixup k v)]
      h (_ :=> Pair1 (ComposeMaybe mToAfter) editBefore) = case (mToAfter, editBefore) of
        (Just toAfter, From_Move fromBefore)
          | isJust $ fromBefore `geq` toAfter
            -> [toAfter :=> Fixup_Delete]
          | otherwise
            -> [ toAfter :=> Fixup_Update (This editBefore)
               , fromBefore :=> Fixup_Update (That (ComposeMaybe mToAfter))
               ]
        (Nothing, From_Move fromBefore) -> [fromBefore :=> Fixup_Update (That (ComposeMaybe mToAfter))] -- The item is destroyed in the second patch, so indicate that it is destroyed in the source map
        (Just toAfter, _) -> [toAfter :=> Fixup_Update (This editBefore)]
        (Nothing, _) -> []
      mergeFixups _ Fixup_Delete Fixup_Delete = Fixup_Delete
      mergeFixups _ (Fixup_Update a) (Fixup_Update b)
        | This x <- a, That y <- b
        = Fixup_Update $ These x y
        | That y <- a, This x <- b
        = Fixup_Update $ These x y
      mergeFixups _ _ _ = error "PatchDMapWithMove: incompatible fixups"
      fixups = DMap.fromListWithKey mergeFixups $ concatMap h connections
      combineNodeInfos _ nia nib = NodeInfo
        { _nodeInfo_from = _nodeInfo_from nia
        , _nodeInfo_to = _nodeInfo_to nib
        }
      applyFixup _ ni = \case
        Fixup_Delete -> Nothing
        Fixup_Update u -> Just $ NodeInfo
          { _nodeInfo_from = fromMaybe (_nodeInfo_from ni) $ getHere u
          , _nodeInfo_to = fromMaybe (_nodeInfo_to ni) $ getThere u
          }
      m = DMap.differenceWithKey applyFixup (DMap.unionWithKey combineNodeInfos ma mb) fixups

-- |Project the @a@ from a @'These' a b@, identical to @preview '_Here'@ but without using preview
getHere :: These a b -> Maybe a
getHere = \case
  This a -> Just a
  These a _ -> Just a
  That _ -> Nothing

-- |Project the @b@ from a @'These' a b@, identical to @preview '_There'@ but without using preview
getThere :: These a b -> Maybe b
getThere = \case
  This _ -> Nothing
  These _ b -> Just b
  That b -> Just b

{-
mappendPatchDMapWithMoveSlow :: forall k v. (ShowTag k v, GCompare k) => PatchDMapWithMove k v -> PatchDMapWithMove k v -> PatchDMapWithMove k v
PatchDMapWithMove dstAfter srcAfter `mappendPatchDMapWithMoveSlow` PatchDMapWithMove dstBefore srcBefore = PatchDMapWithMove dst src
  where
    getDstAction k m = fromMaybe (From_Move k) $ DMap.lookup k m -- Any key that isn't present is treated as that key moving to itself
    removeRedundantDst toKey (From_Move fromKey) | isJust (toKey `geq` fromKey) = Nothing
    removeRedundantDst _ a = Just a
    f :: forall a. k a -> From k v a -> Maybe (From k v a)
    f toKey _ = removeRedundantDst toKey $ case getDstAction toKey dstAfter of
      From_Move fromKey -> getDstAction fromKey dstBefore
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
-}

-- |Make a @'PatchDMapWithMove' k v@ which has the effect of inserting or updating a value @v a@ to the given key @k a@, like 'DMap.insert'.
insertDMapKey :: k a -> v a -> PatchDMapWithMove k v
insertDMapKey k v =
  PatchDMapWithMove . DMap.singleton k $ NodeInfo (From_Insert v) (ComposeMaybe Nothing)

-- |Make a @'PatchDMapWithMove' k v@ which has the effect of moving the value from the first key @k a@ to the second key @k a@, equivalent to:
--
-- @
--     'DMap.delete' src (maybe dmap ('DMap.insert' dst) (DMap.lookup src dmap))
-- @
moveDMapKey :: GCompare k => k a -> k a -> PatchDMapWithMove k v
moveDMapKey src dst = case src `geq` dst of
  Nothing -> PatchDMapWithMove $ DMap.fromList
    [ dst :=> NodeInfo (From_Move src) (ComposeMaybe Nothing)
    , src :=> NodeInfo From_Delete (ComposeMaybe $ Just dst)
    ]
  Just _ -> mempty

-- |Make a @'PatchDMapWithMove' k v@ which has the effect of swapping two keys in the mapping, equivalent to:
--
-- @
--     let aMay = DMap.lookup a dmap
--         bMay = DMap.lookup b dmap
--     in maybe id (DMap.insert a) (bMay `mplus` aMay)
--      . maybe id (DMap.insert b) (aMay `mplus` bMay)
--      . DMap.delete a . DMap.delete b $ dmap
-- @
swapDMapKey :: GCompare k => k a -> k a -> PatchDMapWithMove k v
swapDMapKey src dst = case src `geq` dst of
  Nothing -> PatchDMapWithMove $ DMap.fromList
    [ dst :=> NodeInfo (From_Move src) (ComposeMaybe $ Just src)
    , src :=> NodeInfo (From_Move dst) (ComposeMaybe $ Just dst)
    ]
  Just _ -> mempty

-- |Make a @'PatchDMapWithMove' k v@ which has the effect of deleting a key in the mapping, equivalent to 'DMap.delete'.
deleteDMapKey :: k a -> PatchDMapWithMove k v
deleteDMapKey k = PatchDMapWithMove $ DMap.singleton k $ NodeInfo From_Delete $ ComposeMaybe Nothing

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

-- |Extract the 'DMap' representing the patch changes from the 'PatchDMapWithMove'.
unPatchDMapWithMove :: PatchDMapWithMove k v -> DMap k (NodeInfo k v)
unPatchDMapWithMove (PatchDMapWithMove p) = p

-- |Wrap a 'DMap' representing patch changes into a 'PatchDMapWithMove', without checking any invariants.
--
-- __Warning:__ when using this function, you must ensure that the invariants of 'PatchDMapWithMove' are preserved; they will not be checked.
unsafePatchDMapWithMove :: DMap k (NodeInfo k v) -> PatchDMapWithMove k v
unsafePatchDMapWithMove = PatchDMapWithMove

-- |Wrap a 'DMap' representing patch changes into a 'PatchDMapWithMove' while checking invariants. If the invariants are satisfied, @Right p@ is returned
-- otherwise @Left errors@.
patchDMapWithMove :: (GCompare k, GEq k, GShow k) => DMap k (NodeInfo k v) -> Either [String] (PatchDMapWithMove k v)
patchDMapWithMove dm =
  case validationErrorsForPatchDMapWithMove dm of
    [] -> Right $ unsafePatchDMapWithMove dm
    errs -> Left errs

-- |Map a natural transform @v -> v'@ over the given patch, transforming @'PatchDMapWithMove' k v@ into @'PatchDMapWithMove' k v'@.
mapPatchDMapWithMove :: forall k v v'. (forall a. v a -> v' a) -> PatchDMapWithMove k v -> PatchDMapWithMove k v'
mapPatchDMapWithMove f (PatchDMapWithMove p) = PatchDMapWithMove $
  DMap.map (\ni -> ni { _nodeInfo_from = g $ _nodeInfo_from ni }) p
  where g :: forall a. From k v a -> From k v' a
        g = \case
          From_Insert v -> From_Insert $ f v
          From_Delete -> From_Delete
          From_Move k -> From_Move k

-- |Traverse an effectful function @forall a. v a -> m (v ' a)@ over the given patch, transforming @'PatchDMapWithMove' k v@ into @m ('PatchDMapWithMove' k v')@.
traversePatchDMapWithMove :: forall m k v v'. Applicative m => (forall a. v a -> m (v' a)) -> PatchDMapWithMove k v -> m (PatchDMapWithMove k v')
traversePatchDMapWithMove f = traversePatchDMapWithMoveWithKey $ const f

-- |Map an effectful function @forall a. k a -> v a -> m (v ' a)@ over the given patch, transforming @'PatchDMapWithMove' k v@ into @m ('PatchDMapWithMove' k v')@.
traversePatchDMapWithMoveWithKey :: forall m k v v'. Applicative m => (forall a. k a -> v a -> m (v' a)) -> PatchDMapWithMove k v -> m (PatchDMapWithMove k v')
traversePatchDMapWithMoveWithKey f (PatchDMapWithMove p) = PatchDMapWithMove <$> DMap.traverseWithKey (nodeInfoMapFromM . g) p
  where g :: forall a. k a -> From k v a -> m (From k v' a)
        g k = \case
          From_Insert v -> From_Insert <$> f k v
          From_Delete -> pure From_Delete
          From_Move fromKey -> pure $ From_Move fromKey

-- |Map a function which transforms @'From' k v a@ into a @'From' k v' a@ over a @'NodeInfo' k v a@.
nodeInfoMapFrom :: (From k v a -> From k v' a) -> NodeInfo k v a -> NodeInfo k v' a
nodeInfoMapFrom f ni = ni { _nodeInfo_from = f $ _nodeInfo_from ni }

-- |Map an effectful function which transforms @'From' k v a@ into a @f ('From' k v' a)@ over a @'NodeInfo' k v a@.
nodeInfoMapFromM :: Functor f => (From k v a -> f (From k v' a)) -> NodeInfo k v a -> f (NodeInfo k v' a)
nodeInfoMapFromM f ni = fmap (\result -> ni { _nodeInfo_from = result }) $ f $ _nodeInfo_from ni

-- |Weaken a 'PatchDMapWithMove' to a 'PatchMapWithMove' by weakening the keys from @k a@ to @'Some' k@ and applying a given weakening function @v a -> v'@ to
-- values.
weakenPatchDMapWithMoveWith :: forall k v v'. (forall a. v a -> v') -> PatchDMapWithMove k v -> PatchMapWithMove (Some k) v'
weakenPatchDMapWithMoveWith f (PatchDMapWithMove p) = PatchMapWithMove $ weakenDMapWith g p
  where g :: forall a. NodeInfo k v a -> MapWithMove.NodeInfo (Some k) v'
        g ni = MapWithMove.NodeInfo
          { MapWithMove._nodeInfo_from = case _nodeInfo_from ni of
              From_Insert v -> MapWithMove.From_Insert $ f v
              From_Delete -> MapWithMove.From_Delete
              From_Move k -> MapWithMove.From_Move $ Some.This k
          , MapWithMove._nodeInfo_to = Some.This <$> getComposeMaybe (_nodeInfo_to ni)
          }

-- |"Weaken" a @'PatchDMapWithMove' (Const2 k a) v@ to a @'PatchMapWithMove' k v'@. Weaken is in scare quotes because the 'Const2' has already disabled any
-- dependency in the typing and all points are already @a@, hence the function to map each value to @v'@ is not higher rank.
patchDMapWithMoveToPatchMapWithMoveWith :: forall k v v' a. (v a -> v') -> PatchDMapWithMove (Const2 k a) v -> PatchMapWithMove k v'
patchDMapWithMoveToPatchMapWithMoveWith f (PatchDMapWithMove p) = PatchMapWithMove $ dmapToMapWith g p
  where g :: NodeInfo (Const2 k a) v a -> MapWithMove.NodeInfo k v'
        g ni = MapWithMove.NodeInfo
          { MapWithMove._nodeInfo_from = case _nodeInfo_from ni of
              From_Insert v -> MapWithMove.From_Insert $ f v
              From_Delete -> MapWithMove.From_Delete
              From_Move (Const2 k) -> MapWithMove.From_Move k
          , MapWithMove._nodeInfo_to = unConst2 <$> getComposeMaybe (_nodeInfo_to ni)
          }

-- |"Strengthen" a @'PatchMapWithMove' k v@ into a @'PatchDMapWithMove ('Const2' k a)@; that is, turn a non-dependently-typed patch into a dependently typed
-- one but which always has a constant key type represented by 'Const2'. Apply the given function to each @v@ to produce a @v' a@.
-- Completemented by 'patchDMapWithMoveToPatchMapWithMoveWith'
const2PatchDMapWithMoveWith :: forall k v v' a. (v -> v' a) -> PatchMapWithMove k v -> PatchDMapWithMove (Const2 k a) v'
const2PatchDMapWithMoveWith f (PatchMapWithMove p) = PatchDMapWithMove $ DMap.fromDistinctAscList $ g <$> Map.toAscList p
  where g :: (k, MapWithMove.NodeInfo k v) -> DSum (Const2 k a) (NodeInfo (Const2 k a) v')
        g (k, ni) = Const2 k :=> NodeInfo
          { _nodeInfo_from = case MapWithMove._nodeInfo_from ni of
              MapWithMove.From_Insert v -> From_Insert $ f v
              MapWithMove.From_Delete -> From_Delete
              MapWithMove.From_Move fromKey -> From_Move $ Const2 fromKey
          , _nodeInfo_to = ComposeMaybe $ Const2 <$> MapWithMove._nodeInfo_to ni
          }

instance GCompare k => Patch (PatchDMapWithMove k v) where
  type PatchTarget (PatchDMapWithMove k v) = DMap k v
  apply (PatchDMapWithMove p) old = Just $! insertions `DMap.union` (old `DMap.difference` deletions) --TODO: return Nothing sometimes --Note: the strict application here is critical to ensuring that incremental merges don't hold onto all their prerequisite events forever; can we make this more robust?
    where insertions = DMap.mapMaybeWithKey insertFunc p
          insertFunc :: forall a. k a -> NodeInfo k v a -> Maybe (v a)
          insertFunc _ ni = case _nodeInfo_from ni of
            From_Insert v -> Just v
            From_Move k -> DMap.lookup k old
            From_Delete -> Nothing
          deletions = DMap.mapMaybeWithKey deleteFunc p
          deleteFunc :: forall a. k a -> NodeInfo k v a -> Maybe (Constant () a)
          deleteFunc _ ni = case _nodeInfo_from ni of
            From_Delete -> Just $ Constant ()
            _ -> Nothing

-- | Get the values that will be deleted or moved if the given patch is applied
-- to the given 'DMap'.
getDeletionsAndMoves :: GCompare k => PatchDMapWithMove k v -> DMap k v' -> DMap k (Product v' (ComposeMaybe k))
getDeletionsAndMoves (PatchDMapWithMove p) m = DMap.intersectionWithKey f m p
  where f _ v ni = Pair v $ _nodeInfo_to ni
