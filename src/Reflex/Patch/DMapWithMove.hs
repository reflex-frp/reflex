-- | This module defines the 'Patch' class, which is used by Reflex to manage
-- changes to 'Reflex.Class.Incremental' values.
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
module Reflex.Patch.DMapWithMove where

import Reflex.Patch.Class
import Reflex.Patch.MapWithMove (PatchMapWithMove (..))
import qualified Reflex.Patch.MapWithMove as MapWithMove

import Data.Dependent.Map (DMap, DSum (..), GCompare (..))
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (ShowTag (..), EqTag (..))
import Data.Functor.Constant
import Data.Functor.Misc
import Data.GADT.Compare (GEq (..))
import Data.GADT.Show (GShow (..))
import qualified Data.Map as Map
import Data.Maybe
import Data.Semigroup (Semigroup (..), (<>))
import Data.Some (Some)
import qualified Data.Some as Some
import Data.These
import Data.Typeable (Proxy (..))

-- | Like 'PatchMapWithMove', but for 'DMap'.
data PatchDMapWithMove k v = PatchDMapWithMove (DMap k (NodeInfo k v))
  deriving (Show)

data NodeInfo k v a = NodeInfo
  { _nodeInfo_from :: !(From k v a)
  , _nodeInfo_to :: !(To k a)
  }
  deriving (Show)

instance {-# INCOHERENT #-} (GShow k, ShowTag k v) => ShowTag k (NodeInfo k v) where
  showTagToShow k _ r = gshowToShow k $ showTagToShow k (Proxy :: Proxy v) r

instance {-# INCOHERENT #-} (GEq k, EqTag k v) => EqTag k (From k v) where
  eqTagToEq k _ r = eqTagToEq k (Proxy :: Proxy v) (geqToEq k r)

data From (k :: a -> *) (v :: a -> *) :: a -> * where
  From_Insert :: v a -> From k v a
  From_Delete :: From k v a
  From_Move :: !(k a) -> From k v a
  deriving (Show, Read, Eq, Ord)

type To = ComposeMaybe

validPatchDMapWithMove :: forall k v. (GCompare k, EqTag k (ComposeMaybe k)) => PatchDMapWithMove k v -> Bool
validPatchDMapWithMove (PatchDMapWithMove m) = src == srcFromDst
  where
    src = DMap.map _nodeInfo_to m
    dst = DMap.map _nodeInfo_from m
    srcFromDst :: DMap k (ComposeMaybe k)
    srcFromDst = DMap.fromList $ flip fmap (DMap.toList dst) $ \(to :=> edit) -> case edit of
      From_Move from -> from :=> ComposeMaybe (Just to)
      _ -> to :=> ComposeMaybe Nothing

instance EqTag k (NodeInfo k v) => Eq (PatchDMapWithMove k v) where
  PatchDMapWithMove a == PatchDMapWithMove b = a == b

data Pair1 f g a = Pair1 (f a) (g a)

instance GCompare k => Semigroup (PatchDMapWithMove k v) where
  (<>) = mappend

data Fixup k v a
   = Fixup_Delete
   | Fixup_Update (These (From k v a) (To k a))

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

moveDMapKey :: GCompare k => k a -> k a -> PatchDMapWithMove k v
moveDMapKey src dst = case src `geq` dst of
  Nothing -> PatchDMapWithMove $ DMap.fromList
    [ dst :=> NodeInfo (From_Move src) (ComposeMaybe Nothing)
    , src :=> NodeInfo From_Delete (ComposeMaybe $ Just dst)
    ]
  Just _ -> mempty

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

unPatchDMapWithMove :: PatchDMapWithMove k v -> DMap k (NodeInfo k v)
unPatchDMapWithMove (PatchDMapWithMove p) = p

-- | Warning: when using this function, you must ensure that the invariants of
-- 'PatchDMapWithMove' are preserved; they will not be checked.
unsafePatchDMapWithMove :: DMap k (NodeInfo k v) -> PatchDMapWithMove k v
unsafePatchDMapWithMove = PatchDMapWithMove

mapPatchDMapWithMove :: forall k v v'. (forall a. v a -> v' a) -> PatchDMapWithMove k v -> PatchDMapWithMove k v'
mapPatchDMapWithMove f (PatchDMapWithMove p) = PatchDMapWithMove $
  DMap.map (\ni -> ni { _nodeInfo_from = g $ _nodeInfo_from ni }) p
  where g :: forall a. From k v a -> From k v' a
        g = \case
          From_Insert v -> From_Insert $ f v
          From_Delete -> From_Delete
          From_Move k -> From_Move k

traversePatchDMapWithMove :: forall m k v v'. Applicative m => (forall a. v a -> m (v' a)) -> PatchDMapWithMove k v -> m (PatchDMapWithMove k v')
traversePatchDMapWithMove f = traversePatchDMapWithMoveWithKey $ const f

traversePatchDMapWithMoveWithKey :: forall m k v v'. Applicative m => (forall a. k a -> v a -> m (v' a)) -> PatchDMapWithMove k v -> m (PatchDMapWithMove k v')
traversePatchDMapWithMoveWithKey f (PatchDMapWithMove p) = PatchDMapWithMove <$> DMap.traverseWithKey (nodeInfoMapFromM . g) p
  where g :: forall a. k a -> From k v a -> m (From k v' a)
        g k = \case
          From_Insert v -> From_Insert <$> f k v
          From_Delete -> pure From_Delete
          From_Move fromKey -> pure $ From_Move fromKey

nodeInfoMapFrom :: (From k v a -> From k v' a) -> NodeInfo k v a -> NodeInfo k v' a
nodeInfoMapFrom f ni = ni { _nodeInfo_from = f $ _nodeInfo_from ni }

nodeInfoMapFromM :: Functor f => (From k v a -> f (From k v' a)) -> NodeInfo k v a -> f (NodeInfo k v' a)
nodeInfoMapFromM f ni = fmap (\result -> ni { _nodeInfo_from = result }) $ f $ _nodeInfo_from ni

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

patchDMapWithMoveToPatchMapWithMoveWith :: forall k f v v'. (f v -> v') -> PatchDMapWithMove (Const2 k v) f -> PatchMapWithMove k v'
patchDMapWithMoveToPatchMapWithMoveWith f (PatchDMapWithMove p) = PatchMapWithMove $ dmapToMapWith g p
  where g :: NodeInfo (Const2 k v) f v -> MapWithMove.NodeInfo k v'
        g ni = MapWithMove.NodeInfo
          { MapWithMove._nodeInfo_from = case _nodeInfo_from ni of
              From_Insert v -> MapWithMove.From_Insert $ f v
              From_Delete -> MapWithMove.From_Delete
              From_Move (Const2 k) -> MapWithMove.From_Move k
          , MapWithMove._nodeInfo_to = unConst2 <$> getComposeMaybe (_nodeInfo_to ni)
          }

const2PatchDMapWithMoveWith :: forall k v f a. (v -> f a) -> PatchMapWithMove k v -> PatchDMapWithMove (Const2 k a) f
const2PatchDMapWithMoveWith f (PatchMapWithMove p) = PatchDMapWithMove $ DMap.fromDistinctAscList $ g <$> Map.toAscList p
  where g :: (k, MapWithMove.NodeInfo k v) -> DSum (Const2 k a) (NodeInfo (Const2 k a) f)
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

-- | Get the values that will be deleted if the given patch is applied to the
-- given 'DMap'
getDeletions :: GCompare k => PatchDMapWithMove k v -> DMap k v' -> DMap k v'
getDeletions (PatchDMapWithMove p) m = DMap.mapMaybeWithKey (\_ -> getComposeMaybe) $ DMap.intersectionWithKey f p m
  where f _ ni v = ComposeMaybe $ case getComposeMaybe $ _nodeInfo_to ni of
          Just _ -> Nothing
          Nothing -> Just v
