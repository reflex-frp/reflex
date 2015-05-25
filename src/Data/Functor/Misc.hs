{-# LANGUAGE KindSignatures, GADTs, DeriveDataTypeable, RankNTypes, ScopedTypeVariables, PolyKinds #-}
module Data.Functor.Misc where

import Data.GADT.Compare
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Dependent.Map (DMap, DSum (..))
import qualified Data.Dependent.Map as DMap
import Data.Typeable hiding (Refl)
import Data.These
import Data.Maybe

data WrapArg :: (k -> *) -> (k -> *) -> * -> * where
  WrapArg :: f a -> WrapArg g f (g a)

instance GEq f => GEq (WrapArg g f) where
  geq (WrapArg a) (WrapArg b) = fmap (\Refl -> Refl) $ geq a b

instance GCompare f => GCompare (WrapArg g f) where
  gcompare (WrapArg a) (WrapArg b) = case gcompare a b of
    GLT -> GLT
    GEQ -> GEQ
    GGT -> GGT

data Const2 :: * -> * -> * -> * where
  Const2 :: k -> Const2 k v v
  deriving (Typeable)

instance Eq k => GEq (Const2 k v) where
  geq (Const2 a) (Const2 b) =
    if a == b
    then Just Refl
    else Nothing

instance Ord k => GCompare (Const2 k v) where
  gcompare (Const2 a) (Const2 b) = case compare a b of
    LT -> GLT
    EQ -> GEQ
    GT -> GGT

{-# INLINE sequenceDmap #-}
sequenceDmap :: (Monad m, GCompare f) => DMap (WrapArg m f) -> m (DMap f)
sequenceDmap = DMap.foldrWithKey (\(WrapArg k) mv mx -> mx >>= \x -> mv >>= \v -> return $ DMap.insert k v x) (return DMap.empty)

{-# INLINE combineDMapsWithKey #-}
combineDMapsWithKey :: forall f g h i. GCompare f => (forall a. f a -> These (g a) (h a) -> i a) -> DMap (WrapArg g f) -> DMap (WrapArg h f) -> DMap (WrapArg i f)
combineDMapsWithKey f mg mh = DMap.fromList $ go (DMap.toList mg) (DMap.toList mh)
  where go :: [DSum (WrapArg g f)] -> [DSum (WrapArg h f)] -> [DSum (WrapArg i f)]
        go [] hs = map (\(WrapArg hk :=> hv) -> WrapArg hk :=> f hk (That hv)) hs
        go gs [] = map (\(WrapArg gk :=> gv) -> WrapArg gk :=> f gk (This gv)) gs
        go gs@((WrapArg gk :=> gv) : gs') hs@((WrapArg hk :=> hv) : hs') = case gk `gcompare` hk of
          GLT -> (WrapArg gk :=> f gk (This gv)) : go gs' hs
          GEQ -> (WrapArg gk :=> f gk (These gv hv)) : go gs' hs'
          GGT -> (WrapArg hk :=> f hk (That hv)) : go gs hs'

wrapDMap :: (forall a. a -> f a) -> DMap k -> DMap (WrapArg f k)
wrapDMap f = DMap.fromDistinctAscList . map (\(k :=> v) -> WrapArg k :=> f v) . DMap.toAscList

rewrapDMap :: (forall a. f a -> g a) -> DMap (WrapArg f k) -> DMap (WrapArg g k)
rewrapDMap f = DMap.fromDistinctAscList . map (\(WrapArg k :=> v) -> WrapArg k :=> f v) . DMap.toAscList

unwrapDMap :: (forall a. f a -> a) -> DMap (WrapArg f k) -> DMap k
unwrapDMap f = DMap.fromDistinctAscList . map (\(WrapArg k :=> v) -> k :=> f v) . DMap.toAscList

unwrapDMapMaybe :: (forall a. f a -> Maybe a) -> DMap (WrapArg f k) -> DMap k
unwrapDMapMaybe f = DMap.fromDistinctAscList . catMaybes . map (\(WrapArg k :=> v) -> fmap (k :=>) $ f v) . DMap.toAscList

mapToDMap :: Map k v -> DMap (Const2 k v)
mapToDMap = DMap.fromDistinctAscList . map (\(k, v) -> Const2 k :=> v) . Map.toAscList

mapWithFunctorToDMap :: Map k (f v) -> DMap (WrapArg f (Const2 k v))
mapWithFunctorToDMap = DMap.fromDistinctAscList . map (\(k, v) -> WrapArg (Const2 k) :=> v) . Map.toAscList

dmapToMap :: DMap (Const2 k v) -> Map k v
dmapToMap = Map.fromDistinctAscList . map (\(Const2 k :=> v) -> (k, v)) . DMap.toAscList
