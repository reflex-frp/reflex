{-# LANGUAGE KindSignatures, GADTs, DeriveDataTypeable, RankNTypes, ScopedTypeVariables, PolyKinds, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, StandaloneDeriving #-}
module Data.Functor.Misc where

import Data.GADT.Compare
import Data.Map (Map)
import qualified Data.Map as Map
import Data.GADT.Show
import Data.Dependent.Sum
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Typeable hiding (Refl)
import Data.These
import Control.Monad.Identity

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

deriving instance Show k => Show (Const2 k v v)

instance Show k => GShow (Const2 k v) where
  gshowsPrec n x@(Const2 _) = showsPrec n x

instance (Show k, Show (f v)) => ShowTag (Const2 k v) f where
  showTaggedPrec (Const2 _) = showsPrec

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
sequenceDmap :: (Monad m, GCompare f) => DMap f m -> m (DMap f Identity)
sequenceDmap = DMap.foldrWithKey (\k mv mx -> mx >>= \x -> mv >>= \v -> return $ DMap.insert k (Identity v) x)
                                 (return DMap.empty)

{-# INLINE combineDMapsWithKey #-}
combineDMapsWithKey :: forall f g h i. GCompare f => (forall (a :: *). f a -> These (g a) (h a) -> i a) -> DMap f g -> DMap f h -> DMap f i
combineDMapsWithKey f mg mh = DMap.fromList $ go (DMap.toList mg) (DMap.toList mh)
  where go :: [DSum f g] -> [DSum f h] -> [DSum f i]
        go [] hs = map (\(hk :=> hv) -> hk :=> f hk (That hv)) hs
        go gs [] = map (\(gk :=> gv) -> gk :=> f gk (This gv)) gs
        go gs@((gk :=> gv) : gs') hs@((hk :=> hv) : hs') = case gk `gcompare` hk of
          GLT -> (gk :=> f gk (This gv)) : go gs' hs
          GEQ -> (gk :=> f gk (These gv hv)) : go gs' hs'
          GGT -> (hk :=> f hk (That hv)) : go gs hs'

wrapDMap :: (forall a. a -> f a) -> DMap k Identity -> DMap k f
wrapDMap f = DMap.mapWithKey $ \_ -> f . runIdentity

rewrapDMap :: (forall (a :: *). f a -> g a) -> DMap k f -> DMap k g
rewrapDMap f = DMap.mapWithKey $ \_ -> f

unwrapDMap :: (forall a. f a -> a) -> DMap k f -> DMap k Identity 
unwrapDMap f = DMap.mapWithKey $ \_ -> Identity . f

unwrapDMapMaybe :: (forall a. f a -> Maybe a) -> DMap k f -> DMap k Identity
unwrapDMapMaybe f m = DMap.fromDistinctAscList [k :=> Identity w | (k :=> v) <- DMap.toAscList m, Just w <- [f v]]

mapToDMap :: Map k v -> DMap (Const2 k v) Identity
mapToDMap = DMap.fromDistinctAscList . map (\(k, v) -> Const2 k :=> Identity v) . Map.toAscList

mapWithFunctorToDMap :: Map k (f v) -> DMap (Const2 k v) f
mapWithFunctorToDMap = DMap.fromDistinctAscList . map (\(k, v) -> Const2 k :=> v) . Map.toAscList

extractFunctorDMap :: DMap (Const2 k (f v)) Identity -> DMap (Const2 k v) f
extractFunctorDMap = DMap.fromDistinctAscList . map (\(Const2 k :=> Identity v) -> Const2 k :=> v) . DMap.toAscList

dmapToMap :: DMap (Const2 k v) Identity -> Map k v
dmapToMap = Map.fromDistinctAscList . map (\(Const2 k :=> Identity v) -> (k, v)) . DMap.toAscList
