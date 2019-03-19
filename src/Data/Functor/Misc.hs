{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
-- | This module provides types and functions with no particular theme, but
-- which are relevant to the use of 'Functor'-based datastructures like
-- 'Data.Dependent.Map.DMap'.
module Data.Functor.Misc
  ( -- * Const2
    Const2 (..)
  , unConst2
  , dmapToMap
  , dmapToIntMap
  , dmapToMapWith
  , mapToDMap
  , weakenDMapWith
    -- * WrapArg
  , WrapArg (..)
    -- * Convenience functions for DMap
  , mapWithFunctorToDMap
  , intMapWithFunctorToDMap
  , mapKeyValuePairsMonotonic
  , combineDMapsWithKey
  , EitherTag (..)
  , dmapToThese
  , eitherToDSum
  , dsumToEither
  , ComposeMaybe (..)
  ) where

import Control.Applicative ((<$>))
import Control.Monad.Identity
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum
import Data.GADT.Compare
import Data.GADT.Show
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Some (Some)
import qualified Data.Some as Some
import Data.These
import Data.Typeable hiding (Refl)

--------------------------------------------------------------------------------
-- Const2
--------------------------------------------------------------------------------

-- | 'Const2' stores a value of a given type 'k' and ensures that a particular
-- type 'v' is always given for the last type parameter
data Const2 :: * -> x -> x -> * where
  Const2 :: k -> Const2 k v v
  deriving (Typeable)

-- | Extract the value from a Const2
unConst2 :: Const2 k v v' -> k
unConst2 (Const2 k) = k

deriving instance Eq k => Eq (Const2 k v v')
deriving instance Ord k => Ord (Const2 k v v')
deriving instance Show k => Show (Const2 k v v')
deriving instance Read k => Read (Const2 k v v)

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

-- | Convert a 'DMap' to a regular 'Map'
dmapToMap :: DMap (Const2 k v) Identity -> Map k v
dmapToMap = Map.fromDistinctAscList . map (\(Const2 k :=> Identity v) -> (k, v)) . DMap.toAscList

-- | Convert a 'DMap' to an 'IntMap'
dmapToIntMap :: DMap (Const2 IntMap.Key v) Identity -> IntMap v
dmapToIntMap = IntMap.fromDistinctAscList . map (\(Const2 k :=> Identity v) -> (k, v)) . DMap.toAscList

-- | Convert a 'DMap' to a regular 'Map', applying the given function to remove
-- the wrapping 'Functor'
dmapToMapWith :: (f v -> v') -> DMap (Const2 k v) f -> Map k v'
dmapToMapWith f = Map.fromDistinctAscList . map (\(Const2 k :=> v) -> (k, f v)) . DMap.toAscList

-- | Convert a regular 'Map' to a 'DMap'
mapToDMap :: Map k v -> DMap (Const2 k v) Identity
mapToDMap = DMap.fromDistinctAscList . map (\(k, v) -> Const2 k :=> Identity v) . Map.toAscList

-- | Convert a regular 'Map', where the values are already wrapped in a functor,
-- to a 'DMap'
mapWithFunctorToDMap :: Map k (f v) -> DMap (Const2 k v) f
mapWithFunctorToDMap = DMap.fromDistinctAscList . map (\(k, v) -> Const2 k :=> v) . Map.toAscList

-- | Convert a regular 'IntMap', where the values are already wrapped in a
-- functor, to a 'DMap'
intMapWithFunctorToDMap :: IntMap (f v) -> DMap (Const2 IntMap.Key v) f
intMapWithFunctorToDMap = DMap.fromDistinctAscList . map (\(k, v) -> Const2 k :=> v) . IntMap.toAscList

-- | Convert a 'DMap' to a regular 'Map' by forgetting the types associated with
-- the keys, using a function to remove the wrapping 'Functor'
weakenDMapWith :: (forall a. v a -> v') -> DMap k v -> Map (Some k) v'
weakenDMapWith f = Map.fromDistinctAscList . map (\(k :=> v) -> (Some.This k, f v)) . DMap.toAscList

--------------------------------------------------------------------------------
-- WrapArg
--------------------------------------------------------------------------------

-- | 'WrapArg' can be used to tag a value in one functor with a type
-- representing another functor.  This was primarily used with dependent-map <
-- 0.2, in which the value type was not wrapped in a separate functor.
data WrapArg :: (k -> *) -> (k -> *) -> * -> * where
  WrapArg :: f a -> WrapArg g f (g a)

deriving instance Eq (f a) => Eq (WrapArg g f (g' a))
deriving instance Ord (f a) => Ord (WrapArg g f (g' a))
deriving instance Show (f a) => Show (WrapArg g f (g' a))
deriving instance Read (f a) => Read (WrapArg g f (g a))

instance GEq f => GEq (WrapArg g f) where
  geq (WrapArg a) (WrapArg b) = (\Refl -> Refl) <$> geq a b

instance GCompare f => GCompare (WrapArg g f) where
  gcompare (WrapArg a) (WrapArg b) = case gcompare a b of
    GLT -> GLT
    GEQ -> GEQ
    GGT -> GGT

--------------------------------------------------------------------------------
-- Convenience functions for DMap
--------------------------------------------------------------------------------

-- | Map over all key/value pairs in a 'DMap', potentially altering the key as
-- well as the value.  The provided function MUST preserve the ordering of the
-- keys, or the resulting 'DMap' will be malformed.
mapKeyValuePairsMonotonic :: (DSum k v -> DSum k' v') -> DMap k v -> DMap k' v'
mapKeyValuePairsMonotonic f = DMap.fromDistinctAscList . map f . DMap.toAscList

{-# INLINE combineDMapsWithKey #-}
-- | Union two 'DMap's of different types, yielding another type.  Each key that
-- is present in either input map will be present in the output.
combineDMapsWithKey :: forall f g h i.
                       GCompare f
                    => (forall a. f a -> These (g a) (h a) -> i a)
                    -> DMap f g
                    -> DMap f h
                    -> DMap f i
combineDMapsWithKey f mg mh = DMap.fromList $ go (DMap.toList mg) (DMap.toList mh)
  where go :: [DSum f g] -> [DSum f h] -> [DSum f i]
        go [] hs = map (\(hk :=> hv) -> hk :=> f hk (That hv)) hs
        go gs [] = map (\(gk :=> gv) -> gk :=> f gk (This gv)) gs
        go gs@((gk :=> gv) : gs') hs@((hk :=> hv) : hs') = case gk `gcompare` hk of
          GLT -> (gk :=> f gk (This gv)) : go gs' hs
          GEQ -> (gk :=> f gk (These gv hv)) : go gs' hs'
          GGT -> (hk :=> f hk (That hv)) : go gs hs'

-- | Extract the values of a 'DMap' of 'EitherTag's.
dmapToThese :: DMap (EitherTag a b) Identity -> Maybe (These a b)
dmapToThese m = case (DMap.lookup LeftTag m, DMap.lookup RightTag m) of
  (Nothing, Nothing) -> Nothing
  (Just (Identity a), Nothing) -> Just $ This a
  (Nothing, Just (Identity b)) -> Just $ That b
  (Just (Identity a), Just (Identity b)) -> Just $ These a b

-- | Tag type for 'Either' to use it as a 'DSum'.
data EitherTag l r a where
  LeftTag :: EitherTag l r l
  RightTag :: EitherTag l r r
  deriving (Typeable)

deriving instance Show (EitherTag l r a)
deriving instance Eq (EitherTag l r a)
deriving instance Ord (EitherTag l r a)

instance GEq (EitherTag l r) where
  geq a b = case (a, b) of
    (LeftTag, LeftTag) -> Just Refl
    (RightTag, RightTag) -> Just Refl
    _ -> Nothing

instance GCompare (EitherTag l r) where
  gcompare a b = case (a, b) of
    (LeftTag, LeftTag) -> GEQ
    (LeftTag, RightTag) -> GLT
    (RightTag, LeftTag) -> GGT
    (RightTag, RightTag) -> GEQ

instance GShow (EitherTag l r) where
  gshowsPrec _ a = case a of
    LeftTag -> showString "LeftTag"
    RightTag -> showString "RightTag"

instance (Show l, Show r) => ShowTag (EitherTag l r) Identity where
  showTaggedPrec t n (Identity a) = case t of
    LeftTag -> showsPrec n a
    RightTag -> showsPrec n a

-- | Convert 'Either' to a 'DSum'. Inverse of 'dsumToEither'.
eitherToDSum :: Either a b -> DSum (EitherTag a b) Identity
eitherToDSum = \case
  Left a -> (LeftTag :=> Identity a)
  Right b -> (RightTag :=> Identity b)

-- | Convert 'DSum' to 'Either'. Inverse of 'eitherToDSum'.
dsumToEither :: DSum (EitherTag a b) Identity -> Either a b
dsumToEither = \case
  (LeftTag :=> Identity a) -> Left a
  (RightTag :=> Identity b) -> Right b

--------------------------------------------------------------------------------
-- ComposeMaybe
--------------------------------------------------------------------------------

-- | We can't use @Compose Maybe@ instead of 'ComposeMaybe', because that would
-- make the 'f' parameter have a nominal type role.  We need f to be
-- representational so that we can use safe 'coerce'.
newtype ComposeMaybe f a =
  ComposeMaybe { getComposeMaybe :: Maybe (f a) } deriving (Show, Eq, Ord)

deriving instance Functor f => Functor (ComposeMaybe f)
