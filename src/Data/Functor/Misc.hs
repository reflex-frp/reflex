{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  , dmapToMap
  , mapToDMap
    -- * WrapArg
  , WrapArg (..)
    -- * Convenience functions for DMap
  , mapWithFunctorToDMap
  , mapKeyValuePairsMonotonic
  , combineDMapsWithKey
  , EitherTag (..)
  , dmapToThese
  , eitherToDSum
  , dsumToEither
    -- * Deprecated functions
  , sequenceDmap
  , wrapDMap
  , rewrapDMap
  , unwrapDMap
  , unwrapDMapMaybe
  , extractFunctorDMap
  ) where

import Control.Applicative (Applicative, (<$>))
import Control.Monad.Identity
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum
import Data.GADT.Compare
import Data.GADT.Show
import Data.Map (Map)
import qualified Data.Map as Map
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

-- | Convert a 'DMap' to a regular 'Map'
dmapToMap :: DMap (Const2 k v) Identity -> Map k v
dmapToMap = Map.fromDistinctAscList . map (\(Const2 k :=> Identity v) -> (k, v)) . DMap.toAscList

-- | Convert a regular 'Map' to a 'DMap'
mapToDMap :: Map k v -> DMap (Const2 k v) Identity
mapToDMap = DMap.fromDistinctAscList . map (\(k, v) -> Const2 k :=> Identity v) . Map.toAscList

-- | Convert a regular 'Map', where the values are already wrapped in a functor,
-- to a 'DMap'
mapWithFunctorToDMap :: Map k (f v) -> DMap (Const2 k v) f
mapWithFunctorToDMap = DMap.fromDistinctAscList . map (\(k, v) -> Const2 k :=> v) . Map.toAscList

--------------------------------------------------------------------------------
-- WrapArg
--------------------------------------------------------------------------------

-- | 'WrapArg' can be used to tag a value in one functor with a type
-- representing another functor.  This was primarily used with dependent-map <
-- 0.2, in which the value type was not wrapped in a separate functor.
data WrapArg :: (k -> *) -> (k -> *) -> * -> * where
  WrapArg :: f a -> WrapArg g f (g a)

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
-- Deprecated functions
--------------------------------------------------------------------------------

{-# INLINE sequenceDmap #-}
{-# DEPRECATED sequenceDmap "Use 'Data.Dependent.Map.traverseWithKey (\\_ t -> fmap Identity t)' instead" #-}
-- | Run the actions contained in the 'DMap'
sequenceDmap :: Applicative t => DMap f t -> t (DMap f Identity)
sequenceDmap = DMap.traverseWithKey $ \_ t -> Identity <$> t

{-# DEPRECATED wrapDMap "Use 'Data.Dependent.Map.map (f . runIdentity)' instead" #-}
-- | Replace the 'Identity' functor for a 'DMap''s values with a different functor
wrapDMap :: (forall a. a -> f a) -> DMap k Identity -> DMap k f
wrapDMap f = DMap.map $ f . runIdentity

{-# DEPRECATED rewrapDMap "Use 'Data.Dependent.Map.map' instead" #-}
-- | Replace one functor for a 'DMap''s values with a different functor
rewrapDMap :: (forall (a :: *). f a -> g a) -> DMap k f -> DMap k g
rewrapDMap = DMap.map

{-# DEPRECATED unwrapDMap "Use 'Data.Dependent.Map.map (Identity . f)' instead" #-}
-- | Replace one functor for a 'DMap''s values with the 'Identity' functor
unwrapDMap :: (forall a. f a -> a) -> DMap k f -> DMap k Identity
unwrapDMap f = DMap.map $ Identity . f

{-# DEPRECATED unwrapDMapMaybe "Use 'Data.Dependent.Map.mapMaybeWithKey (\\_ a -> fmap Identity $ f a)' instead" #-}
-- | Like 'unwrapDMap', but possibly delete some values from the DMap
unwrapDMapMaybe :: GCompare k => (forall a. f a -> Maybe a) -> DMap k f -> DMap k Identity
unwrapDMapMaybe f = DMap.mapMaybeWithKey $ \_ a -> Identity <$> f a

{-# DEPRECATED extractFunctorDMap "Use 'mapKeyValuePairsMonotonic (\\(Const2 k :=> Identity v) -> Const2 k :=> v)' instead" #-}
-- | Eliminate the 'Identity' functor in a 'DMap' and replace it with the
-- underlying functor
extractFunctorDMap :: DMap (Const2 k (f v)) Identity -> DMap (Const2 k v) f
extractFunctorDMap = mapKeyValuePairsMonotonic $ \(Const2 k :=> Identity v) -> Const2 k :=> v
