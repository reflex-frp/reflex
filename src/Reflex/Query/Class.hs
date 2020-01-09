{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module:
--   Reflex.Query.Class
-- Description:
--   A class that ties together queries to some data source and their results,
--   providing methods for requesting data from the source and accumulating
--   streamed results.
module Reflex.Query.Class
  ( Query (..)
  , QueryMorphism (..)
  , SelectedCount (..)
  , combineSelectedCounts

  , MonadQuery (..)
  , tellQueryDyn
  , queryDyn
  , mapQuery
  , mapQueryResult
  ) where

import Control.Category (Category)
import qualified Control.Category as Cat
import Control.Monad.Reader
import Data.Bits
import Data.Data
import Data.Ix
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MonoidalMap
import Data.Semigroup (Semigroup(..))
import Foreign.Storable

import Reflex.Class

-- | A 'Query' can be thought of as a declaration of interest in some set of data.
-- A 'QueryResult' is the set of data associated with that interest set.
-- The @crop@ function provides a way to determine what part of a given 'QueryResult'
-- is relevant to a given 'Query'.
class (Monoid (QueryResult a), Semigroup (QueryResult a)) => Query a where
  type QueryResult a :: *
  crop :: a -> QueryResult a -> QueryResult a

instance (Ord k, Query v) => Query (MonoidalMap k v) where
  type QueryResult (MonoidalMap k v) = MonoidalMap k (QueryResult v)
  crop q r = MonoidalMap.intersectionWith (flip crop) r q

-- | QueryMorphism's must be group homomorphisms when acting on the query type
-- and compatible with the query relationship when acting on the query result.
data QueryMorphism q q' = QueryMorphism
  { _queryMorphism_mapQuery :: q -> q'
  , _queryMorphism_mapQueryResult :: QueryResult q' -> QueryResult q
  }

instance Category QueryMorphism where
  id = QueryMorphism id id
  qm . qm' = QueryMorphism
    { _queryMorphism_mapQuery = mapQuery qm . mapQuery qm'
    , _queryMorphism_mapQueryResult = mapQueryResult qm' . mapQueryResult qm
    }

-- | Apply a 'QueryMorphism' to a 'Query'
mapQuery :: QueryMorphism q q' -> q -> q'
mapQuery = _queryMorphism_mapQuery

-- | Map a 'QueryMorphism' to a 'QueryResult'
mapQueryResult :: QueryMorphism q q' -> QueryResult q' -> QueryResult q
mapQueryResult = _queryMorphism_mapQueryResult

-- | This type can be used to track of the frequency of interest in a given 'Query'. See note on
-- 'combineSelectedCounts'
newtype SelectedCount = SelectedCount { unSelectedCount :: Int }
  deriving (Eq, Ord, Show, Read, Integral, Num, Bounded, Enum, Real, Ix, Bits, FiniteBits, Storable, Data)

instance Semigroup SelectedCount where
  SelectedCount a <> SelectedCount b = SelectedCount (a + b)

instance Monoid SelectedCount where
  mempty = SelectedCount 0
  mappend = (<>)

instance Group SelectedCount where
  negateG (SelectedCount a) = SelectedCount (negate a)

instance Additive SelectedCount

-- | The Semigroup/Monoid/Group instances for a Query containing 'SelectedCount's should use
-- this function which returns Nothing if the result is 0. This allows the pruning of leaves
-- of the 'Query' that are no longer wanted.
combineSelectedCounts :: SelectedCount -> SelectedCount -> Maybe SelectedCount
combineSelectedCounts (SelectedCount i) (SelectedCount j) = if i == negate j then Nothing else Just $ SelectedCount (i + j)

-- | A class that allows sending of 'Query's and retrieval of 'QueryResult's. See 'queryDyn' for a commonly
-- used interface.
class (Group q, Additive q, Query q) => MonadQuery t q m | m -> q t where
  tellQueryIncremental :: Incremental t (AdditivePatch q) -> m ()
  askQueryResult :: m (Dynamic t (QueryResult q))
  queryIncremental :: Incremental t (AdditivePatch q) -> m (Dynamic t (QueryResult q))

instance (Monad m, MonadQuery t q m) => MonadQuery t q (ReaderT r m) where
  tellQueryIncremental = lift . tellQueryIncremental
  askQueryResult = lift askQueryResult
  queryIncremental = lift . queryIncremental

-- | Produce and send an 'Incremental' 'Query' from a 'Dynamic' 'Query'.
tellQueryDyn :: (Reflex t, MonadQuery t q m) => Dynamic t q -> m ()
tellQueryDyn d = tellQueryIncremental $ unsafeBuildIncremental (sample (current d)) $ attachWith (\old new -> AdditivePatch $ new ~~ old) (current d) (updated d)

-- | Retrieve 'Dynamic'ally updating 'QueryResult's for a 'Dynamic'ally updating 'Query'.
queryDyn :: (Reflex t, Monad m, MonadQuery t q m) => Dynamic t q -> m (Dynamic t (QueryResult q))
queryDyn q = do
  tellQueryDyn q
  zipDynWith crop q <$> askQueryResult
