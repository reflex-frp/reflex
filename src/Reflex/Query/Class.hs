{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Reflex.Query.Class
  ( Query (..)
  , QueryMorphism (..)
  , SelectedCount (..)
  , combineSelectedCounts

  , MonadQuery (..)
  , tellQueryDyn
  , queryDyn
  ) where

import Control.Monad.Reader
import Data.Bits
import Data.Data
import Data.Ix
import Data.Semigroup
import Foreign.Storable

import Reflex.Class

class (Monoid (QueryResult a), Semigroup (QueryResult a)) => Query a where
  type QueryResult a :: *
  crop :: a -> QueryResult a -> QueryResult a

-- | NB: QueryMorphism's must be group homomorphisms when acting on the query type
-- and compatible with the query relationship when acting on the query result
data QueryMorphism q q' = QueryMorphism
  { _queryMorphism_mapQuery :: q -> q'
  , _queryMorphism_mapQueryResult :: QueryResult q' -> QueryResult q
  }

-- instance (Ord k, Query v) => Query (AppendMap k v) where
--   type QueryResult (AppendMap k v) = AppendMap k (QueryResult v)
--   crop q r = AppendMap.intersectionWith (flip crop) r q

-- singletonQuery :: (Monoid (QueryResult q), Ord k) => k -> QueryMorphism q (AppendMap k q)
-- singletonQuery k = QueryMorphism { _queryMorphism_mapQuery = AppendMap.singleton k
--                                  , _queryMorphism_mapQueryResult = AppendMap.findWithDefault mempty k
--                                  }

-- | This type keeps track of the multiplicity of elements of the view selector that are being used by the app
newtype SelectedCount = SelectedCount { unSelectedCount :: Int }
  deriving (Eq, Ord, Show, Read, Integral, Num, Bounded, Enum, Real, Ix, Bits, FiniteBits, Storable, Data
           -- , ToJSON, FromJSON
           )

instance Semigroup SelectedCount where
  SelectedCount a <> SelectedCount b = SelectedCount (a + b)

instance Monoid SelectedCount where
  mempty = SelectedCount 0
  mappend = (<>)

instance Group SelectedCount where
  negateG (SelectedCount a) = SelectedCount (negate a)

instance Additive SelectedCount

-- | The Semigroup/Monoid/Group instances for a ViewSelector should use this function which returns Nothing if the result is 0. This allows the pruning of leaves that are no longer wanted.
combineSelectedCounts :: SelectedCount -> SelectedCount -> Maybe SelectedCount
combineSelectedCounts (SelectedCount i) (SelectedCount j) = if i == negate j then Nothing else Just $ SelectedCount (i + j)

-- class ( ToJSON (ViewSelector app SelectedCount), FromJSON (ViewSelector app SelectedCount)
--       , ToJSON (View app), FromJSON (View app)
--       , Monoid (ViewSelector app SelectedCount), Semigroup (ViewSelector app SelectedCount)
--       , Query (ViewSelector app SelectedCount), QueryResult (ViewSelector app SelectedCount) ~ View app
--       , Align (ViewSelector app)
--       , Eq (View app)
--       , Show (View app)
--       ) => HasView app where
--   type View app
--   type ViewSelector app :: * -> *

-- cropView :: (Query q) => q -> QueryResult q -> QueryResult q
-- cropView = crop

class (Group q, Additive q, Query q) => MonadQuery t q m | m -> q t where
  tellQueryIncremental :: Incremental t (AdditivePatch q) -> m ()
  askQueryResult :: m (Dynamic t (QueryResult q))
  queryIncremental :: Incremental t (AdditivePatch q) -> m (Dynamic t (QueryResult q))

instance (Monad m, MonadQuery t q m) => MonadQuery t q (ReaderT r m) where
  tellQueryIncremental = lift . tellQueryIncremental
  askQueryResult = lift askQueryResult
  queryIncremental = lift . queryIncremental

tellQueryDyn :: (Reflex t, MonadQuery t q m) => Dynamic t q -> m ()
tellQueryDyn d = tellQueryIncremental $ unsafeBuildIncremental (sample (current d)) $ attachWith (\old new -> AdditivePatch $ new ~~ old) (current d) (updated d)

queryDyn :: (Reflex t, Monad m, MonadQuery t q m) => Dynamic t q -> m (Dynamic t (QueryResult q))
queryDyn q = do
  tellQueryDyn q
  r <- askQueryResult
  return $ zipDynWith crop q r
