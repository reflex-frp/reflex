{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | 'Data.Map' with a better 'Monoid' instance
--
-- 'Data.Map' has @mappend = union@, which is left-biased.  AppendMap has
-- @mappend = unionWith mappend@ instead.
module Data.AppendMap
  ( module Data.AppendMap
  , module Data.Map.Monoidal
  ) where

import Prelude hiding (map, null)

import Data.Coerce
import Data.Default
import Data.Map (Map)
#if MIN_VERSION_containers(0,5,11)
import qualified Data.Map.Internal.Debug as Map (showTree, showTreeWith)
#else
import qualified Data.Map as Map (showTree, showTreeWith)
#endif
import Data.Witherable (Filterable(..))
import Data.Map.Monoidal (MonoidalMap(..), delete, null, empty)
import qualified Data.Map.Monoidal as M

{-# DEPRECATED AppendMap "Use 'MonoidalMap' instead" #-}
type AppendMap = MonoidalMap

{-# DEPRECATED _unAppendMap "Use 'getMonoidalMap' instead" #-}
_unAppendMap :: MonoidalMap k v -> Map k v
_unAppendMap = getMonoidalMap

pattern AppendMap :: Map k v -> MonoidalMap k v
pattern AppendMap m = MonoidalMap m

instance Filterable (MonoidalMap k) where
  mapMaybe = M.mapMaybe

-- | Deletes a key, returning 'Nothing' if the result is empty.
nonEmptyDelete :: Ord k => k -> MonoidalMap k a -> Maybe (MonoidalMap k a)
nonEmptyDelete k vs =
  let deleted = delete k vs
  in if null deleted
       then Nothing
       else Just deleted

mapMaybeNoNull :: (a -> Maybe b)
               -> MonoidalMap token a
               -> Maybe (MonoidalMap token b)
mapMaybeNoNull f as =
  let bs = mapMaybe f as
  in if null bs
       then Nothing
       else Just bs

-- TODO: Move instances to `Reflex.Patch`
showTree :: forall k a. (Show k, Show a) => MonoidalMap k a -> String
showTree = coerce (Map.showTree :: Map k a -> String)

showTreeWith :: forall k a. (k -> a -> String) -> Bool -> Bool -> MonoidalMap k a -> String
showTreeWith = coerce (Map.showTreeWith :: (k -> a -> String) -> Bool -> Bool -> Map k a -> String)

instance Default (MonoidalMap k a) where
  def = empty
