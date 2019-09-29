-- | Uses a non-associative internal structure to represent a NonEmpty list, but
-- prevents external observers from observing the non-associativity.  This
-- allows O(1) '(<>)'.

{-# LANGUAGE LambdaCase #-}
module Data.List.NonEmpty.Deferred
  ( NonEmptyDeferred
  , singleton
  , toNonEmpty
  , toList
  ) where

import Data.List.NonEmpty.Deferred.Internal
