-- | Uses a non-associative internal structure to represent a NonEmpty list, but
-- prevents external observers from observing the non-associativity.  This
-- allows O(1) '(<>)'.

{-# LANGUAGE LambdaCase #-}
module Data.List.NonEmpty.Deferred where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty

data NonEmptyDeferred a
   = NonEmptyDeferred_Singleton a
   | NonEmptyDeferred_Append !(NonEmptyDeferred a) !(NonEmptyDeferred a)

singleton :: a -> NonEmptyDeferred a
singleton = NonEmptyDeferred_Singleton

{-# INLINE toNonEmpty #-}
toNonEmpty :: NonEmptyDeferred a -> NonEmpty a
toNonEmpty = go []
  where go t = \case
          NonEmptyDeferred_Singleton a -> a :| t
          NonEmptyDeferred_Append a b -> go (NonEmpty.toList $ go t b) a

instance Semigroup (NonEmptyDeferred a) where
  (<>) = NonEmptyDeferred_Append
