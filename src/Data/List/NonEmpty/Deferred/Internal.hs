{-# LANGUAGE LambdaCase #-}
module Data.List.NonEmpty.Deferred.Internal where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty

data NonEmptyDeferred a
   = NonEmptyDeferred_Singleton a
   | NonEmptyDeferred_Append !(NonEmptyDeferred a) !(NonEmptyDeferred a)

{-# INLINE singleton #-}
singleton :: a -> NonEmptyDeferred a
singleton = NonEmptyDeferred_Singleton

{-# INLINE toNonEmpty #-}
toNonEmpty :: NonEmptyDeferred a -> NonEmpty a
toNonEmpty = go []
  where go t = \case
          NonEmptyDeferred_Singleton a -> a :| t
          NonEmptyDeferred_Append a b -> go (NonEmpty.toList $ go t b) a

{-# INLINE toList #-}
toList :: NonEmptyDeferred a -> [a]
toList = go []
  where go t = \case
          NonEmptyDeferred_Singleton a -> a : t
          NonEmptyDeferred_Append a b -> go (go t b) a

instance Semigroup (NonEmptyDeferred a) where
  (<>) = NonEmptyDeferred_Append
