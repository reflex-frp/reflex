{-# LANGUAGE LambdaCase #-}
module Data.List.Deferred
  ( Deferred
  , empty
  , singleton
  , toNonEmpty
  , fromNonEmpty
  , toList
  ) where

import Data.List.NonEmpty.Deferred.Internal (NonEmptyDeferred (..))
import qualified Data.List.NonEmpty.Deferred as NonEmpty

data Deferred a
   = Deferred_Empty
   | Deferred_Singleton a
   | Deferred_Append !(NonEmptyDeferred a) !(NonEmptyDeferred a)

{-# INLINE toNonEmpty #-}
toNonEmpty :: Deferred a -> Maybe (NonEmptyDeferred a)
toNonEmpty = \case
  Deferred_Empty -> Nothing
  Deferred_Singleton a -> Just $ NonEmpty.singleton a
  Deferred_Append a b -> Just $ a <> b

{-# INLINE fromNonEmpty #-}
fromNonEmpty :: NonEmptyDeferred a -> Deferred a
fromNonEmpty = \case
  NonEmptyDeferred_Singleton a -> Deferred_Singleton a
  NonEmptyDeferred_Append a b -> Deferred_Append a b

{-# INLINE empty #-}
empty :: Deferred a
empty = Deferred_Empty

{-# INLINE singleton #-}
singleton :: a -> Deferred a
singleton = fromNonEmpty . NonEmpty.singleton

{-# INLINE toList #-}
toList :: Deferred a -> [a]
toList = \case
  Deferred_Empty -> []
  Deferred_Singleton a -> [a]
  Deferred_Append a b -> NonEmpty.toList $ a <> b

instance Semigroup (Deferred a) where
  (<>) = \case
    Deferred_Empty -> id
    a@(Deferred_Singleton va) -> \case
      Deferred_Empty -> a
      Deferred_Singleton vb -> Deferred_Append (NonEmpty.singleton va) (NonEmpty.singleton vb)
      Deferred_Append b1 b2 -> Deferred_Append (NonEmpty.singleton va) (b1 <> b2)
    a@(Deferred_Append a1 a2) -> \case
      Deferred_Empty -> a
      Deferred_Singleton vb -> Deferred_Append (a1 <> a2) (NonEmpty.singleton vb)
      Deferred_Append b1 b2 -> Deferred_Append (a1 <> a2) (b1 <> b2)

instance Monoid (Deferred a) where
  mempty = Deferred_Empty
