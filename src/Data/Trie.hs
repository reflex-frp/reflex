{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module Data.Trie where

import Prelude hiding (null)
import qualified Prelude

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isNothing, isJust)
import Data.Sequence (Seq ((:<|)), (<|))
import qualified Data.Sequence as Seq

-- | A mapping from `Seq a` to `b`, isomorphic to `Map (Seq a) b`
data Trie a b = Trie (Seq a) (Maybe b) (Map a (Trie a b))
  deriving (Show, Functor, Foldable, Traversable)

instance (Ord a, Semigroup b) => Semigroup (Trie a b) where
  (<>) = unionWith (<>)

instance (Ord a, Semigroup b) => Monoid (Trie a b) where
  mempty = empty

empty :: Trie a b
empty = Trie Seq.empty Nothing Map.empty

null :: Trie a b -> Bool
null (Trie _ l c) = isNothing l && Map.null c

fromList :: (Ord a, Semigroup b) => [(Seq a, b)] -> Trie a b
fromList = mconcat . fmap (\(as, b) -> Trie as (Just b) mempty)

toList :: (Ord a, Semigroup b) => Trie a b -> [(Seq a, b)]
toList (Trie prefix mLeaf children) = here <> beneath
  where
    here = case mLeaf of
      Nothing -> []
      Just leaf -> [(prefix, leaf)]
    beneath = do
      (discriminator, child) <- Map.toList children
      (childPrefix, value) <- toList child
      pure (prefix <> Seq.singleton discriminator <> childPrefix, value)

fromMap :: (Ord a, Semigroup b) => Map (Seq a) b -> Trie a b
fromMap = fromList . Map.toList

toMap :: (Ord a, Semigroup b) => Trie a b -> Map (Seq a) b
toMap = Map.fromList . toList

trieInvariants :: [(String, Trie a b -> Bool)]
trieInvariants =
  [ ( "Child `Trie`s cannot be empty"
    , \(Trie _ _ children) ->
        all (not . null) children
    )
  , ( "Child `Trie`s must be valid"
    , \(Trie _ _ children) ->
        all validTrie children
    )
  , ( "If a trie is empty, its prefix must be empty"
    , \(Trie prefix leaf children) ->
        isJust leaf || not (Map.null children) || Prelude.null prefix
    )
  , ( "A trie cannot have just one child unless it has a leaf"
    , \(Trie _ leaf children) ->
        (Map.size children /= 1) || isJust leaf
    )
  ]

validTrie :: Trie a b -> Bool
validTrie t = all (\(_, f) -> f t) trieInvariants

unionWith :: Ord a => (b -> b -> b) -> Trie a b -> Trie a b -> Trie a b
unionWith f t1@(Trie p1 l1 c1) t2@(Trie p2 l2 c2) = if
  | isNothing l1 && Map.null c1 -> t2
  | isNothing l2 && Map.null c2 -> t1
  | otherwise ->
    let (p, s1, s2) = matchPrefixes p1 p2
        l1p = if Prelude.null s1 then l1 else Nothing
        l2p = if Prelude.null s2 then l2 else Nothing
        c1p = case s1 of
          Seq.Empty -> c1
          s1h :<| s1t -> Map.singleton s1h $ Trie s1t l1 c1
        c2p = case s2 of
          Seq.Empty -> c2
          s2h :<| s2t -> Map.singleton s2h $ Trie s2t l2 c2
        l = case (l1p, l2p) of
          (Nothing, Nothing) -> Nothing
          (Just l1v, Nothing) -> Just l1v
          (Nothing, Just l2v) -> Just l2v
          (Just l1v, Just l2v) -> Just $ f l1v l2v
    in Trie p l $ Map.unionWith (unionWith f) c1p c2p

-- | Given two lists, return their common prefix as well as any remaining suffixes
matchPrefixes :: Eq a => Seq a -> Seq a -> (Seq a, Seq a, Seq a)
matchPrefixes Seq.Empty b = (Seq.empty, Seq.empty, b)
matchPrefixes a Seq.Empty = (Seq.empty, a, Seq.empty)
matchPrefixes a@(ah :<| at) b@(bh :<| bt) =
  if ah == bh
  then let (c, as, bs) = matchPrefixes at bt
       in (ah <| c, as, bs)
  else (Seq.empty, a, b)
