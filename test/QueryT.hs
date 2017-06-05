{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Align
import qualified Data.AppendMap as Map
import Data.Semigroup
import Data.These

import Reflex
import Test.Run

newtype MyQuery = MyQuery SelectedCount
  deriving (Show, Read, Eq, Ord, Monoid, Semigroup, Additive, Group)

instance Query MyQuery where
  type QueryResult MyQuery = ()
  crop _ _ = ()

instance (Ord k, Query a, Eq (QueryResult a)) => Query (Selector k a) where
  type QueryResult (Selector k a) = Selector k (QueryResult a)
  crop q r = undefined

newtype Selector k a = Selector { unSelector :: Map.AppendMap k a }
  deriving (Show, Read, Eq, Ord, Functor)

instance (Ord k, Eq a, Monoid a) => Semigroup (Selector k a) where
  (Selector a) <> (Selector b) = Selector $ fmapMaybe id $ f a b
    where
      f = alignWith $ \case
        This x -> Just x
        That y -> Just y
        These x y ->
          let z = x `mappend` y
          in if z == mempty then Nothing else Just z

instance (Ord k, Eq a, Monoid a) => Monoid (Selector k a) where
  mempty = Selector Map.empty
  mappend = (<>)

instance (Eq a, Ord k, Group a) => Group (Selector k a) where
  negateG = fmap negateG

instance (Eq a, Ord k, Group a) => Additive (Selector k a)

main :: IO ()
main = mainWidget $ do
  let w = do
        n <- count =<< button "Increment"
        display n
        queryDyn $ zipDynWith (\x y -> Selector (Map.singleton (x :: Int) y)) n $ pure $ MyQuery $ SelectedCount 1
        return ()
      outer = do
        replace <- button "Replace"
        widgetHold w $ w <$ replace
  (_, q) <- runQueryT outer $ pure mempty
  display $ incrementalToDynamic q
