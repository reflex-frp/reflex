{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Control.Monad.Fix
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
main = do
  [0, 1, 1, 0] <- fmap (map fst . concat) $ runApp app () $ map (Just . That) $
    [ That ()
    , This ()
    , That ()
    ]
  return ()

app :: (Reflex t, MonadHold t m, MonadAdjust t m, MonadFix m)
    =>  AppIn t () (These () ())
    -> m (AppOut t Int Int)
app (AppIn _ pulse) = do
  let replace = fmapMaybe (^? here) pulse
      increment = fmapMaybe (^? there) pulse
      w = do
        n <- count increment
        queryDyn $ zipDynWith (\x y -> Selector (Map.singleton (x :: Int) y)) n $ pure $ MyQuery $ SelectedCount 1
  (_, q) <- runQueryT (runWithReplace w $ w <$ replace) $ pure mempty
  let qDyn = fmap (head . Map.keys . unSelector) $ incrementalToDynamic q
  return $ AppOut
    { _appOut_behavior = current qDyn
    , _appOut_event = updated qDyn
    }
