{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import qualified Data.AppendMap as Map
import Data.Semigroup
import Reflex.Dom

newtype MyQuery = MyQuery SelectedCount
  deriving (Show, Read, Eq, Ord, Monoid, Semigroup, Additive, Group)

instance Query MyQuery where
  type QueryResult MyQuery = ()
  crop _ _ = ()

instance (Ord k, Query a) => Query (Map.AppendMap k a) where
  type QueryResult (Map.AppendMap k a) = Map.AppendMap k (QueryResult a)
  crop q r = undefined

main :: IO ()
main = mainWidget $ do
  let w = do
        n <- count =<< button "Increment"
        display n
        queryDyn $ zipDynWith Map.singleton n $ pure $ MyQuery $ SelectedCount 1
        return ()
      outer = do
        replace <- button "Replace"
        widgetHold w $ w <$ replace
  (_, q) <- runQueryT outer $ pure mempty
  display $ incrementalToDynamic q
