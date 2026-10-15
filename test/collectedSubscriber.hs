{-# LANGUAGE LambdaCase #-}
-- | Regression tests for space leaks & useless propagation. When the only
-- subscriber of an event is garbage collected instead of unsubscribed, the
-- event must still unsubscribe from its parent; otherwise the subscriptions up
-- to the root stay alive for as long as the application keeps a reference to
-- the event.
import Control.Exception (evaluate)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Functor.Misc (Const2 (..))
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Control.Concurrent (newEmptyMVar, putMVar, tryTakeMVar, yield)
import System.Mem.Weak (mkWeakPtr)
import System.Mem (performMajorGC)

import Reflex
import Reflex.Host.Class
import qualified Reflex.Spider.Internal as S
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit

type T = SpiderTimeline Global

collectTheOnlySubscriberThen
  :: (Event T Int -> (Event T Int, keep))
  -> (keep -> SpiderHost Global ())
  -> Assertion
collectTheOnlySubscriberThen build steps = do
  setupCount <- newIORef (0 :: Int)
  (finalCount, keep) <- runSpiderHost $ do
    root <- newEventWithTrigger $ \_ -> do
      modifyIORef' setupCount succ
      pure $ modifyIORef' setupCount pred
    case build root of
      (e, keep) -> do
        -- After this frame nothing references the 'hold'.
        runHostFrame $ void $ hold 0 e
        liftIO $ assertEqual "setupCount before the garbage collection" 1 =<< readIORef setupCount
        liftIO collectAndSettle
        steps keep
        finalCount <- liftIO $ readIORef setupCount
        pure (finalCount, keep)
  _ <- evaluate keep
  assertEqual "setupCount at the end" 0 finalCount

collectTheOnlySubscriber :: (Event T Int -> (Event T Int, keep)) -> Assertion
collectTheOnlySubscriber build = collectTheOnlySubscriberThen build (\_ -> pure ())

collectAndSettle :: IO ()
collectAndSettle = do
  done <- newEmptyMVar
  sentinel <- newIORef ()
  weakSentinel <- mkWeakPtr sentinel (Just (putMVar done ()))
  writeIORef sentinel ()
  performMajorGC
  let spin :: Int -> IO ()
      spin retries = tryTakeMVar done >>= \case
        Just () -> pure ()
        Nothing
          | retries <= 0 -> putStrLn "  warning: sentinel finalizer did not run"
          | otherwise -> yield >> performMajorGC >> spin (retries - 1)
  spin 1000
  _ <- evaluate weakSentinel
  pure ()

main :: IO ()
main = defaultMain $ testGroup "events unsubscribe from parent after GC of last subscriber"
  [ testCase "cacheEvent" $
      collectTheOnlySubscriber $ \root ->
        let cached = S.SpiderEvent $ S.cacheEvent $ S.unSpiderEvent root
        in (cached, cached)
  , testCase "merge" $
      collectTheOnlySubscriber $ \root ->
        let merged = leftmost [root]
        in (merged, merged)
  -- For fan/fanInt we want unsubscribing from the parent even if the event selector is retained:
  , testCase "fan" $
      collectTheOnlySubscriber $ \root ->
        let selector = fanMap $ fmapCheap (Map.singleton (0 :: Int)) root
        in (select selector (Const2 0), selector)
  , testCase "fanInt" $
      collectTheOnlySubscriber $ \root ->
        let selector = fanInt $ fmapCheap (IntMap.singleton 0) root
        in (selectInt selector 0, selector)
  , testCase "root" $
      collectTheOnlySubscriber $ \root -> (root, root)
  , testCase "switch" $
      collectTheOnlySubscriber $ \root ->
        let switched = switch (constant root)
        in (switched, switched)
  , testCase "coincidence" $
      collectTheOnlySubscriber $ \root ->
        let inner = coincidence (fmapCheap (const root) root)
        in (inner, inner)
  ]
