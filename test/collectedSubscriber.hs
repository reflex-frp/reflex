{-# LANGUAGE LambdaCase #-}

-- | Regression tests for space leaks & useless propagation. When the only
-- subscriber of an event is garbage collected instead of unsubscribed, the
-- event must still unsubscribe from its parent; otherwise the subscriptions up
-- to the root stay alive for as long as the application keeps a reference to
-- the event.
module Main where

import Control.Concurrent (newEmptyMVar, putMVar, tryTakeMVar, yield)
import Control.Exception (evaluate)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Dependent.Sum (DSum ((:=>)))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Misc (Const2 (..))
import Data.IORef (IORef, mkWeakIORef, modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import System.Mem (performMajorGC)
import System.Mem.Weak (Weak, deRefWeak, mkWeakPtr)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit

import Reflex
import Reflex.Host.Class
import qualified Reflex.Spider.Internal as S

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
      spin retries =
        tryTakeMVar done >>= \case
          Just () -> pure ()
          Nothing
            | retries <= 0 -> putStrLn "  warning: sentinel finalizer did not run"
            | otherwise -> yield >> performMajorGC >> spin (retries - 1)
  spin 1000
  _ <- evaluate weakSentinel
  pure ()

main :: IO ()
main =
  defaultMain $
    testGroup
      "collectedSubscriber"
      [ unsubscribeAfterCollection
      , unsubscribedSwitchReleasesBehavior
      , unsubscribedSwitchIsNotInvalidated
      , mergeUnsubscribedAfterItsPatchChangedItsHeight
      , coincidenceUnsubscribedAfterItRaisedItsHeight
      ]

unsubscribeAfterCollection :: TestTree
unsubscribeAfterCollection =
  testGroup
    "events unsubscribe from parent after GC of last subscriber"
    [ testCase "cacheEvent" $
        collectTheOnlySubscriber $ \root ->
          let cached = S.SpiderEvent $ S.cacheEvent $ S.unSpiderEvent root
          in (cached, cached)
    , testCase "merge" $
        collectTheOnlySubscriber $ \root ->
          let merged = leftmost [root]
          in (merged, merged)
    , -- For fan/fanInt we want unsubscribing from the parent even if the event selector is retained:
      testCase "fan" $
        collectTheOnlySubscriber $ \root ->
          let selector = fanMap $ fmapCheap (Map.singleton (0 :: Int)) root
          in (select selector (Const2 0), selector)
    , testCase "fanInt" $
        collectTheOnlySubscriber $ \root ->
          let selector = fanInt $ fmapCheap (IntMap.singleton 0) root
          in (selectInt selector 0, selector)
    , testCase "root" $
        collectTheOnlySubscriber $
          \root -> (root, root)
    , testCase "switch" $
        collectTheOnlySubscriber $ \root ->
          let switched = switch (constant root)
          in (switched, switched)
    , testCase "coincidence" $
        collectTheOnlySubscriber $ \root ->
          let inner = coincidence (fmapCheap (const root) root)
          in (inner, inner)
    ]

subscribeSwitchOverPull :: Behavior T Int -> Event T Int -> S.SpiderHostFrame Global (S.EventSubscription Global, Weak (IORef ()))
subscribeSwitchOverPull held inner = S.SpiderHostFrame $ do
  captured <- liftIO $ newIORef ()
  weakCaptured <- liftIO $ mkWeakIORef captured (pure ())
  let behavior = S.pull $ do
        _ <- S.readBehaviorTracked $ S.unSpiderBehavior held
        liftIO $ readIORef captured
        pure $ S.unSpiderEvent inner
      ignore =
        S.Subscriber
          { S.subscriberPropagate = \_ -> pure ()
          , S.subscriberInvalidateHeight = \_ -> pure ()
          , S.subscriberRecalculateHeight = \_ -> pure ()
          }
  S.SubscribeResult subscription _ <- S.subscribeAndRead (S.switchUncached behavior) ignore
  pure (subscription, weakCaptured)

unsubscribedSwitchReleasesBehavior :: TestTree
unsubscribedSwitchReleasesBehavior =
  testCase "an unsubscribed switch does not retain what its behavior captured or read" $ do
    (subscription, weakCaptured) <- runSpiderHost $ do
      (updates, _) <- newEventWithTriggerRef
      runHostFrame $ do
        held <- hold 0 updates
        subscribeSwitchOverPull held updates
    S.unsubscribe subscription
    collectAndSettle
    capturedIsCollected <- isNothing <$> deRefWeak weakCaptured
    _ <- evaluate subscription
    assertBool "the value that the behavior captured is collected" capturedIsCollected

-- | 'S.runFrame' errors if this reconnects the unsubscribed switch.
unsubscribedSwitchIsNotInvalidated :: TestTree
unsubscribedSwitchIsNotInvalidated =
  testCase "an update of a hold that an unsubscribed switch read does not invalidate the switch" $
    runSpiderHost $ do
      (updates, updatesTriggerRef) <- newEventWithTriggerRef
      (subscription, _) <- runHostFrame $ do
        held <- hold 0 updates
        subscribeSwitchOverPull held updates
      liftIO $ S.unsubscribe subscription
      fireEventRef updatesTriggerRef 1
      _ <- liftIO $ evaluate subscription
      pure ()

-- | A merge's patch changes its height in the frame in which its only
-- subscriber drops it. The merge must then keep its height, because that
-- subscriber belongs to a released cacheEvent record (checked under
-- DEBUG_FRAME).
mergeUnsubscribedAfterItsPatchChangedItsHeight :: TestTree
mergeUnsubscribedAfterItsPatchChangedItsHeight =
  testGroup
    "a merge whose patch changes its height in the frame in which its subscriber drops it"
    [ testCase (patchName <> ", " <> dropName) $ do
        occurrence <- runSpiderHost $ do
          (input, inputTriggerRef) <- newEventWithTriggerRef
          handle <- runHostFrame $ do
            members <- holdIncremental (IntMap.singleton 0 input) (patchOnInput input <$ input)
            subscribeEvent =<< dropOnInput input (sum <$> mergeIntIncremental members)
          fireEventRefAndRead inputTriggerRef 1 handle
        occurrence @?= Just 1
    | (patchName, patchOnInput) <-
        [ ("the patch removes the only member", \_ -> PatchIntMap (IntMap.singleton 0 Nothing))
        , -- A merge of the input is higher than the input.
          ("the patch adds a higher member", \input -> PatchIntMap (IntMap.singleton 1 (Just (leftmost [input]))))
        ]
          :: [(String, Event T Int -> PatchIntMap (Event T Int))]
    , (dropName, dropOnInput) <-
        [ ("a switch leaves the merge", \input merged -> switchHold merged (never <$ input))
        ,
          ( "a patch removes the merge from a merge"
          , \input merged -> do
              collection <- holdIncremental (IntMap.singleton 0 merged) (PatchIntMap (IntMap.singleton 0 Nothing) <$ input)
              pure $ sum <$> mergeIntIncremental collection
          )
        , -- The coincidence unsubscribes from its inner event at the end of the
          -- frame.
          ("the merge is the inner event of a coincidence", \input merged -> pure $ coincidence $ merged <$ input)
        ]
          :: [(String, Event T Int -> Event T Int -> HostFrame T (Event T Int))]
    ]

-- | A coincidence that raised its height and was dropped in the same frame
-- must not reset its height (checked under DEBUG_FRAME).
coincidenceUnsubscribedAfterItRaisedItsHeight :: TestTree
coincidenceUnsubscribedAfterItRaisedItsHeight =
  testGroup
    "a coincidence that raised its height, and whose subscriber unsubscribes in the same frame"
    [ testCase name $ do
        occurrence <- runSpiderHost $ do
          (input, inputTriggerRef) <- newEventWithTriggerRef
          handle <- runHostFrame $ do
            -- A merge of the input is higher than the input.
            let raised = coincidence $ leftmost [input] <$ input
            subscribeEvent =<< dropAtFirstOccurrence input raised
          fireEventRefAndRead inputTriggerRef 1 handle
        occurrence @?= Just 1
    | (name, dropAtFirstOccurrence) <-
        [ ("a headE of a merge unsubscribes in the propagation", \_ e -> headE $ leftmost [e])
        ,
          ( "a patch removes the coincidence from a merge"
          , \input e -> do
              members <- holdIncremental (IntMap.singleton 0 e) (PatchIntMap (IntMap.singleton 0 Nothing) <$ input)
              pure $ sum <$> mergeIntIncremental members
          )
        , ("the coincidence is the inner event of a coincidence", \input e -> pure $ coincidence $ e <$ input)
        ]
          :: [(String, Event T Int -> Event T Int -> HostFrame T (Event T Int))]
    ]
