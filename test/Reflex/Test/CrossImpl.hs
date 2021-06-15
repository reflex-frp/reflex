{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import Prelude hiding (and, foldl, mapM, mapM_, sequence, sequence_)

import Control.Monad.Ref
import Reflex.Class
import Reflex.Dynamic
import Reflex.Host.Class
import qualified Reflex.Pure as P
import qualified Reflex.Spider.Internal as S
import qualified Reflex.Profiled as Prof

import Control.Arrow (second, (&&&))
import Control.Monad.Identity hiding (forM, forM_, mapM, mapM_, sequence, sequence_)
import Control.Monad.State.Strict hiding (forM, forM_, mapM, mapM_, sequence, sequence_)
import Data.Dependent.Sum (DSum (..))
import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Monoid
import Data.Traversable
import System.Exit
import System.Mem

mapToPureBehavior :: Map Int a -> Behavior PureReflexDomain a
mapToPureBehavior m = P.Behavior $ \t -> case Map.lookupLE t m of
  Nothing -> error $ "mapToPureBehavior: no value for time " <> show t
  Just (_, v) -> v

mapToPureEvent :: Map Int a -> Event PureReflexDomain a
mapToPureEvent m = P.Event $ flip Map.lookup m

relevantTestingTimes :: (Map Int a, Map Int b) -> [Int]
relevantTestingTimes (b, e) = case Set.minView &&& Set.maxView $ Map.keysSet b `Set.union` Map.keysSet e of
  (Just (t0, _), Just (t1, _)) -> [t0..t1+1] -- We need to go to b+1 to see the result of the final event
  _ -> [] -- Doesn't actually make much sense

type PureReflexDomain = P.Pure Int
type TimeM = (->) Int

testPure :: (t ~ PureReflexDomain, m ~ TimeM) => ((Behavior t a, Event t b) -> m (Behavior t c, Event t d)) -> (Map Int a, Map Int b) -> (Map Int c, Map Int d)
testPure builder (b, e) =
  let (P.Behavior b', P.Event e') = ($ 0) $ builder (mapToPureBehavior b, mapToPureEvent e)
      relevantTimes = relevantTestingTimes (b, e)
      e'' = Map.mapMaybe id $ Map.fromList $ map (id &&& e') relevantTimes
      b'' = Map.fromList $ map (id &&& b') relevantTimes
  in (b'', e'')

class MapMSignals a a' t t' | a -> t, a' -> t', a t' -> a', a' t -> a where
  mapMSignals :: Monad m => (forall b. Behavior t b -> m (Behavior t' b)) -> (forall b. Event t b -> m (Event t' b)) -> a -> m a'

instance MapMSignals (Behavior t a) (Behavior t' a) t t' where
  mapMSignals fb _ = fb

instance MapMSignals (Event t a) (Event t' a) t t' where
  mapMSignals _ fe = fe

instance (MapMSignals a a' t t', MapMSignals b b' t t') => MapMSignals (a, b) (a', b') t t' where
  mapMSignals fb fe (a, b) = liftM2 (,) (mapMSignals fb fe a) (mapMSignals fb fe b)

testTimeline
  :: (forall m t. TestCaseConstraint t m => (Behavior t a, Event t b) -> m (Behavior t c, Event t d))
  -> (Map Int a, Map Int b)
  -> (forall m t. (MonadReflexHost t m, MonadIO m, MonadRef m, Ref m ~ Ref IO, MonadSample t m) => m (Map Int c, Map Int d))
testTimeline builder (bMap, eMap) = do
  (re, reTrigger) <- newEventWithTriggerRef
  (rb, rbTrigger) <- newEventWithTriggerRef
  b <- runHostFrame $ hold (error "testTimeline: No value for input behavior yet") rb
  (b', e') <- runHostFrame $ builder (b, re)
  e'Handle <- subscribeEvent e' --TODO: This should be unnecessary
  let times = relevantTestingTimes (bMap, eMap)
  liftIO performGC
  outputs <- forM times $ \t -> do
    forM_ (Map.lookup t bMap) $ \val -> mapM_ (\ rbt -> fireEvents [rbt :=> Identity val]) =<< readRef rbTrigger
    bOutput <- sample b'
    eOutput <- fmap join $ forM (Map.lookup t eMap) $ \val -> do
      mret <- readRef reTrigger
      let firing = case mret of
            Just ret -> [ret :=> Identity val]
            Nothing -> []
      fireEventsAndRead firing $ sequence =<< readEvent e'Handle
    liftIO performGC
    return (t, (bOutput, eOutput))
  return (Map.fromList $ map (second fst) outputs, Map.mapMaybe id $ Map.fromList $ map (second snd) outputs)

tracePerf :: Show a => a -> b -> b
tracePerf = flip const

testAgreement :: (Eq c, Eq d, Show c, Show d) => (forall m t. TestCaseConstraint t m => (Behavior t a, Event t b) -> m (Behavior t c, Event t d)) -> (Map Int a, Map Int b) -> IO Bool
testAgreement builder inputs = do
  let identityResult = testPure builder inputs
  tracePerf "---------" $ return ()
  spiderResult <- S.runSpiderHost $ testTimeline builder inputs
  tracePerf "---------" $ return ()
  profiledResult <- S.runSpiderHost $ Prof.runProfiledM $ testTimeline builder inputs
  tracePerf "---------" $ return ()
  let resultsAgree = identityResult == spiderResult && identityResult == profiledResult
  if resultsAgree
    then do --putStrLn "Success:"
            --print identityResult
            return ()
    else do putStrLn "Failure:"
            putStrLn $ "Pure result: " <> show identityResult
            putStrLn $ "Spider result:   " <> show spiderResult
            putStrLn $ "Profiled result:   " <> show profiledResult
  return resultsAgree

type TestCaseConstraint t m = (Reflex t, MonadSample t m, MonadHold t m, MonadFix m, MonadFix (PushM t))

data TestCase = forall a b c d. (Eq c, Eq d, Show c, Show d) => TestCase (Map Int a, Map Int b) (forall m t. TestCaseConstraint t m => (Behavior t a, Event t b) -> m (Behavior t c, Event t d))

testCases :: [(String, TestCase)]
testCases =
  [ (,) "hold" $ TestCase (Map.singleton 0 "asdf", Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(_, e) -> do
       b' <- hold "123" e
       return (b', e)
  , (,) "count" $ TestCase (Map.singleton 0 (), Map.fromList [(1, ()), (2, ()), (3, ())]) $ \(_, e) -> do
       e' <- updated <$> count e
       b' <- hold (0 :: Int) e'
       return (b', e')
  , (,) "headE-1" $ TestCase (Map.singleton 0 "asdf", Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       e' <- headE $ leftmost [e, e]
       return (b, e')
  , (,) "switch-1" $ TestCase (Map.singleton 0 "asdf", Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = fmap (const e) e
       b' <- hold never e'
       let e'' = switch b'
       return (b, e'')
  , (,) "switch-2" $ TestCase (Map.singleton 0 "asdf", Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = flip pushAlways e $ const $ do
             let eab = splitRecombineEvent e
             switch <$> hold eab never
           e'' = coincidence e'
       return (b, e'')
  , (,) "switch-3" $ TestCase (Map.singleton 0 "asdf", Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = flip pushAlways e $ const $ do
             let eab = splitRecombineEvent e
             switch <$> hold eab (fmap (const e) e)
           e'' = coincidence e'
       return (b, e'')
  , (,) "switch-4" $ TestCase (Map.singleton 0 "asdf", Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = leftmost [e, e]
       e'' <- switch <$> hold e' (fmap (const e) e)
       return (b, e'')
  , (,) "switchHoldPromptly-1" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = fmap (const e) e
       e'' <- switchHoldPromptly never e'
       return (b, e'')
  , (,) "switchHoldPromptly-2" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = fmap (const e) e
       e'' <- switchHoldPromptly never $ leftmost [e', e']
       return (b, e'')
  , (,) "switchHoldPromptly-3" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = leftmost [e, e]
       e'' <- switchHoldPromptly never (fmap (const e) e')
       return (b, e'')
  , (,) "switchHoldPromptly-4" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj"), (3, "asdf")]) $ \(b, e) -> do
       let e' = leftmost [e, e]
       e'' <- switchHoldPromptly never (fmap (const e') e)
       return (b, e'')
  , (,) "switch-5" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = leftmost [e, e]
       e'' <- switch <$> hold never (fmap (const e') e)
       return (b, e'')
  , (,) "switchHoldPromptly-5" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = flip push e $ \_ -> do
             Just <$> headE e
       e'' <- switchHoldPromptly never e'
       return (b, e'')
  , (,) "switchHoldPromptly-6" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = flip pushAlways e $ \_ -> do
             switchHoldPromptly e never
       e'' <- switchHoldPromptly never e'
       return (b, e'')
  , (,) "coincidence-1" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = flip pushAlways e $ \_ -> return e
           e'' = coincidence e'
       return (b, e'')
  , (,) "coincidence-2" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = flip pushAlways e $ \_ -> return $ leftmost [e, e]
           e'' = coincidence e'
       return (b, e'')
  , (,) "coincidence-3" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = flip pushAlways e $ \_ -> return $ coincidence $ fmap (const e) e
           e'' = coincidence e'
       return (b, e'')
  , (,) "coincidence-4" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj"), (3, "asdf")]) $ \(b, e) -> do
       let e' = flip pushAlways e $ \_ -> headE e
           e'' = coincidence e'
       return (b, e'')
  , (,) "coincidence-5" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer")]) $ \(b, e) -> do
       let eChild = flip pushAlways e $ const $ do
             let eNewValues = leftmost [e, e]
             return $ coincidence $ fmap (const eNewValues) eNewValues
           e' = coincidence eChild
       return (b, e')
  , (,) "coincidence-6" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer")]) $ \(b, e) -> do
       let eChild = flip pushAlways e $ const $ do
             let e' = coincidence $ fmap (const e) e
             return $ leftmost [e', e']
           e'' = coincidence eChild
       return (b, e'')
  , (,) "coincidence-7" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj"), (3, "asdf")]) $ \(b, e) -> do
       let e' = leftmost [e, e]
           eCoincidences = coincidence $ fmap (const e') e
       return (b, eCoincidences)
  , (,) "holdWhileFiring" $ TestCase (Map.singleton 0 "zxc", Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       eo <- headE e
       bb <- hold b $ pushAlways (const $ hold "asdf" eo) eo
       let b' = pull $ sample =<< sample bb
       return (b', e)
  , (,) "foldDynWhileFiring" $ TestCase (Map.singleton 0 "zxc", Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(_, e) -> do
       d <- foldDyn (:) [] $
         pushAlways (\a -> foldDyn (:) [a] e) e
       let b = current (join (fmap distributeListOverDynPure d))
       return (b, e)
  , (,) "joinDyn" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       bb <- hold "b" e
       bd <- hold never . fmap (const e) =<< headE e
       eOuter <- pushAlways sample . fmap (const bb) <$> headE e
       let eInner = switch bd
           e' = leftmost [eOuter, eInner]
       return (b, e')
  , (,) "holdUniqDyn-laziness" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList (zip [1 :: Int ..] [1 :: Int, 2, 2, 3, 3, 3, 4, 4, 4, 4])) $ \(_, e :: Event t Int) -> do
      rec result <- holdUniqDyn d
          d <- holdDyn (0 :: Int) e
      return (current result, updated result)



  {-
  , (,) "mergeIncrementalWithMove" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, PatchDMapWithMove.moveDMapKey LeftTag RightTag), (2, mempty)]) $ \(b, e :: Event t (PatchDMapWithMove (EitherTag () ()) (Const2 () ()))) -> do
       x <- holdIncremental (DMap.singleton LeftTag $ void e) $ PatchDMapWithMove.mapPatchDMapWithMove (\(Const2 _) -> void e) <$> e
       let e' = mergeIncrementalWithMove x :: Event t (DMap (EitherTag () ()) Identity)
       return (b, e')
  -}
  ]

splitRecombineEvent :: Reflex t => Event t a -> Event t String
splitRecombineEvent e =
  let ea = "a" <$ e
      eb = "b" <$ e
  in leftmost [ea, eb]


main :: IO ()
main = do
  results <- forM testCases $ \(name, TestCase inputs builder) -> do
    putStrLn $ "Test: " <> name
    testAgreement builder inputs
  exitWith $ if and results
             then ExitSuccess
             else ExitFailure 1
