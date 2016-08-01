{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Reflex.Test.CrossImpl (test) where

import Prelude hiding (and, foldl, mapM, mapM_, sequence, sequence_)

import Control.Monad.Ref
import Reflex.Class
import Reflex.Dynamic
import Reflex.Host.Class
import qualified Reflex.Pure as P
import qualified Reflex.Spider.Internal as S

import Control.Arrow (second, (&&&))
import Control.Monad.Identity hiding (forM, forM_, mapM, mapM_, sequence, sequence_)
import Control.Monad.State.Strict hiding (forM, forM_, mapM, mapM_, sequence, sequence_)
import Control.Monad.Writer hiding (forM, forM_, mapM, mapM_, sequence, sequence_)
import Data.Dependent.Map (DSum (..))
import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Traversable
import System.Exit
import System.Mem

import System.IO.Unsafe

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

testSpider :: (forall m t. TestCaseConstraint t m => (Behavior t a, Event t b) -> m (Behavior t c, Event t d)) -> (Map Int a, Map Int b) -> (Map Int c, Map Int d)
testSpider builder (bMap, eMap) = unsafePerformIO $ S.runSpiderHost $ do
  (re, reTrigger) <- newEventWithTriggerRef
  (rb, rbTrigger) <- newEventWithTriggerRef
  b <- runHostFrame $ hold (error "testSpider: No value for input behavior yet") rb
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
  let spiderResult = testSpider builder inputs
  tracePerf "---------" $ return ()
  let resultsAgree = identityResult == spiderResult
  if resultsAgree
    then do putStrLn "Success:"
            print identityResult
    else do putStrLn "Failure:"
            putStrLn $ "Pure result: " <> show identityResult
            putStrLn $ "Spider result:   " <> show spiderResult
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
  , (,) "onceE-1" $ TestCase (Map.singleton 0 "asdf", Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       e' <- onceE $ leftmost [e, e]
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
  , (,) "switchPromptly-1" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = fmap (const e) e
       e'' <- switchPromptly never e'
       return (b, e'')
  , (,) "switchPromptly-2" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = fmap (const e) e
       e'' <- switchPromptly never $ leftmost [e', e']
       return (b, e'')
  , (,) "switchPromptly-3" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = leftmost [e, e]
       e'' <- switchPromptly never (fmap (const e) e')
       return (b, e'')
  , (,) "switchPromptly-4" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj"), (3, "asdf")]) $ \(b, e) -> do
       let e' = leftmost [e, e]
       e'' <- switchPromptly never (fmap (const e') e)
       return (b, e'')
  , (,) "switch-5" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = leftmost [e, e]
       e'' <- switch <$> hold never (fmap (const e') e)
       return (b, e'')
  , (,) "switchPromptly-5" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = flip push e $ \_ -> do
             Just <$> onceE e
       e'' <- switchPromptly never e'
       return (b, e'')
  , (,) "switchPromptly-6" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       let e' = flip pushAlways e $ \_ -> do
             switchPromptly e never
       e'' <- switchPromptly never e'
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
       let e' = flip pushAlways e $ \_ -> onceE e
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
       eo <- onceE e
       bb <- hold b $ pushAlways (const $ hold "asdf" eo) eo
       let b' = pull $ sample =<< sample bb
       return (b', e)
  , (,) "joinDyn" $ TestCase (Map.singleton 0 (0 :: Int), Map.fromList [(1, "qwer"), (2, "lkj")]) $ \(b, e) -> do
       bb <- hold "b" e
       bd <- hold never . fmap (const e) =<< onceE e
       eOuter <- pushAlways sample . fmap (const bb) <$> onceE e
       let eInner = switch bd
           e' = leftmost [eOuter, eInner]
       return (b, e')
  ]

splitRecombineEvent :: Reflex t => Event t a -> Event t String
splitRecombineEvent e =
  let ea = "a" <$ e
      eb = "b" <$ e
  in leftmost [ea, eb]


test :: IO ()
test = do
  results <- forM testCases $ \(name, TestCase inputs builder) -> do
    putStrLn $ "Test: " <> name
    testAgreement builder inputs
  exitWith $ if and results
             then ExitSuccess
             else ExitFailure 1
