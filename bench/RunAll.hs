{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Criterion.Main
import Criterion.Types

import Reflex
import Reflex.Host.Class

import Reflex.Plan.Reflex
import Reflex.TestPlan

import qualified Reflex.Bench.Focused as Focused
import Reflex.Spider.Internal (SpiderEventHandle)

import Control.Applicative
import Control.DeepSeq (NFData (..))

import Prelude
import System.IO
import System.Mem

import Control.Arrow
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Data.Bool
import Data.Function
import Data.Int
import Data.IORef
import Data.Monoid
import Data.Time.Clock
import Debug.Trace.LocationTH
import GHC.Stats
import System.Environment
import System.Mem.Weak
import System.Process
import Text.Read

import Unsafe.Coerce

import Data.Map (Map)
import qualified Data.Map as Map

type MonadReflexHost' t m = (MonadReflexHost t m, MonadIORef m, MonadIORef (HostFrame t))


setupFiring ::   (MonadReflexHost t m, MonadIORef m) => Plan t (Event t a) -> m (EventHandle t a, Schedule t)
setupFiring p = do
  (e, s) <- runPlan p
  h <- subscribeEvent e
  return (h, s)

-- Hack to avoid the NFData constraint for EventHandle which is a synonym
newtype Ignore a = Ignore a
instance NFData (Ignore a) where
  rnf !_ = ()

instance NFData (SpiderEventHandle x a) where
  rnf !_ = ()

instance NFData (Behavior t a) where
  rnf !_ = ()

instance NFData (Firing t) where
  rnf !_ = ()

-- Measure the running time
benchFiring :: forall t m. (MonadReflexHost' t m, MonadSample t m) => (forall a. m a -> IO a) -> TestCase -> Int -> IO ()
benchFiring runHost tc n = runHost $ do
  let runIterations :: m a -> m ()
      runIterations test = replicateM_ (10*n) $ do
        result <- test
        liftIO $ evaluate result
  case tc of
    TestE p -> do
      (h, s) <- setupFiring p
      runIterations $ readSchedule_ s $ readEvent' h
    TestB p -> do
      (b, s) <- runPlan p
      runIterations $ readSchedule_ (makeDense s) $ sample b

waitForFinalizers :: IO ()
waitForFinalizers = do
  performGC
  x <- getCurrentTime
  isFinalized <- newIORef False
  mkWeakPtr x $ Just $ writeIORef isFinalized True
  performGC
  fix $ \loop -> do
    f <- readIORef isFinalized
    unless f $ do
      threadDelay 1
      loop

benchmarks :: [(String, Int -> IO ())]
benchmarks = implGroup "spider" runSpiderHost cases
  where
    implGroup :: (MonadReflexHost' t m, MonadSample t m) => String -> (forall a. m a -> IO a) -> [(String, TestCase)] -> [(String, Int -> IO ())]
    implGroup name runHost = group name . fmap (second (benchFiring runHost))
    group name = fmap $ first ((name <> "/") <>)
    sub n frames = group ("subscribing " ++ show (n, frames)) $ Focused.subscribing n frames
    firing n     = group ("firing "    <> show n) $ Focused.firing n
    merging n    = group ("merging "   <> show n) $ Focused.merging n
    dynamics n   = group ("dynamics "  <> show n) $ Focused.dynamics n
    cases = concat
      [ sub 100 40
      , dynamics 100
      , dynamics 1000
      , firing 1000
      , firing 10000
      , merging 10
      , merging 50
      , merging 100
      , merging 200
      ]

pattern RunTestCaseFlag = "--run-test-case"

spawnBenchmark :: String -> Benchmark
spawnBenchmark name = Benchmark name $ Benchmarkable $ \n -> do
  self <- getExecutablePath
  callProcess self [RunTestCaseFlag, name, show n, "+RTS", "-N1"]

foreign import ccall unsafe "myCapabilityHasOtherRunnableThreads" myCapabilityHasOtherRunnableThreads :: IO Bool

main :: IO ()
main = do
  args <- getArgs
  case args of
    RunTestCaseFlag : t -> case t of
      [name, readMaybe -> Just count] -> do
        case lookup name benchmarks of
          Just testCase -> testCase count
        performGC
        fix $ \loop -> bool (return ()) (yield >> loop) =<< myCapabilityHasOtherRunnableThreads
        return ()
      _ -> $failure "--run-test-case: expected test name and iteration count to follow"
    _ -> defaultMainWith (defaultConfig { timeLimit = 20, csvFile = Just "dmap-original.csv", reportFile = Just "report.html" }) $ fmap (spawnBenchmark . fst) benchmarks
