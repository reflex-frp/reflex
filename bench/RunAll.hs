{-# LANGUAGE ConstraintKinds, TypeSynonymInstances, BangPatterns, ScopedTypeVariables, TupleSections, GADTs, RankNTypes, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Criterion.Main
import Criterion.Types

import Reflex
import Reflex.Host.Class

import Reflex.TestPlan
import Reflex.Plan.Reflex

import Reflex.Spider.Internal (SpiderEventHandle)
import qualified Reflex.Bench.Focused as Focused

import Control.Applicative
import Control.DeepSeq (NFData (..))

import System.IO
import System.Mem
import Prelude

type MonadReflexHost' t m = (MonadReflexHost t m, MonadIORef m, MonadIORef (HostFrame t))


setupFiring ::   (MonadReflexHost t m, MonadIORef m) => Plan t (Event t a) -> m (Ignore (EventHandle t a), Schedule t)
setupFiring p = do
  (e, s) <- runPlan p
  h <- subscribeEvent e
  return (Ignore h, s)

-- Hack to avoid the NFData constraint for EventHandle which is a synonym
newtype Ignore a = Ignore a
instance NFData (Ignore a) where
  rnf !_ = ()

instance NFData (SpiderEventHandle a) where
  rnf !_ = ()

instance NFData (Behavior t a) where
  rnf !_ = ()

instance NFData (Firing t) where
  rnf !(Firing _ _) = ()

-- Measure the running time
benchFiring ::  (MonadReflexHost' t m, MonadSample t m) => (forall a. m a -> IO a) -> (String, TestCase) -> Benchmark
benchFiring runHost (name, TestE p) = env setup (\e -> bench name $ whnfIO $ run e) where
    run (Ignore h, s) = runHost (readSchedule s (readEvent' h)) >> performGC
    setup = runHost $ setupFiring p

benchFiring runHost (name, TestB p) = env setup (\e -> bench name $ whnfIO $ run e) where
    run (b, s) = runHost (readSchedule s (sample b)) >> performGC
    setup = runHost $ do
      (b, s) <- runPlan p
      return (b, makeDense s)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  defaultMainWith (defaultConfig { timeLimit = 10, csvFile = Just "dmap-original.csv" })
    [ benchImpl "spider" runSpiderHost
    ]

benchImpl :: (MonadReflexHost' t m, MonadSample t m) => String -> (forall a. m a -> IO a) -> Benchmark
benchImpl name runHost = bgroup name [ sub 100 40
                                     , dynamics 100
                                     , dynamics 1000
                                     , firing 1000
                                     , firing 10000
                                     , merging 10
                                     , merging 50
                                     , merging 100 
                                     , merging 200]
  where
    sub n frames = runGroup ("subscribing " ++ show (n, frames)) $ Focused.subscribing n frames
    firing n     = runGroup ("firing "    ++ show n) $ Focused.firing n
    merging n    = runGroup ("merging "   ++ show n) $ Focused.merging n
    dynamics n   = runGroup ("dynamics "  ++ show n) $ Focused.dynamics n

    runGroup name' benchmarks = bgroup name' (benchFiring runHost <$> benchmarks)



