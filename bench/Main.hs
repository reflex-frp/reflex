{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Data.Functor.Misc
import Control.Monad.Primitive
import Control.Monad.IO.Class
import Data.Dependent.Sum
import Control.Concurrent.STM
import Control.Applicative
import System.IO.Unsafe
import Data.IORef
import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad
import Reflex
import Reflex.Host.Class
import System.Mem
import System.IO
import Criterion.Main

import qualified Data.Traversable as T

import qualified Data.Dependent.Map as DM


main :: IO ()
main = defaultMain
  [ bgroup "micro" micros ]

instance NFData (IORef a) where
  rnf x = seq x ()

instance NFData (TVar a) where
  rnf x = seq x ()

newtype WHNF a = WHNF a
instance NFData (WHNF a) where
  rnf (WHNF a) = seq a ()

withSetup :: NFData b => String -> SpiderHost a -> (a -> SpiderHost b) -> Benchmark
withSetup name setup action = env (WHNF <$> runSpiderHost setup) $ \ ~(WHNF a) ->
  bench name . nfIO $ runSpiderHost (action a)

withSetupWHNF :: String -> SpiderHost a -> (a -> SpiderHost b) -> Benchmark
withSetupWHNF name setup action = env (WHNF <$> runSpiderHost setup) $ \ ~(WHNF a) ->
  bench name . whnfIO $ runSpiderHost (action a)


micros :: [Benchmark]
micros =
  [ bench "newIORef" $ whnfIO $ void $ newIORef ()
  , env (newIORef (42 :: Int)) (bench "readIORef" . whnfIO . readIORef)
  , bench "newTVar" $ whnfIO $ void $ newTVarIO ()
  , env (newTVarIO (42 :: Int)) (bench "readTVar" . whnfIO . readTVarIO)
  , bench "newEventWithTrigger" $ whnfIO . void $ runSpiderHost $ newEventWithTrigger $
      \trigger -> return () <$ evaluate trigger
  , bench "newEventWithTriggerRef" $ whnfIO . void $ runSpiderHost newEventWithTriggerRef
  , withSetupWHNF "subscribeEvent" newEventWithTriggerRef $ subscribeEvent . fst
  , withSetupWHNF "subscribeSwitch"
    (join $ hold <$> fmap fst newEventWithTriggerRef <*> fmap fst newEventWithTriggerRef)
    (subscribeEvent . switch)
  , withSetupWHNF "subscribeMerge(1)" (setupMerge 1) $ \(ev,_) -> subscribeEvent ev
  , withSetupWHNF "subscribeMerge(100)" (setupMerge 100) (subscribeEvent . fst)
  , withSetupWHNF "subscribeMerge(10000)" (setupMerge 10000) (subscribeEvent . fst)
  , bench "runHostFrame" $ whnfIO $ runSpiderHost $ runHostFrame $ return ()
  , withSetupWHNF "fireEventsAndRead(single/single)"
    (newEventWithTriggerRef >>= subscribePair)
    (\(subd, trigger) -> fireAndRead trigger (42 :: Int) subd)
  , withSetupWHNF "fireEventsOnly"
    (newEventWithTriggerRef >>= subscribePair)
    (\(subd, trigger) -> do
        Just key <- liftIO $ readIORef trigger
        fireEvents [key :=> (42 :: Int)])
  , withSetupWHNF "fireEventsAndRead(head/merge1)"
    (setupMerge 1 >>= subscribePair)
    (\(subd, t:riggers) -> fireAndRead t (42 :: Int) subd)
  , withSetupWHNF "fireEventsAndRead(head/merge100)"
    (setupMerge 100 >>= subscribePair)
    (\(subd, t:riggers) -> fireAndRead t (42 :: Int) subd)
  , withSetupWHNF "fireEventsAndRead(head/merge10000)"
      (setupMerge 10000 >>= subscribePair)
      (\(subd, t:riggers) -> fireAndRead t (42 :: Int) subd)
  , withSetupWHNF "fireEventsOnly(head/merge100)"
    (setupMerge 100 >>= subscribePair)
    (\(subd, t:riggers) -> do
        Just key <- liftIO $ readIORef t
        fireEvents [key :=> (42 :: Int)])
  , withSetupWHNF "hold" newEventWithTriggerRef $ \(ev,trigger) -> hold (42 :: Int) ev
  , withSetupWHNF "sample" (newEventWithTriggerRef >>= hold (42 :: Int) . fst) sample    
  ]

setupMerge :: Int
           -> SpiderHost (Event Spider (DM.DMap (Const2 Int a)),
                         [IORef (Maybe (EventTrigger Spider a))])
setupMerge num = do
  (evs, triggers) <- unzip <$> replicateM 100 newEventWithTriggerRef
  let !m = DM.fromList [WrapArg (Const2 i) :=> v | (i,v) <- zip [0..] evs]
  pure (merge m, triggers)

subscribePair :: (Event Spider a, b) -> SpiderHost (EventHandle Spider a, b)
subscribePair (ev, b) = (,b) <$> subscribeEvent ev

fireAndRead :: IORef (Maybe (EventTrigger Spider a)) -> a -> EventHandle Spider b
            -> SpiderHost (Maybe b)
fireAndRead trigger val subd = do
  Just key <- liftIO $ readIORef trigger
  fireEventsAndRead [key :=> val] $ readEvent subd >>= T.sequence
