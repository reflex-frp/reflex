{-# LANGUAGE FlexibleContexts, GADTs #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Ref
import Data.Dependent.Sum
import Data.IORef
import Data.List
import Data.Functor.Identity

import Reflex
import Reflex.EventWriter
import Reflex.Host.Class
import Reflex.PerformEvent.Base

import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  (Just out1, Nothing) <- runApp app1
  print out1
  (Just out2, Just (out3, out4)) <- runApp app2
  print out2
  print out3
  print out4
  when (sort out1 /= out1 || map (+1) out2 /= out3 || out3 /= out4) exitFailure

runApp :: (Show a, t ~ SpiderTimeline Global, m ~ SpiderHost Global)
       => (Event t () -> EventWriterT t [a] (PerformEventT t m) (Maybe (Ref m (Maybe (EventTrigger t ())))))
       -> IO (Maybe [a], Maybe ([a], [a]))
runApp x = runSpiderHost $ do
  (pulseE, pulseTriggerRef) <- newEventWithTriggerRef
  ((mactuateRef, e), FireCommand fire) <- hostPerformEventT $ runEventWriterT (x pulseE)
  hnd <- subscribeEvent e
  out1 <- fireEventRefAndRead pulseTriggerRef () hnd
  out2 <- fmap join $ forM mactuateRef $ \actuateRef -> do
    let readPhase = do
          mGetValue <- readEvent hnd
          case mGetValue of
            Nothing -> return Nothing
            Just getValue -> fmap Just getValue
    mactuate <- readRef actuateRef
    mpulse <- readRef pulseTriggerRef
    forM ((,) <$> mactuate <*> mpulse) $ \(actuate, pulse) -> do
      _ <- fire [actuate :=> Identity ()] $ return ()
      [Just a] <- fire [pulse :=> Identity ()] $ readPhase
      [Just b, Nothing] <- fire [actuate :=> Identity (), pulse :=> Identity ()] $ readPhase
      return (a,b)
  return (out1, out2)

app1 :: (Reflex t, Ref m ~ IORef, EventWriter t [Int] m) => Event t () -> m (Maybe (Ref m (Maybe (EventTrigger t ()))))
app1 e = do
  forM_ [1..10] $ \i -> tellEvent ([i] <$ e)
  return Nothing

app2 :: (Reflex t, Ref m ~ IORef, EventWriter t [Int] m, MonadAdjust t m, MonadReflexCreateTrigger t m, MonadRef m) => Event t () -> m (Maybe (Ref m (Maybe (EventTrigger t ()))))
app2 e = do
  (pulse, pulseTriggerRef) <- newEventWithTriggerRef
  forM_ [1,3..9] $ \i -> runWithReplace (tellEvent ([i] <$ e)) $ ffor pulse $ \_ -> tellEvent ([i+1] <$ e)
  return (Just pulseTriggerRef)
