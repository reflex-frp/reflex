{-# LANGUAGE GADTs #-}
module Test.Run where

import Control.Monad
import Control.Monad.Ref
import Data.Dependent.Sum
import Data.Functor.Identity

import Reflex
import Reflex.Host.Class

runApp :: (Show a, t ~ SpiderTimeline Global, m ~ SpiderHost Global)
       => (Event t () -> PerformEventT t m (Maybe (Ref m (Maybe (EventTrigger t ()))), Event t a))
       -> IO (Maybe a, Maybe (a, a))
runApp x = runSpiderHost $ do
  (pulseE, pulseTriggerRef) <- newEventWithTriggerRef
  ((mactuateRef, e), FireCommand fire) <- hostPerformEventT $ x pulseE
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
