{-# LANGUAGE FlexibleContexts, GADTs #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Ref
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum
import Data.IORef
import Data.List
import Data.Functor.Identity

import Reflex
import Reflex.Host.Class
import Reflex.PerformEvent.Base
import Reflex.Requester.Base
import Reflex.Requester.Class

import System.Exit (exitFailure, exitSuccess)

data RequestInt a where
  RequestInt :: Int -> RequestInt Int

main :: IO ()
main = do
  (Just out1, Nothing) <- runApp app1
  print out1
  (Just out2, Just (out3, out4)) <- runApp app2
  print out2
  print out3
  print out4
  when (sort out1 /= out1 || map (+1) out2 /= out3 || out3 /= out4) exitFailure

runApp :: (t ~ SpiderTimeline Global, m ~ SpiderHost Global)
       => (Event t () -> RequesterT t RequestInt Identity (PerformEventT t m) (Maybe (Ref m (Maybe (EventTrigger t ())))))
       -> IO (Maybe [Int], Maybe ([Int], [Int]))
runApp x = runSpiderHost $ do
  (pulseE, pulseTriggerRef) <- newEventWithTriggerRef
  ((mactuateRef, e), FireCommand fire) <- hostPerformEventT $ runRequesterT (x pulseE) never
  hnd <- subscribeEvent (map unwrapRequest . DMap.toList <$> e)
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

unwrapRequest :: DSum tag RequestInt -> Int
unwrapRequest (_ :=> RequestInt i) = i

app1 :: (Reflex t, Ref m ~ IORef, Requester t m, Response m ~ Identity, Request m ~ RequestInt) => Event t () -> m (Maybe (Ref m (Maybe (EventTrigger t ()))))
app1 e = do
  forM_ [1..10] $ \i -> requestingIdentity (RequestInt i <$ e)
  return Nothing

app2 :: (Reflex t, Ref m ~ IORef, Requester t m, Response m ~ Identity, Request m ~ RequestInt, MonadAdjust t m, MonadReflexCreateTrigger t m, MonadRef m) => Event t () -> m (Maybe (Ref m (Maybe (EventTrigger t ()))))
app2 e = do
  (pulse, pulseTriggerRef) <- newEventWithTriggerRef
  forM_ [1,3..9] $ \i -> runWithReplace (requestingIdentity (RequestInt i <$ e)) $ ffor pulse $ \_ -> requestingIdentity (RequestInt (i+1) <$ e)
  return (Just pulseTriggerRef)

