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
import Test.Run

import System.Exit (exitFailure)


main :: IO ()
main = do
  (Just out1, Nothing) <- runApp $ runEventWriterT . app1
  print out1
  (Just out2, Just (out3, out4)) <- runApp $ runEventWriterT . app2
  print out2
  print out3
  print out4
  when (sort out1 /= out1 || map (+1) out2 /= out3 || out3 /= out4) exitFailure

app1 :: (Reflex t, Ref m ~ IORef, EventWriter t [Int] m) => Event t () -> m (Maybe (Ref m (Maybe (EventTrigger t ()))))
app1 e = do
  forM_ [1..10] $ \i -> tellEvent ([i] <$ e)
  return Nothing

app2 :: (Reflex t, Ref m ~ IORef, EventWriter t [Int] m, MonadAdjust t m, MonadReflexCreateTrigger t m, MonadRef m) => Event t () -> m (Maybe (Ref m (Maybe (EventTrigger t ()))))
app2 e = do
  (pulse, pulseTriggerRef) <- newEventWithTriggerRef
  forM_ [1,3..9] $ \i -> runWithReplace (tellEvent ([i] <$ e)) $ ffor pulse $ \_ -> tellEvent ([i+1] <$ e)
  return (Just pulseTriggerRef)
