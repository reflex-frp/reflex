{-# LANGUAGE FlexibleContexts, GADTs, RankNTypes, ScopedTypeVariables #-}
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
import Test.Run

import System.Exit (exitFailure, exitSuccess)

data RequestInt a where
  RequestInt :: Int -> RequestInt Int

main :: IO ()
main = do
  (Just out1, Nothing) <- runApp $ unwrapApp app1
  print out1
  (Just out2, Just (out3, out4)) <- runApp $ unwrapApp app2
  print out2
  print out3
  print out4
  when (sort out1 /= out1 || map (+1) out2 /= out3 || out3 /= out4) exitFailure

unwrapRequest :: DSum tag RequestInt -> Int
unwrapRequest (_ :=> RequestInt i) = i

unwrapApp :: forall x t a response. (Reflex t, ReflexHost t)
          => (x -> RequesterT t RequestInt response (PerformEventT t (SpiderHost Global)) a)
          -> x -> PerformEventT t (SpiderHost Global) (a, Event t [Int])
unwrapApp x e = do
  (a, b) <- runRequesterT (x e) never
  return (a, fmap (map unwrapRequest . DMap.toList) b)

app1 :: (Reflex t, Ref m ~ IORef, Requester t m, Response m ~ Identity, Request m ~ RequestInt) => Event t () -> m (Maybe (Ref m (Maybe (EventTrigger t ()))))
app1 e = do
  forM_ [1..10] $ \i -> requestingIdentity (RequestInt i <$ e)
  return Nothing

app2 :: (Reflex t, Ref m ~ IORef, Requester t m, Response m ~ Identity, Request m ~ RequestInt, MonadAdjust t m, MonadReflexCreateTrigger t m, MonadRef m) => Event t () -> m (Maybe (Ref m (Maybe (EventTrigger t ()))))
app2 e = do
  (pulse, pulseTriggerRef) <- newEventWithTriggerRef
  forM_ [1,3..9] $ \i -> runWithReplace (requestingIdentity (RequestInt i <$ e)) $ ffor pulse $ \_ -> requestingIdentity (RequestInt (i+1) <$ e)
  return (Just pulseTriggerRef)

