-- | This module defines 'TriggerEventT', the standard implementation of
-- 'TriggerEvent'.
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Reflex.TriggerEvent.Base
  ( TriggerEventT (..)
  , runTriggerEventT
  , askEvents
  , TriggerInvocation (..)
  , EventTriggerRef (..)
  ) where

import Control.Concurrent
import Control.Monad.Exception
import Control.Monad.Reader
import Control.Monad.Ref
import Data.Dependent.Sum
import Reflex.Class
import Reflex.Host.Class
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class
import Reflex.TriggerEvent.Class

-- | A value with which to fire an 'Event', as well as a callback to invoke
-- after its propagation has completed.
data TriggerInvocation a = TriggerInvocation a (IO ())

-- | A reference to an 'EventTrigger' suitable for firing with 'TriggerEventT'.
newtype EventTriggerRef t m a = EventTriggerRef { unEventTriggerRef :: Ref m (Maybe (EventTrigger t a)) }

-- | A basic implementation of 'TriggerEvent'.
newtype TriggerEventT t m a = TriggerEventT { unTriggerEventT :: ReaderT (Chan [DSum (EventTriggerRef t m) TriggerInvocation]) m a } deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadException, MonadAsyncException)

-- | Run a 'TriggerEventT' action.  The argument should be a 'Chan' into which
-- 'TriggerInvocation's can be passed; it is expected that some other thread
-- will be responsible for popping values out of the 'Chan' and firing their
-- 'EventTrigger's.
runTriggerEventT :: TriggerEventT t m a -> Chan [DSum (EventTriggerRef t m) TriggerInvocation] -> m a
runTriggerEventT = runReaderT . unTriggerEventT

instance MonadTrans (TriggerEventT t) where
  lift = TriggerEventT . lift

instance PerformEvent t m => PerformEvent t (TriggerEventT t m) where
  type Performable (TriggerEventT t m) = Performable m
  {-# INLINABLE performEvent_ #-}
  performEvent_ e = lift $ performEvent_ e
  {-# INLINABLE performEvent #-}
  performEvent e = lift $ performEvent e

instance PostBuild t m => PostBuild t (TriggerEventT t m) where
  {-# INLINABLE getPostBuild #-}
  getPostBuild = lift getPostBuild

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (TriggerEventT t m) where
  {-# INLINABLE newEventWithTrigger #-}
  newEventWithTrigger = lift . newEventWithTrigger
  {-# INLINABLE newFanEventWithTrigger #-}
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance (Monad m, MonadRef m, Ref m ~ Ref IO, MonadReflexCreateTrigger t m) => TriggerEvent t (TriggerEventT t m) where
  {-# INLINABLE newTriggerEvent #-}
  newTriggerEvent = do
    (e, t) <- newTriggerEventWithOnComplete
    return (e, \a -> t a $ return ())
  {-# INLINABLE newTriggerEventWithOnComplete #-}
  newTriggerEventWithOnComplete = do
    events <- askEvents
    (eResult, reResultTrigger) <- lift newEventWithTriggerRef
    return . (,) eResult $ \a cb ->
      writeChan events [EventTriggerRef reResultTrigger :=> TriggerInvocation a cb]
  {-# INLINABLE newEventWithLazyTriggerWithOnComplete #-}
  newEventWithLazyTriggerWithOnComplete f = do
    events <- askEvents
    lift . newEventWithTrigger $ \t ->
      f $ \a cb -> do
        reResultTrigger <- newRef $ Just t
        writeChan events [EventTriggerRef reResultTrigger :=> TriggerInvocation a cb]

instance MonadRef m => MonadRef (TriggerEventT t m) where
  type Ref (TriggerEventT t m) = Ref m
  {-# INLINABLE newRef #-}
  newRef = lift . newRef
  {-# INLINABLE readRef #-}
  readRef = lift . readRef
  {-# INLINABLE writeRef #-}
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (TriggerEventT t m) where
  {-# INLINABLE atomicModifyRef #-}
  atomicModifyRef r = lift . atomicModifyRef r

instance MonadSample t m => MonadSample t (TriggerEventT t m) where
  {-# INLINABLE sample #-}
  sample = lift . sample

instance MonadHold t m => MonadHold t (TriggerEventT t m) where
  {-# INLINABLE hold #-}
  hold v0 v' = lift $ hold v0 v'
  {-# INLINABLE holdDyn #-}
  holdDyn v0 v' = lift $ holdDyn v0 v'
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 v' = lift $ holdIncremental v0 v'

-- | Retrieve the current 'Chan'; event trigger invocations pushed into it will
-- be fired.
askEvents :: Monad m => TriggerEventT t m (Chan [DSum (EventTriggerRef t m) TriggerInvocation])
askEvents = TriggerEventT ask
