{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Reflex.TriggerEvent.Base where

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

data TriggerInvocation a = TriggerInvocation a (IO ())

newtype EventTriggerRef t m a = EventTriggerRef { unEventTriggerRef :: Ref m (Maybe (EventTrigger t a)) }

newtype TriggerEventT t m a = TriggerEventT
  { unTriggerEventT :: ReaderT (Chan [DSum (EventTriggerRef t m) TriggerInvocation]) m a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadFix
             , MonadIO
             , MonadException
             , MonadAsyncException
             )

runTriggerEventT :: TriggerEventT t m a -> Chan [DSum (EventTriggerRef t m) TriggerInvocation] -> m a
runTriggerEventT = runReaderT . unTriggerEventT

instance MonadTrans (TriggerEventT t) where
  lift = TriggerEventT . lift

instance PerformEvent t m =>
         PerformEvent t (TriggerEventT t m) where
  type Performable (TriggerEventT t m) = Performable m
  performEvent_ e = lift $ performEvent_ e
  {-# INLINABLE performEvent_ #-}
  performEvent e = lift $ performEvent e
  {-# INLINABLE performEvent #-}

instance PostBuild t m =>
         PostBuild t (TriggerEventT t m) where
  getPostBuild = lift getPostBuild
  {-# INLINABLE getPostBuild #-}

instance MonadReflexCreateTrigger t m =>
         MonadReflexCreateTrigger t (TriggerEventT t m) where
  newEventWithTrigger = lift . newEventWithTrigger
  {-# INLINABLE newEventWithTrigger #-}
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f
  {-# INLINABLE newFanEventWithTrigger #-}

instance (Monad m, MonadRef m, Ref m ~ Ref IO, MonadReflexCreateTrigger t m) =>
         TriggerEvent t (TriggerEventT t m) where
  newTriggerEvent = do
    (e, t) <- newTriggerEventWithOnComplete
    return (e, \a -> t a $ return ())
  {-# INLINABLE newTriggerEvent #-}
  newTriggerEventWithOnComplete = do
    events <- askEvents
    (eResult, reResultTrigger) <- lift newEventWithTriggerRef
    return . (,) eResult $ \a cb ->
      writeChan events [EventTriggerRef reResultTrigger :=> TriggerInvocation a cb]
  {-# INLINABLE newTriggerEventWithOnComplete #-}
  newEventWithLazyTriggerWithOnComplete f = do
    events <- askEvents
    lift . newEventWithTrigger $ \t ->
      f $ \a cb -> do
        reResultTrigger <- newRef $ Just t
        writeChan events [EventTriggerRef reResultTrigger :=> TriggerInvocation a cb]
  {-# INLINABLE newEventWithLazyTriggerWithOnComplete #-}

instance MonadRef m =>
         MonadRef (TriggerEventT t m) where
  type Ref (TriggerEventT t m) = Ref m
  newRef = lift . newRef
  {-# INLINABLE newRef #-}
  readRef = lift . readRef
  {-# INLINABLE readRef #-}
  writeRef r = lift . writeRef r
  {-# INLINABLE writeRef #-}

instance MonadAtomicRef m =>
         MonadAtomicRef (TriggerEventT t m) where
  atomicModifyRef r = lift . atomicModifyRef r
  {-# INLINABLE atomicModifyRef #-}

instance MonadSample t m =>
         MonadSample t (TriggerEventT t m) where
  sample = lift . sample
  {-# INLINABLE sample #-}

instance MonadHold t m =>
         MonadHold t (TriggerEventT t m) where
  hold v0 v' = lift $ hold v0 v'
  {-# INLINABLE hold #-}
  holdDyn v0 v' = lift $ holdDyn v0 v'
  {-# INLINABLE holdDyn #-}
  holdIncremental v0 v' = lift $ holdIncremental v0 v'
  {-# INLINABLE holdIncremental #-}

askEvents
  :: Monad m
  => TriggerEventT t m (Chan [DSum (EventTriggerRef t m) TriggerInvocation])
askEvents = TriggerEventT ask
