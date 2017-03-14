-- | This module defines 'PerformEvent' and 'TriggerEvent', which mediate the
-- interaction between a "Reflex"-based program and the external side-effecting
-- actions such as 'IO'.
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
module Reflex.PerformEvent.Class
  ( PerformEvent (..)
  , ExhaustiblePerformEvent (..)
  , performEventAsync
  ) where

import Reflex.Class
import Reflex.TriggerEvent.Class

import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Control.Monad.State as Lazy

-- | 'PerformEvent' represents actions that can trigger other actions based on
-- 'Event's.
class (Reflex t, Monad (Performable m), Monad m) => PerformEvent t m | m -> t where
  -- | The type of action to be triggered; this is often not the same type as
  -- the triggering action.
  type Performable m :: * -> *
  -- | Perform the action contained in the given 'Event' whenever the 'Event'
  -- fires.  Return the result in another 'Event'.  Note that the output 'Event'
  -- will generally occur later than the input 'Event', since most 'Performable'
  -- actions cannot be performed during 'Event' propagation.
  performEvent :: Event t (Performable m a) -> m (Event t a)
  -- | Like 'performEvent', but do not return the result.  May have slightly
  -- better performance.
  performEvent_ :: Event t (Performable m ()) -> m ()

class PerformEvent t m => ExhaustiblePerformEvent t m where
  -- | Run a 'PerformEvent', and fire an output event whenever that
  -- 'PerformEvent' does not produce any new action events in response to a
  -- batch of incoming action responses.
  --
  -- WARNING: If you use the resulting 'Event' to trigger a 'performEvent' (or
  -- any other function based on it) inside the child 'PerformEvent', it will
  -- produce a causality loop, i.e. that "this 'PerformEvent' takes an action
  -- whenever it is done taking actions".  It is safe to use the 'Event' to
  -- perform an action in the parent context.
  withPerformEventExhausted :: m a -> m (Event t (), a)

-- | Like 'performEvent', but the resulting 'Event' occurs only when the
-- callback (@a -> IO ()@) is called, not when the included action finishes.
--
-- NOTE: Despite the name, 'performEventAsync' does not run its action in a
-- separate thread - although the action is free to invoke forkIO and then call
-- the callback whenever it is ready.  This will work properly, even in GHCJS
-- (which fully implements concurrency even though JavaScript does not have
-- built in concurrency).
{-# INLINABLE performEventAsync #-}
performEventAsync :: (TriggerEvent t m, PerformEvent t m) => Event t ((a -> IO ()) -> Performable m ()) -> m (Event t a)
performEventAsync e = do
  (eOut, triggerEOut) <- newTriggerEvent
  performEvent_ $ fmap ($ triggerEOut) e
  return eOut

instance PerformEvent t m => PerformEvent t (ReaderT r m) where
  type Performable (ReaderT r m) = ReaderT r (Performable m)
  performEvent_ e = do
    r <- ask
    lift $ performEvent_ $ flip runReaderT r <$> e
  performEvent e = do
    r <- ask
    lift $ performEvent $ flip runReaderT r <$> e

instance ExhaustiblePerformEvent t m => ExhaustiblePerformEvent t (ReaderT r m) where
  withPerformEventExhausted a = do
    r <- ask
    lift $ withPerformEventExhausted $ runReaderT a r

instance PerformEvent t m => PerformEvent t (StateT s m) where
  type Performable (StateT s m) = Performable m
  performEvent_ = lift . performEvent_
  performEvent = lift . performEvent

instance ExhaustiblePerformEvent t m => ExhaustiblePerformEvent t (StateT r m) where
  withPerformEventExhausted a = do
    old <- get
    (exhausted, (result, new)) <- lift $ withPerformEventExhausted $ runStateT a old
    put new
    return (exhausted, result)

instance PerformEvent t m => PerformEvent t (Lazy.StateT s m) where
  type Performable (Lazy.StateT s m) = Performable m
  performEvent_ = lift . performEvent_
  performEvent = lift . performEvent

instance ExhaustiblePerformEvent t m => ExhaustiblePerformEvent t (Lazy.StateT r m) where
  withPerformEventExhausted a = do
    old <- Lazy.get
    (exhausted, (result, new)) <- lift $ withPerformEventExhausted $ Lazy.runStateT a old
    Lazy.put new
    return (exhausted, result)
