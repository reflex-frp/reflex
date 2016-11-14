-- | This module defines 'PerformEvent' and 'TriggerEvent', which mediate the
-- interaction between a "Reflex"-based program and the external side-effecting
-- actions such as 'IO'.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
module Reflex.PerformEvent.Class
  ( PerformEvent (..)
  , performEventAsync
  , TriggerEvent (..)
  ) where

import Reflex.Class

import Control.Monad.Reader

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

--TODO: Shouldn't have IO hard-coded
-- | 'TriggerEvent' represents actions that can create 'Event's that can be
-- triggered from the IO monad.
class Monad m => TriggerEvent t m | m -> t where
  -- | Create a triggerable 'Event'.  Whenever the resulting function is called,
  -- the resulting 'Event' will fire at some point in the future.  Note that
  -- this may not be synchronous.
  newTriggerEvent :: m (Event t a, a -> IO ())
  -- | Like 'newTriggerEvent', but the callback itself takes another callback,
  -- to be invoked once the requested 'Event' occurrence has finished firing.
  -- This allows synchronous operation.
  newTriggerEventWithOnComplete :: m (Event t a, a -> IO () -> IO ()) --TODO: This and newTriggerEvent should be unified somehow
  -- | Like 'newTriggerEventWithOnComplete', but with setup and teardown.  This
  -- relatively complex type signature allows any external listeners to be
  -- subscribed lazily and then removed whenever the returned 'Event' is no
  -- longer being listened to.  Note that the setup/teardown may happen multiple
  -- times, and there is no guarantee that the teardown will be executed
  -- promptly, or even at all, in the case of program termination.
  newEventWithLazyTriggerWithOnComplete :: ((a -> IO () -> IO ()) -> IO (IO ())) -> m (Event t a)

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

instance TriggerEvent t m => TriggerEvent t (ReaderT r m) where
  newTriggerEvent = lift newTriggerEvent
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete
