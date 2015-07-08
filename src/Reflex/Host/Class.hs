{-# LANGUAGE ExistentialQuantification, GADTs, ScopedTypeVariables, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, RankNTypes, BangPatterns, UndecidableInstances, EmptyDataDecls, RecursiveDo, RoleAnnotations, FunctionalDependencies #-}
module Reflex.Host.Class where

import Prelude hiding (mapM, mapM_, sequence, sequence_, foldl)

import Reflex.Class

import Control.Monad.Trans.Class
import Control.Monad.Reader (ReaderT())
import Control.Monad.Writer (WriterT())
import Control.Monad.Cont   (ContT())
import Control.Monad.Except (ExceptT())
import Control.Monad.RWS    (RWST())
import Control.Monad.State  (StateT())
import Data.Dependent.Sum (DSum)
import Control.Monad.Ref

class Reflex t => ReflexHost t where
  type EventTrigger t :: * -> *
  type EventHandle t :: * -> *
  type HostFrame t :: * -> *

class (ReflexHost t, Monad m) => MonadReadEvent t m | m -> t where
  readEvent :: EventHandle t a -> m (Maybe (m a))

class (Monad m, ReflexHost t) => MonadReflexCreateTrigger t m | m -> t where
  -- | Creates an original Event (one that is not based on any other event).
  -- When a subscriber first subscribes to an event (building another event
  -- that depends on the subscription) the given callback function is run by
  -- passing a trigger. The event is then set up in IO. The callback
  -- function returns an accompanying teardown action.
  -- Any time between setup and teardown the trigger can be used to fire
  -- the event.
  newEventWithTrigger :: (EventTrigger t a -> IO (IO ())) -> m (Event t a)

class (Monad m, ReflexHost t, MonadReflexCreateTrigger t m) => MonadReflexHost t m | m -> t where
  fireEventsAndRead :: [DSum (EventTrigger t)] -> (forall m'. (MonadReadEvent t m') => m' a) -> m a
  subscribeEvent :: Event t a -> m (EventHandle t a) --TODO: Return a handle, and use them in fireEventsAnRead

  runHostFrame :: HostFrame t a -> m a

fireEvents :: MonadReflexHost t m => [DSum (EventTrigger t)] -> m ()
{-# INLINE fireEvents #-}
fireEvents dm = fireEventsAndRead dm $ return ()

{-# INLINE newEventWithTriggerRef #-}
newEventWithTriggerRef :: (MonadReflexCreateTrigger t m, MonadRef m, Ref m ~ Ref IO) => m (Event t a, Ref m (Maybe (EventTrigger t a)))
newEventWithTriggerRef = do
  rt <- newRef Nothing
  e <- newEventWithTrigger $ \t -> do
    writeRef rt $ Just t
    return $ writeRef rt Nothing
  return (e, rt)

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (ReaderT r m) where
  newEventWithTrigger initializer = do
    lift $ newEventWithTrigger initializer

instance MonadReflexHost t m => MonadReflexHost t (ReaderT r m) where
  fireEventsAndRead dm a = lift $ fireEventsAndRead dm a
  subscribeEvent = lift . subscribeEvent
  runHostFrame = lift . runHostFrame

instance (MonadReflexCreateTrigger t m, Monoid w) => MonadReflexCreateTrigger t (WriterT w m) where
  newEventWithTrigger = lift . newEventWithTrigger

instance (MonadReflexHost t m, Monoid w) => MonadReflexHost t (WriterT w m) where
  fireEventsAndRead dm a = lift $ fireEventsAndRead dm a
  subscribeEvent = lift . subscribeEvent
  runHostFrame = lift . runHostFrame

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (StateT s m) where
  newEventWithTrigger = lift . newEventWithTrigger

instance MonadReflexHost t m => MonadReflexHost t (StateT s m) where
  fireEventsAndRead dm a = lift $ fireEventsAndRead dm a
  subscribeEvent = lift . subscribeEvent
  runHostFrame = lift . runHostFrame

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (ContT r m) where
  newEventWithTrigger = lift . newEventWithTrigger

instance MonadReflexHost t m => MonadReflexHost t (ContT r m) where
  fireEventsAndRead dm a = lift $ fireEventsAndRead dm a
  subscribeEvent = lift . subscribeEvent
  runHostFrame = lift . runHostFrame

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (ExceptT e m) where
  newEventWithTrigger = lift . newEventWithTrigger

instance MonadReflexHost t m => MonadReflexHost t (ExceptT e m) where
  fireEventsAndRead dm a = lift $ fireEventsAndRead dm a
  subscribeEvent = lift . subscribeEvent
  runHostFrame = lift . runHostFrame

instance (MonadReflexCreateTrigger t m, Monoid w) => MonadReflexCreateTrigger t (RWST r w s m) where
  newEventWithTrigger = lift . newEventWithTrigger

instance (MonadReflexHost t m, Monoid w) => MonadReflexHost t (RWST r w s m) where
  fireEventsAndRead dm a = lift $ fireEventsAndRead dm a
  subscribeEvent = lift . subscribeEvent
  runHostFrame = lift . runHostFrame
