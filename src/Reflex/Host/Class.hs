{-# LANGUAGE ExistentialQuantification, GADTs, ScopedTypeVariables, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, RankNTypes, BangPatterns, UndecidableInstances, EmptyDataDecls, RecursiveDo, RoleAnnotations, FunctionalDependencies #-}
module Reflex.Host.Class where

import Prelude hiding (mapM, mapM_, sequence, sequence_, foldl)

import Reflex.Class

import Control.Monad.State.Strict hiding (mapM, mapM_, forM, forM_, sequence, sequence_)
import Control.Monad.Reader hiding (mapM, mapM_, forM, forM_, sequence, sequence_)
import Data.Dependent.Sum (DSum)
import Control.Monad.Ref

class Reflex t => ReflexHost t where
  type EventTrigger t :: * -> *
  type EventHandle t :: * -> *
  type HostFrame t :: * -> *

class (ReflexHost t, Monad m) => MonadReadEvent t m | m -> t where
  readEvent :: EventHandle t a -> m (Maybe (m a))

class (Monad m, ReflexHost t) => MonadReflexCreateTrigger t m | m -> t where
  newEventWithTrigger :: (EventTrigger t a -> IO (IO ())) -> m (Event t a)

class (Monad m, ReflexHost t, MonadReflexCreateTrigger t m) => MonadReflexHost t m | m -> t where
  fireEventsAndRead :: [DSum (EventTrigger t)] -> (forall m'. (MonadReadEvent t m') => m' a) -> m a
  subscribeEvent :: Event t a -> m (EventHandle t a) --TODO: Return a handle, and use them in fireEventsAnRead
  runFrame :: PushM t a -> m a
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

instance (Reflex t, MonadReflexCreateTrigger t m) => MonadReflexCreateTrigger t (ReaderT r m) where
  newEventWithTrigger initializer = do
    lift $ newEventWithTrigger initializer

instance (Reflex t, MonadReflexHost t m) => MonadReflexHost t (ReaderT r m) where
  fireEventsAndRead dm a = lift $ fireEventsAndRead dm a
  subscribeEvent = lift . subscribeEvent
  runFrame = lift . runFrame
  runHostFrame = lift . runHostFrame
