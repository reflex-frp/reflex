{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Reflex.PerformEvent.Class where

import Reflex.Class

import Control.Monad.Identity
import Control.Monad.Reader
import qualified Control.Monad.State.Lazy as Lazy
import Control.Monad.State.Strict

class (Reflex t, Monad (Performable m), Monad m) => PerformEvent t m | m -> t where
  type Performable m :: * -> *
  performEvent :: Event t (Performable m a) -> m (Event t a)
  performEvent_ :: Event t (Performable m ()) -> m ()

--TODO: Shouldn't have IO hard-coded
class Monad m => TriggerEvent t m | m -> t where
  newTriggerEvent :: m (Event t a, a -> IO ())
  newTriggerEventWithOnComplete :: m (Event t a, a -> IO () -> IO ()) --TODO: This and newTriggerEvent should be unified somehow
  newEventWithLazyTriggerWithOnComplete :: ((a -> IO () -> IO ()) -> IO (IO ())) -> m (Event t a)

class (Reflex t, Monad m) => MonadRequest t m | m -> t where
  type Request m :: * -> *
  type Response m :: * -> *
  withRequesting :: (Event t (Response m a) -> m (Event t (Request m a), r)) -> m r

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

instance MonadRequest t m => MonadRequest t (ReaderT r m) where
  type Request (ReaderT r m) = Request m
  type Response (ReaderT r m) = Response m
  withRequesting f = do
    r <- ask
    lift $ withRequesting $ (`runReaderT` r) . f

instance MonadRequest t m => MonadRequest t (StateT s m) where
  type Request (StateT s m) = Request m
  type Response (StateT s m) = Response m
  withRequesting f = do
    old <- get
    (result, new) <- lift $ withRequesting $ \x -> do
      ((request, result), new) <- runStateT (f x) old
      return (request, (result, new))
    put new
    return result

instance MonadRequest t m => MonadRequest t (Lazy.StateT s m) where
  type Request (Lazy.StateT s m) = Request m
  type Response (Lazy.StateT s m) = Response m
  withRequesting f = do
    old <- Lazy.get
    (result, new) <- lift $ withRequesting $ \x -> do
      ((request, result), new) <- Lazy.runStateT (f x) old
      return (request, (result, new))
    Lazy.put new
    return result

requesting :: MonadRequest t m => Event t (Request m a) -> m (Event t (Response m a))
requesting req = withRequesting $ return . (,) req

requesting_ :: MonadRequest t m => Event t (Request m a) -> m ()
requesting_ req = withRequesting $ \_ -> return (req, ())

requestingIdentity :: (MonadRequest t m, Response m ~ Identity) => Event t (Request m a) -> m (Event t a)
requestingIdentity = fmap (fmap runIdentity) . requesting
