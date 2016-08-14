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

import Reflex

import Control.Monad.Reader

class (Reflex t, Monad (Performable m), Monad m) => PerformEvent t m | m -> t where
  type Performable m :: * -> *
  performEvent :: Event t (Performable m a) -> m (Event t a)
  performEvent_ :: Event t (Performable m ()) -> m ()

--TODO: Shouldn't have IO hard-coded
class Monad m => TriggerEvent t m | m -> t where
  newTriggerEvent :: m (Event t a, a -> IO ())
  newTriggerEventWithOnComplete :: m (Event t a, a -> IO () -> IO ()) --TODO: This and newTriggerEvent should be unified somehow
  newEventWithLazyTriggerWithOnComplete :: ((a -> IO () -> IO ()) -> IO (IO ())) -> m (Event t a)

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
