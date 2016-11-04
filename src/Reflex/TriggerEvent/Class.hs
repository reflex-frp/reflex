{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Reflex.TriggerEvent.Class where

import Reflex.Class

import Control.Monad.Reader

--TODO: Shouldn't have IO hard-coded
class Monad m => TriggerEvent t m | m -> t where
  newTriggerEvent :: m (Event t a, a -> IO ())
  newTriggerEventWithOnComplete :: m (Event t a, a -> IO () -> IO ()) --TODO: This and newTriggerEvent should be unified somehow
  newEventWithLazyTriggerWithOnComplete :: ((a -> IO () -> IO ()) -> IO (IO ())) -> m (Event t a)

instance TriggerEvent t m => TriggerEvent t (ReaderT r m) where
  newTriggerEvent = lift newTriggerEvent
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete

