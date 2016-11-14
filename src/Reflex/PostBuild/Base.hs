-- | This module defines 'PostBuildT', the standard implementation of
-- 'PostBuild'.
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
module Reflex.PostBuild.Base
  ( PostBuildT (..)
  , runPostBuildT
  ) where

import Reflex.Class
import Reflex.Host.Class
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class
import Reflex.TriggerEvent.Class

import Control.Monad.Exception
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.Trans.Control
import qualified Data.Dependent.Map as DMap

-- | Provides a basic implementation of 'PostBuild'.
newtype PostBuildT t m a = PostBuildT { unPostBuildT :: ReaderT (Event t ()) m a } deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadTrans, MonadException, MonadAsyncException)

-- | Run a 'PostBuildT' action.  An 'Event' should be provided that fires
-- immediately after the action is finished running; no other 'Event's should
-- fire first.
{-# INLINABLE runPostBuildT #-}
runPostBuildT :: PostBuildT t m a -> Event t () -> m a
runPostBuildT (PostBuildT a) = runReaderT a

instance PrimMonad m => PrimMonad (PostBuildT x m) where
  type PrimState (PostBuildT x m) = PrimState m
  primitive = lift . primitive

instance MonadTransControl (PostBuildT t) where
  type StT (PostBuildT t) a = StT (ReaderT (Event t ())) a
  {-# INLINABLE liftWith #-}
  liftWith = defaultLiftWith PostBuildT unPostBuildT
  {-# INLINABLE restoreT #-}
  restoreT = defaultRestoreT PostBuildT

instance (Reflex t, Monad m) => PostBuild t (PostBuildT t m) where
  {-# INLINABLE getPostBuild #-}
  getPostBuild = PostBuildT ask

instance MonadSample t m => MonadSample t (PostBuildT t m) where
  {-# INLINABLE sample #-}
  sample = lift . sample

instance MonadHold t m => MonadHold t (PostBuildT t m) where
  {-# INLINABLE hold #-}
  hold v0 = lift . hold v0
  {-# INLINABLE holdDyn #-}
  holdDyn v0 = lift . holdDyn v0
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 = lift . holdIncremental v0

instance PerformEvent t m => PerformEvent t (PostBuildT t m) where
  type Performable (PostBuildT t m) = PostBuildT t (Performable m)
  {-# INLINABLE performEvent_ #-}
  performEvent_ e = liftWith $ \run -> performEvent_ $ fmap run e
  {-# INLINABLE performEvent #-}
  performEvent e = liftWith $ \run -> performEvent $ fmap run e

instance (ReflexHost t, MonadReflexCreateTrigger t m) => MonadReflexCreateTrigger t (PostBuildT t m) where
  {-# INLINABLE newEventWithTrigger #-}
  newEventWithTrigger = PostBuildT . lift . newEventWithTrigger
  {-# INLINABLE newFanEventWithTrigger #-}
  newFanEventWithTrigger f = PostBuildT $ lift $ newFanEventWithTrigger f

instance TriggerEvent t m => TriggerEvent t (PostBuildT t m) where
  {-# INLINABLE newTriggerEvent #-}
  newTriggerEvent = lift newTriggerEvent
  {-# INLINABLE newTriggerEventWithOnComplete #-}
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete

instance MonadRef m => MonadRef (PostBuildT t m) where
  type Ref (PostBuildT t m) = Ref m
  {-# INLINABLE newRef #-}
  newRef = lift . newRef
  {-# INLINABLE readRef #-}
  readRef = lift . readRef
  {-# INLINABLE writeRef #-}
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (PostBuildT t m) where
  {-# INLINABLE atomicModifyRef #-}
  atomicModifyRef r = lift . atomicModifyRef r

instance (Reflex t, MonadHold t m, MonadFix m, MonadAdjust t m, PerformEvent t m) => MonadAdjust t (PostBuildT t m) where
  runWithReplace a0 a' = do
    postBuild <- getPostBuild
    lift $ do
      rec result@(_, result') <- runWithReplace (runPostBuildT a0 postBuild) $ fmap (\v -> runPostBuildT v =<< headE voidResult') a'
          let voidResult' = void result'
      return result
  sequenceDMapWithAdjust dm0 dm' = do
    postBuild <- getPostBuild
    let loweredDm0 = DMap.map (`runPostBuildT` postBuild) dm0
    lift $ do
      rec (result0, result') <- sequenceDMapWithAdjust loweredDm0 loweredDm'
          let voidResult' = void result'
          let loweredDm' = ffor dm' $ \(PatchDMap p) -> PatchDMap $
                DMap.map (ComposeMaybe . fmap (\v -> runPostBuildT v =<< headE voidResult') . getComposeMaybe) p --TODO: Avoid doing this headE so many times; once per loweredDm' firing ought to be OK, but it's not totally trivial to do because result' might be firing at the same time, and we don't want *that* to be the postBuild occurrence
      return (result0, result')
