-- | This module defines 'PostBuildT', the standard implementation of
-- 'PostBuild'.
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Reflex.PostBuild.Base
  ( PostBuildT (..)
  , runPostBuildT
  ) where

import Control.Monad.Catch (MonadMask, MonadThrow, MonadCatch)
import Control.Monad.Exception
import Control.Monad.Fix
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Ref
import qualified Control.Monad.Trans.Control as TransControl
import qualified Data.Semigroup as S

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
import Control.Monad.Identity
#endif

import Reflex.Class
import Reflex.Adjustable.Class
import Reflex.Host.Class
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class
import Reflex.TriggerEvent.Class
import Data.Coerce (coerce)

-- | Provides a basic implementation of 'PostBuild'.
newtype PostBuildT t m a = PostBuildT { unPostBuildT :: m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadFix
    , MonadIO
    , MonadException
    , MonadAsyncException
    , MonadMask
    , MonadThrow
    , MonadCatch
    )

instance MonadTrans (PostBuildT t) where
  lift = PostBuildT

-- | Run a 'PostBuildT' action.  An 'Event' should be provided that fires
-- immediately after the action is finished running; no other 'Event's should
-- fire first.
{-# INLINABLE runPostBuildT #-}
runPostBuildT :: PostBuildT t m a -> m a
runPostBuildT = unPostBuildT

-- TODO: Monoid and Semigroup can likely be derived once ReaderT has them.
instance (Monoid a, Applicative m) => Monoid (PostBuildT t m a) where
  mempty = pure mempty

instance (S.Semigroup a, Applicative m) => S.Semigroup (PostBuildT t m a) where
  (<>) = liftA2 (S.<>)

instance PrimMonad m => PrimMonad (PostBuildT x m) where
  type PrimState (PostBuildT x m) = PrimState m
  primitive = lift . primitive

instance (Reflex t, MonadHold t m, Monad m) => PostBuild t (PostBuildT t m) where
  {-# INLINABLE getPostBuild #-}
  getPostBuild = PostBuildT now

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
  {-# INLINABLE buildDynamic #-}
  buildDynamic a0 = lift . buildDynamic a0
  {-# INLINABLE headE #-}
  headE = lift . headE
  {-# INLINABLE now #-}
  now = lift now

instance PerformEvent t m => PerformEvent t (PostBuildT t m) where
  type Performable (PostBuildT t m) = Performable m
  {-# INLINABLE performEvent_ #-}
  performEvent_ = lift . performEvent_
  {-# INLINABLE performEvent #-}
  performEvent = lift . performEvent

instance (ReflexHost t, MonadReflexCreateTrigger t m) => MonadReflexCreateTrigger t (PostBuildT t m) where
  {-# INLINABLE newEventWithTrigger #-}
  newEventWithTrigger = PostBuildT . newEventWithTrigger
  {-# INLINABLE newFanEventWithTrigger #-}
  newFanEventWithTrigger f = PostBuildT $ newFanEventWithTrigger f
  {-# INLINABLE newEventWithTriggerAndRetire #-}
  newEventWithTriggerAndRetire = PostBuildT . newEventWithTriggerAndRetire
  {-# INLINABLE newFanEventWithTriggerAndRetire #-}
  newFanEventWithTriggerAndRetire f = PostBuildT $ newFanEventWithTriggerAndRetire f

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

instance (Reflex t, MonadHold t m, MonadFix m, Adjustable t m, PerformEvent t m) => Adjustable t (PostBuildT t m) where
  runWithReplace a0 a' =
    PostBuildT $ runWithReplace (coerce a0) (coerceEvent a')
  {-# INLINABLE traverseIntMapWithKeyWithAdjust #-}
  traverseIntMapWithKeyWithAdjust f dm0 dm' =
    PostBuildT $ traverseIntMapWithKeyWithAdjust (\k v -> unPostBuildT (f k v)) dm0 dm'
  {-# INLINABLE traverseDMapWithKeyWithAdjust #-}
  traverseDMapWithKeyWithAdjust f dm0 dm' =
    PostBuildT $ traverseDMapWithKeyWithAdjust (\k v -> unPostBuildT (f k v)) dm0 dm'
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' =
    PostBuildT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unPostBuildT (f k v)) dm0 dm'

--------------------------------------------------------------------------------
-- Deprecated functionality
--------------------------------------------------------------------------------

-- | Deprecated
instance TransControl.MonadTransControl (PostBuildT t) where
  type StT (PostBuildT t) a = a
  {-# INLINABLE liftWith #-}
  liftWith f = PostBuildT $ f unPostBuildT
  {-# INLINABLE restoreT #-}
  restoreT = PostBuildT
