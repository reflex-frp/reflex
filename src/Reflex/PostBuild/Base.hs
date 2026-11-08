{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines 'PostBuildT', the standard implementation of
-- 'PostBuild'.
module Reflex.PostBuild.Base
  ( PostBuildT (..)
  , runPostBuildT
  ) where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
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

import Data.Coerce (coerce)

import Reflex.Adjustable.Class
import Reflex.Class
import Reflex.Host.Class
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class
import Reflex.TriggerEvent.Class

-- | Provides a basic implementation of 'PostBuild'.
newtype PostBuildT t m a = PostBuildT {unPostBuildT :: m a}
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadAsyncException
    , MonadCatch
    , MonadException
    , MonadFix
    , MonadIO
    , MonadMask
    , MonadThrow
    )

instance MonadTrans (PostBuildT t) where
  lift = PostBuildT

-- | Run a 'PostBuildT' action.  An 'Event' should be provided that fires
-- immediately after the action is finished running; no other 'Event's should
-- fire first.
{-# INLINEABLE runPostBuildT #-}
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
  {-# INLINEABLE getPostBuild #-}
  getPostBuild = PostBuildT now

instance MonadSample t m => MonadSample t (PostBuildT t m) where
  {-# INLINEABLE sample #-}
  sample = lift . sample

instance MonadHold t m => MonadHold t (PostBuildT t m) where
  {-# INLINEABLE hold #-}
  hold v0 = lift . hold v0
  {-# INLINEABLE holdDyn #-}
  holdDyn v0 = lift . holdDyn v0
  {-# INLINEABLE holdIncremental #-}
  holdIncremental v0 = lift . holdIncremental v0
  {-# INLINEABLE buildDynamic #-}
  buildDynamic a0 = lift . buildDynamic a0
  {-# INLINEABLE headE #-}
  headE = lift . headE
  {-# INLINEABLE now #-}
  now = lift now

instance PerformEvent t m => PerformEvent t (PostBuildT t m) where
  type Performable (PostBuildT t m) = Performable m
  {-# INLINEABLE performEvent_ #-}
  performEvent_ = lift . performEvent_
  {-# INLINEABLE performEvent #-}
  performEvent = lift . performEvent

instance (ReflexHost t, MonadReflexCreateTrigger t m) => MonadReflexCreateTrigger t (PostBuildT t m) where
  {-# INLINEABLE newEventWithTrigger #-}
  newEventWithTrigger = PostBuildT . newEventWithTrigger
  {-# INLINEABLE newFanEventWithTrigger #-}
  newFanEventWithTrigger f = PostBuildT $ newFanEventWithTrigger f

instance TriggerEvent t m => TriggerEvent t (PostBuildT t m) where
  {-# INLINEABLE newTriggerEvent #-}
  newTriggerEvent = lift newTriggerEvent
  {-# INLINEABLE newTriggerEventWithOnComplete #-}
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete

instance MonadRef m => MonadRef (PostBuildT t m) where
  type Ref (PostBuildT t m) = Ref m
  {-# INLINEABLE newRef #-}
  newRef = lift . newRef
  {-# INLINEABLE readRef #-}
  readRef = lift . readRef
  {-# INLINEABLE writeRef #-}
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (PostBuildT t m) where
  {-# INLINEABLE atomicModifyRef #-}
  atomicModifyRef r = lift . atomicModifyRef r

instance (Reflex t, MonadHold t m, MonadFix m, Adjustable t m, PerformEvent t m) => Adjustable t (PostBuildT t m) where
  runWithReplace a0 a' =
    PostBuildT $ runWithReplace (coerce a0) (coerceEvent a')
  {-# INLINEABLE traverseIntMapWithKeyWithAdjust #-}
  traverseIntMapWithKeyWithAdjust f dm0 dm' =
    PostBuildT $ traverseIntMapWithKeyWithAdjust (\k v -> unPostBuildT (f k v)) dm0 dm'
  {-# INLINEABLE traverseDMapWithKeyWithAdjust #-}
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
  {-# INLINEABLE liftWith #-}
  liftWith f = PostBuildT $ f unPostBuildT
  {-# INLINEABLE restoreT #-}
  restoreT = PostBuildT
