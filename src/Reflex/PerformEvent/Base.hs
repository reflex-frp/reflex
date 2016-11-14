-- | This module provides 'PerformEventT', the standard implementation of
-- 'PerformEvent'.
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
module Reflex.PerformEvent.Base
  ( PerformEventT (..)
  , FireCommand (..)
  , hostPerformEventT
  ) where

import Reflex.Class
import Reflex.Host.Class
import Reflex.PerformEvent.Class
import Reflex.Requester.Base
import Reflex.Requester.Class

import Control.Lens
import Control.Monad.Exception
import Control.Monad.Identity
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Ref
import Data.Coerce
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum

-- | A function that fires events for the given 'EventTrigger's and then runs
-- any followup actions provided via 'PerformEvent'.  The given 'ReadPhase'
-- action will be run once for the initial trigger execution as well as once for
-- each followup.
newtype FireCommand t m = FireCommand { runFireCommand :: forall a. [DSum (EventTrigger t) Identity] -> ReadPhase m a -> m [a] } --TODO: The handling of this ReadPhase seems wrong, or at least inelegant; how do we actually make the decision about what order frames run in?

-- | Provides a basic implementation of 'PerformEvent'.  Note that, despite the
-- name, 'PerformEventT' is not an instance of 'MonadTrans'.
newtype PerformEventT t m a = PerformEventT { unPerformEventT :: RequesterT t (HostFrame t) Identity (HostFrame t) a }

deriving instance ReflexHost t => Functor (PerformEventT t m)
deriving instance ReflexHost t => Applicative (PerformEventT t m)
deriving instance ReflexHost t => Monad (PerformEventT t m)
deriving instance ReflexHost t => MonadFix (PerformEventT t m)
deriving instance (ReflexHost t, MonadIO (HostFrame t)) => MonadIO (PerformEventT t m)
deriving instance (ReflexHost t, MonadException (HostFrame t)) => MonadException (PerformEventT t m)

instance (PrimMonad (HostFrame t), ReflexHost t) => PrimMonad (PerformEventT t m) where
  type PrimState (PerformEventT t m) = PrimState (HostFrame t)
  primitive = PerformEventT . lift . primitive

instance (ReflexHost t, Ref m ~ Ref IO, PrimMonad (HostFrame t)) => PerformEvent t (PerformEventT t m) where
  type Performable (PerformEventT t m) = HostFrame t
  {-# INLINABLE performEvent_ #-}
  performEvent_ = PerformEventT . requesting_
  {-# INLINABLE performEvent #-}
  performEvent = PerformEventT . requestingIdentity

instance (ReflexHost t, PrimMonad (HostFrame t)) => MonadAdjust t (PerformEventT t m) where
  runWithReplace outerA0 outerA' = PerformEventT $ runWithReplaceRequesterTWith f (coerce outerA0) (coerceEvent outerA')
    where f :: HostFrame t a -> Event t (HostFrame t b) -> RequesterT t (HostFrame t) Identity (HostFrame t) (a, Event t b)
          f a0 a' = do
            result0 <- lift a0
            result' <- requesting a'
            return (result0, fmap runIdentity result')
  sequenceDMapWithAdjust (outerDm0 :: DMap k (PerformEventT t m)) outerDm' = PerformEventT $ sequenceDMapWithAdjustRequesterTWith f (coerce outerDm0) (coerceEvent outerDm')
    where f :: DMap k' (HostFrame t) -> Event t (PatchDMap k' (HostFrame t)) -> RequesterT t (HostFrame t) Identity (HostFrame t) (DMap k' Identity, Event t (PatchDMap k' Identity))
          f dm0 dm' = do
            result0 <- lift $ DMap.traverseWithKey (\_ v -> Identity <$> v) dm0
            result' <- requesting $ ffor dm' $ \(PatchDMap p) -> do
              PatchDMap <$> DMap.traverseWithKey (\_ (ComposeMaybe mv) -> ComposeMaybe <$> mapM (fmap Identity) mv) p
            return (result0, fmap runIdentity result')

instance ReflexHost t => MonadReflexCreateTrigger t (PerformEventT t m) where
  {-# INLINABLE newEventWithTrigger #-}
  newEventWithTrigger = PerformEventT . lift . newEventWithTrigger
  {-# INLINABLE newFanEventWithTrigger #-}
  newFanEventWithTrigger f = PerformEventT $ lift $ newFanEventWithTrigger f

-- | Run a 'PerformEventT' action, returning a 'FireCommand' that allows the
-- caller to trigger 'Event's while ensuring that 'performEvent' actions are run
-- at the appropriate time.
{-# INLINABLE hostPerformEventT #-}
hostPerformEventT :: forall t m a.
                     ( Monad m
                     , MonadSubscribeEvent t m
                     , MonadReflexHost t m
                     , MonadRef m
                     , Ref m ~ Ref IO
                     )
                  => PerformEventT t m a
                  -> m (a, FireCommand t m)
hostPerformEventT a = do
  (response, responseTrigger) <- newEventWithTriggerRef
  (result, eventToPerform) <- runHostFrame $ runRequesterT (unPerformEventT a) response
  eventToPerformHandle <- subscribeEvent eventToPerform
  return $ (,) result $ FireCommand $ \triggers (readPhase :: ReadPhase m a') -> do
    let go :: [DSum (EventTrigger t) Identity] -> m [a']
        go ts = do
          (result', mToPerform) <- fireEventsAndRead ts $ do
            mToPerform <- sequence =<< readEvent eventToPerformHandle
            result' <- readPhase
            return (result', mToPerform)
          case mToPerform of
            Nothing -> return [result']
            Just toPerform -> do
              responses <- runHostFrame $ DMap.traverseWithKey (\_ v -> Identity <$> v) toPerform
              mrt <- readRef responseTrigger
              let followupEventTriggers = case mrt of
                    Just rt -> [rt :=> Identity responses]
                    Nothing -> []
              (result':) <$> go followupEventTriggers
    go triggers

instance ReflexHost t => MonadSample t (PerformEventT t m) where
  {-# INLINABLE sample #-}
  sample = PerformEventT . lift . sample

instance (ReflexHost t, MonadHold t m) => MonadHold t (PerformEventT t m) where
  {-# INLINABLE hold #-}
  hold v0 v' = PerformEventT $ lift $ hold v0 v'
  {-# INLINABLE holdDyn #-}
  holdDyn v0 v' = PerformEventT $ lift $ holdDyn v0 v'
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 v' = PerformEventT $ lift $ holdIncremental v0 v'

instance (MonadRef (HostFrame t), ReflexHost t) => MonadRef (PerformEventT t m) where
  type Ref (PerformEventT t m) = Ref (HostFrame t)
  {-# INLINABLE newRef #-}
  newRef = PerformEventT . lift . newRef
  {-# INLINABLE readRef #-}
  readRef = PerformEventT . lift . readRef
  {-# INLINABLE writeRef #-}
  writeRef r = PerformEventT . lift . writeRef r

instance (MonadAtomicRef (HostFrame t), ReflexHost t) => MonadAtomicRef (PerformEventT t m) where
  {-# INLINABLE atomicModifyRef #-}
  atomicModifyRef r = PerformEventT . lift . atomicModifyRef r
