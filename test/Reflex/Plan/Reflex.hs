{-# LANGUAGE FunctionalDependencies, BangPatterns, UndecidableInstances, ConstraintKinds, GADTs, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, RankNTypes, RecursiveDo, FlexibleContexts, StandaloneDeriving #-}
module Reflex.Plan.Reflex
  ( TestPlan(..)
  , runPlan
  , Plan
  , Schedule
  , Firing (..)
  , MonadIORef


  , readSchedule
  , testSchedule
  , readEvent'
  , makeDense

  , runTestE
  , runTestB

  ) where

import Reflex.Class
import Reflex.Host.Class
import Reflex.TestPlan

import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State.Strict

import Data.Dependent.Sum (DSum (..))
import Data.Monoid
import Data.Traversable (traverse, sequence)
import Data.Maybe
import qualified Data.IntMap.Strict as IntMap
import Control.Monad.Ref


import Data.IntMap.Strict (IntMap)
import Data.IORef
import System.Mem

-- Note: this import must come last to silence warnings from AMP
import Prelude hiding (sequence)

type MonadIORef m = (MonadIO m, MonadRef m, Ref m ~ Ref IO)

data Firing t where
  Firing :: IORef (Maybe (EventTrigger t a)) -> a -> Firing t


type Schedule t = IntMap [Firing t]

-- Implementation of a TestPlan in terms of ReflexHost
newtype Plan t a = Plan (StateT (Schedule t) (HostFrame t) a)

deriving instance ReflexHost t => Functor (Plan t)
deriving instance ReflexHost t => Applicative (Plan t)
deriving instance ReflexHost t => Monad (Plan t)

deriving instance ReflexHost t => MonadSample t (Plan t)
deriving instance ReflexHost t => MonadHold t (Plan t)
deriving instance ReflexHost t => MonadFix (Plan t)


instance (ReflexHost t, MonadRef (HostFrame t), Ref (HostFrame t) ~ Ref IO) => TestPlan t (Plan t) where
  plan occurances = Plan $ do
    (e, ref) <- newEventWithTriggerRef
    modify (IntMap.unionWith mappend (firings ref))
    return e

    where
      firings ref = IntMap.fromList (makeFiring ref <$> occurances)
      makeFiring ref (t, a) = (fromIntegral t, [Firing ref a])


firingTrigger :: (MonadReflexHost t m, MonadIORef m) => Firing t -> m (Maybe (DSum  (EventTrigger t) Identity))
firingTrigger (Firing ref a) = fmap (:=> Identity a) <$> readRef ref

runPlan :: (MonadReflexHost t m, MonadIORef m) => Plan t a -> m (a, Schedule t)
runPlan (Plan p) = runHostFrame $ runStateT p mempty


makeDense :: Schedule t -> Schedule t
makeDense s = fromMaybe (emptyRange 0) $ do
  (end, _) <- fst <$> IntMap.maxViewWithKey s
  return $ IntMap.union s (emptyRange end)
    where
      emptyRange end = IntMap.fromList (zip [0..end + 1] (repeat []))


-- For the purposes of testing, we add in a zero frame and extend one frame (to observe changes to behaviors
-- after the last event)
-- performGC is called at each frame to test for GC issues
testSchedule :: (MonadReflexHost t m, MonadIORef m) => Schedule t -> ReadPhase m a -> m (IntMap a)
testSchedule schedule readResult = IntMap.traverseWithKey (\t occs -> liftIO performGC *> triggerFrame readResult t occs) (makeDense schedule)

readSchedule :: (MonadReflexHost t m, MonadIORef m) => Schedule t -> ReadPhase m a -> m (IntMap a)
readSchedule schedule readResult = IntMap.traverseWithKey (triggerFrame readResult) schedule

triggerFrame :: (MonadReflexHost t m, MonadIORef m) => ReadPhase m a -> Int -> [Firing t] -> m a
triggerFrame readResult _ occs =  do
    triggers <- catMaybes <$> traverse firingTrigger occs
    fireEventsAndRead triggers readResult


readEvent' :: MonadReadEvent t m => EventHandle t a -> m (Maybe a)
readEvent' = readEvent >=> sequence


-- Convenience functions for running tests producing Events/Behaviors
runTestB :: (MonadReflexHost t m, MonadIORef m) => Plan t (Behavior t a) -> m (IntMap a)
runTestB p = do
  (b, s) <- runPlan p
  testSchedule s $ sample b

runTestE :: (MonadReflexHost t m, MonadIORef m) => Plan t (Event t a) -> m (IntMap (Maybe a))
runTestE p = do
  (e, s) <- runPlan p
  h <- subscribeEvent e
  testSchedule s (readEvent' h)


