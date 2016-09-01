{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Reflex.PerformEvent.Base where

import Reflex.Class
import Reflex.Deletable.Class
import Reflex.Host.Class
import Reflex.PerformEvent.Class

import Control.Lens
import Control.Monad.Exception
import Control.Monad.Identity
import Control.Monad.Ref
import Control.Monad.State.Strict
import Data.Align
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum
import Data.Functor.Compose
import Data.Functor.Misc
import Data.Maybe
import Data.Semigroup
import Data.These
import Data.Word

newtype EventTriggerRef t m a = EventTriggerRef { unEventTriggerRef :: Ref m (Maybe (EventTrigger t a)) }

newtype FireCommand t m = FireCommand { runFireCommand :: forall a. [DSum (EventTrigger t) Identity] -> ReadPhase m a -> m [a] } --TODO: The handling of this ReadPhase seems wrong, or at least inelegant; how do we actually make the decision about what order frames run in?

newtype PerformEventT t m a = PerformEventT { unPerformEventT :: StateT [Event t (These (PerformEventT t m [DSum (EventTriggerRef t m) Identity]) ())] (HostFrame t) a }

deriving instance ReflexHost t => Functor (PerformEventT t m)
deriving instance ReflexHost t => Applicative (PerformEventT t m)
deriving instance ReflexHost t => Monad (PerformEventT t m)
deriving instance ReflexHost t => MonadFix (PerformEventT t m)
deriving instance (ReflexHost t, MonadIO (HostFrame t)) => MonadIO (PerformEventT t m)
deriving instance (ReflexHost t, MonadException (HostFrame t)) => MonadException (PerformEventT t m)

instance (ReflexHost t, MonadAsyncException (HostFrame t)) => MonadAsyncException (PerformEventT t m) where
  mask f = PerformEventT $ mask $ \unmask -> unPerformEventT $ f $ PerformEventT . unmask . unPerformEventT

instance (ReflexHost t, Ref m ~ Ref IO, MonadRef (HostFrame t), Ref (HostFrame t) ~ Ref IO) => PerformEvent t (PerformEventT t m) where
  type Performable (PerformEventT t m) = PerformEventT t m
  {-# INLINABLE performEvent_ #-}
  performEvent_ = PerformEventT . modify . (:) . fmap (This . (>> return []))
  {-# INLINABLE performEvent #-}
  performEvent e = PerformEventT $ do
    (eResult, reResultTrigger) <- newEventWithTriggerRef
    modify $ (:) $ ffor e $ \o -> This $ do
      result <- o
      return [EventTriggerRef reResultTrigger :=> Identity result]
    return eResult

instance (ReflexHost t, Monad (HostFrame t), Ref (HostFrame t) ~ Ref IO, Ref m ~ Ref IO) => Deletable t (PerformEventT t m) where
  {-# INLINABLE deletable #-}
  deletable d a = PerformEventT $ do
    (result, reverseEventsToPerform) <- lift $ runStateT (unPerformEventT a) []
    let (numberedEvents, _) = numberWith (\n e -> Const2 n :=> e) (reverse reverseEventsToPerform) (1 :: Word64)
    eventToPerform <- lift $ mergePerformEventsAndAdd (DMap.fromList numberedEvents) never
    modify (align eventToPerform d :)
    return result

instance ReflexHost t => MonadReflexCreateTrigger t (PerformEventT t m) where
  {-# INLINABLE newEventWithTrigger #-}
  newEventWithTrigger = PerformEventT . lift . newEventWithTrigger
  {-# INLINABLE newFanEventWithTrigger #-}
  newFanEventWithTrigger f = PerformEventT $ lift $ newFanEventWithTrigger f

{-# INLINABLE numberWith #-}
numberWith :: (Enum n, Traversable t) => (n -> a -> b) -> t a -> n -> (t b, n)
numberWith f t n0 = flip runState n0 $ forM t $ \x -> state $ \n -> (f n x, succ n)

{-# INLINABLE mergePerformEventsAndAdd #-}
mergePerformEventsAndAdd :: forall t m. ReflexHost t
  => DMap (Const2 Word64 (These (PerformEventT t m [DSum (EventTriggerRef t m) Identity]) ())) (Event t)
  -> Event t (PatchDMap (DMap (Const2 Word64 (These (PerformEventT t m [DSum (EventTriggerRef t m) Identity]) ())) (Event t)))
  -> HostFrame t (Event t (PerformEventT t m [DSum (EventTriggerRef t m) Identity]))
mergePerformEventsAndAdd initial eAddNew = do
  rec events :: Incremental t PatchDMap (DMap (Const2 Word64 (These (PerformEventT t m [DSum (EventTriggerRef t m) Identity]) ())) (Event t))
        <- holdIncremental initial $ eAddNew <> eDelete --TODO: Eliminate empty firings
      let event = mergeWith DMap.union [mergeIncremental events, coincidence (fmap (\(PatchDMap m) -> merge $ DMap.mapMaybeWithKey (const getCompose) m) eAddNew)]
          eventToPerform :: Event t (PerformEventT t m [DSum (EventTriggerRef t m) Identity])
          eventToPerform = ffor event $
            fmap concat .
            sequence .
            fmapMaybe (\(Const2 _ :=> Identity x) -> x ^? here) .
            DMap.toList
          eDelete :: Event t (PatchDMap (DMap (Const2 Word64 (These (PerformEventT t m [DSum (EventTriggerRef t m) Identity]) ())) (Event t)))
          eDelete = fforMaybe event $
            fmap (PatchDMap . DMap.fromList) .
            (\l -> if null l then Nothing else Just l) .
            fmapMaybe (\(Const2 k :=> Identity x) -> (Const2 k :=> Compose Nothing) <$ x ^? there) .
            DMap.toList
  return eventToPerform

{-# INLINABLE runPerformEventT #-}
runPerformEventT :: forall t m a. (ReflexHost t, MonadRef (HostFrame t), Ref (HostFrame t) ~ Ref IO, Ref m ~ Ref IO) => PerformEventT t m a -> HostFrame t (a, Event t (HostFrame t [DSum (EventTriggerRef t m) Identity]))
runPerformEventT (PerformEventT a) = do
  (result, reverseEventsToPerform) <- runStateT a []
  (eAddNew, reAddNewTrigger) <- newEventWithTriggerRef
  let (numberedEvents, nextNumber) = numberWith (\n e -> Const2 n :=> e) (reverse reverseEventsToPerform) (1 :: Word64)
  nextNumberRef <- newRef nextNumber
  let runResult :: PerformEventT t m [DSum (EventTriggerRef t m) Identity] -> HostFrame t [DSum (EventTriggerRef t m) Identity]
      runResult (PerformEventT r) = do
        (followups, reverseNewEventsToPerform) <- runStateT r []
        oldN <- readRef nextNumberRef
        let (numberedNewEvents, newN) = numberWith (\n e -> Const2 n :=> Compose (Just e)) (reverse reverseNewEventsToPerform) oldN
        if newN == oldN then return followups else do
          writeRef nextNumberRef newN
          return $ (EventTriggerRef reAddNewTrigger :=> Identity (PatchDMap (DMap.fromList numberedNewEvents))) : followups
  eventToPerform <- mergePerformEventsAndAdd (DMap.fromList numberedEvents) eAddNew
  return (result, runResult <$> eventToPerform)

{-# INLINABLE hostPerformEventT #-}
hostPerformEventT :: forall t m a. (Monad m, MonadSubscribeEvent t m, MonadReflexHost t m, MonadRef m, Ref m ~ Ref IO, MonadRef (HostFrame t), Ref (HostFrame t) ~ Ref IO) => PerformEventT t m a -> m (a, FireCommand t m)
hostPerformEventT a = do
  (result, eventToPerform) <- runHostFrame $ runPerformEventT a
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
              followupEventTriggerRefs <- runHostFrame toPerform
              followupEventTriggers <- forM followupEventTriggerRefs $ \(EventTriggerRef rt :=> x) -> do
                mt <- readRef rt
                return $ fmap (:=> x) mt
              fmap (result':) $ go $ catMaybes followupEventTriggers
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
