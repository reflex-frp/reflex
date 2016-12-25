-- | This module provides 'EventWriterT', the standard implementation of
-- 'EventWriter'.
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
module Reflex.EventWriter
  ( EventWriterT (..)
  , runEventWriterT
  , EventWriter(..)
  , runWithReplaceEventWriterTWith
  , sequenceDMapWithAdjustEventWriterTWith
  ) where

import Reflex.Class
import Reflex.Host.Class
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class
import Reflex.TriggerEvent.Class

import Control.Monad.Exception
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.State.Strict
import Data.Dependent.Map (DMap, DSum (..))
import qualified Data.Dependent.Map as DMap
import Data.Foldable
import Data.Functor.Misc
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Data.Semigroup
import Data.Sequence
import Data.Some (Some)
import qualified Data.Some as Some
import Data.Tuple
import Data.Unique.Tag

-- | 'EventWriter' efficiently collects 'Event' values using 'tellEvent'
-- and combines them monoidally to provide an 'Event' result.
class (Monad m, Monoid w) => EventWriter t w m | m -> t w where
  tellEvent :: Event t w -> m ()

-- | A basic implementation of 'EventWriter'.
newtype EventWriterT t w m a = EventWriterT { unEventWriterT :: StateT (Seq (Event t w)) m a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadException)

-- | Run a 'EventWriterT' action.
runEventWriterT :: (Reflex t, Monad m, Semigroup w) => EventWriterT t w m a -> m (a, Event t w)
runEventWriterT (EventWriterT a) = do
  (result, requests) <- runStateT a mempty
  return (result, mconcat $ toList requests)

instance (Reflex t, Monad m, Monoid w) => EventWriter t w (EventWriterT t w m) where
  tellEvent w = EventWriterT $ modify (|> w)

instance MonadTrans (EventWriterT t w) where
  lift = EventWriterT . lift

instance MonadSample t m => MonadSample t (EventWriterT t w m) where
  sample = lift . sample

instance MonadHold t m => MonadHold t (EventWriterT t w m) where
  hold v0 = lift . hold v0
  holdDyn v0 = lift . holdDyn v0
  holdIncremental v0 = lift . holdIncremental v0

instance (Reflex t, MonadAdjust t m, MonadHold t m, Monoid w, Semigroup w) => MonadAdjust t (EventWriterT t w m) where
  runWithReplace = runWithReplaceEventWriterTWith $ \dm0 dm' -> lift $ runWithReplace dm0 dm'
  sequenceDMapWithAdjust = sequenceDMapWithAdjustEventWriterTWith $ \dm0 dm' -> lift $ sequenceDMapWithAdjust dm0 dm'

-- | Given a function like 'runWithReplace' for the underlying monad, implement
-- 'runWithReplace' for 'EventWriterT'.  This is necessary when the underlying
-- monad doesn't have a 'MonadAdjust' instance or to override the default
-- 'MonadAdjust' behavior.
runWithReplaceEventWriterTWith :: forall m t w a b. (Reflex t, MonadHold t m, Monoid w)
                               => (forall a' b'. m a' -> Event t (m b') -> EventWriterT t w m (a', Event t b'))
                               -> EventWriterT t w m a
                               -> Event t (EventWriterT t w m b)
                               -> EventWriterT t w m (a, Event t b)
runWithReplaceEventWriterTWith f a0 a' = do
  let g :: EventWriterT t w m c -> m (c, Seq (Event t w))
      g (EventWriterT r) = runStateT r mempty
  (result0, result') <- f (g a0) $ g <$> a'
  request <- holdDyn (fmap (mconcat . NonEmpty.toList) $ mergeList $ toList $ snd result0) $ fmap (mconcat . NonEmpty.toList) . mergeList . toList . snd <$> result'
  -- We add these two separately to take advantage of the free merge being done later.  The coincidence case must come first so that it has precedence if both fire simultaneously.  (Really, we should probably block the 'switch' whenever 'updated' fires, but switchPromptlyDyn has the same issue.)
  EventWriterT $ modify $ flip (|>) $ coincidence $ updated request
  EventWriterT $ modify $ flip (|>) $ switch $ current request
  return (fst result0, fst <$> result')

-- | Like 'runWithReplaceEventWriterTWith', but for 'sequenceDMapWithAdjust'.
sequenceDMapWithAdjustEventWriterTWith :: (GCompare k, Reflex t, MonadHold t m, Monoid w, Semigroup w)
                                       => (forall k'. GCompare k'
                                           => DMap k' m
                                           -> Event t (PatchDMap k' m)
                                           -> EventWriterT t w m (DMap k' Identity, Event t (PatchDMap k' Identity))
                                          )
                                       -> DMap k (EventWriterT t w m)
                                       -> Event t (PatchDMap k (EventWriterT t w m))
                                       -> EventWriterT t w m (DMap k Identity, Event t (PatchDMap k Identity))
sequenceDMapWithAdjustEventWriterTWith f (dm0 :: DMap k (EventWriterT t w m)) dm' = do
  let inputTransform :: forall a. DMapTransform a k (WrapArg ((,) (Seq (Event t w))) k) (EventWriterT t w m) m
      inputTransform = DMapTransform WrapArg (\(EventWriterT v) -> swap <$> runStateT v mempty)
  (children0, children') <- f (mapKeysAndValuesMonotonic inputTransform dm0) $ mapPatchKeysAndValuesMonotonic inputTransform <$> dm'
  let result0 = mapKeyValuePairsMonotonic (\(WrapArg k :=> Identity (_, v)) -> k :=> Identity v) children0
      result' = ffor children' $ \(PatchDMap p) -> PatchDMap $
        mapKeyValuePairsMonotonic (\(WrapArg k :=> ComposeMaybe mv) -> k :=> ComposeMaybe (fmap (Identity . snd . runIdentity) mv)) p
      requests0 :: DMap (Const2 (Some k) w) (Event t)
      requests0 = mapKeyValuePairsMonotonic (\(WrapArg k :=> Identity (r, _)) -> Const2 (Some.This k) :=> mconcat (toList r)) children0
      requests' :: Event t (PatchDMap (Const2 (Some k) w) (Event t))
      requests' = ffor children' $ \(PatchDMap p) -> PatchDMap $
        mapKeyValuePairsMonotonic (\(WrapArg k :=> ComposeMaybe mv) -> Const2 (Some.This k) :=> ComposeMaybe (fmap (mconcat . toList . fst . runIdentity) mv)) p
  childRequestMap <- holdIncremental requests0 requests'
  -- We add these two separately to take advantage of the free merge being done later.  The coincidence case must come first so that it has precedence if both fire simultaneously.  (Really, we should probably block the 'switch' whenever 'updated' fires, but switchPromptlyDyn has the same issue.)
  EventWriterT $ modify $ flip (|>) $ coincidence $ ffor requests' $ \(PatchDMap p) -> mconcat $ toList $ catMaybes $ ffor (DMap.toList p) $ \(Const2 _ :=> ComposeMaybe me) -> me
  EventWriterT $ modify $ flip (|>) $ ffor (mergeIncremental childRequestMap) $ \m ->
    mconcat $ (\(Const2 _ :=> Identity reqs) -> reqs) <$> DMap.toList m
  return (result0, result')

instance PerformEvent t m => PerformEvent t (EventWriterT t w m) where
  type Performable (EventWriterT t w m) = Performable m
  performEvent_ = lift . performEvent_
  performEvent = lift . performEvent

instance PostBuild t m => PostBuild t (EventWriterT t w m) where
  getPostBuild = lift getPostBuild

instance TriggerEvent t m => TriggerEvent t (EventWriterT t w m) where
  newTriggerEvent = lift newTriggerEvent
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete

instance MonadReader r m => MonadReader r (EventWriterT t w m) where
  ask = lift ask
  local f (EventWriterT a) = EventWriterT $ mapStateT (local f) a
  reader = lift . reader

instance MonadRef m => MonadRef (EventWriterT t w m) where
  type Ref (EventWriterT t w m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (EventWriterT t w m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

--------------------------------------------------------------------------------
-- Internal functions
--------------------------------------------------------------------------------

data DMapTransform (a :: *) k k' v v' = forall (b :: *). DMapTransform !(k a -> k' b) !(v a -> v' b)

mapPatchKeysAndValuesMonotonic :: (forall a. DMapTransform a k k' v v') -> PatchDMap k v -> PatchDMap k' v'
mapPatchKeysAndValuesMonotonic x (PatchDMap p) = PatchDMap $ mapKeyValuePairsMonotonic (\(k :=> ComposeMaybe mv) -> case x of DMapTransform f g -> f k :=> ComposeMaybe (fmap g mv)) p

mapKeysAndValuesMonotonic :: (forall a. DMapTransform a k k' v v') -> DMap k v -> DMap k' v'
mapKeysAndValuesMonotonic x = mapKeyValuePairsMonotonic $ \(k :=> v) -> case x of
  DMapTransform f g -> f k :=> g v
