-- | This module provides 'EventWriterT', the standard implementation of
-- 'EventWriter'.
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , EventWriter (..)
  , runWithReplaceEventWriterTWith
  , sequenceDMapWithAdjustEventWriterTWith
  ) where

import Reflex.Class
import Reflex.Host.Class
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class
import Reflex.Query.Class
import Reflex.Requester.Class
import Reflex.TriggerEvent.Class

import Control.Monad.Exception
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.State.Strict
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Foldable
import Data.Functor.Compose
import Data.Functor.Misc
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Semigroup
import Data.Sequence
import Data.Some (Some)
import Data.Tuple

-- | 'EventWriter' efficiently collects 'Event' values using 'tellEvent'
-- and combines them monoidally to provide an 'Event' result.
class (Monad m, Semigroup w) => EventWriter t w m | m -> t w where
  tellEvent :: Event t w -> m ()

-- | A basic implementation of 'EventWriter'.
newtype EventWriterT t w m a = EventWriterT { unEventWriterT :: StateT (Seq (Event t w)) m a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadException, MonadAsyncException)

-- | Run a 'EventWriterT' action.
runEventWriterT :: (Reflex t, Monad m, Semigroup w) => EventWriterT t w m a -> m (a, Event t w)
runEventWriterT (EventWriterT a) = do
  (result, requests) <- runStateT a mempty
  return (result, mconcat $ toList requests)

instance (Reflex t, Monad m, Semigroup w) => EventWriter t w (EventWriterT t w m) where
  tellEvent w = EventWriterT $ modify (|> w)

instance EventWriter t w m => EventWriter t w (ReaderT r m) where
  tellEvent = lift . tellEvent

instance MonadTrans (EventWriterT t w) where
  lift = EventWriterT . lift

instance MonadSample t m => MonadSample t (EventWriterT t w m) where
  sample = lift . sample

instance MonadHold t m => MonadHold t (EventWriterT t w m) where
  hold v0 = lift . hold v0
  holdDyn v0 = lift . holdDyn v0
  holdIncremental v0 = lift . holdIncremental v0

instance (Reflex t, MonadAdjust t m, MonadHold t m, Semigroup w) => MonadAdjust t (EventWriterT t w m) where
  runWithReplace = runWithReplaceEventWriterTWith $ \dm0 dm' -> lift $ runWithReplace dm0 dm'
  traverseDMapWithKeyWithAdjust = sequenceDMapWithAdjustEventWriterTWith (\f dm0 dm' -> lift $ traverseDMapWithKeyWithAdjust f dm0 dm') mapPatchDMap weakenPatchDMapWith patchMapNewElements mergeMapIncremental
  traverseDMapWithKeyWithAdjustWithMove = sequenceDMapWithAdjustEventWriterTWith (\f dm0 dm' -> lift $ traverseDMapWithKeyWithAdjustWithMove f dm0 dm') mapPatchDMapWithMove weakenPatchDMapWithMoveWith patchMapWithMoveNewElements mergeMapIncrementalWithMove

instance Requester t m => Requester t (EventWriterT t w m) where
  type Request (EventWriterT t w m) = Request m
  type Response (EventWriterT t w m) = Response m
  requesting = lift . requesting
  requesting_ = lift . requesting_

-- | Given a function like 'runWithReplace' for the underlying monad, implement
-- 'runWithReplace' for 'EventWriterT'.  This is necessary when the underlying
-- monad doesn't have a 'MonadAdjust' instance or to override the default
-- 'MonadAdjust' behavior.
runWithReplaceEventWriterTWith :: forall m t w a b. (Reflex t, MonadHold t m, Semigroup w)
                               => (forall a' b'. m a' -> Event t (m b') -> EventWriterT t w m (a', Event t b'))
                               -> EventWriterT t w m a
                               -> Event t (EventWriterT t w m b)
                               -> EventWriterT t w m (a, Event t b)
runWithReplaceEventWriterTWith f a0 a' = do
  let g :: EventWriterT t w m c -> m (c, Seq (Event t w))
      g (EventWriterT r) = runStateT r mempty
  (result0, result') <- f (g a0) $ fmap g a'
  request <- holdDyn (fmapCheap sconcat $ mergeList $ toList $ snd result0) $ fmapCheap (fmapCheap sconcat . mergeList . toList . snd) result'
  -- We add these two separately to take advantage of the free merge being done later.  The coincidence case must come first so that it has precedence if both fire simultaneously.  (Really, we should probably block the 'switch' whenever 'updated' fires, but switchPromptlyDyn has the same issue.)
  EventWriterT $ modify $ flip (|>) $ coincidence $ updated request
  EventWriterT $ modify $ flip (|>) $ switch $ current request
  return (fst result0, fmapCheap fst result')

-- | Like 'runWithReplaceEventWriterTWith', but for 'sequenceDMapWithAdjust'.
sequenceDMapWithAdjustEventWriterTWith :: forall t m p p' w k v v'. (Reflex t, MonadHold t m, Semigroup w, Patch (p' (Some k) (Event t w)), PatchTarget (p' (Some k) (Event t w)) ~ Map (Some k) (Event t w))
                                       => (   (forall a. k a -> v a -> m (Compose ((,) (Seq (Event t w))) v' a))
                                           -> DMap k v
                                           -> Event t (p k v)
                                           -> EventWriterT t w m (DMap k (Compose ((,) (Seq (Event t w))) v'), Event t (p k (Compose ((,) (Seq (Event t w))) v')))
                                          )
                                       -> ((forall a. Compose ((,) (Seq (Event t w))) v' a -> v' a) -> p k (Compose ((,) (Seq (Event t w))) v') -> p k v')
                                       -> ((forall a. Compose ((,) (Seq (Event t w))) v' a -> Event t w) -> p k (Compose ((,) (Seq (Event t w))) v') -> p' (Some k) (Event t w))
                                       -> (p' (Some k) (Event t w) -> [Event t w])
                                       -> (Incremental t (p' (Some k) (Event t w)) -> Event t (Map (Some k) w))
                                       -> (forall a. k a -> v a -> EventWriterT t w m (v' a))
                                       -> DMap k v
                                       -> Event t (p k v)
                                       -> EventWriterT t w m (DMap k v', Event t (p k v'))
sequenceDMapWithAdjustEventWriterTWith base mapPatch weakenPatchWith patchNewElements mergePatchIncremental f dm0 dm' = do
  let f' :: forall a. k a -> v a -> m (Compose ((,) (Seq (Event t w))) v' a)
      f' k v = Compose . swap <$> runStateT (unEventWriterT $ f k v) mempty
  (children0, children') <- base f' dm0 dm'
  let result0 = DMap.map (snd . getCompose) children0
      result' = fforCheap children' $ mapPatch $ snd . getCompose
      requests0 :: Map (Some k) (Event t w)
      requests0 = weakenDMapWith (mconcat . toList . fst . getCompose) children0
      requests' :: Event t (p' (Some k) (Event t w))
      requests' = fforCheap children' $ weakenPatchWith $ mconcat . toList . fst . getCompose
  childRequestMap :: Incremental t (p' (Some k) (Event t w)) <- holdIncremental requests0 requests'
  -- We add these two separately to take advantage of the free merge being done later.  The coincidence case must come first so that it has precedence if both fire simultaneously.  (Really, we should probably block the 'switch' whenever 'updated' fires, but switchPromptlyDyn has the same issue.)
  EventWriterT $ modify $ flip (|>) $ coincidence $ fforCheap requests' $ \p -> mconcat $ patchNewElements p --TODO: Create a mergeIncrementalPromptly, and use that to eliminate this 'coincidence'
  EventWriterT $ modify $ flip (|>) $ fforMaybeCheap (mergePatchIncremental childRequestMap) $ \m -> case toList m of
    [] -> Nothing
    h : t -> Just $ sconcat $ h :| t
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

instance MonadAtomicRef m => MonadAtomicRef (EventWriterT t w m) where
  atomicModifyRef r = lift . atomicModifyRef r

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (EventWriterT t w m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance (MonadQuery t q m, Monad m) => MonadQuery t q (EventWriterT t w m) where
  tellQueryIncremental = lift . tellQueryIncremental
  askQueryResult = lift askQueryResult
  queryIncremental = lift . queryIncremental
