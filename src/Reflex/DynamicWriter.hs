-- | This module defines 'MonadDynamicWriter' and 'DynamicWriterT', its standard
-- implementation.
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
module Reflex.DynamicWriter
  ( MonadDynamicWriter (..)
  , DynamicWriterT (..)
  , runDynamicWriterT
  , withDynamicWriterT
  ) where

import Control.Monad.Exception
import Control.Monad.Identity
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.State.Strict
import Data.Dependent.Map (DMap, DSum (..))
import qualified Data.Dependent.Map as DMap
import Data.Functor.Misc
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup
import Data.Some (Some)
import qualified Data.Some as Some
import Reflex.Class
import Reflex.Host.Class
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class
import Reflex.Requester.Class
import Reflex.TriggerEvent.Class

instance MonadTrans (DynamicWriterT t w) where
  lift = DynamicWriterT . lift

mapIncrementalMapValuesWithKey :: (Reflex t, Ord k) => (k -> v -> v') -> Incremental t (PatchMap k v) -> Incremental t (PatchMap k v')
mapIncrementalMapValuesWithKey f = unsafeMapIncremental (Map.mapWithKey f) $ \(PatchMap m) -> PatchMap $ Map.mapWithKey (\k mv -> fmap (f k) mv) m

mapIncrementalMapValues :: (Reflex t, Ord k) => (v -> v') -> Incremental t (PatchMap k v) -> Incremental t (PatchMap k v')
mapIncrementalMapValues f = mapIncrementalMapValuesWithKey $ const f

unsafeMapIncremental :: (Reflex t, Patch p, Patch p') => (PatchTarget p -> PatchTarget p') -> (p -> p') -> Incremental t p -> Incremental t p'
unsafeMapIncremental f g a = unsafeBuildIncremental (fmap f $ sample $ currentIncremental a) $ g <$> updatedIncremental a

incrementalExtractFunctorDMap :: (Reflex t, Ord k) => Incremental t (PatchMap k (f v)) -> Incremental t (PatchDMap (Const2 k v) f)
incrementalExtractFunctorDMap = unsafeMapIncremental mapWithFunctorToDMap $ \(PatchMap m) -> PatchDMap $ mapWithFunctorToDMap $ fmap ComposeMaybe m

mergeIncrementalMap :: (Reflex t, Ord k) => Incremental t (PatchMap k (Event t v)) -> Event t (Map k v)
mergeIncrementalMap = fmap dmapToMap . mergeIncremental . incrementalExtractFunctorDMap

mergeDynIncremental :: (Reflex t, Ord k) => Incremental t (PatchMap k (Dynamic t v)) -> Incremental t (PatchMap k v)
mergeDynIncremental a = unsafeBuildIncremental (mapM (sample . current) =<< sample (currentIncremental a)) $ addedAndRemovedValues <> changedValues
  where changedValues = fmap (PatchMap . fmap Just) $ mergeIncrementalMap $ mapIncrementalMapValues updated a
        addedAndRemovedValues = flip pushAlways (updatedIncremental a) $ \(PatchMap m) -> PatchMap <$> mapM (mapM (sample . current)) m

-- | A basic implementation of 'MonadDynamicWriter'.
newtype DynamicWriterT t w m a = DynamicWriterT { unDynamicWriterT :: StateT [Dynamic t w] m a } deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadHold t, MonadSample t, MonadAsyncException, MonadException) -- The list is kept in reverse order

instance MonadRef m => MonadRef (DynamicWriterT t w m) where
  type Ref (DynamicWriterT t w m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (DynamicWriterT t w m) where
  atomicModifyRef r = lift . atomicModifyRef r

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (DynamicWriterT t w m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

-- | Run a 'DynamicWriterT' action.  The dynamic writer output will be provided
-- along with the result of the action.
runDynamicWriterT :: (MonadFix m, Reflex t, Monoid w) => DynamicWriterT t w m a -> m (a, Dynamic t w)
runDynamicWriterT (DynamicWriterT a) = do
  (result, ws) <- runStateT a []
  return (result, mconcat $ reverse ws)

-- | 'MonadDynamicWriter' efficiently collects 'Dynamic' values using 'tellDyn'
-- and combines them monoidally to provide a 'Dynamic' result.
class (Monad m, Monoid w) => MonadDynamicWriter t w m | m -> t w where
  tellDyn :: Dynamic t w -> m ()

instance (Monad m, Monoid w, Reflex t) => MonadDynamicWriter t w (DynamicWriterT t w m) where
  tellDyn w = DynamicWriterT $ modify (w :)

instance MonadReader r m => MonadReader r (DynamicWriterT t w m) where
  ask = lift ask
  local f (DynamicWriterT a) = DynamicWriterT $ mapStateT (local f) a
  reader = lift . reader

instance PerformEvent t m => PerformEvent t (DynamicWriterT t w m) where
  type Performable (DynamicWriterT t w m) = Performable m
  performEvent_ = lift . performEvent_
  performEvent = lift . performEvent

instance TriggerEvent t m => TriggerEvent t (DynamicWriterT t w m) where
  newTriggerEvent = lift newTriggerEvent
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete

instance PostBuild t m => PostBuild t (DynamicWriterT t w m) where
  getPostBuild = lift getPostBuild

instance MonadDynamicWriter t w m => MonadDynamicWriter t w (ReaderT r m) where
  tellDyn = lift . tellDyn

instance MonadState s m => MonadState s (DynamicWriterT t w m) where
  get = lift get
  put = lift . put

newtype DynamicWriterTLoweredResult t w v = DynamicWriterTLoweredResult (v, Dynamic t w)

-- | When the execution of a 'DynamicWriterT' action is adjusted using
-- 'MonadAdjust', the 'Dynamic' output of that action will also be updated to
-- match.
instance (MonadAdjust t m, MonadFix m, Monoid w, MonadHold t m, Reflex t) => MonadAdjust t (DynamicWriterT t w m) where
  runWithReplace a0 a' = do
    (result0, result') <- lift $ runWithReplace (runDynamicWriterT a0) $ runDynamicWriterT <$> a'
    tellDyn . join =<< holdDyn (snd result0) (snd <$> result')
    return (fst result0, fst <$> result')
  sequenceDMapWithAdjust (dm0 :: DMap k (DynamicWriterT t w m)) dm' = do
    let loweredDm0 = mapKeyValuePairsMonotonic (\(k :=> v) -> WrapArg k :=> fmap DynamicWriterTLoweredResult (runDynamicWriterT v)) dm0
    let loweredDm' = ffor dm' $ \(PatchDMap p) -> PatchDMap $
          mapKeyValuePairsMonotonic (\(k :=> ComposeMaybe mv) -> WrapArg k :=> ComposeMaybe (fmap (fmap DynamicWriterTLoweredResult . runDynamicWriterT) mv)) p
    (result0, result') <- lift $ sequenceDMapWithAdjust loweredDm0 loweredDm'
    let getValue (DynamicWriterTLoweredResult (v, _)) = v
        getWritten (DynamicWriterTLoweredResult (_, w)) = w
        liftedResult0 = mapKeyValuePairsMonotonic (\(WrapArg k :=> Identity r) -> k :=> Identity (getValue r)) result0
        liftedResult' = ffor result' $ \(PatchDMap p) -> PatchDMap $
          mapKeyValuePairsMonotonic (\(WrapArg k :=> ComposeMaybe mr) -> k :=> ComposeMaybe (fmap (Identity . getValue . runIdentity) mr)) p
        liftedWritten0 :: Map (Some k) (Dynamic t w)
        liftedWritten0 = Map.fromDistinctAscList $ (\(WrapArg k :=> Identity r) -> (Some.This k, getWritten r)) <$> DMap.toList result0
        liftedWritten' = ffor result' $ \(PatchDMap p) -> PatchMap $
          Map.fromDistinctAscList $ (\(WrapArg k :=> ComposeMaybe mr) -> (Some.This k, fmap (getWritten . runIdentity) mr)) <$> DMap.toList p
    --TODO: We should be able to improve the performance here by incrementally updating the mconcat of the merged Dynamics
    i <- holdIncremental liftedWritten0 liftedWritten'
    tellDyn $ fmap (mconcat . Map.elems) $ incrementalToDynamic $ mergeDynIncremental i
    return (liftedResult0, liftedResult')

-- | Map a function over the output of a 'DynamicWriterT'.
withDynamicWriterT :: (Monoid w, Monoid w', Reflex t, MonadHold t m, MonadFix m)
                   => (w -> w')
                   -> DynamicWriterT t w m a
                   -> DynamicWriterT t w' m a
withDynamicWriterT f dw = do
  (r, d) <- lift $ do
    (r, d) <- runDynamicWriterT dw
    let d' = fmap f d
    return (r, d')
  tellDyn d
  return r

instance Requester t m => Requester t (DynamicWriterT t w m) where
  type Request (DynamicWriterT t w m) = Request m
  type Response (DynamicWriterT t w m) = Response m
  withRequesting f = DynamicWriterT $ withRequesting $ unDynamicWriterT . f
