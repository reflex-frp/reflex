{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
module Reflex.DynamicWriter where

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
import Reflex.Dynamic
import Reflex.Host.Class
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class

instance MonadTrans (DynamicWriterT t w) where
  lift = DynamicWriterT . lift

mconcatIncremental :: (Reflex t, MonadHold t m, MonadFix m, Monoid v, Ord k) => Map k v -> Event t (PatchMap k v) -> m (Dynamic t v)
mconcatIncremental m0 e = do
  d <- foldDynMaybe apply m0 e
  return $ fmap (mconcat . Map.elems) d

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

holdIncrementalReplaceableMap :: forall t k v m. (MonadFix m, MonadHold t m, Reflex t, Ord k) => Map k (Replaceable t v) -> Event t (PatchMap k (Replaceable t v)) -> m (Incremental t (PatchMap k v))
holdIncrementalReplaceableMap m0 m' = do
  rec vals <- holdIncremental m0 $ m' <> valChanges
      let valChanges = fmap PatchMap $ mergeIncrementalMap $ mapIncrementalMapValues _replaceable_modify vals
  return $ mapIncrementalMapValues _replaceable_value vals

mergeDynIncremental :: (Reflex t, Ord k) => Incremental t (PatchMap k (Dynamic t v)) -> Incremental t (PatchMap k v)
mergeDynIncremental a = unsafeBuildIncremental (mapM (sample . current) =<< sample (currentIncremental a)) $ addedAndRemovedValues <> changedValues
  where changedValues = fmap (PatchMap . fmap Just) $ mergeIncrementalMap $ mapIncrementalMapValues updated a
        addedAndRemovedValues = flip pushAlways (updatedIncremental a) $ \(PatchMap m) -> PatchMap <$> mapM (mapM (sample . current)) m

data Replaceable t w = Replaceable
  { _replaceable_value :: w
  , _replaceable_modify :: Event t (Maybe (Replaceable t w)) -- "Please delete" is represented with an event of Nothing; all subsequent events will be ignored
  }

type DynamicWriterAccumulator t w = [Dynamic t w]

newtype DynamicWriterT t w m a = DynamicWriterT { unDynamicWriterT :: StateT (DynamicWriterAccumulator t w) m a } deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadHold t, MonadSample t, MonadAsyncException, MonadException) -- The list is kept in reverse order

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

runDynamicWriterTInternal :: DynamicWriterT t w m a -> m (a, DynamicWriterAccumulator t w)
runDynamicWriterTInternal (DynamicWriterT a) = runStateT a []

mconcatIncrementalReplaceableDynMap :: forall m t k v.
                                       (MonadFix m, MonadHold t m, Reflex t, Monoid v, Ord k)
                                    => Map k (Replaceable t (Dynamic t v))
                                    -> Event t (PatchMap k (Replaceable t (Dynamic t v)))
                                    -> Event t ()
                                    -> m (Replaceable t (Dynamic t v))
mconcatIncrementalReplaceableDynMap m0 m' additionsCeased = do
  rec vals <- holdIncremental m0 $ m' <> valChanges
      let valChanges = fmap PatchMap $ mergeIncrementalMap $ mapIncrementalMapValues _replaceable_modify vals
  let i = mapIncrementalMapValues _replaceable_value vals
  noMoreAdditions <- holdDyn False $ True <$ additionsCeased
  let replaceSelf nma x = do
        guard nma
        case Map.toList x of
          [] -> return Nothing -- Delete self
          [(_, n)] -> return $ Just n --TODO: What if this one is also replacing itself simultaneously?
          _ -> mzero
      result = fmap (mconcat . Map.elems) $ incrementalToDynamic $ mergeDynIncremental i
  return $ Replaceable result $ fmapMaybe id $ updated $ zipDynWith replaceSelf noMoreAdditions $ incrementalToDynamic vals

runDynamicWriterT :: (MonadFix m, Reflex t, Monoid w) => DynamicWriterT t w m a -> m (a, Dynamic t w)
runDynamicWriterT (DynamicWriterT a) = do
  (result, ws) <- runStateT a []
  return (result, mconcat $ reverse ws)

class Monad m => MonadDynamicWriter t w m | m -> t w where
  tellDyn :: Dynamic t w -> m ()

instance (Monad m, Reflex t) => MonadDynamicWriter t w (DynamicWriterT t w m) where
  tellDyn w = DynamicWriterT $ modify (w :)

instance MonadReader r m => MonadReader r (DynamicWriterT t w m) where
  ask = lift ask
  local f (DynamicWriterT a) = DynamicWriterT $ mapStateT (local f) a
  reader = lift . reader

liftDynamicWriterTThroughSync :: Monad m' => (m (a, DynamicWriterAccumulator t w) -> m' (b, (a, DynamicWriterAccumulator t w))) -> DynamicWriterT t w m a -> DynamicWriterT t w m' (b, a)
liftDynamicWriterTThroughSync f (DynamicWriterT child) = DynamicWriterT $ do
    s <- get
    (b, (a, newS)) <- lift $ f $ runStateT child s
    put newS
    return (b, a)

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

newtype DynamicWriterTLoweredResult t w v = DynamicWriterTLoweredResult { unDynamicWriterTLoweredResult :: (v, Dynamic t w) }

instance (MonadAdjust t m, MonadFix m, Monoid w, MonadHold t m, Reflex t) => MonadAdjust t (DynamicWriterT t w m) where
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
        liftedWritten0 = Map.fromDistinctAscList $ fmap (\(WrapArg k :=> Identity r) -> (Some.This k, getWritten r)) $ DMap.toList result0
        liftedWritten' = ffor result' $ \(PatchDMap p) -> PatchMap $
          Map.fromDistinctAscList $ fmap (\(WrapArg k :=> ComposeMaybe mr) -> (Some.This k, fmap (getWritten . runIdentity) mr)) $ DMap.toList p
    --TODO: We should be able to improve the performance here by incrementally updating the mconcat of the merged Dynamics
    i <- holdIncremental liftedWritten0 liftedWritten'
    tellDyn $ fmap (mconcat . Map.elems) $ incrementalToDynamic $ mergeDynIncremental i
    return (liftedResult0, liftedResult')

withDynamicWriterT :: (Monoid w, Reflex t, MonadHold t m, MonadFix m)
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

instance MonadRequest t m => MonadRequest t (DynamicWriterT t w m) where
  type Request (DynamicWriterT t w m) = Request m
  type Response (DynamicWriterT t w m) = Response m
  withRequesting f = DynamicWriterT $ withRequesting $ unDynamicWriterT . f
