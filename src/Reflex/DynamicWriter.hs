-- | This module defines 'MonadDynamicWriter' and 'DynamicWriterT', its standard
-- implementation.
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
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
import Data.Align
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Functor.Misc
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup
import Data.Some (Some)
import Data.These
import Reflex.Class
import Reflex.Host.Class
import qualified Reflex.Patch.MapWithMove as MapWithMove
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class
import Reflex.Requester.Class
import Reflex.TriggerEvent.Class

instance MonadTrans (DynamicWriterT t w) where
  lift = DynamicWriterT . lift

mapIncrementalMapValues :: (Reflex t, Patch (p v), Patch (p v'), PatchTarget (p v) ~ f v, PatchTarget (p v') ~ f v', Functor p, Functor f) => (v -> v') -> Incremental t (p v) -> Incremental t (p v')
mapIncrementalMapValues f = unsafeMapIncremental (fmap f) (fmap f)

mergeDynIncremental :: (Reflex t, Ord k) => Incremental t (PatchMap k (Dynamic t v)) -> Incremental t (PatchMap k v)
mergeDynIncremental a = unsafeBuildIncremental (mapM (sample . current) =<< sample (currentIncremental a)) $ addedAndRemovedValues <> changedValues
  where changedValues = fmap (PatchMap . fmap Just) $ mergeMapIncremental $ mapIncrementalMapValues updated a
        addedAndRemovedValues = flip pushAlways (updatedIncremental a) $ \(PatchMap m) -> PatchMap <$> mapM (mapM (sample . current)) m

mergeDynIncrementalWithMove :: forall t k v. (Reflex t, Ord k) => Incremental t (PatchMapWithMove k (Dynamic t v)) -> Incremental t (PatchMapWithMove k v)
mergeDynIncrementalWithMove a = unsafeBuildIncremental (mapM (sample . current) =<< sample (currentIncremental a)) $ alignWith f addedAndRemovedValues changedValues
  where changedValues = mergeMapIncrementalWithMove $ mapIncrementalMapValues updated a
        addedAndRemovedValues = flip pushAlways (updatedIncremental a) $ fmap unsafePatchMapWithMove . mapM (mapM (sample . current)) . unPatchMapWithMove
        f :: These (PatchMapWithMove k v) (Map k v) -> PatchMapWithMove k v
        f x = unsafePatchMapWithMove $
          let (p, changed) = case x of
                This p_ -> (unPatchMapWithMove p_, mempty)
                That c -> (mempty, c)
                These p_ c -> (unPatchMapWithMove p_, c)
              (pWithNewVals, noLongerMoved) = flip runState [] $ forM p $ MapWithMove.nodeInfoMapMFrom $ \case
                MapWithMove.From_Insert v -> return $ MapWithMove.From_Insert v
                MapWithMove.From_Delete -> return $ MapWithMove.From_Delete
                MapWithMove.From_Move k -> case Map.lookup k changed of
                  Nothing -> return $ MapWithMove.From_Move k
                  Just v -> do
                    modify (k:)
                    return $ MapWithMove.From_Insert v
              noLongerMovedMap = Map.fromList $ fmap (\k -> (k, ())) noLongerMoved
          in Map.differenceWith (\e _ -> Just $ MapWithMove.nodeInfoSetTo Nothing e) pWithNewVals noLongerMovedMap --TODO: Check if any in the second map are not covered?

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

newtype DynamicWriterTLoweredResult t w v a = DynamicWriterTLoweredResult (v a, Dynamic t w)

-- | When the execution of a 'DynamicWriterT' action is adjusted using
-- 'MonadAdjust', the 'Dynamic' output of that action will also be updated to
-- match.
instance (MonadAdjust t m, MonadFix m, Monoid w, MonadHold t m, Reflex t) => MonadAdjust t (DynamicWriterT t w m) where
  runWithReplace a0 a' = do
    (result0, result') <- lift $ runWithReplace (runDynamicWriterT a0) $ runDynamicWriterT <$> a'
    tellDyn . join =<< holdDyn (snd result0) (snd <$> result')
    return (fst result0, fst <$> result')
  traverseDMapWithKeyWithAdjust = traverseDMapWithKeyWithAdjustImpl traverseDMapWithKeyWithAdjust mapPatchDMap weakenPatchDMapWith mergeDynIncremental
  traverseDMapWithKeyWithAdjustWithMove = traverseDMapWithKeyWithAdjustImpl traverseDMapWithKeyWithAdjustWithMove mapPatchDMapWithMove weakenPatchDMapWithMoveWith mergeDynIncrementalWithMove

traverseDMapWithKeyWithAdjustImpl :: forall t w k v' p p' v m. (PatchTarget (p' (Some k) (Dynamic t w)) ~ Map (Some k) (Dynamic t w), PatchTarget (p' (Some k) w) ~ Map (Some k) w, Patch (p' (Some k) w), Patch (p' (Some k) (Dynamic t w)), MonadFix m, Monoid w, Reflex t, MonadHold t m)
  => (   (forall a. k a -> v a -> m (DynamicWriterTLoweredResult t w v' a))
      -> DMap k v
      -> Event t (p k v)
      -> m (DMap k (DynamicWriterTLoweredResult t w v'), Event t (p k (DynamicWriterTLoweredResult t w v')))
     )
  -> ((forall a. DynamicWriterTLoweredResult t w v' a -> v' a) -> p k (DynamicWriterTLoweredResult t w v') -> p k v')
  -> ((forall a. DynamicWriterTLoweredResult t w v' a -> Dynamic t w) -> p k (DynamicWriterTLoweredResult t w v') -> p' (Some k) (Dynamic t w))
  -> (Incremental t (p' (Some k) (Dynamic t w)) -> Incremental t (p' (Some k) w))
  -> (forall a. k a -> v a -> DynamicWriterT t w m (v' a))
  -> DMap k v
  -> Event t (p k v)
  -> DynamicWriterT t w m (DMap k v', Event t (p k v'))
traverseDMapWithKeyWithAdjustImpl base mapPatch weakenPatchWith mergeMyDynIncremental f (dm0 :: DMap k v) dm' = do
  (result0, result') <- lift $ base (\k v -> fmap DynamicWriterTLoweredResult $ runDynamicWriterT $ f k v) dm0 dm'
  let getValue (DynamicWriterTLoweredResult (v, _)) = v
      getWritten (DynamicWriterTLoweredResult (_, w)) = w
      liftedResult0 = DMap.map getValue result0
      liftedResult' = ffor result' $ mapPatch getValue
      liftedWritten0 :: Map (Some k) (Dynamic t w)
      liftedWritten0 = weakenDMapWith getWritten result0
      liftedWritten' = ffor result' $ weakenPatchWith getWritten
  --TODO: We should be able to improve the performance here by incrementally updating the mconcat of the merged Dynamics
  i <- holdIncremental liftedWritten0 liftedWritten'
  tellDyn $ fmap (mconcat . Map.elems) $ incrementalToDynamic $ mergeMyDynIncremental i
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
  requesting = lift . requesting
  requesting_ = lift . requesting_
