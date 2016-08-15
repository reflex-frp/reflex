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
module Reflex.DynamicWriter where

import Control.Monad.Exception
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.State.Strict
import Data.Dependent.Map (DMap)
import Data.Functor.Compose
import Data.Functor.Misc
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup
import Reflex.Class
import Reflex.Deletable.Class
import Reflex.Dynamic
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class
import Reflex.Host.Class

instance MonadTrans (DynamicWriterT t w) where
  lift = DynamicWriterT . lift

data PatchMap a where
  PatchMap :: Ord k => Map k (Maybe v) -> PatchMap (Map k v)

instance Patch PatchMap where
  apply (PatchMap p) old = Just $! insertions `Map.union` (old `Map.difference` deletions) --TODO: return Nothing sometimes --Note: the strict application here is critical to ensuring that incremental merges don't hold onto all their prerequisite events forever; can we make this more robust?
    where insertions = Map.mapMaybeWithKey (const id) p
          deletions = Map.mapMaybeWithKey (const nothingToJust) p
          nothingToJust = \case
            Nothing -> Just ()
            Just _ -> Nothing

instance Ord k => Semigroup (PatchMap (Map k v)) where
  PatchMap a <> PatchMap b = PatchMap $ a `mappend` b --TODO: Add a semigroup instance for Map
  -- PatchMap is idempotent, so stimes n is id for every n
#if MIN_VERSION_semigroups(0,17,0)
  stimes = stimesIdempotentMonoid
#else
  times1p n x = case compare n 0 of
    LT -> error "stimesIdempotentMonoid: negative multiplier"
    EQ -> mempty
    GT -> x
#endif

instance Ord k => Monoid (PatchMap (Map k v)) where
  mempty = PatchMap mempty
  mappend = (<>)

mconcatIncremental :: (Reflex t, MonadHold t m, MonadFix m, Monoid v) => Map k v -> Event t (PatchMap (Map k v)) -> m (Dynamic t v)
mconcatIncremental m0 e = do
  d <- foldDynMaybe apply m0 e
  return $ fmap (mconcat . Map.elems) d

mapIncrementalMapValuesWithKey :: Reflex t => (k -> v -> v') -> Incremental t PatchMap (Map k v) -> Incremental t PatchMap (Map k v')
mapIncrementalMapValuesWithKey f = unsafeMapIncremental (Map.mapWithKey f) $ \(PatchMap m) -> PatchMap $ Map.mapWithKey (\k mv -> fmap (f k) mv) m

mapIncrementalMapValues :: Reflex t => (v -> v') -> Incremental t PatchMap (Map k v) -> Incremental t PatchMap (Map k v')
mapIncrementalMapValues f = mapIncrementalMapValuesWithKey $ const f

unsafeMapIncremental :: (Reflex t, Patch p, Patch p') => (a -> a') -> (p a -> p' a') -> Incremental t p a -> Incremental t p' a'
unsafeMapIncremental f g a = unsafeBuildIncremental (fmap f $ sample $ currentIncremental a) $ g <$> updatedIncremental a

incrementalExtractFunctorDMap :: Reflex t => Incremental t PatchMap (Map k (f v)) -> Incremental t PatchDMap (DMap (Const2 k v) f)
incrementalExtractFunctorDMap = unsafeMapIncremental mapWithFunctorToDMap $ \(PatchMap m) -> PatchDMap $ mapWithFunctorToDMap $ fmap Compose m

mergeIncrementalMap :: (Reflex t, Ord k) => Incremental t PatchMap (Map k (Event t v)) -> Event t (Map k v)
mergeIncrementalMap = fmap dmapToMap . mergeIncremental . incrementalExtractFunctorDMap

holdIncrementalReplaceableMap :: forall t k v m. (MonadFix m, MonadHold t m, Reflex t, Ord k) => Map k (Replaceable t v) -> Event t (PatchMap (Map k (Replaceable t v))) -> m (Incremental t PatchMap (Map k v))
holdIncrementalReplaceableMap m0 m' = do
  rec vals <- holdIncremental m0 $ m' <> valChanges
      let valChanges = fmap PatchMap $ mergeIncrementalMap $ mapIncrementalMapValues _replaceable_modify vals
  return $ mapIncrementalMapValues _replaceable_value vals

mergeDynIncremental :: (Reflex t, Ord k) => Incremental t PatchMap (Map k (Dynamic t v)) -> Incremental t PatchMap (Map k v)
mergeDynIncremental a = unsafeBuildIncremental (mapM (sample . current) =<< sample (currentIncremental a)) $ addedAndRemovedValues <> changedValues
  where changedValues = fmap (PatchMap . fmap Just) $ mergeIncrementalMap $ mapIncrementalMapValues updated a
        addedAndRemovedValues = flip pushAlways (updatedIncremental a) $ \(PatchMap m) -> PatchMap <$> mapM (mapM (sample . current)) m

data Replaceable t w = Replaceable
  { _replaceable_value :: w
  , _replaceable_modify :: Event t (Maybe (Replaceable t w)) -- "Please delete" is represented with an event of Nothing; all subsequent events will be ignored
  }

type DynamicWriterAccumulator t w = [Replaceable t (Dynamic t w)]

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

mconcatIncrementalReplaceableDynMap :: forall m t k v. (MonadFix m, MonadHold t m, Reflex t, Monoid v, Ord k) => Map k (Replaceable t (Dynamic t v)) -> Event t (PatchMap (Map k (Replaceable t (Dynamic t v)))) -> Event t () -> m (Replaceable t (Dynamic t v))
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

runDynamicWriterT :: (MonadFix m, Reflex t, MonadHold t m, Monoid w) => DynamicWriterT t w m a -> m (a, Dynamic t w)
runDynamicWriterT (DynamicWriterT a) = do
  (result, ws) <- runStateT a []
  Replaceable w _ <- mconcatIncrementalReplaceableDynMap (Map.fromList $ zip [1 :: Int ..] $ reverse ws) never never
  return (result, w)

class Monad m => MonadDynamicWriter t w m | m -> t w where
  tellDyn :: Dynamic t w -> m ()

instance (Monad m, Reflex t) => MonadDynamicWriter t w (DynamicWriterT t w m) where
  tellDyn w = DynamicWriterT $ modify (Replaceable w never :)

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

instance (Deletable t m, Reflex t, Monoid w, MonadHold t m, MonadFix m) => Deletable t (DynamicWriterT t w m) where
  deletable delete child = do
    (result, output) <- lift $ deletable delete $ runDynamicWriterT child
    DynamicWriterT $ modify (Replaceable output (Nothing <$ delete) :)
    return result

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
