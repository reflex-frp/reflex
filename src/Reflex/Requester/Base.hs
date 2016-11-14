-- | This module provides 'RequesterT', the standard implementation of
-- 'Requester'.
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
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
module Reflex.Requester.Base
  ( RequesterT (..)
  , runRequesterT
  , runWithReplaceRequesterTWith
  , sequenceDMapWithAdjustRequesterTWith
  ) where

import Reflex.Class
import Reflex.Host.Class
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class
import Reflex.Requester.Class
import Reflex.TriggerEvent.Class

import Control.Monad.Exception
import Control.Monad.Identity
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.State.Strict
import Data.Dependent.Map (DMap, DSum (..))
import qualified Data.Dependent.Map as DMap
import Data.Functor.Misc
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Data.Some (Some)
import qualified Data.Some as Some
import Data.Tuple
import Data.Unique.Tag

-- | A basic implementation of 'Requester'.
newtype RequesterT t request response m a = RequesterT { unRequesterT :: StateT [Event t (DMap (Tag (PrimState m)) request)] (ReaderT (EventSelector t (WrapArg response (Tag (PrimState m)))) m) a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadException)

-- | Run a 'RequesterT' action.  The resulting 'Event' will fire whenever
-- requests are made, and responses should be provided in the input 'Event'.
-- The 'Tag' keys will be used to return the responses to the same place the
-- requests were issued.
runRequesterT :: (Reflex t, Monad m)
              => RequesterT t request response m a
              -> Event t (DMap (Tag (PrimState m)) response)
              -> m (a, Event t (DMap (Tag (PrimState m)) request))
runRequesterT (RequesterT a) responses = do
  (result, requests) <- runReaderT (runStateT a mempty) $ fan $ mapKeyValuePairsMonotonic (\(t :=> e) -> WrapArg t :=> Identity e) <$> responses
  return (result, mergeWith DMap.union requests)

instance (Reflex t, PrimMonad m) => Requester t (RequesterT t request response m) where
  type Request (RequesterT t request response m) = request
  type Response (RequesterT t request response m) = response
  withRequesting a = do
    t <- lift newTag
    s <- RequesterT ask
    (req, result) <- a $ select s $ WrapArg t
    RequesterT $ modify $ (:) $ DMap.singleton t <$> req
    return result

instance MonadTrans (RequesterT t request response) where
  lift = RequesterT . lift . lift

instance MonadSample t m => MonadSample t (RequesterT t request response m) where
  sample = lift . sample

instance MonadHold t m => MonadHold t (RequesterT t request response m) where
  hold v0 = lift . hold v0
  holdDyn v0 = lift . holdDyn v0
  holdIncremental v0 = lift . holdIncremental v0

instance (Reflex t, MonadAdjust t m, MonadHold t m) => MonadAdjust t (RequesterT t request response m) where
  runWithReplace = runWithReplaceRequesterTWith $ \dm0 dm' -> lift $ runWithReplace dm0 dm'
  sequenceDMapWithAdjust = sequenceDMapWithAdjustRequesterTWith $ \dm0 dm' -> lift $ sequenceDMapWithAdjust dm0 dm'

-- | Given a function like 'runWithReplace' for the underlying monad, implement
-- 'runWithReplace' for 'RequesterT'.  This is necessary when the underlying
-- monad doesn't have a 'MonadAdjust' instance or to override the default
-- 'MonadAdjust' behavior.
runWithReplaceRequesterTWith :: forall m t request response a b. (Reflex t, MonadHold t m)
                           => (forall a' b'.
                                  m a'
                               -> Event t (m b')
                               -> RequesterT t request response m (a', Event t b')
                              )
                           -> RequesterT t request response m a
                           -> Event t (RequesterT t request response m b)
                           -> RequesterT t request response m (a, Event t b)
runWithReplaceRequesterTWith f a0 a' = do
  response <- RequesterT ask
  let g :: RequesterT t request response m c -> m (c, [Event t (DMap (Tag (PrimState m)) request)])
      g (RequesterT r) = runReaderT (runStateT r mempty) response
  (result0, result') <- f (g a0) $ g <$> a'
  request <- holdDyn (fmap (mconcat . NonEmpty.toList) $ mergeList $ snd result0) $ fmap (mconcat . NonEmpty.toList) . mergeList . snd <$> result'
  -- We add these two separately to take advantage of the free merge being done later.  The coincidence case must come first so that it has precedence if both fire simultaneously.  (Really, we should probably block the 'switch' whenever 'updated' fires, but switchPromptlyDyn has the same issue.)
  RequesterT $ modify $ (:) $ coincidence $ updated request
  RequesterT $ modify $ (:) $ switch $ current request
  return (fst result0, fst <$> result')

-- | Like 'runWithReplaceRequesterTWith', but for 'sequenceDMapWithAdjust'.
sequenceDMapWithAdjustRequesterTWith :: (GCompare k, Reflex t, MonadHold t m)
                                   => (forall k'. GCompare k'
                                       => DMap k' m
                                       -> Event t (PatchDMap k' m)
                                       -> RequesterT t request response m (DMap k' Identity, Event t (PatchDMap k' Identity))
                                      )
                                   -> DMap k (RequesterT t request response m)
                                   -> Event t (PatchDMap k (RequesterT t request response m))
                                   -> RequesterT t request response m (DMap k Identity, Event t (PatchDMap k Identity))
sequenceDMapWithAdjustRequesterTWith f (dm0 :: DMap k (RequesterT t request response m)) dm' = do
  response <- RequesterT ask
  let inputTransform :: forall a. DMapTransform a k (WrapArg ((,) [Event t (DMap (Tag (PrimState m)) request)]) k) (RequesterT t request response m) m
      inputTransform = DMapTransform WrapArg (\(RequesterT v) -> swap <$> runReaderT (runStateT v mempty) response)
  (children0, children') <- f (mapKeysAndValuesMonotonic inputTransform dm0) $ mapPatchKeysAndValuesMonotonic inputTransform <$> dm'
  let result0 = mapKeyValuePairsMonotonic (\(WrapArg k :=> Identity (_, v)) -> k :=> Identity v) children0
      result' = ffor children' $ \(PatchDMap p) -> PatchDMap $
        mapKeyValuePairsMonotonic (\(WrapArg k :=> ComposeMaybe mv) -> k :=> ComposeMaybe (fmap (Identity . snd . runIdentity) mv)) p
      requests0 :: DMap (Const2 (Some k) (DMap (Tag (PrimState m)) request)) (Event t)
      requests0 = mapKeyValuePairsMonotonic (\(WrapArg k :=> Identity (r, _)) -> Const2 (Some.This k) :=> mergeWith DMap.union r) children0
      requests' :: Event t (PatchDMap (Const2 (Some k) (DMap (Tag (PrimState m)) request)) (Event t))
      requests' = ffor children' $ \(PatchDMap p) -> PatchDMap $
        mapKeyValuePairsMonotonic (\(WrapArg k :=> ComposeMaybe mv) -> Const2 (Some.This k) :=> ComposeMaybe (fmap (mergeWith DMap.union . fst . runIdentity) mv)) p
  childRequestMap <- holdIncremental requests0 requests'
  -- We add these two separately to take advantage of the free merge being done later.  The coincidence case must come first so that it has precedence if both fire simultaneously.  (Really, we should probably block the 'switch' whenever 'updated' fires, but switchPromptlyDyn has the same issue.)
  RequesterT $ modify $ (:) $ coincidence $ ffor requests' $ \(PatchDMap p) -> mergeWith DMap.union $ catMaybes $ ffor (DMap.toList p) $ \(Const2 _ :=> ComposeMaybe me) -> me
  RequesterT $ modify $ (:) $ ffor (mergeIncremental childRequestMap) $ \m ->
    mconcat $ (\(Const2 _ :=> Identity reqs) -> reqs) <$> DMap.toList m
  return (result0, result')

instance PerformEvent t m => PerformEvent t (RequesterT t request response m) where
  type Performable (RequesterT t request response m) = Performable m
  performEvent_ = lift . performEvent_
  performEvent = lift . performEvent

instance PostBuild t m => PostBuild t (RequesterT t request response m) where
  getPostBuild = lift getPostBuild

instance TriggerEvent t m => TriggerEvent t (RequesterT t request response m) where
  newTriggerEvent = lift newTriggerEvent
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete

instance MonadReader r m => MonadReader r (RequesterT t request response m) where
  ask = lift ask
  local f (RequesterT a) = RequesterT $ mapStateT (mapReaderT $ local f) a
  reader = lift . reader

instance MonadRef m => MonadRef (RequesterT t request response m) where
  type Ref (RequesterT t request response m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (RequesterT t request response m) where
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
