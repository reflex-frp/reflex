{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines 'PostBuildT', the standard implementation of
-- 'PostBuild'.
module Reflex.PostBuild.Base
  ( PostBuildT (..)
  , runPostBuildT

    -- * Internal
  , mapIntMapWithAdjustImpl
  , mapDMapWithAdjustImpl
  ) where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Exception
import Control.Monad.Fix
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Ref
import qualified Control.Monad.Trans.Control as TransControl
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Functor.Compose
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Semigroup as S

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
import Control.Monad.Identity
#endif

import Reflex.Adjustable.Class
import Reflex.Class
import Reflex.Host.Class
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class
import Reflex.TriggerEvent.Class

-- | Provides a basic implementation of 'PostBuild'.
newtype PostBuildT t m a = PostBuildT {unPostBuildT :: ReaderT (Event t ()) m a}
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadAsyncException
    , MonadCatch
    , MonadException
    , MonadFix
    , MonadIO
    , MonadMask
    , MonadThrow
    , MonadTrans
    )

-- | Run a 'PostBuildT' action.  An 'Event' should be provided that fires
-- immediately after the action is finished running; no other 'Event's should
-- fire first.
{-# INLINEABLE runPostBuildT #-}
runPostBuildT :: PostBuildT t m a -> Event t () -> m a
runPostBuildT (PostBuildT a) = runReaderT a

-- TODO: Monoid and Semigroup can likely be derived once ReaderT has them.
instance (Monoid a, Applicative m) => Monoid (PostBuildT t m a) where
  mempty = pure mempty

instance (S.Semigroup a, Applicative m) => S.Semigroup (PostBuildT t m a) where
  (<>) = liftA2 (S.<>)

instance PrimMonad m => PrimMonad (PostBuildT x m) where
  type PrimState (PostBuildT x m) = PrimState m
  primitive = lift . primitive

instance (Reflex t, Monad m) => PostBuild t (PostBuildT t m) where
  {-# INLINEABLE getPostBuild #-}
  getPostBuild = PostBuildT ask

instance MonadSample t m => MonadSample t (PostBuildT t m) where
  {-# INLINEABLE sample #-}
  sample = lift . sample

instance MonadHold t m => MonadHold t (PostBuildT t m) where
  {-# INLINEABLE hold #-}
  hold v0 = lift . hold v0
  {-# INLINEABLE holdDyn #-}
  holdDyn v0 = lift . holdDyn v0
  {-# INLINEABLE holdIncremental #-}
  holdIncremental v0 = lift . holdIncremental v0
  {-# INLINEABLE buildDynamic #-}
  buildDynamic a0 = lift . buildDynamic a0
  {-# INLINEABLE headE #-}
  headE = lift . headE
  {-# INLINEABLE now #-}
  now = lift now

instance PerformEvent t m => PerformEvent t (PostBuildT t m) where
  type Performable (PostBuildT t m) = Performable m
  {-# INLINEABLE performEvent_ #-}
  performEvent_ = lift . performEvent_
  {-# INLINEABLE performEvent #-}
  performEvent = lift . performEvent

instance (ReflexHost t, MonadReflexCreateTrigger t m) => MonadReflexCreateTrigger t (PostBuildT t m) where
  {-# INLINEABLE newEventWithTrigger #-}
  newEventWithTrigger = PostBuildT . lift . newEventWithTrigger
  {-# INLINEABLE newFanEventWithTrigger #-}
  newFanEventWithTrigger f = PostBuildT $ lift $ newFanEventWithTrigger f

instance TriggerEvent t m => TriggerEvent t (PostBuildT t m) where
  {-# INLINEABLE newTriggerEvent #-}
  newTriggerEvent = lift newTriggerEvent
  {-# INLINEABLE newTriggerEventWithOnComplete #-}
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete

instance MonadRef m => MonadRef (PostBuildT t m) where
  type Ref (PostBuildT t m) = Ref m
  {-# INLINEABLE newRef #-}
  newRef = lift . newRef
  {-# INLINEABLE readRef #-}
  readRef = lift . readRef
  {-# INLINEABLE writeRef #-}
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (PostBuildT t m) where
  {-# INLINEABLE atomicModifyRef #-}
  atomicModifyRef r = lift . atomicModifyRef r

instance (Reflex t, MonadHold t m, MonadFix m, Adjustable t m, PerformEvent t m) => Adjustable t (PostBuildT t m) where
  runWithReplace a0 a' = do
    postBuild <- getPostBuild
    lift $ do
      rec result@(_, result') <- runWithReplace (runPostBuildT a0 postBuild) $ fmap (\v -> runPostBuildT v =<< headE voidResult') a'
          let voidResult' = fmapCheap (const ()) result'
      return result
  {-# INLINEABLE traverseIntMapWithKeyWithAdjust #-}
  traverseIntMapWithKeyWithAdjust = mapIntMapWithAdjustImpl traverseIntMapWithKeyWithAdjust
  {-# INLINEABLE traverseDMapWithKeyWithAdjust #-}
  traverseDMapWithKeyWithAdjust = mapDMapWithAdjustImpl traverseDMapWithKeyWithAdjust mapPatchDMap
  traverseDMapWithKeyWithAdjustWithMove = mapDMapWithAdjustImpl traverseDMapWithKeyWithAdjustWithMove mapPatchDMapWithMove

{-# INLINEABLE mapIntMapWithAdjustImpl #-}
mapIntMapWithAdjustImpl
  :: forall t m v v' p
   . (Reflex t, MonadFix m, MonadHold t m, Functor p)
  => ( (IntMap.Key -> (Event t (), v) -> m v')
       -> IntMap (Event t (), v)
       -> Event t (p (Event t (), v))
       -> m (IntMap v', Event t (p v'))
     )
  -> (IntMap.Key -> v -> PostBuildT t m v')
  -> IntMap v
  -> Event t (p v)
  -> PostBuildT t m (IntMap v', Event t (p v'))
mapIntMapWithAdjustImpl base f dm0 dm' = do
  postBuild <- getPostBuild
  let loweredDm0 = fmap (postBuild,) dm0
      f' :: IntMap.Key -> (Event t (), v) -> m v'
      f' k (e, v) = do
        runPostBuildT (f k v) e
  lift $ do
    rec (result0, result') <- base f' loweredDm0 loweredDm'
        cohortDone <- numberOccurrencesFrom_ 1 result'
        numberedDm' <- numberOccurrencesFrom 1 dm'
        let postBuild' = fanInt $ fmapCheap (`IntMap.singleton` ()) cohortDone
            loweredDm' = flip pushAlways numberedDm' $ \(n, p) -> do
              return $ fmap (selectInt postBuild' n,) p
    return (result0, result')

{-# INLINEABLE mapDMapWithAdjustImpl #-}
mapDMapWithAdjustImpl
  :: forall t m k v v' p
   . (Reflex t, MonadFix m, MonadHold t m)
  => ( (forall a. k a -> Compose ((,) (Bool, Event t ())) v a -> m (v' a))
       -> DMap k (Compose ((,) (Bool, Event t ())) v)
       -> Event t (p k (Compose ((,) (Bool, Event t ())) v))
       -> m (DMap k v', Event t (p k v'))
     )
  -> ((forall a. v a -> Compose ((,) (Bool, Event t ())) v a) -> p k v -> p k (Compose ((,) (Bool, Event t ())) v))
  -> (forall a. k a -> v a -> PostBuildT t m (v' a))
  -> DMap k v
  -> Event t (p k v)
  -> PostBuildT t m (DMap k v', Event t (p k v'))
mapDMapWithAdjustImpl base mapPatch f dm0 dm' = do
  postBuild <- getPostBuild
  let loweredDm0 = DMap.map (Compose . (,) (False, postBuild)) dm0
      f' :: forall a. k a -> Compose ((,) (Bool, Event t ())) v a -> m (v' a)
      f' k (Compose ((shouldHeadE, e), v)) = do
        eOnce <-
          if shouldHeadE
            then headE e -- TODO: Avoid doing this headE so many times; once per loweredDm' firing ought to be OK, but it's not totally trivial to do because result' might be firing at the same time, and we don't want *that* to be the postBuild occurrence
            else return e
        runPostBuildT (f k v) eOnce
  lift $ do
    rec (result0, result') <- base f' loweredDm0 loweredDm'
        let voidResult' = fmapCheap (const ()) result'
        let loweredDm' = ffor dm' $ mapPatch (Compose . (,) (True, voidResult'))
    return (result0, result')

--------------------------------------------------------------------------------
-- Deprecated functionality
--------------------------------------------------------------------------------

-- | Deprecated
instance TransControl.MonadTransControl (PostBuildT t) where
  type StT (PostBuildT t) a = TransControl.StT (ReaderT (Event t ())) a
  {-# INLINEABLE liftWith #-}
  liftWith = TransControl.defaultLiftWith PostBuildT unPostBuildT
  {-# INLINEABLE restoreT #-}
  restoreT = TransControl.defaultRestoreT PostBuildT
