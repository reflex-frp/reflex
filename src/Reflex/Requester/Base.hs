-- | This module provides 'RequesterT', the standard implementation of
-- 'Requester'.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
module Reflex.Requester.Base
  ( RequesterT (..)
  , RequestItem (..)
  , ResponseItem (..)
  , runRequesterT'
  , traverseRequests
  , runWithReplaceRequesterTWith
  , traverseIntMapWithKeyWithAdjustRequesterTWith
  , traverseDMapWithKeyWithAdjustRequesterTWith
  ) where

import Reflex.Adjustable.Class
import Reflex.Class
import Reflex.EventWriter.Base
import Reflex.EventWriter.Class
import Reflex.Host.Class
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class
import Reflex.Requester.Class
import Reflex.TriggerEvent.Class

import Control.Applicative (liftA2)
import Control.Monad.Exception
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Ref
import Data.Dependent.Map (DMap)
import Data.Foldable
import Data.Functor.Compose
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map (Map)
import Data.Maybe
import qualified Data.Semigroup as S
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Some (Some)
import Data.Unique.Tag

-- | A basic implementation of 'Requester'.
newtype RequesterT t x (request :: * -> *) (response :: * -> *) m a = RequesterT { unRequesterT :: ReaderT (FanCell t x) (EventWriterT t (Seq (RequestItem t x request response)) m) a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadException
-- MonadAsyncException can't be derived on ghc-8.0.1; we use base-4.9.1 as a proxy for ghc-8.0.2
#if MIN_VERSION_base(4,9,1)
           , MonadAsyncException
#endif
           )

deriving instance MonadSample t m => MonadSample t (RequesterT t x request response m)
instance MonadHold t m => MonadHold t (RequesterT t x request response m)
deriving instance PostBuild t m => PostBuild t (RequesterT t x request response m)
deriving instance TriggerEvent t m => TriggerEvent t (RequesterT t x request response m)

-- TODO: Monoid and Semigroup can likely be derived once StateT has them.
instance (Monoid a, Monad m) => Monoid (RequesterT t x request response m a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance (S.Semigroup a, Monad m) => S.Semigroup (RequesterT t x request response m a) where
  (<>) = liftA2 (S.<>)


data RequestItem t x request response = forall a. RequestItem
  { _requestItem_trigger :: !(Maybe (CellTrigger t (response a) x))
  , _requestItem_value :: request a
  }

data ResponseItem t x response = forall a. ResponseItem
  { _responseItem_trigger :: !(CellTrigger t (response a) x)
  , _responseItem_value :: response a
  }

runRequesterT'
  :: forall t m request response a
  .  ( Reflex t
     , Monad m
     , MonadFix m
     , MonadHold t m
     )
  => (forall x. Event t (Seq (RequestItem t x request response)) -> RequesterT t x request response m (Event t (Seq (ResponseItem t x response)), a))
  -> m a
runRequesterT' a = withHoldFanCell $ \(c :: FanCell t x) -> do
  let update :: Seq (ResponseItem t x response) -> CellM t x ()
      update responses = withMonadCellM @t @x $
        forM_ responses $ \(ResponseItem t v) ->
        fireCellEvent t v
  rec ((responses, result :: a), requests) <- runEventWriterT $ runReaderT (unRequesterT (a requests)) c
  pure (fmapCheap update responses, result)

traverseRequests
  :: Applicative f
  => (forall a. request a -> f (response a))
  -> Seq (RequestItem t x request response)
  -> f (Seq (ResponseItem t x response))
traverseRequests f = fmap (Seq.fromList . catMaybes) . traverse f' . toList
  where f' (RequestItem mt req) = case mt of
          Nothing -> Nothing <$ f req
          Just t -> Just . ResponseItem t <$> f req

instance (Reflex t, Monad m, MonadMutate t m) => Requester t (RequesterT t x request response m) where
  type Request (RequesterT t x request response m) = request
  type Response (RequesterT t x request response m) = response
  requesting request = RequesterT $ do
    c <- ask
    (t, response) <- lift $ lift $ mutateFanCell c newCellEvent
    tellEvent $ fforCheap request $ \req -> Seq.singleton $ RequestItem
      { _requestItem_trigger = Just t
      , _requestItem_value = req
      }
    pure response
  requesting_ request = RequesterT $ do
    tellEvent $ fforCheap request $ \req -> Seq.singleton $ RequestItem
      { _requestItem_trigger = Nothing
      , _requestItem_value = req
      }

instance MonadTrans (RequesterT t x request response) where
  lift = RequesterT . lift . lift

instance PerformEvent t m => PerformEvent t (RequesterT t x request response m) where
  type Performable (RequesterT t x request response m) = Performable m
  performEvent_ = lift . performEvent_
  performEvent = lift . performEvent

instance MonadRef m => MonadRef (RequesterT t x request response m) where
  type Ref (RequesterT t x request response m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (RequesterT t x request response m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance MonadReader r m => MonadReader r (RequesterT t x request response m) where
  ask = lift ask
  local f (RequesterT a) = RequesterT $ mapReaderT (local f) a
  reader = lift . reader

instance (Reflex t, Adjustable t m, MonadHold t m, MonadFix m) => Adjustable t (RequesterT t x request response m) where
  runWithReplace = runWithReplaceRequesterTWith $ \dm0 dm' -> lift $ runWithReplace dm0 dm'
  traverseIntMapWithKeyWithAdjust = traverseIntMapWithKeyWithAdjustRequesterTWith (\f dm0 dm' -> lift $ traverseIntMapWithKeyWithAdjust f dm0 dm') mergeIntIncremental coincidencePatchIntMap
  {-# INLINABLE traverseDMapWithKeyWithAdjust #-}
  traverseDMapWithKeyWithAdjust = traverseDMapWithKeyWithAdjustRequesterTWith (\f dm0 dm' -> lift $ traverseDMapWithKeyWithAdjust f dm0 dm') mapPatchDMap weakenPatchDMapWith mergeMapIncremental coincidencePatchMap
  traverseDMapWithKeyWithAdjustWithMove = traverseDMapWithKeyWithAdjustRequesterTWith (\f dm0 dm' -> lift $ traverseDMapWithKeyWithAdjustWithMove f dm0 dm') mapPatchDMapWithMove weakenPatchDMapWithMoveWith mergeMapIncrementalWithMove coincidencePatchMapWithMove

{-# INLINABLE runWithReplaceRequesterTWith #-}
runWithReplaceRequesterTWith
  :: forall m t x request response a b
  .  ( Reflex t
     , MonadHold t m
     )
  => (forall a' b'. m a' -> Event t (m b') -> RequesterT t x request response m (a', Event t b'))
  -> RequesterT t x request response m a
  -> Event t (RequesterT t x request response m b)
  -> RequesterT t x request response m (a, Event t b)
runWithReplaceRequesterTWith f a0 a' = do
  env <- RequesterT ask
  let r :: forall a'. RequesterT t x request response m a' -> EventWriterT t (Seq (RequestItem t x request response)) m a'
      r (RequesterT a) = runReaderT a env
  RequesterT $ lift $ runWithReplaceEventWriterTWith (\x0 x' -> r $ f x0 x') (r a0) $ fmapCheap r a'

{-# INLINE traverseIntMapWithKeyWithAdjustRequesterTWith #-}
traverseIntMapWithKeyWithAdjustRequesterTWith
  :: forall t x request response m v v' p w
  .  ( Reflex t
     , MonadHold t m
     , Patch (p (Event t w))
     , PatchTarget (p (Event t w)) ~ IntMap (Event t w)
     , Patch (p w)
     , PatchTarget (p w) ~ IntMap w
     , Functor p
     , w ~ Seq (RequestItem t x request response)
     )
  => (forall v1 v2.
         (IntMap.Key -> v1 -> m v2)
      -> IntMap v1
      -> Event t (p v1)
      -> RequesterT t x request response m (IntMap v2, Event t (p v2))
     )
  -> (Incremental t (p (Event t w)) -> Event t (PatchTarget (p w)))
  -> (Event t (p (Event t w)) -> Event t (p w))
  -> (IntMap.Key -> v -> RequesterT t x request response m v')
  -> IntMap v
  -> Event t (p v)
  -> RequesterT t x request response m (IntMap v', Event t (p v'))
traverseIntMapWithKeyWithAdjustRequesterTWith base mergePatchIncremental coincidencePatch f dm0 dm' = do
  env <- RequesterT ask
  RequesterT $ lift $ sequenceIntMapWithAdjustEventWriterTWith
    (\f' x0 x' -> runReaderT (unRequesterT $ base f' x0 x') env)
    mergePatchIncremental
    coincidencePatch
    (\x0 x' -> runReaderT (unRequesterT $ f x0 x') env)
    dm0
    dm'

{-# INLINE traverseDMapWithKeyWithAdjustRequesterTWith #-}
traverseDMapWithKeyWithAdjustRequesterTWith
  :: forall k t x request response m v v' p p' w
  .  ( GCompare k
     , Reflex t
     , MonadHold t m
     , PatchTarget (p' (Some k) (Event t w)) ~ Map (Some k) (Event t w)
     , Patch (p' (Some k) (Event t w))
     , Patch (p' (Some k) w)
     , PatchTarget (p' (Some k) w) ~ Map (Some k) w
     , w ~ Seq (RequestItem t x request response)
     )
  => (forall k' v1 v2. GCompare k'
      => (forall a. k' a -> v1 a -> m (v2 a))
      -> DMap k' v1
      -> Event t (p k' v1)
      -> RequesterT t x request response m (DMap k' v2, Event t (p k' v2))
     )
  -> ((forall a. Compose ((,) (Event t w)) v' a -> v' a) -> p k (Compose ((,) (Event t w)) v') -> p k v')
  -> ((forall a. Compose ((,) (Event t w)) v' a -> Event t w) -> p k (Compose ((,) (Event t w)) v') -> p' (Some k) (Event t w))
  -> (Incremental t (p' (Some k) (Event t w)) -> Event t (PatchTarget (p' (Some k) w)))
  -> (Event t (p' (Some k) (Event t w)) -> Event t (p' (Some k) w))
  -> (forall a. k a -> v a -> RequesterT t x request response m (v' a))
  -> DMap k v
  -> Event t (p k v)
  -> RequesterT t x request response m (DMap k v', Event t (p k v'))
traverseDMapWithKeyWithAdjustRequesterTWith base mapPatch weakenPatchWith mergePatchIncremental coincidencePatch f dm0 dm' = do
  env <- RequesterT ask
  RequesterT $ lift $ sequenceDMapWithAdjustEventWriterTWith
    (\f' x0 x' -> runReaderT (unRequesterT $ base f' x0 x') env)
    mapPatch
    weakenPatchWith
    mergePatchIncremental
    coincidencePatch
    (\x0 x' -> runReaderT (unRequesterT $ f x0 x') env)
    dm0
    dm'
