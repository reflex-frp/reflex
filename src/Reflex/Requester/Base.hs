-- | This module provides 'RequesterT', the standard implementation of
-- 'Requester'.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
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
  , runRequesterT
  , fireResponses
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
import Data.Constraint
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

import Debug.Trace

-- | A basic implementation of 'Requester'.
newtype RequesterT t (request :: * -> *) (response :: * -> *) m a = RequesterT { unRequesterT :: forall x. ReaderT (FanCell t x) (EventWriterT t (Seq (RequestItem t x request response)) m) a }

instance Functor m => Functor (RequesterT t request response m) where
  fmap f (RequesterT a) = RequesterT $ fmap f a

instance Monad m => Applicative (RequesterT t request response m) where
  pure a = RequesterT $ pure a
  RequesterT f <*> RequesterT x = RequesterT $ f <*> x
  liftA2 f (RequesterT x) (RequesterT y) = RequesterT $ liftA2 f x y
  RequesterT x *> RequesterT y = RequesterT $ x *> y
  RequesterT x <* RequesterT y = RequesterT $ x <* y

instance Monad m => Monad (RequesterT t request response m) where
  RequesterT x >>= f = RequesterT $ x >>= unRequesterT . f
  (>>) = (*>)
  return = pure
  fail s = RequesterT $ fail s

instance MonadFix m => MonadFix (RequesterT t request response m) where
  mfix f = RequesterT $ mfix $ unRequesterT . f

instance MonadIO m => MonadIO (RequesterT t request response m) where
  liftIO a = RequesterT $ liftIO a

instance MonadException m => MonadException (RequesterT t request response m) where
  throw e = RequesterT $ throw e
  catch (RequesterT a) h = RequesterT $ catch a (unRequesterT . h)
  finally (RequesterT a) (RequesterT f) = RequesterT $ finally a f

instance MonadAsyncException m => MonadAsyncException (RequesterT t request response m) where
  mask f = undefined -- RequesterT $ liftMask (liftMask mask) $ \restore -> unRequesterT $ f $ \(RequesterT a) -> RequesterT $ restore a

instance MonadSample t m => MonadSample t (RequesterT t request response m)
instance MonadHold t m => MonadHold t (RequesterT t request response m)
instance MonadMutate t m => MonadMutate t (RequesterT t request response m)
instance PostBuild t m => PostBuild t (RequesterT t request response m)
instance TriggerEvent t m => TriggerEvent t (RequesterT t request response m)

-- TODO: Monoid and Semigroup can likely be derived once StateT has them.
instance (Monoid a, Monad m) => Monoid (RequesterT t request response m a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance (S.Semigroup a, Monad m) => S.Semigroup (RequesterT t request response m a) where
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
  => (forall x. Event t (Seq (RequestItem t x request response)) -> RequesterT t request response m (Event t (Seq (ResponseItem t x response)), a))
  -> m a
runRequesterT' a = withHoldFanCell $ \(c :: FanCell t x) -> do
  traceM "runRequesterT'.a"
  rec ~(~(responses, result :: a), requests) <- runRequesterT (a requests) c
  traceM "runRequesterT'.a"
  pure (fmapCheap fireResponses responses, result)

fireResponses
  :: forall f t x response
  .  ( Reflex t
     , Traversable f
     )
  => f (ResponseItem t x response)
  -> CellM t x ()
fireResponses responses = withMonadCellM @t @x $
  forM_ responses $ \(ResponseItem t v) ->
  fireCellEvent t v

runRequesterT
  :: forall t x m request response a
  .  ( Reflex t
     , MonadFix m
     )
  => RequesterT t request response m a
  -> FanCell t x
  -> m (a, Event t (Seq (RequestItem t x request response)))
runRequesterT a c = do
  runEventWriterT $ runReaderT (unRequesterT a) c

traverseRequests
  :: Applicative f
  => (forall a. request a -> f (response a))
  -> Seq (RequestItem t x request response)
  -> f (Seq (ResponseItem t x response))
traverseRequests f = fmap (Seq.fromList . catMaybes) . traverse f' . toList
  where f' (RequestItem mt req) = case mt of
          Nothing -> Nothing <$ f req
          Just t -> Just . ResponseItem t <$> f req

instance (Reflex t, Monad m, MonadMutate t m) => Requester t (RequesterT t request response m) where
  type Request (RequesterT t request response m) = request
  type Response (RequesterT t request response m) = response
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

instance MonadTrans (RequesterT t request response) where
  lift a = RequesterT $ lift $ lift a

instance PerformEvent t m => PerformEvent t (RequesterT t request response m) where
  type Performable (RequesterT t request response m) = Performable m
  performEvent_ = lift . performEvent_
  performEvent = lift . performEvent

instance MonadRef m => MonadRef (RequesterT t request response m) where
  type Ref (RequesterT t request response m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (RequesterT t request response m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance MonadReader r m => MonadReader r (RequesterT t request response m) where
  ask = lift ask
  local f (RequesterT a) = RequesterT $ mapReaderT (local f) a
  reader = lift . reader

instance (Reflex t, Adjustable t m, MonadHold t m, MonadFix m) => Adjustable t (RequesterT t request response m) where
  runWithReplace = runWithReplaceRequesterTWith $ \dm0 dm' -> lift $ runWithReplace dm0 dm'
  traverseIntMapWithKeyWithAdjust = traverseIntMapWithKeyWithAdjustRequesterTWith $ \f dm0 dm' -> lift $ traverseIntMapWithKeyWithAdjust f dm0 dm'
  {-# INLINABLE traverseDMapWithKeyWithAdjust #-}
  traverseDMapWithKeyWithAdjust = traverseDMapWithKeyWithAdjustRequesterTWith (\f dm0 dm' -> lift $ traverseDMapWithKeyWithAdjust f dm0 dm') Dict mapPatchDMap weakenPatchDMapWith mergeMapIncremental coincidencePatchMap
  traverseDMapWithKeyWithAdjustWithMove = traverseDMapWithKeyWithAdjustRequesterTWith (\f dm0 dm' -> lift $ traverseDMapWithKeyWithAdjustWithMove f dm0 dm') Dict mapPatchDMapWithMove weakenPatchDMapWithMoveWith mergeMapIncrementalWithMove coincidencePatchMapWithMove

{-# INLINABLE runWithReplaceRequesterTWith #-}
runWithReplaceRequesterTWith
  :: forall m t request response a b
  .  ( Reflex t
     , MonadHold t m
     )
  => (forall a' b'. m a' -> Event t (m b') -> RequesterT t request response m (a', Event t b'))
  -> RequesterT t request response m a
  -> Event t (RequesterT t request response m b)
  -> RequesterT t request response m (a, Event t b)
runWithReplaceRequesterTWith f a0 a' = RequesterT $ do
  traceM "runWithReplaceRequesterTWith.a"
  env :: FanCell t y <- ask
  let r :: forall a'. RequesterT t request response m a' -> EventWriterT t (Seq (RequestItem t y request response)) m a'
      r (RequesterT a) = runReaderT a env
  traceM "runWithReplaceRequesterTWith.b"
  result <- lift $ runWithReplaceEventWriterTWith (\x0 x' -> r $ f x0 x') (r a0) $ fmapCheap r a'
  traceM "runWithReplaceRequesterTWith.c"
  pure result

{-# INLINE traverseIntMapWithKeyWithAdjustRequesterTWith #-}
traverseIntMapWithKeyWithAdjustRequesterTWith
  :: forall t request response m v v'
  .  ( Reflex t
     , MonadHold t m
     )
  => (forall v1 v2.
         (IntMap.Key -> v1 -> m v2)
      -> IntMap v1
      -> Event t (PatchIntMap v1)
      -> RequesterT t request response m (IntMap v2, Event t (PatchIntMap v2))
     )
  -> (IntMap.Key -> v -> RequesterT t request response m v')
  -> IntMap v
  -> Event t (PatchIntMap v)
  -> RequesterT t request response m (IntMap v', Event t (PatchIntMap v'))
traverseIntMapWithKeyWithAdjustRequesterTWith base f dm0 dm' = RequesterT $ do
  env <- ask
  lift $ sequenceIntMapWithAdjustEventWriterTWith
    (\f' x0 x' -> runReaderT (unRequesterT $ base f' x0 x') env)
    mergeIntMapIncremental
    coincidencePatchIntMap
    (\x0 x' -> runReaderT (unRequesterT $ f x0 x') env)
    dm0
    dm'

type TraverseDMapPatchConstraints p' k t x request response =
  ( Patch (p' (Some k) (Event t (Seq (RequestItem t x request response))))
  , PatchTarget (p' (Some k) (Event t (Seq (RequestItem t x request response)))) ~ Map (Some k) (Event t (Seq (RequestItem t x request response)))
  , Patch (p' (Some k) (Seq (RequestItem t x request response)))
  , PatchTarget (p' (Some k) (Seq (RequestItem t x request response))) ~ Map (Some k) (Seq (RequestItem t x request response))
  )

{-# INLINE traverseDMapWithKeyWithAdjustRequesterTWith #-}
traverseDMapWithKeyWithAdjustRequesterTWith
  :: forall k t request response m v v' p p'
  .  ( GCompare k
     , Reflex t
     , MonadHold t m
     )
  => (forall k' v1 v2. GCompare k'
      => (forall a. k' a -> v1 a -> m (v2 a))
      -> DMap k' v1
      -> Event t (p k' v1)
      -> RequesterT t request response m (DMap k' v2, Event t (p k' v2))
     )
  -> (forall x. Dict (TraverseDMapPatchConstraints p' k t x request response)
     )
  -> (forall x. (forall a. Compose ((,) (Event t (Seq (RequestItem t x request response)))) v' a -> v' a) -> p k (Compose ((,) (Event t (Seq (RequestItem t x request response)))) v') -> p k v')
  -> (forall x. (forall a. Compose ((,) (Event t (Seq (RequestItem t x request response)))) v' a -> Event t (Seq (RequestItem t x request response))) -> p k (Compose ((,) (Event t (Seq (RequestItem t x request response)))) v') -> p' (Some k) (Event t (Seq (RequestItem t x request response))))
  -> (forall x. Incremental t (p' (Some k) (Event t (Seq (RequestItem t x request response)))) -> Event t (PatchTarget (p' (Some k) (Seq (RequestItem t x request response)))))
  -> (forall x. Event t (p' (Some k) (Event t (Seq (RequestItem t x request response)))) -> Event t (p' (Some k) (Seq (RequestItem t x request response))))
  -> (forall a. k a -> v a -> RequesterT t request response m (v' a))
  -> DMap k v
  -> Event t (p k v)
  -> RequesterT t request response m (DMap k v', Event t (p k v'))
traverseDMapWithKeyWithAdjustRequesterTWith base d mapPatch weakenPatchWith mergePatchIncremental coincidencePatch f dm0 dm' = RequesterT $ do
  env :: FanCell t x <- ask
  case d :: Dict (TraverseDMapPatchConstraints p' k t x request response) of
    Dict -> lift $ sequenceDMapWithAdjustEventWriterTWith
      (\f' x0 x' -> runReaderT (unRequesterT $ base f' x0 x') env)
      mapPatch
      weakenPatchWith
      mergePatchIncremental
      coincidencePatch
      (\x0 x' -> runReaderT (unRequesterT $ f x0 x') env)
      dm0
      dm'
