-- | This module provides 'RequesterT', the standard implementation of
-- 'Requester'.
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
module Reflex.Requester.Base
  ( RequesterT (..)
  , runRequesterT
  , mapMWithReplaceRequesterTWith
  , sequenceDMapWithAdjustRequesterTWith
  ) where

import Reflex.Class
import Reflex.EventWriter
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
import Data.Coerce
import Data.Dependent.Map (DMap, DSum (..))
import qualified Data.Dependent.Map as DMap
import Data.Functor.Misc
import Data.Map (Map)
import Data.Unique.Tag
import Data.Some (Some)

-- | A basic implementation of 'Requester'.
newtype RequesterT t request response m a = RequesterT { unRequesterT :: EventWriterT t (DMap (Tag (PrimState m)) request) (ReaderT (EventSelector t (WrapArg response (Tag (PrimState m)))) m) a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadException
-- MonadAsyncException can't be derived on ghc-8.0.1; we use base-4.9.1 as a proxy for ghc-8.0.2
#if MIN_VERSION_base(4,9,1)
           , MonadAsyncException
#endif
           )

deriving instance MonadSample t m => MonadSample t (RequesterT t request response m)
deriving instance MonadHold t m => MonadHold t (RequesterT t request response m)
deriving instance PostBuild t m => PostBuild t (RequesterT t request response m)
deriving instance TriggerEvent t m => TriggerEvent t (RequesterT t request response m)

-- | Run a 'RequesterT' action.  The resulting 'Event' will fire whenever
-- requests are made, and responses should be provided in the input 'Event'.
-- The 'Tag' keys will be used to return the responses to the same place the
-- requests were issued.

runRequesterT :: (Reflex t, Monad m)
              => RequesterT t request response m a
              -> Event t (DMap (Tag (PrimState m)) response)
              -> m (a, Event t (DMap (Tag (PrimState m)) request))
runRequesterT (RequesterT a) responses = do
  (result, requests) <- runReaderT (runEventWriterT a) $ fan $
    mapKeyValuePairsMonotonic (\(t :=> e) -> WrapArg t :=> Identity e) <$> responses
  return (result, requests)

instance (Reflex t, PrimMonad m) => Requester t (RequesterT t request response m) where
  type Request (RequesterT t request response m) = request
  type Response (RequesterT t request response m) = response
  withRequesting a = do
    !t <- lift newTag --TODO: Fix this upstream
    s <- RequesterT ask
    (req, result) <- a $ select s $ WrapArg t
    RequesterT $ tellEvent $ fmapCheap (DMap.singleton t) req
    return result

instance MonadTrans (RequesterT t request response) where
  lift = RequesterT . lift . lift

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
  local f (RequesterT (EventWriterT a)) = RequesterT $ EventWriterT $ mapStateT (mapReaderT $ local f) a
  reader = lift . reader

instance (Reflex t, MonadAdjust t m, MonadHold t m) => MonadAdjust t (RequesterT t request response m) where
  mapMWithReplace f g a0 a' = RequesterT $ mapMWithReplace (coerce f) (coerce g) a0 a'
  traverseDMapWithKeyWithAdjust f dm edm = RequesterT $ traverseDMapWithKeyWithAdjust (\k v -> unRequesterT $ f k v) (coerce dm) (coerceEvent edm)
  traverseDMapWithKeyWithAdjustWithMove f dm edm = RequesterT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unRequesterT $ f k v) (coerce dm) (coerceEvent edm)

mapMWithReplaceRequesterTWith :: forall m t request response a a' b b'. (Reflex t, MonadHold t m)
                              => (forall aa aa' bb bb'.
                                   (aa -> m bb) ->
                                   (aa' -> m bb') ->
                                   aa ->
                                   Event t aa' ->
                                   RequesterT t request response m (bb, Event t bb')
                                 )
                              -> (a -> RequesterT t request response m b)
                              -> (a' -> RequesterT t request response m b')
                              -> a
                              -> Event t a'
                              -> RequesterT t request response m (b, Event t b')
mapMWithReplaceRequesterTWith base f g a0 a' =
  let base' :: forall aaa aaa' bbb bbb'.
               (aaa -> ReaderT (EventSelector t (WrapArg response (Tag (PrimState m)))) m bbb)
            -> (aaa' -> ReaderT (EventSelector t (WrapArg response (Tag (PrimState m)))) m bbb')
            -> aaa
            -> Event t aaa'
            -> EventWriterT t (DMap (Tag (PrimState m)) request) (ReaderT (EventSelector t (WrapArg response (Tag (PrimState m)))) m) (bbb, Event t bbb')
      base' f' g' x y = do
        r <- EventWriterT $ ask
        unRequesterT $ base ((`runReaderT` r) . f') ((`runReaderT` r) . g') x y
  in RequesterT $ mapMWithReplaceEventWriterTWith base' (coerce f) (coerce g) a0 a'

sequenceDMapWithAdjustRequesterTWith :: forall k t request response m v v' p p'.
                                        ( GCompare k
                                        , Reflex t
                                        , MonadHold t m
                                        , PatchTarget (p' (Some k) (Event t (DMap (Tag (PrimState m)) request))) ~ Map (Some k) (Event t (DMap (Tag (PrimState m)) request))
                                        , Patch (p' (Some k) (Event t (DMap (Tag (PrimState m)) request)))
                                        )
                                     => (forall k' v1 v2. GCompare k'
                                         => (forall a. k' a -> v1 a -> m (v2 a))
                                         -> DMap k' v1
                                         -> Event t (p k' v1)
                                         -> RequesterT t request response m (DMap k' v2, Event t (p k' v2))
                                        )
                                     -> (forall v1. (forall a. v1 a -> v' a) -> p k v1 -> p k v')
                                     -> (forall v1 v2. (forall a. v1 a -> v2) -> p k v1 -> p' (Some k) v2)
                                     -> (forall v2. p' (Some k) v2 -> [v2])
                                     -> (forall a. Incremental t (p' (Some k) (Event t a)) -> Event t (Map (Some k) a))
                                     -> (forall a. k a -> v a -> RequesterT t request response m (v' a))
                                     -> DMap k v
                                     -> Event t (p k v)
                                     -> RequesterT t request response m (DMap k v', Event t (p k v'))
sequenceDMapWithAdjustRequesterTWith base mapPatch weakenPatchWith patchNewElements mergePatchIncremental f dm0 dm' =
  let base' :: forall v1 v2.
           (forall a. k a -> v1 a -> ReaderT (EventSelector t (WrapArg response (Tag (PrimState m)))) m (v2 a))
        -> DMap k v1
        -> Event t (p k v1)
        -> EventWriterT t (DMap (Tag (PrimState m)) request) (ReaderT (EventSelector t (WrapArg response (Tag (PrimState m)))) m) (DMap k v2, Event t (p k v2))
      base' f' x y = do
        r <- EventWriterT ask
        unRequesterT $ base (\k v -> runReaderT (f' k v) r) x y
  in RequesterT $ sequenceDMapWithAdjustEventWriterTWith base' mapPatch weakenPatchWith patchNewElements mergePatchIncremental (\k v -> unRequesterT $ f k v) (coerce dm0) (coerceEvent dm')
