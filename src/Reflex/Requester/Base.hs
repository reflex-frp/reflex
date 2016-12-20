-- | This module provides 'RequesterT', the standard implementation of
-- 'Requester'.
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
  , runWithReplaceRequesterTWith
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
import Data.Unique.Tag

-- | A basic implementation of 'Requester'.
newtype RequesterT t request response m a = RequesterT { unRequesterT :: EventWriterT t (DMap (Tag (PrimState m)) request) (ReaderT (EventSelector t (WrapArg response (Tag (PrimState m)))) m) a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadException)

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
    t <- lift newTag
    s <- RequesterT ask
    (req, result) <- a $ select s $ WrapArg t
    RequesterT $ tellEvent $ DMap.singleton t <$> req
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
  runWithReplace (RequesterT a) em = RequesterT $ runWithReplace a (coerceEvent em)
  sequenceDMapWithAdjust dm edm = RequesterT $ sequenceDMapWithAdjust (coerce dm) (coerceEvent edm)

runWithReplaceRequesterTWith :: forall m t request response a b. (Reflex t, MonadHold t m)
                             => (forall a' b'. m a' -> Event t (m b') -> RequesterT t request response m (a', Event t b'))
                             -> RequesterT t request response m a
                             -> Event t (RequesterT t request response m b)
                             -> RequesterT t request response m (a, Event t b)
runWithReplaceRequesterTWith f a0 a' =
  let f' :: forall a' b'. ReaderT (EventSelector t (WrapArg response (Tag (PrimState m)))) m a'
         -> Event t (ReaderT (EventSelector t (WrapArg response (Tag (PrimState m)))) m b')
         -> EventWriterT t (DMap (Tag (PrimState m)) request) (ReaderT (EventSelector t (WrapArg response (Tag (PrimState m)))) m) (a', Event t b')
      f' x y = do
        r <- EventWriterT $ ask
        unRequesterT (f (runReaderT x r) (fmap (`runReaderT` r) y))
  in RequesterT $ runWithReplaceEventWriterTWith f' (coerce a0) (coerceEvent a')

sequenceDMapWithAdjustRequesterTWith :: (GCompare k, Reflex t, MonadHold t m)
                                     => (forall k'. GCompare k' => DMap k' m -> Event t (PatchDMap k' m) -> RequesterT t request response m (DMap k' Identity, Event t (PatchDMap k' Identity)))
                                     -> DMap k (RequesterT t request response m)
                                     -> Event t (PatchDMap k (RequesterT t request response m))
                                     -> RequesterT t request response m (DMap k Identity, Event t (PatchDMap k Identity))
sequenceDMapWithAdjustRequesterTWith f (dm0 :: DMap k (RequesterT t request response m)) dm' =
  let dmapUnwrap r = mapKeyValuePairsMonotonic $ \(k :=> v) -> k :=> runReaderT v r
      patchDmapUnwrap r (PatchDMap p) = PatchDMap $ mapKeyValuePairsMonotonic
        (\(k :=> ComposeMaybe mv) -> k :=> ComposeMaybe (fmap (`runReaderT` r) mv)) p
      f' :: forall k'. GCompare k'
         => DMap k' (ReaderT (EventSelector t (WrapArg response (Tag (PrimState m)))) m)
         -> Event t (PatchDMap k' (ReaderT (EventSelector t (WrapArg response (Tag (PrimState m)))) m))
         -> EventWriterT t (DMap (Tag (PrimState m)) request) (ReaderT (EventSelector t (WrapArg response (Tag (PrimState m)))) m) (DMap k' Identity, Event t (PatchDMap k' Identity))
      f' x y = do
        r <- EventWriterT $ ask
        unRequesterT (f (dmapUnwrap r x) (fmap (patchDmapUnwrap r) y))
  in RequesterT $ sequenceDMapWithAdjustEventWriterTWith f' (coerce dm0) (coerceEvent dm')
