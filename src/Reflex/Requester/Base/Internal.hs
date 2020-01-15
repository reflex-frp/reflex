{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
module Reflex.Requester.Base.Internal where

import Reflex.Class
import Reflex.Adjustable.Class
import Reflex.Host.Class
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class
import Reflex.Requester.Class
import Reflex.TriggerEvent.Class
import Reflex.EventWriter.Base
import Reflex.EventWriter.Class

import Control.Applicative (liftA2)
import Control.Monad.Exception
import Control.Monad.Fail
import Control.Monad.Identity
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.State
import Data.Coerce
import Data.Dependent.Map (DSum (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty.Deferred (NonEmptyDeferred)
import qualified Data.List.NonEmpty.Deferred as NonEmptyDeferred
import Data.Monoid ((<>))
import qualified Data.Semigroup as S
import Data.Some (Some(Some))
import Data.Type.Equality
import Data.TagMap (TagMap)
import qualified Data.TagMap as TagMap
import Reflex.FanTag
import Data.Unique.Tag.Local
import Data.GADT.Compare
import Data.Witherable

import Unsafe.Coerce

import Debug.Trace

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import GHC.Exts (Any)

data RequestData ps request = forall s. RequestData !(TagGen ps s) !(NonEmptyDeferred (RequestEnvelope s request))

data RequestEnvelope s request = forall a. RequestEnvelope {-# UNPACK #-} !(Maybe (Tag s a)) !(request a)

data ResponseData ps response = forall s. ResponseData !(TagGen ps s) !(TagMap s response)

traverseRequesterData :: forall m request response. Applicative m => (forall a. request a -> m (response a)) -> RequestData (PrimState m) request -> m (ResponseData (PrimState m) response)
traverseRequesterData f (RequestData tg es) = ResponseData tg . TagMap.fromList <$> wither g (NonEmpty.toList $ NonEmptyDeferred.toNonEmpty es)
  where g (RequestEnvelope mt req) = case mt of
          Just t -> Just . (t :=>) <$> f req
          Nothing -> Nothing <$ f req

-- | 'traverseRequesterData' with its arguments flipped
forRequesterData :: forall request response m. Applicative m => RequestData (PrimState m) request -> (forall a. request a -> m (response a)) -> m (ResponseData (PrimState m) response)
forRequesterData r f = traverseRequesterData f r

runRequesterT :: forall t request response m a
  .  ( Reflex t
     , PrimMonad m
     , MonadFix m
     )
  => RequesterT t request response m a
  -> Event t (ResponseData (PrimState m) response)
  -> m (a, Event t (RequestData (PrimState m) request))
runRequesterT (RequesterT a) wrappedResponses = withRequesterInternalT $ \requests -> do
  (_, tg) <- RequesterInternalT ask
  result <- a
  let responses = fforMaybe wrappedResponses $ \(ResponseData tg' m) -> case tg `geq` tg' of
        Nothing -> trace ("runRequesterT: bad TagGen: expected " <> show tg <> " but got " <> show tg') Nothing --TODO: Warn somehow
        Just Refl -> Just m
  pure (responses, (result, fmapCheap (RequestData tg) requests))

instance MonadTrans (RequesterInternalT s t request response) where
  lift = RequesterInternalT . lift . lift

instance MonadTrans (RequesterT t request response) where
  lift a = RequesterT $ RequesterInternalT $ lift $ lift a

-- | Run a 'RequesterT' action.  The resulting 'Event' will fire whenever
-- requests are made, and responses should be provided in the input 'Event'.
-- The 'Tag' keys will be used to return the responses to the same place the
-- requests were issued.
withRequesterInternalT
  :: forall t m request response a.
     ( Reflex t
     , PrimMonad m
     , MonadFix m
     )
  => (Event t (NonEmptyDeferred (RequestEnvelope FakeRequesterStatePhantom request)) -> RequesterInternalT FakeRequesterStatePhantom  t request response m (Event t (TagMap FakeRequesterStatePhantom response), a))
  -> m a
withRequesterInternalT f = do
  stg <- newTagGen
  let tg = case stg of
        Some tg' -> (unsafeCoerce :: TagGen (PrimState m) s -> TagGen (PrimState m) FakeRequesterStatePhantom) tg'
  rec let RequesterInternalT a = (unsafeCoerce :: RequesterInternalT s t request response m (Event t (TagMap FakeRequesterStatePhantom response), a) -> RequesterInternalT FakeRequesterStatePhantom t request response m (Event t (TagMap FakeRequesterStatePhantom response), a)) $ f requests
      ((responses, result), requests) <- runEventWriterT $ runReaderT a (fanTag responses, tg)
  pure result

-- This is because using forall ruins inlining
data FakeRequesterStatePhantom

newtype RequesterT t (request :: * -> *) (response :: * -> *) m a = RequesterT { unRequesterT :: RequesterInternalT FakeRequesterStatePhantom t request response m a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadException)

newtype RequesterInternalT s t request response m a = RequesterInternalT { unRequesterInternalT :: ReaderT (EventSelectorTag t s response, TagGen (PrimState m) s) (EventWriterT t (NonEmptyDeferred (RequestEnvelope s request)) m) a }
  deriving
    ( Functor, Applicative, Monad, MonadFix, MonadIO, MonadException
#if MIN_VERSION_base(4,9,1)
    , MonadAsyncException
#endif
    )

-- I don't think this can actually be supported without unsafeCoercing around the fact that the phantoms don't match up.  In fact, implementations could probably supply `unmask` functions that would actually do the wrong thing.
-- #if MIN_VERSION_base(4,9,1)
-- instance MonadAsyncException m => MonadAsyncException (RequesterT t request response m) where
--   mask f = RequesterT $ mask $ \unmask -> unRequesterT $ f $ \x -> RequesterT $ unmask $ unRequesterT x
-- #endif

deriving instance MonadSample t m => MonadSample t (RequesterT t request response m)
deriving instance MonadHold t m => MonadHold t (RequesterT t request response m)
deriving instance PostBuild t m => PostBuild t (RequesterT t request response m)
deriving instance TriggerEvent t m => TriggerEvent t (RequesterT t request response m)

deriving instance MonadSample t m => MonadSample t (RequesterInternalT s t request response m)
deriving instance MonadHold t m => MonadHold t (RequesterInternalT s t request response m)
deriving instance PostBuild t m => PostBuild t (RequesterInternalT s t request response m)
deriving instance TriggerEvent t m => TriggerEvent t (RequesterInternalT s t request response m)

instance PrimMonad m => PrimMonad (RequesterT t request response m) where
  type PrimState (RequesterT t request response m) = PrimState m
  primitive = lift . primitive

-- TODO: Monoid and Semigroup can likely be derived once StateT has them.
instance (Monoid a, Monad m) => Monoid (RequesterT t request response m a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance (S.Semigroup a, Monad m) => S.Semigroup (RequesterT t request response m a) where
  (<>) = liftA2 (S.<>)

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
  local f (RequesterT a) = RequesterT $ RequesterInternalT $ mapReaderT (mapEventWriterT $ local f) $ unRequesterInternalT a
  reader = lift . reader

instance (Reflex t, PrimMonad m) => Requester t (RequesterT t request response m) where
  type Request (RequesterT t request response m) = request
  type Response (RequesterT t request response m) = response
  requesting e = RequesterT $ requesting e
  requesting_ e = RequesterT $ requesting_ e

instance (Reflex t, PrimMonad m) => Requester t (RequesterInternalT s t request response m) where
  type Request (RequesterInternalT s t request response m) = request
  type Response (RequesterInternalT s t request response m) = response
  requesting e = RequesterInternalT $ do
    (s, tg) <- ask
    t <- lift $ lift $ newTag tg
    tellEvent $ fmapCheap (NonEmptyDeferred.singleton . RequestEnvelope (Just t)) e
    pure $ selectTag s t
  requesting_ e = RequesterInternalT $ do
    tellEvent $ fmapCheap (NonEmptyDeferred.singleton . RequestEnvelope Nothing) e

instance (Adjustable t m, MonadHold t m) => Adjustable t (RequesterInternalT s t request response m) where
  {-# INLINE runWithReplace #-}
  runWithReplace (RequesterInternalT a0) a' = RequesterInternalT $ runWithReplace a0 (coerceEvent a')
  {-# INLINE traverseIntMapWithKeyWithAdjust #-}
  traverseIntMapWithKeyWithAdjust f dm0 dm' = RequesterInternalT $ traverseIntMapWithKeyWithAdjust (coerce . f) dm0 dm'
  {-# INLINE traverseDMapWithKeyWithAdjust #-}
  traverseDMapWithKeyWithAdjust f dm0 dm' = RequesterInternalT $ traverseDMapWithKeyWithAdjust (coerce . f) dm0 dm'
  {-# INLINE traverseDMapWithKeyWithAdjustWithMove #-}
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = RequesterInternalT $ traverseDMapWithKeyWithAdjustWithMove (coerce . f) dm0 dm'

instance (Adjustable t m, MonadHold t m) => Adjustable t (RequesterT t request response m) where
  {-# INLINE runWithReplace #-}
  runWithReplace (RequesterT a0) a' = RequesterT $ runWithReplace a0 (fmapCheap unRequesterT a')
  {-# INLINE traverseIntMapWithKeyWithAdjust #-}
  traverseIntMapWithKeyWithAdjust f dm0 dm' = RequesterT $ traverseIntMapWithKeyWithAdjust (\k v -> unRequesterT $ f k v) dm0 dm'
  {-# INLINE traverseDMapWithKeyWithAdjust #-}
  traverseDMapWithKeyWithAdjust f dm0 dm' = RequesterT $ traverseDMapWithKeyWithAdjust (\k v -> unRequesterT $ f k v) dm0 dm'
  {-# INLINE traverseDMapWithKeyWithAdjustWithMove #-}
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = RequesterT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unRequesterT $ f k v) dm0 dm'


data Decoder rawResponse response =
  forall s a. Decoder (Tag s a) (rawResponse -> response a)

-- | Matches incoming responses with previously-sent requests
-- and uses the provided request "decoder" function to process
-- incoming responses.
matchResponsesWithRequests
  :: forall t rawRequest rawResponse request response m.
     ( MonadFix m
     , MonadHold t m
     , MonadFail m
     , PrimMonad m
     , Reflex t
     )
  => (forall a. request a -> (rawRequest, rawResponse -> response a))
  -- ^ Given a request (from 'Requester'), produces the wire format of the
  -- request and a function used to process the associated response
  -> Event t (RequestData (PrimState m) request)
  -- ^ The outgoing requests
  -> Event t (Int, rawResponse)
  -- ^ The incoming responses, tagged by an identifying key
  -> m ( Event t (Map Int rawRequest)
       , Event t (ResponseData (PrimState m) response)
       )
  -- ^ A map of outgoing wire-format requests and an event of responses keyed
  -- by the 'RequesterData' key of the associated outgoing request
matchResponsesWithRequests f send recv = withTagGen $ \tagGen ->  mdo
  waitingFor <- holdIncremental mempty $
    leftmost [ fst <$> outgoing, snd <$> incoming ]
  let outgoing = processOutgoing send
      incoming = processIncoming waitingFor tagGen recv
  return (snd <$> outgoing, fst <$> incoming)
  where

    -- Tags each outgoing request with an identifying integer key
    -- and returns the next available key, a map of response decoders
    -- for requests for which there are outstanding responses, and the
    -- raw requests to be sent out.
    processOutgoing
      :: Event t (RequestData (PrimState m) request)
      -- The outgoing request
      -> Event t ( PatchMap Int (Decoder rawResponse response)
                 , Map Int rawRequest )
      -- A map of requests expecting responses, and the tagged raw requests
    processOutgoing eventOutgoingRequest = flip pushAlways eventOutgoingRequest $ \(RequestData tagGen requestEnvelopes) -> do
      let results = ffor (requestEnvelopesToList $ NonEmptyDeferred.toList requestEnvelopes) $ \(k :=> v) ->
            let (rawRequest, responseDecoder) = f v
            in (tagId k, rawRequest, Decoder k responseDecoder)
      let patchWaitingFor = PatchMap $ Map.fromList $ (\(n, _, dec) -> (n, Just dec)) <$> results
      let toSend = Map.fromList $ (\(n, rawRequest, _) -> (n, rawRequest)) <$> results
      return (patchWaitingFor, toSend)

    -- Looks up the each incoming raw response in a map of response
    -- decoders and returns the decoded response and a patch that can
    -- be used to clear the ID of the consumed response out of the queue
    -- of expected responses.
    processIncoming
      :: Incremental t (PatchMap Int (Decoder rawResponse response))
      -> TagGen (PrimState m) s
      -- A map of outstanding expected responses
      -> Event t (Int, rawResponse)
      -- A incoming response paired with its identifying key
      -> Event t (ResponseData (PrimState m) response, PatchMap Int v)
      -- The decoded response and a patch that clears the outstanding responses queue
    processIncoming incWaitingFor tagGen inc = flip push inc $ \(n, rawRsp) -> do
      waitingFor <- sample $ currentIncremental incWaitingFor
      case Map.lookup n waitingFor of
        Nothing -> return Nothing -- TODO How should lookup failures be handled here? They shouldn't ever happen..
        Just (Decoder tag responseDecoder) ->
          return $ Just
            ( ResponseData tagGen $ TagMap.singletonTagMap (unsafeTagFromId n) (responseDecoder rawRsp)
            , PatchMap $ Map.singleton n Nothing
            )

requestEnvelopesToList :: forall s request. [RequestEnvelope s request] -> [DSum (Tag s) request]
requestEnvelopesToList reqEnvs = f <$> reqEnvs
  where f :: (RequestEnvelope s request) -> DSum (Tag s) request
        f (RequestEnvelope (Just tag) v) = tag :=> v
