{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wunused-binds #-}
{-# LANGUAGE TupleSections #-}

-- | This module is the implementation of the 'Spider' 'Reflex' engine.  It uses
-- a graph traversal algorithm to propagate 'Event's and 'Behavior's.
module Reflex.Spider.Internal
  ( module Reflex.Spider.Internal
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad hiding (forM, forM_, mapM, mapM_)
import Control.Monad.Catch (MonadMask, MonadThrow, MonadCatch)
import Control.Monad.Exception
import Control.Monad.Fix
import Control.Monad.Primitive
import Control.Monad.Reader.Class
import Control.Monad.IO.Class
import Control.Monad.ReaderIO
import Control.Monad.Ref
import qualified Control.Monad.Fail as MonadFail
import Data.Align
import Data.Coerce
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum (..))
import Data.Foldable hiding (concat, elem, sequence_)
import Data.Functor.Constant
import Data.Functor.Misc
import Data.Functor.Product
import Data.GADT.Compare
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef
import Data.Kind (Type)
import Data.Maybe hiding (mapMaybe, catMaybes)
import Data.Proxy
import Data.These
import Data.Traversable
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts hiding (toList, build)
import GHC.IORef (IORef (..))
import GHC.Stack
import System.IO.Unsafe
import System.Mem.Weak
import Unsafe.Coerce
import Witherable (Filterable (..), mapMaybe, Witherable (..))

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
import Control.Monad.Identity hiding (forM, forM_, mapM, mapM_)
import Control.Monad.Fail (MonadFail)
import Data.List (isPrefixOf)
import Data.Monoid (mempty, (<>))
#else
import Control.Monad.Identity
#endif

#ifdef MIN_VERSION_semialign
#if MIN_VERSION_these(0,8,0)
import Data.These.Combinators (justThese)
#endif
#if MIN_VERSION_semialign(1,1,0)
import Data.Zip (Zip (..))
#endif
#endif

#ifdef DEBUG_CYCLES
import Control.Monad.State hiding (forM, forM_, mapM, mapM_, sequence)
#endif

import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Tree (Forest, Tree (..), drawForest)

#ifdef DEBUG_HIDE_INTERNALS
import Data.List (isPrefixOf)
#endif

import Data.Reflection
import Data.Some (Some(Some))
import Data.Type.Coercion
import Data.Profunctor.Unsafe ((#.), (.#))
import Data.WeakBag (WeakBag, WeakBagTicket, _weakBag_children)
import qualified Data.WeakBag as WeakBag
import qualified Reflex.Class
import qualified Reflex.Class as R
import qualified Reflex.Host.Class
import Reflex.NotReady.Class
import Data.Patch
import qualified Data.Patch.DMapWithMove as PatchDMapWithMove
import Reflex.PerformEvent.Base (PerformEventT)
import Prelude hiding (filter)

#ifdef DEBUG_TRACE_EVENTS
import qualified Data.ByteString.Char8 as BS8
import System.IO (stderr)
import Data.List (isPrefixOf)
#endif

import Reflex.Spider.Height

-- TODO stdout might not be the best channel for debug output
debugStrLn :: String -> IO ()
debugStrLn = putStrLn

#ifdef DEBUG_TRACE_EVENTS
withStackOneLine :: (BS8.ByteString -> a) -> a
withStackOneLine expr = unsafePerformIO $ do
  stack <- currentCallStack
  return (expr . BS8.pack  . unwords . dropInternal . reverse $ stack)
    where dropInternal = filterStack "Reflex.Spider.Internal"

#endif

debugPropagate :: Bool

debugInvalidateHeight :: Bool

debugInvalidate :: Bool

#ifdef DEBUG
#define DEBUG_NODEIDS
#define DEBUG_FRAME

#ifdef DEBUG_TRACE_PROPAGATION
debugPropagate = True
#else
debugPropagate = False
#endif

#ifdef DEBUG_TRACE_HEIGHT
debugInvalidateHeight = True
#else
debugInvalidateHeight = False
#endif

#ifdef DEBUG_TRACE_INVALIDATION
debugInvalidate = True
#else
debugInvalidate = False
#endif

class HasNodeId a where
  getNodeId :: a -> Int

instance HasNodeId (CacheSubscribed x a) where
  getNodeId = _cacheSubscribed_nodeId

instance HasNodeId (Hold x p) where
  getNodeId = holdNodeId

instance HasNodeId (SwitchSubscribed x a) where
  getNodeId = switchSubscribedNodeId

instance HasNodeId (Fan x map v) where
  getNodeId = _fan_nodeId

instance HasNodeId (CoincidenceSubscribed x a) where
  getNodeId = coincidenceSubscribedNodeId

instance HasNodeId (RootSubscribed x a) where
  getNodeId = rootSubscribedNodeId

instance HasNodeId (Pull x a) where
  getNodeId = pullNodeId

{-# INLINE showNodeId #-}
showNodeId :: HasNodeId a => a -> String
showNodeId = showNodeId' . getNodeId

showNodeId' :: Int -> String
showNodeId' = ("#"<>) . show


#else

debugPropagate = False
debugInvalidateHeight = False
debugInvalidate = False

-- This must be inline, or error messages will cause memory leaks due to retaining the node in question
{-# INLINE showNodeId #-}
showNodeId :: a -> String
showNodeId _ = ""

{-# INLINE showNodeId' #-}
showNodeId' :: Int -> String
showNodeId' _ = ""

#endif

#ifdef DEBUG_NODEIDS
{-# NOINLINE nextNodeIdRef #-}
nextNodeIdRef :: IORef Int
nextNodeIdRef = unsafePerformIO $ newIORef 1

newNodeId :: IO Int
newNodeId = atomicModifyIORef' nextNodeIdRef $ \n -> (succ n, n)
#endif

#ifdef DEBUG_FRAME
debugOccurrence :: Applicative m => String -> Maybe a -> m ()
debugOccurrence message = \case
  Just _ -> error message
  Nothing -> pure ()
#endif

--------------------------------------------------------------------------------
-- EventSubscription
--------------------------------------------------------------------------------

--NB: Once you subscribe to an Event, you must always hold on the the WHOLE EventSubscription you get back
-- If you do not retain the subscription, you may be prematurely unsubscribed from the parent event.
data EventSubscription x = EventSubscription
  { _eventSubscription_unsubscribe :: !(IO ())
  , _eventSubscription_subscribed :: {-# UNPACK #-} !(EventSubscribed x)
  }

unsubscribe :: EventSubscription x -> IO ()
unsubscribe (EventSubscription u _) = u

--------------------------------------------------------------------------------
-- Event
--------------------------------------------------------------------------------

newtype Event x a = Event { unEvent :: Subscriber x a -> EventM x (SubscribeResult x a) }

data SubscribeResult x a = SubscribeResult
  { subscribeSubscription :: EventSubscription x
  , subscribeOccurrence :: Maybe a
  }
  deriving (Functor)

instance Foldable (SubscribeResult x) where
  {-# INLINE foldMap #-}
  foldMap f = foldMap f . subscribeOccurrence

instance Traversable (SubscribeResult x) where
  {-# INLINE traverse #-}
  traverse f (SubscribeResult s occ) = SubscribeResult s <$> traverse f occ

instance Filterable (SubscribeResult x) where
  {-# INLINE mapMaybe #-}
  mapMaybe f (SubscribeResult s occ) = SubscribeResult s (occ >>= f)

instance Witherable (SubscribeResult x) where
  {-# INLINE wither #-}
  wither f (SubscribeResult s occ) = SubscribeResult s <$> wither f occ


{-# INLINE subscribeAndRead #-}
subscribeAndRead :: Event x a -> Subscriber x a -> EventM x (SubscribeResult x a)
subscribeAndRead = unEvent

{-# RULES
"cacheEvent/cacheEvent" forall e. cacheEvent (cacheEvent e) = cacheEvent e
"cacheEvent/pushCheap" forall f e. pushCheap f (cacheEvent e) = cacheEvent (pushCheap f e)
"hold/cacheEvent" forall f e. hold f (cacheEvent e) = hold f e
  #-}

-- | Construct an 'Event' equivalent to that constructed by 'push', but with no
-- caching; if the computation function is very cheap, this is (much) more
-- efficient than 'push'
{-# INLINE [1] pushCheap #-}
pushCheap :: forall (x :: Type) a b. HasSpiderTimeline x => (a -> ComputeM x (Maybe b)) -> Event x a -> Event x b
pushCheap !f e = Event $ \sub -> do
  wither f <=< subscribeAndRead e $ debugSubscriber' "push" $ sub
    { subscriberPropagate = \a -> do
        mb <- f a
        mapM_ (subscriberPropagate sub) mb
    }

-- | A subscriber that never triggers other 'Event's
{-# INLINE terminalSubscriber #-}
terminalSubscriber :: (a -> EventM x ()) -> Subscriber x a
terminalSubscriber p = Subscriber
  { subscriberPropagate = p
  , subscriberInvalidateHeight = \_ -> return ()
  , subscriberRecalculateHeight = \_ -> return ()
  }

data HeadESubscribed x a = HeadESubscribed
  { headESubscribedSubscribers :: {-# UNPACK #-} !(WeakBag (Subscriber x a))
  , headESubscribedParent :: !(EventSubscription x)
  , headESubscribedOccurrence :: {-# UNPACK #-} !(IORef (Maybe a))
  , headESubscribedHeight :: {-# UNPACK #-} !(IORef Height)
  }

-- | State of a 'headE' node, retains exactly what's needed for each phase.
data HeadEState x a
  = HeadEStateInitial (Event x a)
  | HeadEStateSubscribed !(HeadESubscribed x a)
  | HeadEStateOccurred {-# UNPACK #-} !(IORef (Maybe a))

-- | Specialized implementation of 'Reflex.Class.slowHeadE'.
headE :: forall x m a. (HasSpiderTimeline x, Defer (SomeMergeInit x) m) => Event x a -> m (Event x a)
headE originalE = do
  stateRef <- liftIO $ newIORef (HeadEStateInitial originalE :: HeadEState x a)
  let unsubscribeAfterOccurrence occRef = liftIO (readIORef stateRef) >>= \case
        HeadEStateSubscribed subscribed -> liftIO $ do
          writeIORef stateRef $ HeadEStateOccurred occRef
          unsubscribe $ headESubscribedParent subscribed
          deferRecalculateHeight @x $ SomeRecalculateHeight $ do
            let heightRef = headESubscribedHeight subscribed
                subscribers = headESubscribedSubscribers subscribed
            invalidateOwnHeight heightRef $ WeakBag.traverse_ subscribers . invalidateSubscriberHeight
            recalculateOwnHeight heightRef (WeakBag.traverse_ subscribers . recalculateSubscriberHeight) (pure zeroHeight)
        _ -> pure ()
      subscribeUntilHead :: EventM x (HeadEState x a)
      subscribeUntilHead = liftIO (readIORef stateRef) >>= \case
        HeadEStateInitial parentEvent -> do
          subscribers <- liftIO WeakBag.empty
          occRef <- liftIO $ newIORef Nothing
          heightRef <- liftIO $ newIORef zeroHeight
          SubscribeResult parentSubscription occ <- subscribeAndRead parentEvent $ Subscriber
            { subscriberPropagate = \a -> do
#ifdef DEBUG_FRAME
                liftIO $ readIORef occRef >>= debugOccurrence "headE: two occurrences in one frame"
#endif
                liftIO $ writeIORef occRef $ Just a
                scheduleClear occRef
                -- Propagating before unsubscribing can help avoid
                -- teardown-then-revive when downstream would re-subscribe:
                propagate a subscribers
                unsubscribeAfterOccurrence occRef
            , subscriberInvalidateHeight = \_ ->
                invalidateOwnHeight heightRef $ WeakBag.traverse_ subscribers . invalidateSubscriberHeight
            , subscriberRecalculateHeight =
                recalculateOwnHeight heightRef (WeakBag.traverse_ subscribers . recalculateSubscriberHeight)
                . pure
            }
          liftIO $ writeIORef heightRef =<< eventSubscriptionHeight parentSubscription
          let !subscribed = HeadESubscribed
                { headESubscribedSubscribers = subscribers
                , headESubscribedParent = parentSubscription
                , headESubscribedOccurrence = occRef
                , headESubscribedHeight = heightRef
                }
          liftIO $ writeIORef stateRef $ HeadEStateSubscribed subscribed
          if isJust occ
            then do
              liftIO $ writeIORef occRef occ
              scheduleClear occRef
              unsubscribeAfterOccurrence occRef
              pure $ HeadEStateOccurred occRef
            else pure $ HeadEStateSubscribed subscribed
        alreadySubscribed -> pure alreadySubscribed
  defer $ SomeMergeInit $ void subscribeUntilHead
  pure $ Event $ \sub -> do
    liftIO $ touch stateRef
    state <- liftIO (readIORef stateRef) >>= \case
      HeadEStateInitial _ -> subscribeUntilHead
      alreadySubscribed -> pure alreadySubscribed
    case state of
      HeadEStateInitial _ -> error "headE in impossible HeadEStateInitial state"
      HeadEStateSubscribed subscribed -> liftIO $ do
        ticket <- WeakBag.insertWithoutCallback sub $ headESubscribedSubscribers subscribed
        occ <- readIORef $ headESubscribedOccurrence subscribed
        pure $ SubscribeResult
              (EventSubscription
                   { _eventSubscription_unsubscribe = do
                       WeakBag.remove ticket
                       touch ticket
                   , _eventSubscription_subscribed = EventSubscribed
                       { eventSubscribedHeightRef = headESubscribedHeight subscribed
                       , eventSubscribedRetained = toAny stateRef
#ifdef DEBUG_CYCLES
                       , eventSubscribedGetParents = pure [_eventSubscription_subscribed $ headESubscribedParent subscribed]
                       , eventSubscribedHasOwnHeightRef = True
                       , eventSubscribedWhoCreated = whoCreatedIORef stateRef
#endif
                       }
                   })
               occ
      HeadEStateOccurred occRef -> do
        occ <- liftIO $ readIORef occRef
        pure $ SubscribeResult (EventSubscription (pure ()) eventSubscribedNever) occ

data CacheSubscribed x a
   = CacheSubscribed { _cacheSubscribed_subscribers :: {-# UNPACK #-} !(WeakBag (Subscriber x a))
                     , _cacheSubscribed_parent :: {-# UNPACK #-} !(EventSubscription x)
                     , _cacheSubscribed_occurrence :: {-# UNPACK #-} !(IORef (Maybe a))
#ifdef DEBUG_NODEIDS
                     , _cacheSubscribed_nodeId :: {-# UNPACK #-} !Int
#endif
                     }

nowSpiderEventM :: HasSpiderTimeline x => EventM x (R.Event (SpiderTimeline x) ())
nowSpiderEventM = SpiderEvent <$> now

now :: Defer (Some Clear) m => m (Event x ())
now = do
  nowOrNot <- liftIO $ newIORef $ Just ()
  scheduleClear nowOrNot
  return . Event $ \_ -> do
    SubscribeResult (EventSubscription (return ()) eventSubscribedNow) <$> (liftIO . readIORef $ nowOrNot)

-- | Construct an 'Event' whose value is guaranteed not to be recomputed
-- repeatedly
--
--TODO: Try a caching strategy where we subscribe directly to the parent when
--there's only one subscriber, and then build our own WeakBag only when a second
--subscriber joins
{-# NOINLINE [0] cacheEvent #-}
cacheEvent :: forall x a. HasSpiderTimeline x => Event x a -> Event x a
cacheEvent e =
#ifdef DEBUG_TRACE_EVENTS
  withStackOneLine $ \callSite -> Event $
#else
  Event $
#endif
  unsafePerformIO $ do
    mSubscribedRef :: IORef (Maybe (Weak (CacheSubscribed x a)))
        <- newIORef Nothing
    pure $ \sub -> {-# SCC "cacheEvent" #-} do
#ifdef DEBUG_TRACE_EVENTS
          unless (BS8.null callSite) $ liftIO $ BS8.hPutStrLn stderr callSite
#endif
          subscribed <- liftIO (readIORef mSubscribedRef >>= maybe (pure Nothing) deRefWeak) >>= \case
            Just subscribed -> return subscribed
            Nothing -> do
#ifdef DEBUG_NODEIDS
              nodeId <- liftIO newNodeId
#endif
              subscribers <- liftIO WeakBag.empty
              occRef <- liftIO $ newIORef Nothing -- This should never be read prior to being set below
              SubscribeResult parentSub occ <- subscribeAndRead e
#ifdef DEBUG_NODEIDS
                  $ debugSubscriber' ("cacheEvent" <> showNodeId' nodeId)
#endif
                  $ Subscriber
                  { subscriberPropagate = \a -> do
#ifdef DEBUG_FRAME
                      liftIO $ readIORef occRef >>= debugOccurrence "cacheEvent: two occurrences in one frame"
#endif
                      liftIO $ writeIORef occRef (Just a)
                      scheduleClear occRef
                      propagate a subscribers
                  , subscriberInvalidateHeight = WeakBag.traverse_ subscribers . invalidateSubscriberHeight
                  , subscriberRecalculateHeight = WeakBag.traverse_ subscribers . recalculateSubscriberHeight
                  }
              when (isJust occ) $ do
                liftIO $ writeIORef occRef occ -- Set the initial value of occRef; we don't need to do this if occ is Nothing
                scheduleClear occRef
              let !subscribed = CacheSubscribed
                    { _cacheSubscribed_subscribers = subscribers
                    , _cacheSubscribed_parent = parentSub
                    , _cacheSubscribed_occurrence = occRef
#ifdef DEBUG_NODEIDS
                    , _cacheSubscribed_nodeId = nodeId
#endif
                    }
              liftIO $ writeIORef mSubscribedRef . Just =<< mkWeakPtrWithDebug subscribed "CacheSubscribed"
              return subscribed
          liftIO $ cacheSubscription sub mSubscribedRef subscribed

cacheSubscription :: Subscriber x a -> IORef (Maybe (Weak (CacheSubscribed x a))) -> CacheSubscribed x a -> IO (SubscribeResult x a)
cacheSubscription sub mSubscribedRef subscribed = do
  -- 'lazy' stops GHC <= 9.0 from reboxing subscribed, so that
  -- 'eventSubscribedRetained' keeps the key of the weak pointer in
  -- 'mSubscribedRef' alive:
  let fields = lazy subscribed
  ticket <- WeakBag.insertWithoutCallback sub $ _cacheSubscribed_subscribers fields
  occ <- readIORef $ _cacheSubscribed_occurrence fields
  let parentSub = _cacheSubscribed_parent fields
      -- Prevent reboxing of the unpacked _cacheSubscribed_parent on every
      -- subscribe.
      unsubscribeParent = _eventSubscription_unsubscribe parentSub
      es = EventSubscription
        { _eventSubscription_unsubscribe = do
            WeakBag.remove ticket
            isEmpty <- WeakBag.isEmpty $ _cacheSubscribed_subscribers fields
            when isEmpty $ do
              writeIORef mSubscribedRef Nothing
              unsubscribeParent
            touch ticket
        , _eventSubscription_subscribed = EventSubscribed
          { eventSubscribedHeightRef = eventSubscribedHeightRef $ _eventSubscription_subscribed parentSub
          , eventSubscribedRetained = toAny subscribed
#ifdef DEBUG_CYCLES
          , eventSubscribedGetParents = return [_eventSubscription_subscribed parentSub]
          , eventSubscribedHasOwnHeightRef = False
          , eventSubscribedWhoCreated = whoCreatedIORef mSubscribedRef
#endif
          }
        }
  pure $ SubscribeResult es occ

subscribe :: Event x a -> Subscriber x a -> EventM x (EventSubscription x)
subscribe e s = subscribeSubscription <$> subscribeAndRead e s

{-# INLINE wrap #-}
wrap :: MonadIO m => (t -> EventSubscribed x) -> (Subscriber x a -> m (WeakBagTicket, t, Maybe a)) -> Subscriber x a -> m (SubscribeResult x a)
wrap tag getSpecificSubscribed sub = do
  (sln, subd, occ) <- getSpecificSubscribed sub
  let es = tag subd
  pure $ SubscribeResult (EventSubscription (WeakBag.remove sln >> touch sln) es) occ

eventRoot :: (GCompare k, HasSpiderTimeline x) => k a -> Root x k -> Event x a
eventRoot !k !r = Event $ wrap eventSubscribedRoot $ liftIO . getRootSubscribed k r

subscribeAndReadNever :: EventM x (SubscribeResult x a)
subscribeAndReadNever = pure $ SubscribeResult (EventSubscription (return ()) eventSubscribedNever) Nothing

eventNever :: Event x a
eventNever = Event $ const subscribeAndReadNever

{-# INLINE eventFan #-}
eventFan :: (HasSpiderTimeline x, IsMap map) => MapKey map a -> Event x (map v) -> Fan x map v -> Event x (v a)
eventFan !k p !self = Event $ \sub -> do
  isEmpty <- liftIO $ mapNull <$> readIORef (_fan_subscribers self)
  parentSubscription <-
    if isEmpty
    then do
      s <- liftIO $ newSubscriberFan self
      SubscribeResult subscription parentOcc <- subscribeAndRead p s
      liftIO $ do
        writeIORef (_fan_subscriptionRef self) $ Just subscription
        writeIORef (_fan_occRef self) parentOcc
      when (isJust parentOcc) $ scheduleClear $ _fan_occRef self
      pure subscription
    else liftIO $ fromMaybe (error "fan: a key has a subscriber, but the parent is not subscribed")
                  <$!> readIORef (_fan_subscriptionRef self)
  liftIO $ do
    subscribers <- readIORef (_fan_subscribers self)
    bag <- case mapLookup k subscribers of
      Nothing -> do
        bag <- WeakBag.empty
        writeIORef (_fan_subscribers self) $! mapInsert k (FanSubscribers bag) subscribers
        pure bag
      Just (FanSubscribers bag) -> pure bag
    ticket <- WeakBag.insert sub bag (_fan_weakSelf self) $ \fan -> do
      modifyIORef' (_fan_subscribers fan) $ mapDelete k
      noKeys <- mapNull <$> readIORef (_fan_subscribers fan)
      when noKeys $
        traverse_ unsubscribe =<< atomicModifyIORef' (_fan_subscriptionRef fan) (Nothing,)
    currentOcc <- readIORef (_fan_occRef self)
    let subscribedParent = _eventSubscription_subscribed parentSubscription
        subscribed = EventSubscribed
          { eventSubscribedHeightRef = eventSubscribedHeightRef subscribedParent
          , eventSubscribedRetained = toAny self
#ifdef DEBUG_CYCLES
          , eventSubscribedGetParents = pure [subscribedParent]
          , eventSubscribedHasOwnHeightRef = False
          , eventSubscribedWhoCreated = whoCreatedIORef $ _fan_subscriptionRef self
#endif
          }
    pure $ SubscribeResult (EventSubscription (WeakBag.remove ticket >> touch ticket) subscribed) $ mapLookup k =<< currentOcc

eventHold :: Hold x p -> Event x p
eventHold !h = Event $ subscribeHoldEvent h

eventDyn :: (HasSpiderTimeline x, Patch p) => Dyn x p -> Event x p
eventDyn !j = Event $ \sub -> getDynHold j >>= \h -> subscribeHoldEvent h sub

{-# INLINE subscribeCoincidenceInner #-}
-- | Returns Maybe Height where Nothing means the height was not raised to the inner's height.
subscribeCoincidenceInner :: HasSpiderTimeline x => Event x a -> Height -> CoincidenceSubscribed x a -> EventM x (Maybe a, Maybe Height, EventSubscribed x)
subscribeCoincidenceInner inner outerHeight subscribed = do
  subInner <- liftIO $ newSubscriberCoincidenceInner subscribed
  SubscribeResult subscription@(EventSubscription _ innerSubd) innerOcc <- subscribeAndRead inner subInner
  innerHeight <- liftIO $ getEventSubscribedHeight innerSubd
  let maybeRaised = raiseHeight outerHeight innerHeight
  defer $ SomeResetCoincidence subscription $ subscribed <$ maybeRaised
  return (innerOcc, maybeRaised, innerSubd)

--------------------------------------------------------------------------------
-- Subscriber
--------------------------------------------------------------------------------

data Subscriber x a = Subscriber
  { subscriberPropagate :: !(a -> EventM x ())
  , subscriberInvalidateHeight :: !(Height -> IO ())
  , subscriberRecalculateHeight :: !(Height -> IO ())
  }

newSubscriberHold :: (HasSpiderTimeline x, Patch p) => Hold x p -> IO (Subscriber x p)
newSubscriberHold h = return $ Subscriber
  { subscriberPropagate = {-# SCC "traverseHold" #-} propagateSubscriberHold h
  , subscriberInvalidateHeight = \_ -> return ()
  , subscriberRecalculateHeight = \_ -> return ()
  }

{-# INLINE newSubscriberFan #-}
newSubscriberFan :: forall x map v. (HasSpiderTimeline x, IsMap map) => Fan x map v -> IO (Subscriber x (map v))
newSubscriberFan self = debugSubscriber ("SubscriberFan " <> showNodeId self) $ Subscriber
  { subscriberPropagate = \a -> {-# SCC "traverseFan" #-} do
#ifdef DEBUG_FRAME
      liftIO $ readIORef (_fan_occRef self) >>= debugOccurrence "fan: two occurrences in one frame"
#endif
      subs <- liftIO $ readIORef $ _fan_subscribers self
      tracePropagate (Proxy :: Proxy x) $ show (mapSize subs) <> " keys subscribed, " <> show (mapSize a) <> " keys firing"
      liftIO $ writeIORef (_fan_occRef self) $ Just a
      scheduleClear $ _fan_occRef self
      mapForIntersection_ subs a $ \(FanSubscribers bag) v -> propagate v bag
  , subscriberInvalidateHeight = \old -> do
      subscribers <- readIORef $ _fan_subscribers self
      mapFor_ subscribers $ \(FanSubscribers bag) -> WeakBag.traverse_ bag $ invalidateSubscriberHeight old
  , subscriberRecalculateHeight = \new -> do
      subscribers <- readIORef $ _fan_subscribers self
      mapFor_ subscribers $ \(FanSubscribers bag) -> WeakBag.traverse_ bag $ recalculateSubscriberHeight new
  }

newSubscriberCoincidenceOuter :: forall x b. HasSpiderTimeline x => CoincidenceSubscribed x b -> IO (Subscriber x (Event x b))
newSubscriberCoincidenceOuter subscribed = debugSubscriber ("SubscriberCoincidenceOuter" <> showNodeId subscribed) $ Subscriber
  { subscriberPropagate = \a -> {-# SCC "traverseCoincidenceOuter" #-} do
      outerHeight <- liftIO $ readIORef $ coincidenceSubscribedHeight subscribed
      tracePropagate (Proxy :: Proxy x) $ "  outerHeight = " <> show outerHeight
      (occ, maybeRaisedToInnerHeight, innerSubd) <- subscribeCoincidenceInner a outerHeight subscribed
      tracePropagate (Proxy :: Proxy x) $ "  isJust occ = " <> show (isJust occ)
      tracePropagate (Proxy :: Proxy x) $ "  innerHeight = " <> show maybeRaisedToInnerHeight

      liftIO $ writeIORef (coincidenceSubscribedInnerParent subscribed) $ Just innerSubd
      scheduleClear $ coincidenceSubscribedInnerParent subscribed
      case occ of
        Nothing ->
          for_ maybeRaisedToInnerHeight $ \innerHeight -> liftIO $ do
            -- During event propagation heights have to stay valid, so invalidate and immediately recalculate
            -- TODO: Why not delay?
            invalidateOwnHeight (coincidenceSubscribedHeight subscribed) (invalidateCoincidenceSubscribers subscribed)
            recalculateOwnHeight (coincidenceSubscribedHeight subscribed) (recalculateCoincidenceSubscribers subscribed) (pure innerHeight)
        Just o -> -- Since it's already firing, no need to adjust height
          subscriberPropagate (coincidenceSubscribedSubscriber subscribed) o
  , subscriberInvalidateHeight  = \_ -> invalidateCoincidenceHeight subscribed
  , subscriberRecalculateHeight = \_ -> recalculateCoincidenceHeight subscribed
  }

newSubscriberCoincidenceInner :: forall x a. HasSpiderTimeline x => CoincidenceSubscribed x a -> IO (Subscriber x a)
newSubscriberCoincidenceInner subscribed = debugSubscriber ("SubscriberCoincidenceInner" <> showNodeId subscribed) $ (coincidenceSubscribedSubscriber subscribed)
  { subscriberInvalidateHeight  = \_ -> invalidateCoincidenceHeight subscribed
  , subscriberRecalculateHeight = \_ -> recalculateCoincidenceHeight subscribed
  }

invalidateSubscriberHeight :: Height -> Subscriber x a -> IO ()
invalidateSubscriberHeight = flip subscriberInvalidateHeight

recalculateSubscriberHeight :: Height -> Subscriber x a -> IO ()
recalculateSubscriberHeight = flip subscriberRecalculateHeight

-- | Propagate everything at the current height
propagate :: forall x a. HasSpiderTimeline x => a -> WeakBag (Subscriber x a) -> EventM x ()
propagate a subscribers = withIncreasedDepth (Proxy::Proxy x) $
  -- Note: in the following traversal, we do not visit nodes that are added to the list during our traversal; they are new events, which will necessarily have full information already, so there is no need to traverse them
  --TODO: Should we check if nodes already have their values before propagating?  Maybe we're re-doing work
  WeakBag.traverse_ subscribers $ \s -> subscriberPropagate s a

--------------------------------------------------------------------------------
-- EventSubscribed
--------------------------------------------------------------------------------

toAny :: a -> Any
toAny = unsafeCoerce

-- Why do we use Any here, instead of just giving eventSubscribedRetained an
-- existential type? Sadly, GHC does not currently know how to unbox types
-- with existentially quantified fields. So instead we just coerce values
-- to type Any on the way in. Since we never coerce them back, this is
-- perfectly safe.
data EventSubscribed x = EventSubscribed
  { eventSubscribedHeightRef :: {-# UNPACK #-} !(IORef Height)
  , eventSubscribedRetained :: {-# NOUNPACK #-} !Any
#ifdef DEBUG_CYCLES
  , eventSubscribedGetParents :: !(IO [EventSubscribed x]) -- For debugging loops
  , eventSubscribedHasOwnHeightRef :: !Bool
  , eventSubscribedWhoCreated :: !(IO [String])
#endif
  }

eventSubscribedRoot :: RootSubscribed x a -> EventSubscribed x
eventSubscribedRoot !r = EventSubscribed
  { eventSubscribedHeightRef = zeroRef
  , eventSubscribedRetained = toAny r
#ifdef DEBUG_CYCLES
  , eventSubscribedGetParents = return []
  , eventSubscribedHasOwnHeightRef = False
  , eventSubscribedWhoCreated = return ["root"]
#endif
  }

eventSubscribedNever :: EventSubscribed x
eventSubscribedNever = EventSubscribed
  { eventSubscribedHeightRef = zeroRef
  , eventSubscribedRetained = toAny ()
#ifdef DEBUG_CYCLES
  , eventSubscribedGetParents = return []
  , eventSubscribedHasOwnHeightRef = False
  , eventSubscribedWhoCreated = return ["never"]
#endif
  }
eventSubscribedNow :: EventSubscribed x
eventSubscribedNow = EventSubscribed
  { eventSubscribedHeightRef = zeroRef
  , eventSubscribedRetained = toAny ()
#ifdef DEBUG_CYCLES
  , eventSubscribedGetParents = return []
  , eventSubscribedHasOwnHeightRef = False
  , eventSubscribedWhoCreated = return ["now"]
#endif
  }

getEventSubscribedHeight :: EventSubscribed x -> IO Height
getEventSubscribedHeight es = readIORef $ eventSubscribedHeightRef es

{-# INLINE eventSubscriptionHeight #-}
eventSubscriptionHeight :: EventSubscription x -> IO Height
eventSubscriptionHeight = getEventSubscribedHeight . _eventSubscription_subscribed

#ifdef DEBUG_CYCLES
whoCreatedEventSubscribed :: EventSubscribed x -> IO [String]
whoCreatedEventSubscribed = eventSubscribedWhoCreated

walkInvalidHeightParents :: EventSubscribed x -> IO [EventSubscribed x]
walkInvalidHeightParents s0 = do
  subscribers <- flip execStateT mempty $ ($ s0) $ fix $ \loop s -> do
    h <- liftIO $ readIORef $ eventSubscribedHeightRef s
    when (isUnmarkedInvalidHeight h) $ do
      when (eventSubscribedHasOwnHeightRef s) $ liftIO $ writeIORef (eventSubscribedHeightRef s) $! invalidHeightBeingTraversed
      modify (s :)
      mapM_ loop =<< liftIO (eventSubscribedGetParents s)
  forM_ subscribers $ \s -> writeIORef (eventSubscribedHeightRef s) $! invalidHeight
  return subscribers
#endif

{-# INLINE subscribeHoldEvent #-}
subscribeHoldEvent :: Hold x p -> Subscriber x p -> EventM x (SubscribeResult x p)
subscribeHoldEvent = subscribeAndRead . holdEvent

--------------------------------------------------------------------------------
-- Behavior
--------------------------------------------------------------------------------

newtype Behavior x a = Behavior { readBehaviorTracked :: BehaviorM x a }

behaviorHold :: Hold x p -> Behavior x (PatchTarget p)
behaviorHold !h = Behavior $ readHoldTracked h

behaviorHoldIdentity :: Hold x (Identity a) -> Behavior x a
behaviorHoldIdentity = behaviorHold

behaviorConst :: a -> Behavior x a
behaviorConst !a = Behavior $ return a

behaviorPull :: Pull x a -> Behavior x a
behaviorPull !p = Behavior $ do
    val <- liftIO $ readIORef $ pullValue p
    case val of
      Just subscribed -> do
        askParentsRef >>= mapM_ (\r -> liftIO $ modifyIORef' r (SomeBehaviorSubscribed (Some (BehaviorSubscribedPull subscribed)) :))
        askInvalidator >>= mapM_ (liftIO . addInvalidatorAmortized (pullSubscribedInvalidators subscribed))
        liftIO $ touch $ pullSubscribedOwnInvalidator subscribed
        return $ pullSubscribedValue subscribed
      Nothing -> do
        i <- liftIO $ newInvalidatorPull p
        wi <- liftIO $ mkWeakPtrWithDebug i "InvalidatorPull"
        parentsRef <- liftIO $ newIORef []
        holdInits <- askBehaviorHoldInits
        a <- liftIO $ runReaderIO (unBehaviorM $ pullCompute p) (Just (wi, parentsRef), holdInits)
        inv0 <- maybeToList <$> askInvalidator
        invsRef <- liftIO $ newIORef $ InvalidatorList (length inv0) invalidatorPruneThreshold inv0
        parents <- liftIO $ readIORef parentsRef
        let subscribed = PullSubscribed
              { pullSubscribedValue = a
              , pullSubscribedInvalidators = invsRef
              , pullSubscribedOwnInvalidator = i
              , pullSubscribedParents = parents
              }
        liftIO $ writeIORef (pullValue p) $ Just subscribed
        askParentsRef >>= mapM_ (\r -> liftIO $ modifyIORef' r (SomeBehaviorSubscribed (Some (BehaviorSubscribedPull subscribed)) :))
        return a

behaviorDyn :: Patch p => Dyn x p -> Behavior x (PatchTarget p)
behaviorDyn !d = Behavior $ readHoldTracked =<< getDynHold d

{-# INLINE readHoldTracked #-}
readHoldTracked :: Hold x p -> BehaviorM x (PatchTarget p)
readHoldTracked h = do
  result <- liftIO $ readIORef $ holdValue h
  askInvalidator >>= mapM_ (liftIO . addInvalidatorAmortized (holdInvalidators h))
  askParentsRef >>= mapM_ (\r -> liftIO $ modifyIORef' r (SomeBehaviorSubscribed (Some (BehaviorSubscribedHold h)) :))
  liftIO $ touch h -- Otherwise, if this gets inlined enough, the hold's parent reference may get collected
  return result

data InvalidatorList x = InvalidatorList
  { invalidatorListSize :: !Int
  , invalidatorListPruneAt :: !Int
  , invalidatorListElems :: ![Weak (Invalidator x)]
  }

emptyInvalidatorList :: InvalidatorList x
emptyInvalidatorList = InvalidatorList
  { invalidatorListSize = 0
  , invalidatorListPruneAt = invalidatorPruneThreshold
  , invalidatorListElems = []
  }

-- | Add invalidator weak ref to invalidator list, pruning finalized entries
-- once the list grows to its "prune at" size.
{-# INLINE addInvalidatorAmortized #-}
addInvalidatorAmortized :: IORef (InvalidatorList x) -> Weak (Invalidator x) -> IO ()
addInvalidatorAmortized ref weakInvalidator = do
  InvalidatorList listSize pruneAt weakInvalidators <- readIORef ref
  if listSize < pruneAt
    then writeIORef ref $! InvalidatorList (listSize + 1) pruneAt (weakInvalidator : weakInvalidators)
    else do
      (liveCount, liveInvalidators) <-
        foldrM
        (\weakInvalidator' (!liveCount, liveInvalidators) ->
            (\case Just _ -> (liveCount + 1, weakInvalidator' : liveInvalidators)
                   Nothing -> (liveCount, liveInvalidators))
            <$> deRefWeak weakInvalidator')
        (0, [])
        weakInvalidators
      writeIORef ref $! InvalidatorList
        (liveCount + 1)
        (max invalidatorPruneThreshold (2 * liveCount))
        (weakInvalidator : liveInvalidators)

invalidatorPruneThreshold :: Int
invalidatorPruneThreshold = 100

{-# INLINABLE readBehaviorUntracked #-}
readBehaviorUntracked :: Defer (SomeHoldInit x) m => Behavior x a -> m a
readBehaviorUntracked b = do
  holdInits <- getDeferralQueue
  liftIO $ runBehaviorM (readBehaviorTracked b) Nothing holdInits --TODO: Specialize readBehaviorTracked to the Nothing and Just cases

--------------------------------------------------------------------------------
-- Dynamic
--------------------------------------------------------------------------------

type DynamicS x p = Dynamic x (PatchTarget p) p

data Dynamic x target p = Dynamic
  { dynamicCurrent :: !(Behavior x target)
  , dynamicUpdated :: Event x p -- This must be lazy; see the comment on holdEvent --TODO: Would this let us eliminate `Dyn`?
  }

deriving instance (HasSpiderTimeline x) => Functor (Dynamic x target)




dynamicHold :: Hold x p -> DynamicS x p
dynamicHold !h = Dynamic
  { dynamicCurrent = behaviorHold h
  , dynamicUpdated = eventHold h
  }

dynamicHoldIdentity :: Hold x (Identity a) -> DynamicS x (Identity a)
dynamicHoldIdentity = dynamicHold

dynamicConst :: PatchTarget p -> DynamicS x p
dynamicConst !a = Dynamic
  { dynamicCurrent = behaviorConst a
  , dynamicUpdated = eventNever
  }

dynamicDyn :: (HasSpiderTimeline x, Patch p) => Dyn x p -> DynamicS x p
dynamicDyn !d = Dynamic
  { dynamicCurrent = behaviorDyn d
  , dynamicUpdated = eventDyn d
  }

dynamicDynIdentity :: HasSpiderTimeline x => Dyn x (Identity a) -> DynamicS x (Identity a)
dynamicDynIdentity = dynamicDyn

--------------------------------------------------------------------------------
-- Combinators
--------------------------------------------------------------------------------

--type role Hold representational
data Hold x p
   = Hold { holdValue :: !(IORef (PatchTarget p))
          , holdInvalidators :: !(IORef (InvalidatorList x))
          , holdEvent :: Event x p -- This must be lazy, or holds cannot be defined before their input Events
          , holdParent :: !(IORef (Maybe (EventSubscription x))) -- Keeps its parent alive (will be undefined until the hold is initialized) --TODO: Probably shouldn't be an IORef
#ifdef DEBUG_NODEIDS
          , holdNodeId :: Int
#endif
          }

-- | A statically allocated 'SpiderTimeline'
data Global

{-# NOINLINE globalSpiderTimelineEnv #-}
globalSpiderTimelineEnv :: SpiderTimelineEnv Global
globalSpiderTimelineEnv = unsafePerformIO unsafeNewSpiderTimelineEnv

-- | Stores all global data relevant to a particular Spider timeline; only one
-- value should exist for each type @x@
newtype SpiderTimelineEnv x = STE {unSTE :: SpiderTimelineEnv' x}
-- We implement SpiderTimelineEnv with a newtype wrapper so
-- we can get the coercions we want safely.
type role SpiderTimelineEnv nominal

data SpiderTimelineEnv' x = SpiderTimelineEnv
  { _spiderTimeline_lock :: {-# UNPACK #-} !(MVar ())
  , _spiderTimeline_eventEnv :: {-# UNPACK #-} !(EventEnv x)
#ifdef DEBUG
  , _spiderTimeline_depth :: {-# UNPACK #-} !(IORef Int)
#endif
  }
type role SpiderTimelineEnv' phantom

instance Eq (SpiderTimelineEnv x) where
  _ == _ = True -- Since only one exists of each type

instance GEq SpiderTimelineEnv where
  a `geq` b = if _spiderTimeline_lock (unSTE a) == _spiderTimeline_lock (unSTE b)
              then Just $ unsafeCoerce Refl -- This unsafeCoerce is safe because the same SpiderTimelineEnv can't have two different 'x' arguments
              else Nothing

data EventEnv x
   = EventEnv { eventEnvAssignments :: !(IORef [SomeAssignment x]) -- Needed for Subscribe
              , eventEnvHoldInits :: !(IORef [SomeHoldInit x]) -- Needed for Subscribe
              , eventEnvDynInits :: !(IORef [SomeDynInit x])
              , eventEnvMergeUpdates :: !(IORef [SomeMergeUpdate x])
              , eventEnvMergeInits :: !(IORef [SomeMergeInit x]) -- Needed for Subscribe
              , eventEnvClears :: !(IORef [Some Clear]) -- Needed for Subscribe
              , eventEnvRootClears :: !(IORef [Some RootClear])
              , eventEnvCurrentHeight :: !(IORef Height) -- Needed for Subscribe
              , eventEnvResetCoincidences :: !(IORef [SomeResetCoincidence x]) -- Needed for Subscribe
              , eventEnvPendingRecalculations :: !(IORef [SomeRecalculateHeight x]) -- Nodes that invalidated their own height this frame and must recalculate it at the end of the frame; populated by 'deferRecalculateHeight', drained by 'runPendingRecalculations'
              , eventEnvDelayedMerges :: !(IORef (HeightQueue (EventM x ())))
              }

{-# INLINE runEventM #-}
runEventM :: EventM x a -> IO a
runEventM = unEventM

asksEventEnv :: forall x a. HasSpiderTimeline x => (EventEnv x -> a) -> EventM x a
asksEventEnv f = return $ f $ _spiderTimeline_eventEnv (unSTE (spiderTimeline :: SpiderTimelineEnv x))

class MonadIO m => Defer a m where
  getDeferralQueue :: m (IORef [a])

{-# INLINE defer #-}
defer :: Defer a m => a -> m ()
defer a = do
  q <- getDeferralQueue
  liftIO $ modifyIORef' q (a:)

instance HasSpiderTimeline x => Defer (SomeAssignment x) (EventM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = asksEventEnv eventEnvAssignments

instance HasSpiderTimeline x => Defer (SomeHoldInit x) (EventM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = asksEventEnv eventEnvHoldInits

instance HasSpiderTimeline x => Defer (SomeDynInit x) (EventM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = asksEventEnv eventEnvDynInits

instance Defer (SomeHoldInit x) (BehaviorM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = BehaviorM $ asks snd

instance HasSpiderTimeline x => Defer (SomeMergeUpdate x) (EventM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = asksEventEnv eventEnvMergeUpdates

instance HasSpiderTimeline x => Defer (SomeMergeInit x) (EventM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = asksEventEnv eventEnvMergeInits

class HasSpiderTimeline x => HasCurrentHeight x m | m -> x where
  getCurrentHeight :: m Height
  scheduleMerge :: Height -> EventM x () -> m ()

instance HasSpiderTimeline x => HasCurrentHeight x (EventM x) where
  {-# INLINE getCurrentHeight #-}
  getCurrentHeight = do
    heightRef <- asksEventEnv eventEnvCurrentHeight
    liftIO $ readIORef heightRef
  {-# INLINE scheduleMerge #-}
  scheduleMerge height subscribed = do
    delayedRef <- asksEventEnv eventEnvDelayedMerges
    liftIO $ modifyIORef' delayedRef $ heightQueueInsert height subscribed

class HasSpiderTimeline x where
  -- | Retrieve the current SpiderTimelineEnv
  spiderTimeline :: SpiderTimelineEnv x

instance HasSpiderTimeline Global where
  spiderTimeline = globalSpiderTimelineEnv

putCurrentHeight :: HasSpiderTimeline x => Height -> EventM x ()
putCurrentHeight h = do
  heightRef <- asksEventEnv eventEnvCurrentHeight
  liftIO $ writeIORef heightRef $! h

instance HasSpiderTimeline x => Defer (Some Clear) (EventM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = asksEventEnv eventEnvClears

{-# INLINE scheduleClear #-}
scheduleClear :: Defer (Some Clear) m => IORef (Maybe a) -> m ()
scheduleClear r = defer $ Some $ Clear r

instance HasSpiderTimeline x => Defer (Some RootClear) (EventM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = asksEventEnv eventEnvRootClears

{-# INLINE scheduleRootClear #-}
scheduleRootClear :: Defer (Some RootClear) m => IORef (DMap k Identity) -> m ()
scheduleRootClear r = defer $ Some $ RootClear r

instance HasSpiderTimeline x => Defer (SomeResetCoincidence x) (EventM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = asksEventEnv eventEnvResetCoincidences

-- Note: hold cannot examine its event until after the phase is over
{-# INLINE [1] hold #-}
hold :: (Patch p, Defer (SomeHoldInit x) m) => PatchTarget p -> Event x p -> m (Hold x p)
hold v0 e = do
  valRef <- liftIO $ newIORef v0
  invsRef <- liftIO $ newIORef emptyInvalidatorList
  parentRef <- liftIO $ newIORef Nothing
#ifdef DEBUG_NODEIDS
  nodeId <- liftIO newNodeId
#endif
  let h = Hold
        { holdValue = valRef
        , holdInvalidators = invsRef
        , holdEvent = e
        , holdParent = parentRef
#ifdef DEBUG_NODEIDS
        , holdNodeId = nodeId
#endif
        }
  defer $ SomeHoldInit h
  return h

{-# INLINE getHoldEventSubscription #-}
getHoldEventSubscription :: forall p x. (HasSpiderTimeline x, Patch p) => Hold x p -> EventM x (EventSubscription x)
getHoldEventSubscription h = do
  ep <- liftIO $ readIORef $ holdParent h
  case ep of
    Just subd -> return subd
    Nothing -> do
      let e = holdEvent h
      subscriptionRef <- liftIO $ newIORef $ error "getHoldEventSubscription: subdRef uninitialized"
      SubscribeResult subscription@(EventSubscription _ _) occ <- subscribeAndRead e =<< liftIO (newSubscriberHold h)
      liftIO $ writeIORef subscriptionRef $! subscription
      case occ of
        Nothing -> return ()
        Just o -> do
          old <- liftIO $ readIORef $ holdValue h
          case apply o old of
            Nothing -> return ()
            Just new -> do
              -- Need to evaluate these so that we don't retain the Hold itself
              v <- liftIO $ evaluate $ holdValue h
              i <- liftIO $ evaluate $ holdInvalidators h
              defer $ SomeAssignment v i new
      liftIO $ writeIORef (holdParent h) $ Just subscription
      return subscription

type BehaviorEnv x = (Maybe (Weak (Invalidator x), IORef [SomeBehaviorSubscribed x]), IORef [SomeHoldInit x])

-- BehaviorM can sample behaviors
newtype BehaviorM x a = BehaviorM { unBehaviorM :: ReaderIO (BehaviorEnv x) a }
  deriving (Functor, Applicative, MonadIO, MonadFix, MonadReader (BehaviorEnv x))

instance Monad (BehaviorM x) where
  {-# INLINE (>>=) #-}
  BehaviorM x >>= f = BehaviorM $ x >>= unBehaviorM . f
#if !MIN_VERSION_base(4,13,0)
  {-# INLINE fail #-}
  fail s = BehaviorM $ fail s
#endif

data BehaviorSubscribed x a
   = forall p. BehaviorSubscribedHold (Hold x p)
   | BehaviorSubscribedPull (PullSubscribed x a)

newtype SomeBehaviorSubscribed x = SomeBehaviorSubscribed (Some (BehaviorSubscribed x))

--type role PullSubscribed representational
data PullSubscribed x a
   = PullSubscribed { pullSubscribedValue :: !a
                    , pullSubscribedInvalidators :: !(IORef (InvalidatorList x))
                    , pullSubscribedOwnInvalidator :: !(Invalidator x)
                    , pullSubscribedParents :: ![SomeBehaviorSubscribed x] -- Need to keep parent behaviors alive, or they won't let us know when they're invalidated
                    }

--type role Pull representational
data Pull x a
   = Pull { pullValue :: !(IORef (Maybe (PullSubscribed x a)))
          , pullCompute :: !(BehaviorM x a)
#ifdef DEBUG_NODEIDS
          , pullNodeId :: Int
#endif
          }

data Invalidator x
   = forall a. InvalidatorPull (Pull x a)
   | forall a. InvalidatorSwitch (SwitchSubscribed x a)

data RootSubscribed x a = forall k. GCompare k => RootSubscribed
  { rootSubscribedKey :: !(k a)
  , rootSubscribedCachedSubscribed :: !(IORef (DMap k (RootSubscribed x))) -- From the original Root
  , rootSubscribedSubscribers :: !(WeakBag (Subscriber x a))
  , rootSubscribedOccurrence :: !(IO (Maybe a)) -- Lookup from rootOccurrence
  , rootSubscribedUninit :: IO ()
  , rootSubscribedWeakSelf :: !(IORef (Weak (RootSubscribed x a))) --TODO: Can we make this a lazy non-IORef and then force it manually to avoid an indirection each time we use it?
#ifdef DEBUG_NODEIDS
  , rootSubscribedNodeId :: Int
#endif
  }

data Root x k
   = Root { rootOccurrence :: !(IORef (DMap k Identity)) -- The currently-firing occurrence of this event
          , rootSubscribed :: !(IORef (DMap k (RootSubscribed x)))
          , rootInit :: !(forall a. k a -> RootTrigger x a -> IO (IO ()))
          }

data SomeHoldInit x = forall p. Patch p => SomeHoldInit !(Hold x p)

data SomeDynInit x = forall p. Patch p => SomeDynInit !(Dyn x p)

data SomeMergeUpdate x = SomeMergeUpdate
  { _someMergeUpdate_update :: !(EventM x [EventSubscription x])
  , _someMergeUpdate_invalidateHeight :: !(IO ())
  , _someMergeUpdate_recalculateHeight :: !(IO ())
  }

newtype SomeMergeInit x = SomeMergeInit { unSomeMergeInit :: EventM x () }

-- EventM can do everything BehaviorM can, plus create holds
newtype EventM x a = EventM { unEventM :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadException, MonadAsyncException, MonadCatch, MonadThrow, MonadMask)

newtype MergeSubscribedParent x a = MergeSubscribedParent { unMergeSubscribedParent :: EventSubscription x }

data MergeSubscribedParentWithMove x k a = MergeSubscribedParentWithMove
  { _mergeSubscribedParentWithMove_subscription :: !(EventSubscription x)
  , _mergeSubscribedParentWithMove_key :: !(IORef (k a))
  }

class IsMap (map :: (i -> Type) -> Type) where
  type MapKey map :: i -> Type
  mapEmpty :: map v
  mapNull :: map v -> Bool
  mapSize :: map v -> Int
  mapLookup :: MapKey map a -> map v -> Maybe (v a)
  mapInsert :: MapKey map a -> v a -> map v -> map v
  mapDelete :: MapKey map a -> map v -> map v
  mapForIntersection_ :: Monad m => map v -> map w -> (forall a. v a -> w a -> m ()) -> m ()
  mapFor_ :: Monad m => map v -> (forall a. v a -> m ()) -> m ()
  mapToList :: map v -> [DSum (MapKey map) v]
  mapFromDistinctAscList :: [DSum (MapKey map) v] -> map v

instance GCompare k => IsMap (DMap k) where
  type MapKey (DMap k) = k
  mapEmpty = DMap.empty
  mapNull = DMap.null
  mapSize = DMap.size
  mapLookup = DMap.lookup
  mapInsert = DMap.insert
  mapDelete = DMap.delete
  mapForIntersection_ left right f =
    DMap.foldrWithKey (\_ (Pair v w) rest -> f v w >> rest) (pure ())
    $ DMap.intersectionWithKey (\_ -> Pair) left right
  mapFor_ m f = DMap.foldrWithKey (\_ v rest -> f v >> rest) (pure ()) m
  mapToList = DMap.toList
  mapFromDistinctAscList = DMap.fromDistinctAscList

newtype IntMapOf a v = IntMapOf (IntMap (v a))

instance IsMap (IntMapOf a) where
  type MapKey (IntMapOf a) = Const2 Int a
  mapEmpty = IntMapOf IntMap.empty
  mapNull (IntMapOf m) = IntMap.null m
  mapSize (IntMapOf m) = IntMap.size m
  mapLookup (Const2 k) (IntMapOf m) = IntMap.lookup k m
  mapInsert (Const2 k) v (IntMapOf m) = IntMapOf $ IntMap.insert k v m
  mapDelete (Const2 k) (IntMapOf m) = IntMapOf $ IntMap.delete k m
  mapForIntersection_ (IntMapOf left) (IntMapOf right) f = traverse_ (uncurry f) $ IntMap.intersectionWith (,) left right
  mapFor_ (IntMapOf m) f = traverse_ f m
  mapToList (IntMapOf m) = (\(k, v) -> Const2 k :=> v) <$> IntMap.toList m
  mapFromDistinctAscList entries = IntMapOf $ IntMap.fromDistinctAscList $ fmap toPair entries
    where
      toPair :: forall b w. DSum (Const2 Int b) w -> (Int, w b)
      toPair (Const2 k :=> v) = (k, v)

newtype FanSubscribers x v a = FanSubscribers (WeakBag (Subscriber x (v a)))

data Fan x map v = Fan
  { _fan_subscribers :: {-# UNPACK #-} !(IORef (map (FanSubscribers x v)))
  , _fan_subscriptionRef :: {-# UNPACK #-} !(IORef (Maybe (EventSubscription x)))
  , _fan_occRef :: {-# UNPACK #-} !(IORef (Maybe (map v)))
  , _fan_weakSelf :: !(IORef (Weak (Fan x map v)))
#ifdef DEBUG_NODEIDS
  , _fan_nodeId :: {-# UNPACK #-} !Int
#endif
  }

{-# INLINE newFan #-}
newFan :: IsMap map => IO (Fan x map v)
newFan = do
  subscribers <- newIORef mapEmpty
  subscriptionRef <- newIORef Nothing
  occRef <- newIORef Nothing
  weakSelf <- newIORef $ error "newFan: weakSelf not initialized"
#ifdef DEBUG_NODEIDS
  nodeId <- newNodeId
#endif
  let !self = Fan
        { _fan_subscribers = subscribers
        , _fan_subscriptionRef = subscriptionRef
        , _fan_occRef = occRef
        , _fan_weakSelf = weakSelf
#ifdef DEBUG_NODEIDS
        , _fan_nodeId = nodeId
#endif
        }
  writeIORef weakSelf =<< mkWeakPtrWithDebug self "Fan"
  pure self

data SwitchSubscribed x a
   = SwitchSubscribed { switchSubscribedHeight :: !(IORef Height)
                      , switchSubscribedSubscriber :: !(Subscriber x a)
                      , switchSubscribedOwnInvalidator :: !(IORef (Invalidator x))
                      , switchSubscribedOwnWeakInvalidator :: !(IORef (Weak (Invalidator x)))
                      , switchSubscribedBehaviorParents :: !(IORef [SomeBehaviorSubscribed x])
                      , switchSubscribedParent :: !(Behavior x (Event x a))
                      , switchSubscribedCurrentParent :: !(IORef (EventSubscription x))
#ifdef DEBUG_NODEIDS
                      , switchSubscribedNodeId :: Int
#endif
                      }

data CoincidenceSubscribed x a
   = CoincidenceSubscribed { coincidenceSubscribedHeight :: !(IORef Height)
                           , coincidenceSubscribedSubscriber :: !(Subscriber x a)
                           , coincidenceSubscribedOuterParent :: !(IORef (EventSubscription x))
                           , coincidenceSubscribedInnerParent :: !(IORef (Maybe (EventSubscribed x)))
#ifdef DEBUG_NODEIDS
                           , coincidenceSubscribedNodeId :: Int
#endif
                           }

{-# NOINLINE newInvalidatorSwitch #-}
newInvalidatorSwitch :: SwitchSubscribed x a -> IO (Invalidator x)
newInvalidatorSwitch subd = return $! InvalidatorSwitch subd

{-# NOINLINE newInvalidatorPull #-}
newInvalidatorPull :: Pull x a -> IO (Invalidator x)
newInvalidatorPull p = return $! InvalidatorPull p

instance HasSpiderTimeline x => Filterable (Event x) where
  mapMaybe f = push $ return . f

instance HasSpiderTimeline x => Align (Event x) where
  nil = eventNever
#if MIN_VERSION_these(0, 8, 0)
instance HasSpiderTimeline x => Semialign (Event x) where
#endif
  align ea eb = mapMaybe dmapToThese $ mergeG coerce $ dynamicConst $
     DMap.fromDistinctAscList [LeftTag :=> ea, RightTag :=> eb]

#ifdef MIN_VERSION_semialign
#if MIN_VERSION_semialign(1,1,0)
instance HasSpiderTimeline x => Zip (Event x) where
#endif
  zip x y = mapMaybe justThese $ align x y
#endif

data DynType x p = UnsafeDyn !(BehaviorM x (PatchTarget p), Event x p)
                 | BuildDyn  !(EventM x (PatchTarget p), Event x p)
                 | HoldDyn   !(Hold x p)

newtype Dyn (x :: Type) p = Dyn { unDyn :: IORef (DynType x p) }

newMapDyn :: HasSpiderTimeline x => (a -> b) -> DynamicS x (Identity a) -> DynamicS x (Identity b)
newMapDyn f d = dynamicDynIdentity $ unsafeBuildDynamic (fmap f $ readBehaviorTracked $ dynamicCurrent d) (Identity . f . runIdentity <$> dynamicUpdated d)

--TODO: Avoid the duplication between this and R.zipDynWith
zipDynWith :: HasSpiderTimeline x => (a -> b -> c) -> DynamicS x (Identity a) -> DynamicS x (Identity b) -> DynamicS x (Identity c)
zipDynWith f da db =
  let eab = align (dynamicUpdated da) (dynamicUpdated db)
      ec = flip push eab $ \o -> do
        (a, b) <- case o of
          This (Identity a) -> do
            b <- readBehaviorUntracked $ dynamicCurrent db
            return (a, b)
          That (Identity b) -> do
            a <- readBehaviorUntracked $ dynamicCurrent da
            return (a, b)
          These (Identity a) (Identity b) -> return (a, b)
        return $ Just $ Identity $ f a b
  in dynamicDynIdentity $ unsafeBuildDynamic (f <$> readBehaviorUntracked (dynamicCurrent da) <*> readBehaviorUntracked (dynamicCurrent db)) ec

buildDynamic :: (Defer (SomeDynInit x) m, Patch p) => EventM x (PatchTarget p) -> Event x p -> m (Dyn x p)
buildDynamic readV0 v' = do
  result <- liftIO $ newIORef $ BuildDyn (readV0, v')
  let !d = Dyn result
  defer $ SomeDynInit d
  return d

unsafeBuildDynamic :: BehaviorM x (PatchTarget p) -> Event x p -> Dyn x p
unsafeBuildDynamic readV0 v' =
  Dyn $ unsafePerformIO $ newIORef $ UnsafeDyn (readV0, v')

-- ResultM can read behaviors and events
type ResultM = EventM

instance HasSpiderTimeline x => Functor (Event x) where
  fmap f = push $ return . Just . f

instance Functor (Behavior x) where
  fmap f = pull . fmap f . readBehaviorTracked

{-# INLINE push #-}
push :: HasSpiderTimeline x => (a -> ComputeM x (Maybe b)) -> Event x a -> Event x b
push f e = cacheEvent (pushCheap f e)

{-# INLINABLE pull #-}
pull :: BehaviorM x a -> Behavior x a
pull a = unsafePerformIO $ do
  ref <- newIORef Nothing
#ifdef DEBUG_NODEIDS
  nid <- newNodeId
#endif
  pure $ behaviorPull $ Pull
    { pullCompute = a
    , pullValue = ref
#ifdef DEBUG_NODEIDS
    , pullNodeId = nid
#endif
    }

{-# INLINE switch #-}
switch :: HasSpiderTimeline x => Behavior x (Event x a) -> Event x a
switch b = cacheEvent (switchUncached b)

{-# INLINE [1] switchUncached #-}
switchUncached :: forall x a. HasSpiderTimeline x => Behavior x (Event x a) -> Event x a
switchUncached b = Event $ \sub -> do
  heightRef <- liftIO $ newIORef $ error "switchUncached: height not yet known"
  currentParentRef <- liftIO $ newIORef $ error "switchUncached: current parent not yet subscribed"
  invalidatorRef <- liftIO $ newIORef $ error "switchUncached: invalidator not yet created"
  weakInvalidatorRef <- liftIO $ newIORef $ error "switchUncached: weak invalidator not yet created"
  parentsRef <- liftIO $ newIORef [] --TODO: This should be unnecessary, because it will always be filled with just the single parent behavior
#ifdef DEBUG_NODEIDS
  nid <- liftIO newNodeId
#endif
  let !subscribed = SwitchSubscribed
        { switchSubscribedHeight = heightRef
        , switchSubscribedSubscriber = debugSubscriber' ("SubscriberSwitch" <> showNodeId subscribed) $ sub
            { subscriberInvalidateHeight = \_ -> invalidateOwnHeight heightRef $ subscriberInvalidateHeight sub
            , subscriberRecalculateHeight = recalculateOwnHeight heightRef (subscriberRecalculateHeight sub) . pure
            }
        , switchSubscribedOwnInvalidator = invalidatorRef
        , switchSubscribedOwnWeakInvalidator = weakInvalidatorRef
        , switchSubscribedBehaviorParents = parentsRef
        , switchSubscribedParent = b
        , switchSubscribedCurrentParent = currentParentRef
#ifdef DEBUG_NODEIDS
        , switchSubscribedNodeId = nid
#endif
        }
  i <- liftIO $ newInvalidatorSwitch subscribed
  wi <- liftIO $ mkWeakPtrWithDebug i "InvalidatorSwitch"
  liftIO $ writeIORef invalidatorRef i
  liftIO $ writeIORef weakInvalidatorRef wi
  holdInits <- getDeferralQueue
  e <- liftIO $ runBehaviorM (readBehaviorTracked b) (Just (wi, parentsRef)) holdInits
  SubscribeResult subscription parentOcc <- subscribeAndRead e $ switchSubscribedSubscriber subscribed
  liftIO $ writeIORef heightRef =<< eventSubscriptionHeight subscription
  liftIO $ writeIORef currentParentRef subscription
  pure $ SubscribeResult
    (EventSubscription
      { _eventSubscription_unsubscribe = do
          unsubscribe =<< readIORef currentParentRef
          finalize =<< readIORef weakInvalidatorRef -- We don't need to get invalidated if we're dead
      , _eventSubscription_subscribed = EventSubscribed
          { eventSubscribedHeightRef = heightRef
          , eventSubscribedRetained = toAny subscribed
#ifdef DEBUG_CYCLES
          , eventSubscribedGetParents = do
              s <- readIORef currentParentRef
              return [_eventSubscription_subscribed s]
          , eventSubscribedHasOwnHeightRef = True
          , eventSubscribedWhoCreated = whoCreatedIORef currentParentRef
#endif
          }
      })
    parentOcc

{-# INLINE coincidence #-}
coincidence :: HasSpiderTimeline x => Event x (Event x a) -> Event x a
coincidence e = cacheEvent (coincidenceUncached e)

{-# INLINE [1] coincidenceUncached #-}
coincidenceUncached :: forall x a. HasSpiderTimeline x => Event x (Event x a) -> Event x a
coincidenceUncached outer = Event $ \sub -> do
  heightRef <- liftIO $ newIORef $ error "coincidenceUncached: height not yet known"
  outerParentRef <- liftIO $ newIORef $ error "coincidenceUncached: outer parent not yet subscribed"
  innerParentRef <- liftIO $ newIORef Nothing
#ifdef DEBUG_NODEIDS
  nid <- liftIO newNodeId
#endif
  let !subscribed = CoincidenceSubscribed
        { coincidenceSubscribedHeight = heightRef
        , coincidenceSubscribedSubscriber = sub
        , coincidenceSubscribedOuterParent = outerParentRef
        , coincidenceSubscribedInnerParent = innerParentRef
#ifdef DEBUG_NODEIDS
        , coincidenceSubscribedNodeId = nid
#endif
        }
  subOuter <- liftIO $ newSubscriberCoincidenceOuter subscribed
  SubscribeResult outerSubscription outerOcc <- subscribeAndRead outer subOuter
  liftIO $ writeIORef outerParentRef outerSubscription
  outerHeight <- liftIO $ eventSubscriptionHeight outerSubscription
  (occ, height, mInnerSubd) <- case outerOcc of
    Nothing -> return (Nothing, outerHeight, Nothing)
    Just o -> do
      (occ, maybeRaisedToInnerHeight, innerSubd) <- subscribeCoincidenceInner o outerHeight subscribed
      return (occ, fromMaybe outerHeight maybeRaisedToInnerHeight, Just innerSubd)
  liftIO $ writeIORef heightRef $! height
  liftIO $ writeIORef innerParentRef mInnerSubd
  scheduleClear innerParentRef
  pure $ SubscribeResult
    (EventSubscription
      { _eventSubscription_unsubscribe = unsubscribe =<< readIORef outerParentRef
      , _eventSubscription_subscribed = EventSubscribed
          { eventSubscribedHeightRef = heightRef
          , eventSubscribedRetained = toAny subscribed
#ifdef DEBUG_CYCLES
          , eventSubscribedGetParents = do
              outerParent <- _eventSubscription_subscribed <$> readIORef outerParentRef
              innerParents <- maybeToList <$> readIORef innerParentRef
              return $ outerParent : innerParents
          , eventSubscribedHasOwnHeightRef = True
          , eventSubscribedWhoCreated = whoCreatedIORef outerParentRef
#endif
          }
      })
    occ

-- | Propagate root triggers and process delayed merges, then run the
-- given read action. Runs inside the single 'runFrame' of
-- 'runHostFrameFireAndRead', after the build and hold-init phases.
propagateAndRead :: forall x b. HasSpiderTimeline x => [DSum (RootTrigger x) Identity] -> ResultM x b -> EventM x b
propagateAndRead roots after = do
    forM_ roots $ \(RootTrigger (_, occRef, k) :=> a) -> do
      occBefore <- liftIO $ do
        occBefore <- readIORef occRef
#ifdef DEBUG_FRAME
        debugOccurrence "propagateAndRead: one trigger fired twice in one frame" $ DMap.lookup k occBefore
#endif
        writeIORef occRef $! DMap.insert k a occBefore
        return occBefore
      when (DMap.null occBefore) $ scheduleRootClear occRef
    forM_ roots $ \(RootTrigger (subscribersRef, _, _) :=> Identity a) ->
      propagate a subscribersRef
    delayedRef <- asksEventEnv eventEnvDelayedMerges
    let go = do
          delayed <- liftIO $ readIORef delayedRef
          case heightQueueNext delayed of
            Nothing -> return ()
            Just (currentHeight, cur, future) -> do
              tracePropagate (Proxy :: Proxy x) $ "Running height " ++ show currentHeight
              putCurrentHeight currentHeight
              liftIO $ writeIORef delayedRef $! future
              sequence_ cur
              go
    go
    putCurrentHeight maxHeight
    after

newtype Clear a = Clear (IORef (Maybe a))

newtype RootClear k = RootClear (IORef (DMap k Identity))

data SomeAssignment x = forall a. SomeAssignment {-# UNPACK #-} !(IORef a) {-# UNPACK #-} !(IORef (InvalidatorList x)) a

debugFinalize :: Bool
debugFinalize = False

mkWeakPtrWithDebug :: a -> String -> IO (Weak a)
mkWeakPtrWithDebug x debugNote = do
  x' <- evaluate x
  mkWeakPtr x' $
    if debugFinalize
    then Just $ debugStrLn $ "finalizing: " ++ debugNote
    else Nothing

type CanTrace x m = (HasSpiderTimeline x, MonadIO m)


#ifdef DEBUG

debugSubscriber :: forall x a. HasSpiderTimeline x => String -> Subscriber x a -> IO (Subscriber x a)
debugSubscriber description = return . debugSubscriber' description

debugSubscriber' :: forall x a. HasSpiderTimeline x => String -> Subscriber x a -> Subscriber x a
debugSubscriber' description subscribed = Subscriber
  {
    subscriberPropagate = \m -> do
      tracePropagate (Proxy :: Proxy x) ("subscriberPropagate: " <> description)
      subscriberPropagate subscribed m
  , subscriberInvalidateHeight = \old -> do
      traceInvalidateHeight $ "invalidateSubscriberHeight: " <> description <> ", old = " <> show old
      subscriberInvalidateHeight subscribed old
      traceInvalidateHeight $ "invalidateSubscriberHeight: " <> description <> ", done"
  , subscriberRecalculateHeight = \new -> do
      traceInvalidateHeight $ "subscriberRecalculateHeight: " <> description <> ", new = " <> show new
      subscriberRecalculateHeight subscribed new
      traceInvalidateHeight $ "subscriberRecalculateHeight: " <> description <> ", done"
  }


{-# INLINE withIncreasedDepth #-}
withIncreasedDepth :: forall proxy x m a. CanTrace x m => proxy x -> m a -> m a
withIncreasedDepth _ a = do
  liftIO $ modifyIORef' (_spiderTimeline_depth $ unSTE (spiderTimeline :: SpiderTimelineEnv x)) succ
  result <- a
  liftIO $ modifyIORef' (_spiderTimeline_depth $ unSTE (spiderTimeline :: SpiderTimelineEnv x)) pred
  return result

{-# INLINE tracePropagate #-}
tracePropagate :: (CanTrace x m) => proxy x -> String -> m ()
tracePropagate p = when debugPropagate . trace p

{-# INLINE traceInvalidate #-}
traceInvalidate :: String -> IO ()
traceInvalidate = when debugInvalidate . liftIO . debugStrLn

{-# INLINE traceInvalidateHeight #-}
traceInvalidateHeight :: String -> IO ()
traceInvalidateHeight = when debugInvalidateHeight . liftIO . debugStrLn

{-# INLINE trace #-}
trace :: (CanTrace x m) => proxy x ->  String -> m ()
trace p message = traceM p $ return message

{-# INLINE traceM #-}
traceM :: forall x proxy m. (CanTrace x m) => proxy x -> m String -> m ()
traceM _ getMessage = do
  message <- getMessage
  d <- liftIO $ readIORef $ _spiderTimeline_depth $ unSTE (spiderTimeline :: SpiderTimelineEnv x)
  liftIO $ debugStrLn $ replicate d ' ' <> message

#else

{-# INLINE withIncreasedDepth #-}
withIncreasedDepth ::  proxy x -> m a -> m a
withIncreasedDepth _ = id

{-# INLINE tracePropagate #-}
tracePropagate :: (CanTrace x m) => proxy x -> String -> m ()
tracePropagate _ _ = return ()

{-# INLINE traceInvalidate #-}
traceInvalidate :: String -> IO ()
traceInvalidate _ = return ()

{-# INLINE traceInvalidateHeight #-}
traceInvalidateHeight :: String -> IO ()
traceInvalidateHeight _ = return ()

{-# INLINE debugSubscriber #-}
debugSubscriber :: String -> Subscriber x a -> IO (Subscriber x a)
debugSubscriber _ = return

{-# INLINE debugSubscriber' #-}
debugSubscriber' :: String -> Subscriber x a -> Subscriber x a
debugSubscriber' _ = id



{-# INLINE trace #-}
trace :: (CanTrace x m) => proxy x ->  String -> m ()
trace _ _ = return ()

{-# INLINE traceM #-}
traceM :: (CanTrace x m) => proxy x -> m String -> m ()
traceM _ _ = return ()

#endif

whoCreatedIORef :: IORef a -> IO [String]
whoCreatedIORef (IORef a) = whoCreated $! a

groupByHead :: Eq a => [NonEmpty a] -> [(a, NonEmpty [a])]
groupByHead = \case
  [] -> []
  (x :| xs) : t -> case groupByHead t of
    [] -> [(x, xs :| [])]
    l@((y, yss) : t')
      | x == y -> (x, xs `NonEmpty.cons` yss) : t'
      | otherwise -> (x, xs :| []) : l

listsToForest :: Eq a => [[a]] -> Forest a
listsToForest lists = buildForest <$> groupByHead (mapMaybe nonEmpty lists)
    where buildForest (a, lists') = Node a $ listsToForest $ toList lists'

showStacks :: [[String]] -> String
showStacks = drawForest . listsToForest . fmap (filterStack "Reflex.Spider.Internal")

filterStack :: String -> [String] -> [String]
#ifdef DEBUG_HIDE_INTERNALS
filterStack prefix = filter (not . (prefix `isPrefixOf`))
#else
filterStack _prefix = id
#endif

#ifdef DEBUG_CYCLES

data EventLoopException = EventLoopException [[String]]
instance Exception EventLoopException

instance Show EventLoopException where
  show (EventLoopException stacks) = "causality loop detected:\n" <> if null stacks
    then "no location information, compile with profiling enabled for stack tree"
    else showStacks stacks

#else

data EventLoopException = EventLoopException
instance Exception EventLoopException

instance Show EventLoopException where
  show EventLoopException = "causality loop detected: \n" <>
    "compile reflex with flag 'debug-cycles' and compile with profiling enabled for stack tree"

#endif


{-# INLINE propagateSubscriberHold #-}
propagateSubscriberHold :: forall x p. (HasSpiderTimeline x, Patch p) => Hold x p -> p -> EventM x ()
propagateSubscriberHold h a = do
  {-# SCC "trace" #-} when debugPropagate $ traceM (Proxy :: Proxy x) $ liftIO $ do
    InvalidatorList n _ _ <- liftIO $ readIORef $ holdInvalidators h
    return $ "SubscriberHold" <> showNodeId h <> ": " ++ show n

  v <- {-# SCC "read" #-} liftIO $ readIORef $ holdValue h
  case {-# SCC "apply" #-} apply a v of
    Nothing -> return ()
    Just v' -> do
      {-# SCC "trace2" #-} withIncreasedDepth (Proxy :: Proxy x) $
        tracePropagate (Proxy :: Proxy x) ("propagateSubscriberHold: assigning Hold" <> showNodeId h)
      vRef <- {-# SCC "vRef" #-} liftIO $ evaluate $ holdValue h
      iRef <- {-# SCC "iRef" #-} liftIO $ evaluate $ holdInvalidators h
      defer $ {-# SCC "assignment" #-} SomeAssignment vRef iRef v'

-- | 'CoincidenceSubscribed' is present only when the coincidence raised its height while subscribing to the inner event.
data SomeResetCoincidence x = forall a. SomeResetCoincidence !(EventSubscription x) !(Maybe (CoincidenceSubscribed x a))

newtype SomeRecalculateHeight x = SomeRecalculateHeight (IO ())

runBehaviorM :: BehaviorM x a -> Maybe (Weak (Invalidator x), IORef [SomeBehaviorSubscribed x]) -> IORef [SomeHoldInit x] -> IO a
runBehaviorM a mwi holdInits = runReaderIO (unBehaviorM a) (mwi, holdInits)

askInvalidator :: BehaviorM x (Maybe (Weak (Invalidator x)))
askInvalidator = do
  (!m, _) <- ask
  case m of
    Nothing -> return Nothing
    Just (!wi, _) -> return $ Just wi

askParentsRef :: BehaviorM x (Maybe (IORef [SomeBehaviorSubscribed x]))
askParentsRef = do
  (!m, _) <- ask
  case m of
    Nothing -> return Nothing
    Just (_, !p) -> return $ Just p

askBehaviorHoldInits :: BehaviorM x (IORef [SomeHoldInit x])
askBehaviorHoldInits = do
  (_, !his) <- ask
  return his

{-# INLINE getDynHold #-}
getDynHold :: (Defer (SomeHoldInit x) m, Patch p) => Dyn x p -> m (Hold x p)
getDynHold d = do
  mh <- liftIO $ readIORef $ unDyn d
  case mh of
    HoldDyn h -> return h
    UnsafeDyn (readV0, v') -> do
      holdInits <- getDeferralQueue
      v0 <- liftIO $ runBehaviorM readV0 Nothing holdInits
      hold' v0 v'
    BuildDyn (readV0, v') -> do
      v0 <- liftIO $ runEventM readV0
      hold' v0 v'
  where
    hold' v0 v' = do
      h <- hold v0 v'
      liftIO $ writeIORef (unDyn d) $ HoldDyn h
      return h


-- Always refers to 0
{-# NOINLINE zeroRef #-}
zeroRef :: IORef Height
zeroRef = unsafePerformIO $ newIORef zeroHeight

getRootSubscribed :: forall k x a. (GCompare k, HasSpiderTimeline x) => k a -> Root x k -> Subscriber x a -> IO (WeakBagTicket, RootSubscribed x a, Maybe a)
getRootSubscribed k r sub = do
  mSubscribed <- readIORef $ rootSubscribed r
  let getOcc = fmap (coerce . DMap.lookup k) $ readIORef $ rootOccurrence r
  case DMap.lookup k mSubscribed of
    Just subscribed -> {-# SCC "hitRoot" #-} do
      sln <- subscribeRootSubscribed subscribed sub
      occ <- getOcc
      return (sln, subscribed, occ)
    Nothing -> {-# SCC "missRoot" #-} do
      weakSelf <- newIORef $ error "getRootSubscribed: weakSelfRef not initialized"
      let !cached = rootSubscribed r
      uninitRef <- newIORef $ error "getRootsubscribed: uninitRef not initialized"
      (subs, sln) <- WeakBag.singleton sub weakSelf cleanupRootSubscribed

      tracePropagate (Proxy::Proxy x) $  "getRootSubscribed: calling rootInit"

      uninit <- rootInit r k $ RootTrigger (subs, rootOccurrence r, k)
      writeIORef uninitRef $! uninit
#ifdef DEBUG_NODEIDS
      nid <- newNodeId
#endif
      let !subscribed = RootSubscribed
            { rootSubscribedKey = k
            , rootSubscribedCachedSubscribed = cached
            , rootSubscribedOccurrence = getOcc
            , rootSubscribedSubscribers = subs
            , rootSubscribedUninit = uninit
            , rootSubscribedWeakSelf = weakSelf
#ifdef DEBUG_NODEIDS
            , rootSubscribedNodeId = nid
#endif
            }
          -- If we die at the same moment that all our children die, they will
          -- try to clean us up but will fail because their Weak reference to us
          -- will also be dead.  So, if we are dying, check if there are any
          -- children; since children don't bother cleaning themselves up if
          -- their parents are already dead, I don't think there's a race
          -- condition here.  However, if there are any children, then we can
          -- infer that we need to clean ourselves up, so we do.
          finalCleanup = do
            cs <- readIORef $ _weakBag_children subs
            when (not $ IntMap.null cs) (cleanupRootSubscribed subscribed)
      writeIORef weakSelf =<< evaluate =<< mkWeakPtr subscribed (Just finalCleanup)
      modifyIORef' (rootSubscribed r) $ DMap.insertWith (error $ "getRootSubscribed: duplicate key inserted into Root") k subscribed --TODO: I think we can just write back mSubscribed rather than re-reading it
      occ <- getOcc
      return (sln, subscribed, occ)

{-# ANN cleanupRootSubscribed "HLint: ignore Redundant bracket" #-}
cleanupRootSubscribed :: RootSubscribed x a -> IO ()
cleanupRootSubscribed self@RootSubscribed { rootSubscribedKey = k, rootSubscribedCachedSubscribed = cached } = do
  rootSubscribedUninit self
  modifyIORef' cached $ DMap.delete k

{-# INLINE subscribeRootSubscribed #-}
subscribeRootSubscribed :: RootSubscribed x a -> Subscriber x a -> IO WeakBagTicket
subscribeRootSubscribed subscribed sub = WeakBag.insert sub (rootSubscribedSubscribers subscribed) (rootSubscribedWeakSelf subscribed) cleanupRootSubscribed

newtype EventSelectorInt x a = EventSelectorInt { selectInt :: Int -> Event x a }

{-# INLINE mergeG #-}
mergeG :: forall k q x v. (HasSpiderTimeline x, GCompare k)
  => (forall a. q a -> Event x (v a))
  -> DynamicS x (PatchDMap k q) -> Event x (DMap k v)
mergeG nt d = cacheEvent (mergeCheap nt d)

{-# INLINE mergeWithMove #-}
mergeWithMove :: forall k v q x. (HasSpiderTimeline x, GCompare k)
  => (forall a. q a -> Event x (v a))
  -> DynamicS x (PatchDMapWithMove k q) -> Event x (DMap k v)
mergeWithMove nt d = cacheEvent (mergeCheapWithMove nt d)

{-# INLINE [1] mergeCheap #-}
mergeCheap
  :: forall k x q v. (HasSpiderTimeline x, GCompare k)
  => (forall a. q a -> Event x (v a))
  -> DynamicS x (PatchDMap k q)
  -> Event x (DMap k v)
mergeCheap nt = genericMergeCheap unMergeSubscribedParent getInitialSubscribers updateMe
  where
      updateMe :: MergeUpdateFunc (DMap k) v x (PatchDMap k q) (MergeSubscribedParent x)
      updateMe subscriber heightBagRef oldParents (PatchDMap p) =
        updateMergeParents nt subscriber heightBagRef oldParents p

      getInitialSubscribers :: MergeInitFunc (DMap k) v (DMap k q) x (MergeSubscribedParent x)
      getInitialSubscribers = initMergeParents nt (\k -> pure (pure k, MergeSubscribedParent))

{-# INLINE [1] mergeCheapWithMove #-}
mergeCheapWithMove :: forall k x v q. (HasSpiderTimeline x, GCompare k)
  => (forall a. q a -> Event x (v a))
  -> DynamicS x (PatchDMapWithMove k q)
  -> Event x (DMap k v)
mergeCheapWithMove nt = genericMergeCheap _mergeSubscribedParentWithMove_subscription getInitialSubscribers updateMe
  where
      updateMe :: MergeUpdateFunc (DMap k) v x (PatchDMapWithMove k q) (MergeSubscribedParentWithMove x k)
      updateMe subscriber heightBagRef oldParents p = do
        -- Prepare new parents for insertion
        let subscribeParent :: forall a. k a -> Event x (v a) -> EventM x (MergeSubscribedParentWithMove x k a)
            subscribeParent k e = do
              keyRef <- liftIO $ newIORef k
              (`MergeSubscribedParentWithMove` keyRef) <$>
                subscribeMergeParent heightBagRef e (subscriber $ liftIO $ readIORef keyRef)
        p' <- PatchDMapWithMove.traversePatchDMapWithMoveWithKey (\k q -> subscribeParent k (nt q)) p
        -- Collect old parents for deletion and update the keys of moved parents
        let moveOrDelete :: forall a. k a -> PatchDMapWithMove.NodeInfo k q a -> MergeSubscribedParentWithMove x k a -> Constant (EventM x (Maybe (EventSubscription x))) a
            moveOrDelete _ ni parent = Constant $ case getComposeMaybe $ PatchDMapWithMove._nodeInfo_to ni of
              Nothing -> do
                let subscription = _mergeSubscribedParentWithMove_subscription parent
                liftIO $ forgetMergeParentHeight heightBagRef subscription
                return $ Just subscription
              Just toKey -> do
                liftIO $ writeIORef (_mergeSubscribedParentWithMove_key parent) $! toKey
                return Nothing
        toDelete <- fmap catMaybes $ mapM (\(_ :=> v) -> getConstant v) $ DMap.toList $
          DMap.intersectionWithKey moveOrDelete (unPatchDMapWithMove p) oldParents

        return (toDelete, applyAlways p' oldParents)
      getInitialSubscribers :: MergeInitFunc (DMap k) v (DMap k q) x (MergeSubscribedParentWithMove x k)
      getInitialSubscribers = initMergeParents nt parentAt
        where
          -- A moved parent keeps its subscription, so its subscriber reads the
          -- key from a ref that the patch function updates.
          parentAt :: forall a. k a -> EventM x (EventM x (k a), EventSubscription x -> MergeSubscribedParentWithMove x k a)
          parentAt k = do
            keyRef <- liftIO $ newIORef k
            pure (liftIO $ readIORef keyRef, (`MergeSubscribedParentWithMove` keyRef))

type MergeUpdateFunc map v x p s
   = (forall a. EventM x (MapKey map a) -> Subscriber x (v a))
  -> IORef HeightBag
  -> map s
  -> p
  -> EventM x ([EventSubscription x], map s)

type MergeGetSubscription x s = forall a. s a -> EventSubscription x

type MergeInitFunc map v t x s
   = t
  -> (forall a. EventM x (MapKey map a) -> Subscriber x (v a))
  -> EventM x (map v, [Height], map s)

{-# INLINE initMergeParents #-}
initMergeParents
  :: forall map q v x s. IsMap map
  => (forall a. q a -> Event x (v a))
  -> (forall a. MapKey map a -> EventM x (EventM x (MapKey map a), EventSubscription x -> s a))
  -> MergeInitFunc map v (map q) x s
initMergeParents nt parentAt initialParents subscriber = do
  occurrencesRef <- liftIO $ newIORef mapEmpty
  heightsRef <- liftIO $ newIORef []
  parents <- forM (mapToList initialParents) $ \(k :=> e) -> do
    (getKey, toParent) <- parentAt k
    SubscribeResult subscription parentOcc <- subscribeAndRead (nt e) (subscriber getKey)
    liftIO $ do
      for_ parentOcc $ \occurrence -> modifyIORef' occurrencesRef $ mapInsert k occurrence
      height <- eventSubscriptionHeight subscription
      modifyIORef' heightsRef (height :)
    pure $ k :=> toParent subscription
  occurrences <- liftIO $ readIORef occurrencesRef
  heights <- liftIO $ readIORef heightsRef --TODO: Assert that there's no invalidHeight in here
  pure (occurrences, heights, mapFromDistinctAscList parents)

{-# INLINE updateMergeParents #-}
updateMergeParents
  :: forall map q v x. IsMap map
  => (forall a. q a -> Event x (v a))
  -> MergeUpdateFunc map v x (map (ComposeMaybe q)) (MergeSubscribedParent x)
updateMergeParents nt subscriber heightBagRef oldParents patch =
  foldM updateParent ([], oldParents) $ mapToList patch
  where
    updateParent (subscriptionsToKill, parents) (k :=> ComposeMaybe maybeEvent) = do
      let maybeOldSubscribed = mapLookup k parents
      newParents <- case maybeEvent of
        Nothing -> pure $ mapDelete k parents
        Just e -> do
          newParent <- MergeSubscribedParent <$> subscribeMergeParent heightBagRef (nt e) (subscriber $ pure k)
          pure $ mapInsert k newParent parents
      for_ maybeOldSubscribed $
        liftIO . forgetMergeParentHeight heightBagRef . unMergeSubscribedParent
      pure ( maybeToList (unMergeSubscribedParent <$> maybeOldSubscribed) ++ subscriptionsToKill
           , newParents
           )

subscribeMergeParent :: IORef HeightBag -> Event x a -> Subscriber x a -> EventM x (EventSubscription x)
subscribeMergeParent heightBagRef e sub = do
  subscription <- subscribe e sub
  liftIO $ modifyIORef' heightBagRef . heightBagAdd =<< eventSubscriptionHeight subscription
  pure subscription

forgetMergeParentHeight :: IORef HeightBag -> EventSubscription x -> IO ()
forgetMergeParentHeight heightBagRef subscription =
  modifyIORef' heightBagRef . heightBagRemove =<< eventSubscriptionHeight subscription

checkCycle :: HasSpiderTimeline x => EventSubscribed x -> EventM x ()
checkCycle subscribed = do
    height <- liftIO $ readIORef (eventSubscribedHeightRef subscribed)
    case height of
      ValidHeight -> do
#ifdef DEBUG
        currentHeight <- getCurrentHeight
        when ((height `compareHeight` currentHeight) /= GT) $
          error $ "checkCycle: merge height " <> show height <> " not above current height " <> show currentHeight
#else
        pure ()
#endif
      InvalidHeight -> liftIO $
#ifdef DEBUG_CYCLES
          do
            nodesInvolvedInCycle <- walkInvalidHeightParents subscribed
            stacks <- forM nodesInvolvedInCycle whoCreatedEventSubscribed
            throwIO (EventLoopException stacks)
#else
          throwIO EventLoopException
#endif


{-# INLINE genericMergeCheap #-}
genericMergeCheap :: forall map v x p s. (HasSpiderTimeline x, IsMap map)
  => MergeGetSubscription x s -> MergeInitFunc map v (PatchTarget p) x s -> MergeUpdateFunc map v x p s -> DynamicS x p -> Event x (map v)
genericMergeCheap getParent getInitialSubscribers updateFunc d = Event $ \sub -> do
  initialParents <- readBehaviorUntracked $ dynamicCurrent d
  accumRef <- liftIO $ newIORef $ error "merge: accumRef not yet initialized"
  heightRef <- liftIO $ newIORef $ error "merge: heightRef not yet initialized"
  heightBagRef <- liftIO $ newIORef $ error "merge: heightBagRef not yet initialized"
  parentsRef :: IORef (map s) <- liftIO $ newIORef $ error "merge: parentsRef not yet initialized"
  changeSubdRef <- liftIO $ newIORef $ error "merge: changeSubdRef not yet initialized"

  let subscribed = EventSubscribed
        { eventSubscribedHeightRef = heightRef
        , eventSubscribedRetained = toAny (parentsRef, changeSubdRef)
#ifdef DEBUG_CYCLES
      , eventSubscribedGetParents =
          fmap (\(_ :=> parent) -> _eventSubscription_subscribed (getParent parent)) . mapToList <$> readIORef parentsRef
      , eventSubscribedHasOwnHeightRef = True
      , eventSubscribedWhoCreated = whoCreatedIORef heightRef
#endif
      }

      numParents = mapSize <$> readIORef parentsRef

      scheduleSelf height = scheduleMerge height propagateAtHeight

      -- propagateAtHeight and propagateAccumulated are defined outside
      -- scheduleSelf so that they are allocated once per subscription
      -- instead of on every call.
      propagateAtHeight = do
        height <- liftIO $ readIORef heightRef
        currentHeight <- getCurrentHeight
        case height `compareHeight` currentHeight of
          LT -> error "Somehow a merge's height has been decreased after it was scheduled"
          GT -> scheduleMerge height propagateAtHeight -- The height has been increased by a coincidence event.
          EQ -> propagateAccumulated

      propagateAccumulated = do
        vals <- liftIO $ readIORef accumRef
        liftIO $ writeIORef accumRef $! mapEmpty
        --TODO: Assert that vals is not empty
        subscriberPropagate sub vals

      invalidateMyHeight = invalidateOwnHeight heightRef (subscriberInvalidateHeight sub)

      recalculateMyHeight = recalculateOwnHeight heightRef (subscriberRecalculateHeight sub) $ do
        n <- numParents
        heights <- readIORef heightBagRef
        pure $ case heightBagSize heights `compare` n of
          LT -> invalidHeight
          EQ -> heightBagSuccHeight heights
          GT -> error $ "merge: more heights (" <> show (heightBagSize heights) <> ") than parents (" <> show n <> ")"

      invalidateMyHeightOnParentChange = do
        n <- numParents
        heights <- readIORef heightBagRef
        own <- readIORef heightRef
        case own of
          ValidHeight ->
            when (heightBagSize heights /= n || compareHeight own (heightBagSuccHeight heights) /= EQ)
              invalidateMyHeight
          InvalidHeight -> pure ()

      {-# INLINE parentSubscriber #-}
      parentSubscriber :: forall a. EventM x (MapKey map a) -> Subscriber x (v a)
      parentSubscriber getKey = Subscriber
        { subscriberPropagate = \a -> do
            checkCycle subscribed
            oldM <- liftIO $ readIORef accumRef
            k <- getKey
#ifdef DEBUG_FRAME
            debugOccurrence "merge: one parent fired twice in one frame" $ mapLookup k oldM
#endif
            let newM = mapInsert k a oldM
            tracePropagate (Proxy :: Proxy x) $ "  size oldM = " <> show (mapSize oldM) <> "; size newM = " <> show (mapSize newM)
            liftIO $ writeIORef accumRef $! newM
            when (mapNull oldM) $ do -- Only schedule the firing once
              height <- liftIO $ readIORef heightRef
              scheduleSelf height
        , subscriberInvalidateHeight = \old -> do
            heights <- readIORef heightBagRef
            let heights' = heightBagRemove old heights
            writeIORef heightBagRef $! heights'
            own <- readIORef heightRef
            case own of
              ValidHeight ->
                when (compareHeight (heightBagSuccHeight heights') (heightBagSuccHeight heights) == LT)
                  invalidateMyHeight
              InvalidHeight -> pure ()
        , subscriberRecalculateHeight = \new -> do
            modifyIORef' heightBagRef $ heightBagAdd new
            own <- readIORef heightRef
            case own of
              ValidHeight -> when (compareHeight own new /= GT) invalidateMyHeight
              InvalidHeight -> pure ()
            recalculateMyHeight
        }

      --TODO: Be able to run as much of this as possible promptly
      updateParents patch = do
        oldParents <- liftIO $ readIORef parentsRef
        (subscriptionsToKill, newParents) <- updateFunc parentSubscriber heightBagRef oldParents patch
        liftIO $ writeIORef parentsRef $! newParents
        return subscriptionsToKill

      deferParentUpdate patch =
        defer $ SomeMergeUpdate (updateParents patch) invalidateMyHeightOnParentChange recalculateMyHeight

  (dm, heights, initialParentState) <- getInitialSubscribers initialParents parentSubscriber
  let (myHeightBag, myHeight) = heightBagFromParents heights
  currentHeight <- getCurrentHeight
  let (occ, accum) = if heightSlotPassed currentHeight myHeight -- The merge would have fired already in this frame
                     then (if mapNull dm then Nothing else Just dm, mapEmpty)
                     else (Nothing, dm)
  unless (mapNull accum) $ scheduleSelf myHeight
  liftIO $ writeIORef accumRef $! accum
  liftIO $ writeIORef heightRef $! myHeight
  liftIO $ writeIORef heightBagRef $! myHeightBag
  liftIO $ writeIORef parentsRef $! initialParentState
  defer $ SomeMergeInit $ do
    let changeSubscriber = terminalSubscriber $ \patch -> {-# SCC "traverseMergeChange" #-} do
          tracePropagate (Proxy :: Proxy x) "SubscriberMerge/Change"
          deferParentUpdate patch
    SubscribeResult changeSubscription change <- subscribeAndRead (dynamicUpdated d) changeSubscriber
    for_ change deferParentUpdate
    -- We explicitly hold on to the unsubscribe function from subscribing to the update event.
    -- If we don't do this, there are certain cases where mergeCheap will fail to properly retain
    -- its subscription.
    liftIO $ writeIORef changeSubdRef (changeSubscriber, changeSubscription)
  let unsubscribeAll = readIORef parentsRef >>= \parents ->
        mapFor_ parents $ unsubscribe . getParent
  pure $ SubscribeResult (EventSubscription unsubscribeAll subscribed) occ


mergeInt :: forall x a. (HasSpiderTimeline x) => DynamicS x (PatchIntMap (Event x a)) -> Event x (IntMap a)
mergeInt = cacheEvent . mergeIntCheap

{-# INLINABLE mergeIntCheap #-}
mergeIntCheap :: forall x a. (HasSpiderTimeline x) => DynamicS x (PatchIntMap (Event x a)) -> Event x (IntMap a)
mergeIntCheap = coerceEvent . genericMergeCheap unMergeSubscribedParent getInitialSubscribers updateMe
  where
      coerceEvent :: Event x (IntMapOf a Identity) -> Event x (IntMap a)
      coerceEvent = coerce

      eventOfIdentity :: forall b. Event x b -> Event x (Identity b)
      eventOfIdentity = coerce

      updateMe :: MergeUpdateFunc (IntMapOf a) Identity x (PatchIntMap (Event x a)) (MergeSubscribedParent x)
      updateMe subscriber heightBagRef oldParents (PatchIntMap p) =
        updateMergeParents eventOfIdentity subscriber heightBagRef oldParents (IntMapOf (coerce p) :: IntMapOf a (ComposeMaybe (Event x)))

      -- Not re-using 'initMergeParents' here; inserting each parent into
      -- IntMaps directly allocates less than going through lists of 'DSum'
      -- entries with 'Const2' keys and 'mapFromDistinctAscList'.
      getInitialSubscribers :: MergeInitFunc (IntMapOf a) Identity (IntMap (Event x a)) x (MergeSubscribedParent x)
      getInitialSubscribers initialParents subscriber = do
        occurrencesRef <- liftIO $ newIORef IntMap.empty
        parentsRef <- liftIO $ newIORef IntMap.empty
        heights <- forM (IntMap.toList initialParents) $ \(k, e) -> do
          SubscribeResult subscription parentOcc <- subscribeAndRead (eventOfIdentity e) (subscriber $ pure $ Const2 k)
          liftIO $ do
            for_ parentOcc $ \occurrence -> modifyIORef' occurrencesRef $ IntMap.insert k occurrence
            modifyIORef' parentsRef $ IntMap.insert k $ MergeSubscribedParent subscription
            eventSubscriptionHeight subscription
        occurrences <- liftIO $ readIORef occurrencesRef
        parents <- liftIO $ readIORef parentsRef
        pure (IntMapOf occurrences, heights, IntMapOf parents)

newtype EventSelector x k = EventSelector { select :: forall a. k a -> Event x a }
newtype EventSelectorG x k v = EventSelectorG { selectG :: forall a. k a -> Event x (v a) }

fanG :: (HasSpiderTimeline x, GCompare k) => Event x (DMap k v) -> EventSelectorG x k v
fanG e = unsafePerformIO $ do
  self <- newFan
  pure $ EventSelectorG $ \k -> eventFan k e self

fanInt :: forall x a. HasSpiderTimeline x => Event x (IntMap a) -> EventSelectorInt x a
fanInt e = unsafePerformIO $ do
  self <- newFan
  pure $ EventSelectorInt $ \k -> coerce $ eventFan (Const2 k) (coerce e :: Event x (IntMapOf a Identity)) self

runHoldInits :: HasSpiderTimeline x => IORef [SomeHoldInit x] -> IORef [SomeDynInit x] -> IORef [SomeMergeInit x] -> EventM x ()
runHoldInits holdInitRef dynInitRef mergeInitRef = do
  holdInits <- liftIO $ readIORef holdInitRef
  dynInits <- liftIO $ readIORef dynInitRef
  mergeInits <- liftIO $ readIORef mergeInitRef
  unless (null holdInits && null dynInits && null mergeInits) $ do
    liftIO $ writeIORef holdInitRef []
    liftIO $ writeIORef dynInitRef []
    liftIO $ writeIORef mergeInitRef []
    mapM_ initHold holdInits
    mapM_ initDyn dynInits
    mapM_ unSomeMergeInit mergeInits
    runHoldInits holdInitRef dynInitRef mergeInitRef

initHold :: HasSpiderTimeline x => SomeHoldInit x -> EventM x ()
initHold (SomeHoldInit h) = void $ getHoldEventSubscription h

initDyn :: HasSpiderTimeline x => SomeDynInit x -> EventM x ()
initDyn (SomeDynInit d) = void $ getDynHold d

newEventEnv :: IO (EventEnv x)
newEventEnv = do
  toAssignRef <- newIORef [] -- This should only actually get used when events are firing
  holdInitRef <- newIORef []
  dynInitRef <- newIORef []
  mergeUpdateRef <- newIORef []
  mergeInitRef <- newIORef []
  heightRef <- newIORef zeroHeight
  toClearRef <- newIORef []
  toClearRootRef <- newIORef []
  coincidenceInfosRef <- newIORef []
  pendingRecalculationsRef <- newIORef []
  delayedRef <- newIORef heightQueueEmpty
  return $ EventEnv toAssignRef holdInitRef dynInitRef mergeUpdateRef mergeInitRef toClearRef toClearRootRef heightRef coincidenceInfosRef pendingRecalculationsRef delayedRef

clearEventEnv :: EventEnv x -> IO ()
clearEventEnv (EventEnv toAssignRef holdInitRef dynInitRef mergeUpdateRef mergeInitRef toClearRef toClearRootRef heightRef coincidenceInfosRef _pendingRecalculationsRef delayedRef) = do
  writeIORef toAssignRef []
  writeIORef holdInitRef []
  writeIORef dynInitRef []
  writeIORef mergeUpdateRef []
  writeIORef mergeInitRef []
  writeIORef heightRef zeroHeight
  writeIORef toClearRef []
  writeIORef toClearRootRef []
  writeIORef coincidenceInfosRef []
#ifdef DEBUG_FRAME
  do delayed <- readIORef delayedRef
     case heightQueueNext delayed of
       Nothing -> pure ()
       Just _ -> error "clearEventEnv: unprocessed scheduled merge"
#endif
  writeIORef delayedRef heightQueueEmpty

-- | Run an event action outside of a frame
runFrame :: forall x a. HasSpiderTimeline x => EventM x a -> SpiderHost x a --TODO: This function also needs to hold the mutex
runFrame a = SpiderHost $ do
  let go = do
        result <- a
        runHoldInits (eventEnvHoldInits env) (eventEnvDynInits env) (eventEnvMergeInits env) -- This must happen before doing the assignments, in case subscribing a Hold causes existing Holds to be read by the newly-propagated events
        return result
  result <- runEventM go
  clearOccurrences
  toAssign <- readIORef $ eventEnvAssignments env
  toReconnectRef <- newIORef []
  coincidenceInfos <- readIORef $ eventEnvResetCoincidences env
  forM_ toAssign $ \(SomeAssignment vRef iRef v) -> {-# SCC "assignment" #-} do
    writeIORef vRef v
    traceInvalidate $ "Invalidating Hold"
    writeIORef iRef =<< evaluate =<< invalidate toReconnectRef =<< readIORef iRef
  mergeUpdates <- readIORef $ eventEnvMergeUpdates env
  writeIORef (eventEnvMergeUpdates env) []
  tracePropagate (Proxy::Proxy x) $ "Updating merges"
  mergeSubscriptionsToKill <- runEventM $ do
    subscriptionsToKill <- concat <$> mapM _someMergeUpdate_update mergeUpdates
    -- Subscribing to a new merge parent coming from a patch can cause new
    -- deferred inits being added, evaluate them:
    runHoldInits (eventEnvHoldInits env) (eventEnvDynInits env) (eventEnvMergeInits env)
    pure subscriptionsToKill
  tracePropagate (Proxy::Proxy x) $ "Updating merges done"
  toReconnect <- readIORef toReconnectRef
  clearEventEnv env
  switchSubscriptionsToKill <- forM toReconnect $ \(SomeSwitchSubscribed subscribed) -> {-# SCC "switchSubscribed" #-} do
    oldSubscription <- readIORef $ switchSubscribedCurrentParent subscribed
    wi <- readIORef $ switchSubscribedOwnWeakInvalidator subscribed
    traceInvalidate $ "Finalizing invalidator for Switch" <> showNodeId subscribed
    finalize wi
    i <- readIORef $ switchSubscribedOwnInvalidator subscribed
    wi' <- mkWeakPtrWithDebug i "wi'"
    writeIORef (switchSubscribedOwnWeakInvalidator subscribed) $! wi'
    writeIORef (switchSubscribedBehaviorParents subscribed) []
    writeIORef (eventEnvHoldInits env) [] --TODO: Should we reuse this?
    e <- runBehaviorM (readBehaviorTracked (switchSubscribedParent subscribed)) (Just (wi', switchSubscribedBehaviorParents subscribed)) $ eventEnvHoldInits env
    runEventM $ runHoldInits (eventEnvHoldInits env) (eventEnvDynInits env) (eventEnvMergeInits env) --TODO: Is this actually OK? It seems like it should be, since we know that no events are firing at this point, but it still seems inelegant
    --TODO: Make sure we touch the pieces of the SwitchSubscribed at the appropriate times
    subscription <- {-# SCC "subscribeSwitch" #-} subscribeReconnect e (switchSubscribedSubscriber subscribed) --TODO: Assert that the event isn't firing
    {-
    stackTrace <- liftIO $ fmap renderStack $ ccsToStrings =<< (getCCSOf $! switchSubscribedParent subscribed)
    liftIO $ debugStrLn $ (++stackTrace) $ "subd' subscribed to " ++ case e of
      EventRoot _ -> "EventRoot"
      EventNever -> "EventNever"
      _ -> "something else"
    -}
    writeIORef (switchSubscribedCurrentParent subscribed) $! subscription
    return oldSubscription
  liftIO $ mapM_ unsubscribe mergeSubscriptionsToKill
  liftIO $ mapM_ unsubscribe switchSubscriptionsToKill
  forM_ toReconnect $ \(SomeSwitchSubscribed subscribed) -> {-# SCC "switchSubscribed" #-} do
    parentHeight <- eventSubscriptionHeight =<< readIORef (switchSubscribedCurrentParent subscribed)
    myHeight <- readIORef $ switchSubscribedHeight subscribed
    case myHeight of
      InvalidHeight -> pure ()
      ValidHeight -> do
        let heightChanged = case parentHeight of
              ValidHeight -> compareHeight parentHeight myHeight /= EQ
              InvalidHeight -> True
        when heightChanged $
          subscriberInvalidateHeight (switchSubscribedSubscriber subscribed) myHeight
  mapM_ _someMergeUpdate_invalidateHeight mergeUpdates
  forM_ coincidenceInfos $ \(SomeResetCoincidence subscription mInvalidate) -> do
    unsubscribe subscription
    mapM_ invalidateCoincidenceHeight mInvalidate
  runPendingRecalculations env
  mapM_ _someMergeUpdate_recalculateHeight mergeUpdates
  forM_ toReconnect $ \(SomeSwitchSubscribed subscribed) -> do
    height <- calculateSwitchHeight subscribed
    subscriberRecalculateHeight (switchSubscribedSubscriber subscribed) height
#ifdef DEBUG_FRAME
  do leftover <- readIORef $ eventEnvPendingRecalculations env
     unless (null leftover) $
       error "runFrame: pending recalculations at frame end"
#endif
  return result
  where
    env = _spiderTimeline_eventEnv $ unSTE (spiderTimeline :: SpiderTimelineEnv x)

    clearOccurrences :: IO ()
    clearOccurrences = do
      toClear <- readIORef $ eventEnvClears env
      forM_ toClear $ \(Some (Clear ref)) -> {-# SCC "clear" #-} writeIORef ref Nothing
      toClearRoot <- readIORef $ eventEnvRootClears env
      forM_ toClearRoot $ \(Some (RootClear ref)) -> {-# SCC "rootClear" #-} writeIORef ref $! DMap.empty

    subscribeReconnect :: forall b. Event x b -> Subscriber x b -> IO (EventSubscription x)
    subscribeReconnect e sub = do
#ifdef DEBUG
      pendingRecalculationsBefore <- length <$> readIORef (eventEnvPendingRecalculations env)
#endif
      result <- runEventM $ do
        subscription <- subscribe e sub
        runHoldInits (eventEnvHoldInits env) (eventEnvDynInits env) (eventEnvMergeInits env)
        return subscription
      clearOccurrences
#ifdef DEBUG
      toAssign <- readIORef $ eventEnvAssignments env
      coincidenceInfos <- readIORef $ eventEnvResetCoincidences env
      pendingRecalculationsAfter <- length <$> readIORef (eventEnvPendingRecalculations env)
      unless (null toAssign && null coincidenceInfos && pendingRecalculationsAfter == pendingRecalculationsBefore) $ error "subscribeReconnect: the reconnect subscribe deferred a hold assignment, coincidence reset or height recalculation, which should be impossible when no event is firing"
#endif
      clearEventEnv env
      return result

--------------------------------------------------------------------------------
-- Height algo
--------------------------------------------------------------------------------

-- | The first phase of a height change: notify downstream of the height you're
-- leaving. The heightRef reads invalidHeight before notifying and until
-- the recalculation phase.
{-# INLINE invalidateOwnHeight #-}
invalidateOwnHeight :: IORef Height -> (Height -> IO ()) -> IO ()
invalidateOwnHeight heightRef notify = do
  oldHeight <- readIORef heightRef
  case oldHeight of
    InvalidHeight -> pure ()
    ValidHeight -> do
      writeIORef heightRef $! invalidHeight
      notify oldHeight

-- | The second phase of a height change: the heightRef reads the new height
-- during downstream notification of recalculation.
{-# INLINE recalculateOwnHeight #-}
recalculateOwnHeight :: IORef Height -> (Height -> IO ()) -> IO Height -> IO ()
recalculateOwnHeight heightRef notify calculate = do
  oldHeight <- readIORef heightRef
  case oldHeight of
    ValidHeight -> pure ()
    InvalidHeight -> calculate >>= \case
      InvalidHeight -> pure ()
      newHeight@ValidHeight -> do
        traceInvalidateHeight $ "recalculateOwnHeight: height: " <> show newHeight
        writeIORef heightRef $! newHeight
        notify newHeight

-- | Ask for a height recalculation at the end of the current frame.
--
-- Call this from the node that just invalidated its own height.  Every
-- invalidation owes exactly one recalculation, or the height bags of the
-- subscribers stay one entry short and never complete again.
{-# INLINE deferRecalculateHeight #-}
deferRecalculateHeight :: forall x. HasSpiderTimeline x => SomeRecalculateHeight x -> IO ()
deferRecalculateHeight recalculate = modifyIORef' (eventEnvPendingRecalculations env) (recalculate :)
  where env = _spiderTimeline_eventEnv (unSTE (spiderTimeline :: SpiderTimelineEnv x))

runPendingRecalculations :: EventEnv x -> IO ()
runPendingRecalculations env = go
  where
    go = readIORef (eventEnvPendingRecalculations env) >>= \case
      [] -> pure ()
      pending -> do
        writeIORef (eventEnvPendingRecalculations env) []
        forM_ pending $ \(SomeRecalculateHeight recalculate) -> recalculate
        go

invalidateCoincidenceHeight :: forall x a. HasSpiderTimeline x => CoincidenceSubscribed x a -> IO ()
invalidateCoincidenceHeight subscribed =
  invalidateOwnHeight (coincidenceSubscribedHeight subscribed) $ \oldHeight -> do
    deferRecalculateHeight @x $ SomeRecalculateHeight $ recalculateCoincidenceHeight subscribed
    invalidateCoincidenceSubscribers subscribed oldHeight

{-# INLINE invalidateCoincidenceSubscribers #-}
invalidateCoincidenceSubscribers :: CoincidenceSubscribed x a -> Height -> IO ()
invalidateCoincidenceSubscribers = subscriberInvalidateHeight . coincidenceSubscribedSubscriber

{-# INLINE recalculateCoincidenceSubscribers #-}
recalculateCoincidenceSubscribers :: CoincidenceSubscribed x a -> Height -> IO ()
recalculateCoincidenceSubscribers = subscriberRecalculateHeight . coincidenceSubscribedSubscriber

recalculateCoincidenceHeight :: CoincidenceSubscribed x a -> IO ()
recalculateCoincidenceHeight subscribed =
  recalculateOwnHeight (coincidenceSubscribedHeight subscribed) (recalculateCoincidenceSubscribers subscribed) (calculateCoincidenceHeight subscribed)

calculateSwitchHeight :: SwitchSubscribed x a -> IO Height
calculateSwitchHeight subscribed = eventSubscriptionHeight =<< readIORef (switchSubscribedCurrentParent subscribed)

calculateCoincidenceHeight :: CoincidenceSubscribed x a -> IO Height
calculateCoincidenceHeight subscribed = do
  outerHeight <- eventSubscriptionHeight =<< readIORef (coincidenceSubscribedOuterParent subscribed)
  innerHeight <- maybe (return zeroHeight) getEventSubscribedHeight =<< readIORef (coincidenceSubscribedInnerParent subscribed)
  pure $ coincidenceHeight outerHeight innerHeight

data SomeSwitchSubscribed x = forall a. SomeSwitchSubscribed {-# NOUNPACK #-} (SwitchSubscribed x a)

invalidate :: IORef [SomeSwitchSubscribed x] -> InvalidatorList x -> IO (InvalidatorList x)
invalidate toReconnectRef (InvalidatorList _ _ wis) = do
  forM_ wis $ \wi -> do
    mi <- deRefWeak wi
    case mi of
      Nothing -> do
        traceInvalidate "invalidate Dead"
        return () --TODO: Should we clean this up here?
      Just i -> do
        finalize wi -- Once something's invalidated, it doesn't need to hang around; this will change when some things are strict
        case i of
          InvalidatorPull p -> do
            traceInvalidate $ "invalidate: Pull" <> showNodeId p
            mVal <- readIORef $ pullValue p
            forM_ mVal $ \val -> do
              writeIORef (pullValue p) Nothing
              writeIORef (pullSubscribedInvalidators val) =<< evaluate =<< invalidate toReconnectRef =<< readIORef (pullSubscribedInvalidators val)
          InvalidatorSwitch subscribed -> do
            traceInvalidate $ "invalidate: Switch" <> showNodeId subscribed
            modifyIORef' toReconnectRef (SomeSwitchSubscribed subscribed :)
  return emptyInvalidatorList -- Since we always finalize everything, always return an empty list --TODO: There are some things that will need to be re-subscribed every time; we should try to avoid finalizing them

--------------------------------------------------------------------------------
-- Reflex integration
--------------------------------------------------------------------------------

-- | Designates the default, global Spider timeline
data SpiderTimeline x
type role SpiderTimeline nominal

-- | The default, global Spider environment
type Spider = SpiderTimeline Global

instance HasSpiderTimeline x => Reflex.Class.MonadSample (SpiderTimeline x) (EventM x) where
  {-# SPECIALIZE instance Reflex.Class.MonadSample (SpiderTimeline Global) (EventM Global) #-}
  {-# INLINABLE sample #-}
  sample (SpiderBehavior b) = readBehaviorUntracked b

instance HasSpiderTimeline x => Reflex.Class.MonadHold (SpiderTimeline x) (EventM x) where
  {-# SPECIALIZE instance Reflex.Class.MonadHold (SpiderTimeline Global) (EventM Global) #-}
  {-# INLINABLE hold #-}
  hold = holdSpiderEventM
  {-# INLINABLE holdDyn #-}
  holdDyn = holdDynSpiderEventM
  {-# INLINABLE holdIncremental #-}
  holdIncremental = holdIncrementalSpiderEventM
  {-# INLINABLE buildDynamic #-}
  buildDynamic = buildDynamicSpiderEventM
  {-# INLINABLE headE #-}
  headE (SpiderEvent e) = SpiderEvent <$> Reflex.Spider.Internal.headE e
  {-# INLINABLE now #-}
  now = nowSpiderEventM

instance Reflex.Class.MonadSample (SpiderTimeline x) (SpiderPullM x) where
  {-# INLINABLE sample #-}
  sample = coerce . readBehaviorTracked . unSpiderBehavior

instance HasSpiderTimeline x => Reflex.Class.MonadSample (SpiderTimeline x) (SpiderPushM x) where
  {-# SPECIALIZE instance Reflex.Class.MonadSample (SpiderTimeline Global) (SpiderPushM Global) #-}
  {-# INLINABLE sample #-}
  sample (SpiderBehavior b) = SpiderPushM $ readBehaviorUntracked b

instance HasSpiderTimeline x => Reflex.Class.MonadHold (SpiderTimeline x) (SpiderPushM x) where
  {-# SPECIALIZE instance Reflex.Class.MonadHold (SpiderTimeline Global) (SpiderPushM Global) #-}
  {-# INLINABLE hold #-}
  hold v0 e = Reflex.Class.current <$> Reflex.Class.holdDyn v0 e
  {-# INLINABLE holdDyn #-}
  holdDyn v0 (SpiderEvent e) = SpiderPushM $ fmap (SpiderDynamic . dynamicHoldIdentity) $ Reflex.Spider.Internal.hold v0 $ coerce e
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 (SpiderEvent e) = SpiderPushM $ SpiderIncremental . dynamicHold <$> Reflex.Spider.Internal.hold v0 e
  {-# INLINABLE buildDynamic #-}
  buildDynamic getV0 (SpiderEvent e) = SpiderPushM $ fmap (SpiderDynamic . dynamicDynIdentity) $ Reflex.Spider.Internal.buildDynamic (coerce getV0) $ coerce e
  {-# INLINABLE headE #-}
  headE (SpiderEvent e) = SpiderPushM $ SpiderEvent <$> Reflex.Spider.Internal.headE e
  {-# INLINABLE now #-}
  now = SpiderPushM nowSpiderEventM


instance HasSpiderTimeline x => Monad (Reflex.Class.Dynamic (SpiderTimeline x)) where
  {-# INLINE (>>=) #-}
  x >>= f = SpiderDynamic $ dynamicDynIdentity $ newJoinDyn $ newMapDyn (unSpiderDynamic . f) $ unSpiderDynamic x
#if !MIN_VERSION_base(4,13,0)
  {-# INLINE fail #-}
  fail _ = error "Dynamic does not support 'fail'"
#endif

{-# INLINABLE newJoinDyn #-}
newJoinDyn :: HasSpiderTimeline x => DynamicS x (Identity (DynamicS x (Identity a))) -> Reflex.Spider.Internal.Dyn x (Identity a)
newJoinDyn d =
  let readV0 = readBehaviorTracked . dynamicCurrent =<< readBehaviorTracked (dynamicCurrent d)
      eOuter = Reflex.Spider.Internal.push (fmap (Just . Identity) . readBehaviorUntracked . dynamicCurrent . runIdentity) $ dynamicUpdated d
      eInner = Reflex.Spider.Internal.switch $ dynamicUpdated <$> dynamicCurrent d
      eBoth = Reflex.Spider.Internal.coincidence $ dynamicUpdated . runIdentity <$> dynamicUpdated d
      v' = unSpiderEvent $ Reflex.Class.leftmost $ map SpiderEvent [eBoth, eOuter, eInner]
  in Reflex.Spider.Internal.unsafeBuildDynamic readV0 v'

instance HasSpiderTimeline x => Functor (Reflex.Class.Dynamic (SpiderTimeline x)) where
  fmap = mapDynamicSpider
  x <$ d = R.unsafeBuildDynamic (return x) $ x <$ R.updated d

mapDynamicSpider :: HasSpiderTimeline x => (a -> b) -> Reflex.Class.Dynamic (SpiderTimeline x) a -> Reflex.Class.Dynamic (SpiderTimeline x) b
mapDynamicSpider f = SpiderDynamic . newMapDyn f . unSpiderDynamic
{-# INLINE [1] mapDynamicSpider #-}

instance HasSpiderTimeline x => Applicative (Reflex.Class.Dynamic (SpiderTimeline x)) where
  pure = SpiderDynamic . dynamicConst
  liftA2 f a b = SpiderDynamic $ Reflex.Spider.Internal.zipDynWith f (unSpiderDynamic a) (unSpiderDynamic b)
  SpiderDynamic a <*> SpiderDynamic b = SpiderDynamic $ Reflex.Spider.Internal.zipDynWith ($) a b
  a *> b = R.unsafeBuildDynamic (R.sample $ R.current b) $ R.leftmost [R.updated b, R.tag (R.current b) $ R.updated a]
  (<*) = flip (*>) -- There are no effects, so order doesn't matter

holdSpiderEventM :: HasSpiderTimeline x => a -> Reflex.Class.Event (SpiderTimeline x) a -> EventM x (Reflex.Class.Behavior (SpiderTimeline x) a)
holdSpiderEventM v0 e = fmap (SpiderBehavior . behaviorHoldIdentity) $ Reflex.Spider.Internal.hold v0 $ coerce $ unSpiderEvent e

holdDynSpiderEventM :: HasSpiderTimeline x => a -> Reflex.Class.Event (SpiderTimeline x) a -> EventM x (Reflex.Class.Dynamic (SpiderTimeline x) a)
holdDynSpiderEventM v0 e = fmap (SpiderDynamic . dynamicHoldIdentity) $ Reflex.Spider.Internal.hold v0 $ coerce $ unSpiderEvent e

holdIncrementalSpiderEventM :: (HasSpiderTimeline x, Patch p) => PatchTarget p -> Reflex.Class.Event (SpiderTimeline x) p -> EventM x (Reflex.Class.Incremental (SpiderTimeline x) p)
holdIncrementalSpiderEventM v0 e = fmap (SpiderIncremental . dynamicHold) $ Reflex.Spider.Internal.hold v0 $ unSpiderEvent e

buildDynamicSpiderEventM :: HasSpiderTimeline x => SpiderPushM x a -> Reflex.Class.Event (SpiderTimeline x) a -> EventM x (Reflex.Class.Dynamic (SpiderTimeline x) a)
buildDynamicSpiderEventM getV0 e = fmap (SpiderDynamic . dynamicDynIdentity) $ Reflex.Spider.Internal.buildDynamic (coerce getV0) $ coerce $ unSpiderEvent e

instance HasSpiderTimeline x => Reflex.Class.MonadSample (SpiderTimeline x) (SpiderHostFrame x) where
  {-# SPECIALIZE instance Reflex.Class.MonadSample (SpiderTimeline Global) (SpiderHostFrame Global) #-}
  sample = SpiderHostFrame . readBehaviorUntracked . unSpiderBehavior --TODO: This can cause problems with laziness, so we should get rid of it if we can

instance HasSpiderTimeline x => Reflex.Class.MonadHold (SpiderTimeline x) (SpiderHostFrame x) where
  {-# SPECIALIZE instance Reflex.Class.MonadHold (SpiderTimeline Global) (SpiderHostFrame Global) #-}
  {-# INLINABLE hold #-}
  hold v0 e = SpiderHostFrame $ fmap (SpiderBehavior . behaviorHoldIdentity) $ Reflex.Spider.Internal.hold v0 $ coerce $ unSpiderEvent e
  {-# INLINABLE holdDyn #-}
  holdDyn v0 e = SpiderHostFrame $ fmap (SpiderDynamic . dynamicHoldIdentity) $ Reflex.Spider.Internal.hold v0 $ coerce $ unSpiderEvent e
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 e = SpiderHostFrame $ fmap (SpiderIncremental . dynamicHold) $ Reflex.Spider.Internal.hold v0 $ unSpiderEvent e
  {-# INLINABLE buildDynamic #-}
  buildDynamic getV0 e = SpiderHostFrame $ fmap (SpiderDynamic . dynamicDynIdentity) $ Reflex.Spider.Internal.buildDynamic (coerce getV0) $ coerce $ unSpiderEvent e
  {-# INLINABLE headE #-}
  headE (SpiderEvent e) = SpiderHostFrame $ SpiderEvent <$> Reflex.Spider.Internal.headE e
  {-# INLINABLE now #-}
  now = SpiderHostFrame Reflex.Class.now

instance HasSpiderTimeline x => Reflex.Class.MonadSample (SpiderTimeline x) (Reflex.Spider.Internal.ReadPhase x) where
  {-# SPECIALIZE instance Reflex.Class.MonadSample (SpiderTimeline Global) (Reflex.Spider.Internal.ReadPhase Global) #-}
  {-# INLINABLE sample #-}
  sample = Reflex.Spider.Internal.ReadPhase . Reflex.Class.sample

instance HasSpiderTimeline x => Reflex.Class.MonadHold (SpiderTimeline x) (Reflex.Spider.Internal.ReadPhase x) where
  {-# SPECIALIZE instance Reflex.Class.MonadHold (SpiderTimeline Global) (Reflex.Spider.Internal.ReadPhase Global) #-}
  {-# INLINABLE hold #-}
  hold v0 e = Reflex.Spider.Internal.ReadPhase $ Reflex.Class.hold v0 e
  {-# INLINABLE holdDyn #-}
  holdDyn v0 e = Reflex.Spider.Internal.ReadPhase $ Reflex.Class.holdDyn v0 e
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 e = Reflex.Spider.Internal.ReadPhase $ Reflex.Class.holdIncremental v0 e
  {-# INLINABLE buildDynamic #-}
  buildDynamic getV0 e = Reflex.Spider.Internal.ReadPhase $ Reflex.Class.buildDynamic getV0 e
  {-# INLINABLE headE #-}
  headE e = Reflex.Spider.Internal.ReadPhase $ Reflex.Class.headE e
  {-# INLINABLE now #-}
  now = Reflex.Spider.Internal.ReadPhase Reflex.Class.now

--------------------------------------------------------------------------------
-- Deprecated items
--------------------------------------------------------------------------------

-- | 'SpiderEnv' is the old name for 'SpiderTimeline'
{-# DEPRECATED SpiderEnv "Use 'SpiderTimelineEnv' instead" #-}
type SpiderEnv = SpiderTimeline
instance HasSpiderTimeline x => Reflex.Host.Class.MonadSubscribeEvent (SpiderTimeline x) (SpiderHostFrame x) where
  {-# INLINABLE subscribeEvent #-}
  subscribeEvent e = SpiderHostFrame $ do
    --TODO: Unsubscribe eventually (manually and/or with weak ref)
    val <- liftIO $ newIORef Nothing
    let recordOccurrence a = do
#ifdef DEBUG_FRAME
          liftIO $ readIORef val >>= debugOccurrence "subscribeEvent: two occurrences in one frame"
#endif
          liftIO $ writeIORef val $ Just a
          scheduleClear val
    SubscribeResult subscription occ <- subscribeAndRead (unSpiderEvent e) $ terminalSubscriber recordOccurrence
    forM_ occ recordOccurrence
    return $ SpiderEventHandle
      { spiderEventHandleSubscription = subscription
      , spiderEventHandleValue = val
      }

instance HasSpiderTimeline x => Reflex.Host.Class.ReflexHost (SpiderTimeline x) where
  {-# SPECIALIZE instance Reflex.Host.Class.ReflexHost (SpiderTimeline Global) #-}
  type EventTrigger (SpiderTimeline x) = RootTrigger x
  type EventHandle (SpiderTimeline x) = SpiderEventHandle x
  type HostFrame (SpiderTimeline x) = SpiderHostFrame x

instance HasSpiderTimeline x => Reflex.Host.Class.MonadReadEvent (SpiderTimeline x) (Reflex.Spider.Internal.ReadPhase x) where
  {-# NOINLINE readEvent #-}
  readEvent h = Reflex.Spider.Internal.ReadPhase $ fmap (fmap return) $ liftIO $ do
    result <- readIORef $ spiderEventHandleValue h
    touch h
    return result

instance HasSpiderTimeline x => Reflex.Host.Class.MonadReflexCreateTrigger (SpiderTimeline x) (SpiderHost x) where
  newEventWithTrigger = SpiderHost . fmap SpiderEvent . newEventWithTriggerIO
  newFanEventWithTrigger f = SpiderHost $ do
    es <- newFanEventWithTriggerIO f
    return $ Reflex.Class.EventSelector $ SpiderEvent . Reflex.Spider.Internal.select es

instance HasSpiderTimeline x => Reflex.Host.Class.MonadReflexCreateTrigger (SpiderTimeline x) (SpiderHostFrame x) where
  newEventWithTrigger = SpiderHostFrame . EventM . liftIO . fmap SpiderEvent . newEventWithTriggerIO
  newFanEventWithTrigger f = SpiderHostFrame $ EventM $ liftIO $ do
    es <- newFanEventWithTriggerIO f
    return $ Reflex.Class.EventSelector $ SpiderEvent . Reflex.Spider.Internal.select es

instance HasSpiderTimeline x => Reflex.Host.Class.MonadReflexHost (SpiderTimeline x) (SpiderHost x) where
  {-# SPECIALIZE instance Reflex.Host.Class.MonadReflexHost (SpiderTimeline Global) (SpiderHost Global) #-}
  type ReadPhase (SpiderHost x) = Reflex.Spider.Internal.ReadPhase x
  hostFrameAndRead build getTriggers readPhase = runHostFrameFireAndRead (runSpiderHostFrame build) (runSpiderHostFrame . getTriggers) (\a -> let Reflex.Spider.Internal.ReadPhase r = readPhase a in r)

runHostFrameFireAndRead :: forall x a b. HasSpiderTimeline x => EventM x a -> (a -> EventM x [DSum (RootTrigger x) Identity]) -> (a -> ResultM x b) -> SpiderHost x b
runHostFrameFireAndRead build getTriggers readPhase = do
  let t = spiderTimeline :: SpiderTimelineEnv x
  SpiderHost $ withMVar (_spiderTimeline_lock (unSTE t)) $ \_ -> unSpiderHost $ runFrame $ do
    a <- build
    env <- asksEventEnv id
    runHoldInits (eventEnvHoldInits env) (eventEnvDynInits env) (eventEnvMergeInits env)
    roots <- getTriggers a
    propagateAndRead roots (readPhase a)

unsafeNewSpiderTimelineEnv :: forall x. IO (SpiderTimelineEnv x)
unsafeNewSpiderTimelineEnv = do
  lock <- newMVar ()
  env <- newEventEnv
#ifdef DEBUG
  depthRef <- newIORef 0
#endif
  return $ STE $ SpiderTimelineEnv
    { _spiderTimeline_lock = lock
    , _spiderTimeline_eventEnv = env
#ifdef DEBUG
    , _spiderTimeline_depth = depthRef
#endif
    }

-- | Create a new SpiderTimelineEnv
newSpiderTimeline :: IO (Some SpiderTimelineEnv)
newSpiderTimeline = withSpiderTimeline (pure . Some)

data LocalSpiderTimeline (x :: Type) s

instance Reifies s (SpiderTimelineEnv x) =>
         HasSpiderTimeline (LocalSpiderTimeline x s) where
  spiderTimeline = localSpiderTimeline Proxy $ reflect (Proxy :: Proxy s)

localSpiderTimeline
  :: proxy s
  -> SpiderTimelineEnv x
  -> SpiderTimelineEnv (LocalSpiderTimeline x s)
localSpiderTimeline _ = coerce

-- | Pass a new timeline to the given function.
withSpiderTimeline :: (forall x. HasSpiderTimeline x => SpiderTimelineEnv x -> IO r) -> IO r
withSpiderTimeline k = do
  env <- unsafeNewSpiderTimelineEnv
  reify env $ \s -> k $ localSpiderTimeline s env

newtype SpiderPullM (x :: Type) a = SpiderPullM (BehaviorM x a) deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

type ComputeM = EventM

newtype SpiderPushM (x :: Type) a = SpiderPushM (ComputeM x a) deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

instance HasSpiderTimeline x => R.Reflex (SpiderTimeline x) where
  {-# SPECIALIZE instance R.Reflex (SpiderTimeline Global) #-}
  newtype Behavior (SpiderTimeline x) a = SpiderBehavior { unSpiderBehavior :: Behavior x a }
  newtype Event (SpiderTimeline x) a = SpiderEvent { unSpiderEvent :: Event x a }
  newtype Dynamic (SpiderTimeline x) a = SpiderDynamic { unSpiderDynamic :: DynamicS x (Identity a) } -- deriving (Functor, Applicative, Monad)
  newtype Incremental (SpiderTimeline x) p = SpiderIncremental { unSpiderIncremental :: DynamicS x p }
  type PullM (SpiderTimeline x) = SpiderPullM x
  type PushM (SpiderTimeline x) = SpiderPushM x
  {-# INLINABLE never #-}
  never = SpiderEvent eventNever
  {-# INLINABLE constant #-}
  constant = SpiderBehavior . behaviorConst
  {-# INLINE push #-}
  push f = SpiderEvent . push (coerce f) . unSpiderEvent
  {-# INLINE pushCheap #-}
  pushCheap f = SpiderEvent . pushCheap (coerce f) . unSpiderEvent
  {-# INLINABLE pull #-}
  pull = SpiderBehavior . pull . coerce
  {-# INLINABLE fanG #-}
  fanG e = R.EventSelectorG $ SpiderEvent . selectG (fanG (unSpiderEvent e))
  {-# INLINABLE mergeG #-}
  mergeG
    :: forall k2 (k :: k2 -> Type) q (v :: k2 -> Type). GCompare k
    => (forall a. q a -> R.Event (SpiderTimeline x) (v a))
    -> DMap k q
    -> R.Event (SpiderTimeline x) (DMap k v)
  mergeG nt = SpiderEvent . mergeG (unSpiderEvent #. nt) . dynamicConst
  {-# INLINABLE switch #-}
  switch = SpiderEvent . switch . (coerce :: Behavior x (R.Event (SpiderTimeline x) a) -> Behavior x (Event x a)) . unSpiderBehavior
  {-# INLINABLE coincidence #-}
  coincidence = SpiderEvent . coincidence . (coerce :: Event x (R.Event (SpiderTimeline x) a) -> Event x (Event x a)) . unSpiderEvent
  {-# INLINABLE current #-}
  current = SpiderBehavior . dynamicCurrent . unSpiderDynamic
  {-# INLINABLE updated #-}
  updated = SpiderEvent #. dynamicUpdated .# fmap coerce . unSpiderDynamic
  {-# INLINABLE unsafeBuildDynamic #-}
  unsafeBuildDynamic readV0 v' = SpiderDynamic $ dynamicDynIdentity $ unsafeBuildDynamic (coerce readV0) $ coerce $ unSpiderEvent v'
  {-# INLINABLE unsafeBuildIncremental #-}
  unsafeBuildIncremental readV0 dv = SpiderIncremental $ dynamicDyn $ unsafeBuildDynamic (coerce readV0) $ unSpiderEvent dv
  {-# INLINABLE mergeIncrementalG #-}
  mergeIncrementalG nt = SpiderEvent #. mergeG (coerce #. nt) .# unSpiderIncremental
  {-# INLINABLE mergeIncrementalWithMoveG #-}
  mergeIncrementalWithMoveG nt = SpiderEvent #. mergeWithMove (coerce #. nt) .# unSpiderIncremental
  {-# INLINABLE currentIncremental #-}
  currentIncremental = SpiderBehavior . dynamicCurrent . unSpiderIncremental
  {-# INLINABLE updatedIncremental #-}
  updatedIncremental = SpiderEvent . dynamicUpdated . unSpiderIncremental
  {-# INLINABLE incrementalToDynamic #-}
  incrementalToDynamic (SpiderIncremental i) = SpiderDynamic $ dynamicDynIdentity $ unsafeBuildDynamic (readBehaviorUntracked $ dynamicCurrent i) $ flip push (dynamicUpdated i) $ \p -> do
    c <- readBehaviorUntracked $ dynamicCurrent i
    return $ Identity <$> apply p c --TODO: Avoid the redundant 'apply'
  eventCoercion Coercion = Coercion
  behaviorCoercion Coercion = Coercion
  dynamicCoercion Coercion = Coercion
  incrementalCoercion Coercion Coercion = Coercion
  {-# INLINABLE mergeIntIncremental #-}
  mergeIntIncremental = SpiderEvent . mergeInt . coerce
  {-# INLINABLE fanInt #-}
  fanInt e = R.EventSelectorInt $ SpiderEvent . selectInt (fanInt (unSpiderEvent e))

data RootTrigger x a = forall k. GCompare k => RootTrigger (WeakBag (Subscriber x a), IORef (DMap k Identity), k a)

data SpiderEventHandle x a = SpiderEventHandle
  { spiderEventHandleSubscription :: EventSubscription x
  , spiderEventHandleValue :: IORef (Maybe a)
  }

instance MonadRef (EventM x) where
  type Ref (EventM x) = Ref IO
  {-# INLINABLE newRef #-}
  {-# INLINABLE readRef #-}
  {-# INLINABLE writeRef #-}
  newRef = liftIO . newRef
  readRef = liftIO . readRef
  writeRef r a = liftIO $ writeRef r a

instance MonadAtomicRef (EventM x) where
  {-# INLINABLE atomicModifyRef #-}
  atomicModifyRef r f = liftIO $ atomicModifyRef r f

-- | The monad for actions that manipulate a Spider timeline identified by @x@
newtype SpiderHost (x :: Type) a = SpiderHost { unSpiderHost :: IO a }
  deriving (Functor, Applicative, MonadFix, MonadIO, MonadException, MonadAsyncException)

instance Monad (SpiderHost x) where
  {-# INLINABLE (>>=) #-}
  SpiderHost x >>= f = SpiderHost $ x >>= unSpiderHost . f
#if !MIN_VERSION_base(4,13,0)
  {-# INLINABLE fail #-}
  fail = MonadFail.fail
#endif

instance MonadFail (SpiderHost x) where
  {-# INLINABLE fail #-}
  fail s = SpiderHost $ MonadFail.fail s

-- | Run an action affecting the global Spider timeline; this will be guarded by
-- a mutex for that timeline
runSpiderHost :: SpiderHost Global a -> IO a
runSpiderHost (SpiderHost a) = a

-- | Run an action affecting a given Spider timeline; this will be guarded by a
-- mutex for that timeline
runSpiderHostForTimeline :: SpiderHost x a -> SpiderTimelineEnv x -> IO a
runSpiderHostForTimeline (SpiderHost a) _ = a

newtype SpiderHostFrame (x :: Type) a = SpiderHostFrame { runSpiderHostFrame :: EventM x a }
  deriving (Functor, Applicative, MonadFix, MonadIO, MonadException, MonadAsyncException, MonadMask, MonadThrow, MonadCatch)

instance Monad (SpiderHostFrame x) where
  {-# INLINABLE (>>=) #-}
  SpiderHostFrame x >>= f = SpiderHostFrame $ x >>= runSpiderHostFrame . f
#if !MIN_VERSION_base(4,13,0)
  {-# INLINABLE fail #-}
  fail s = SpiderHostFrame $ fail s
#endif

instance NotReady (SpiderTimeline x) (SpiderHostFrame x) where
  notReadyUntil _ = pure ()
  notReady = pure ()

newEventWithTriggerIO :: forall x a. HasSpiderTimeline x => (RootTrigger x a -> IO (IO ())) -> IO (Event x a)
newEventWithTriggerIO f = do
  es <- newFanEventWithTriggerIO $ \Refl -> f
  return $ select es Refl

newFanEventWithTriggerIO :: (HasSpiderTimeline x, GCompare k) => (forall a. k a -> RootTrigger x a -> IO (IO ())) -> IO (EventSelector x k)
newFanEventWithTriggerIO f = do
  occRef <- newIORef DMap.empty
  subscribedRef <- newIORef DMap.empty
  let !r = Root
        { rootOccurrence = occRef
        , rootSubscribed = subscribedRef
        , rootInit = f
        }
  return $ EventSelector $ \k -> eventRoot k r

newtype ReadPhase x a = ReadPhase (ResultM x a) deriving (Functor, Applicative, Monad, MonadFix)

instance MonadRef (SpiderHost x) where
  type Ref (SpiderHost x) = Ref IO
  newRef = SpiderHost . newRef
  readRef = SpiderHost . readRef
  writeRef r = SpiderHost . writeRef r

instance MonadAtomicRef (SpiderHost x) where
  atomicModifyRef r = SpiderHost . atomicModifyRef r

instance MonadRef (SpiderHostFrame x) where
  type Ref (SpiderHostFrame x) = Ref IO
  newRef = SpiderHostFrame . newRef
  readRef = SpiderHostFrame . readRef
  writeRef r = SpiderHostFrame . writeRef r

instance MonadAtomicRef (SpiderHostFrame x) where
  atomicModifyRef r = SpiderHostFrame . atomicModifyRef r

instance PrimMonad (SpiderHostFrame x) where
  type PrimState (SpiderHostFrame x) = PrimState IO
  primitive = SpiderHostFrame . EventM . primitive

instance NotReady (SpiderTimeline x) (SpiderHost x) where
  notReadyUntil _ = return ()
  notReady = return ()

instance HasSpiderTimeline x => NotReady (SpiderTimeline x) (PerformEventT (SpiderTimeline x) (SpiderHost x)) where
  notReadyUntil _ = return ()
  notReady = return ()
