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
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
#ifdef SPECIALIZE_TO_SPIDERTIMELINE_GLOBAL
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif
-- | This module is the implementation of the 'Spider' 'Reflex' engine.  It uses
-- a graph traversal algorithm to propagate 'Event's and 'Behavior's.
module Reflex.Spider.Internal where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad hiding (forM, forM_, mapM, mapM_, sequence)
import Control.Monad.Exception
import Control.Monad.Identity hiding (forM, forM_, mapM, mapM_, sequence)
import Control.Monad.Primitive
import Control.Monad.Reader hiding (forM, forM_, mapM, mapM_, sequence)
import Control.Monad.Ref
import Data.Align
import Data.Coerce
import Data.Dependent.Map (DMap, DSum (..))
import qualified Data.Dependent.Map as DMap
import Data.Foldable hiding (concat, elem, sequence_)
import Data.Function
import Data.Functor.Constant
import Data.Functor.Misc
import Data.Functor.Product
import Data.GADT.Compare
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef
import Data.List hiding (foldl')
import Data.Maybe
import Data.Monoid ((<>))
import Data.Proxy
import Data.These
import Data.Traversable
import Data.Word
import GHC.Exts
import GHC.IORef (IORef (..))
import GHC.Stack
import Reflex.FunctorMaybe
import System.IO.Unsafe
import System.Mem.Weak
import Unsafe.Coerce

#ifdef DEBUG_CYCLES
import Control.Monad.State hiding (forM, forM_, mapM, mapM_, sequence)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Monoid (mempty)
import Data.Tree (Forest, Tree (..), drawForest)
#endif

import Data.WeakBag (WeakBag, WeakBagTicket)
import qualified Data.WeakBag as WeakBag
#ifndef SPECIALIZE_TO_SPIDERTIMELINE_GLOBAL
import Data.Reflection
import Data.Some (Some)
import qualified Data.Some as Some
import Data.Type.Coercion
import qualified Reflex.Class
import qualified Reflex.Class as R
import qualified Reflex.Host.Class
#endif
import Reflex.Patch

-- Note: must come last to silence warnings due to AMP on GHC < 7.10
import Prelude hiding (any, concat, mapM, mapM_, sequence)

debugPropagate :: Bool

debugInvalidateHeight :: Bool

debugInvalidate :: Bool

#ifdef DEBUG

#define DEBUG_NODEIDS

debugPropagate = True

debugInvalidateHeight = False

debugInvalidate = True

class HasNodeId a where
  getNodeId :: a -> Int

instance HasNodeId (Hold x p a) where
  getNodeId = holdNodeId

instance HasNodeId (PushSubscribed x a b) where
  getNodeId = pushSubscribedNodeId

instance HasNodeId (SwitchSubscribed x a) where
  getNodeId = switchSubscribedNodeId

instance HasNodeId (FanSubscribed x a) where
  getNodeId = fanSubscribedNodeId

instance HasNodeId (CoincidenceSubscribed x a) where
  getNodeId = coincidenceSubscribedNodeId

instance HasNodeId (RootSubscribed x a) where
  getNodeId = rootSubscribedNodeId

instance HasNodeId (Pull x a) where
  getNodeId = pullNodeId

{-# INLINE showNodeId #-}
showNodeId :: HasNodeId a => a -> String
showNodeId = ("#"<>) . show . getNodeId

showSubscriberType :: Subscriber x a -> String
showSubscriberType = \case
  SubscriberPush _ _ -> "SubscriberPush"
  SubscriberMerge _ _ -> "SubscriberMerge"
  SubscriberFan _ -> "SubscriberFan"
  SubscriberMergeChange _ -> "SubscriberMergeChange"
  SubscriberHold _ -> "SubscriberHold"
  SubscriberHoldIdentity _ -> "SubscriberHoldIdentity"
  SubscriberSwitch _ -> "SubscriberSwitch"
  SubscriberCoincidenceOuter _ -> "SubscriberCoincidenceOuter"
  SubscriberCoincidenceInner _ -> "SubscriberCoincidenceInner"
  SubscriberHandle -> "SubscriberHandle"

showEventType :: Event x a -> String
showEventType = \case
  EventRoot _ _ -> "EventRoot"
  EventNever -> "EventNever"
  EventPush _ -> "EventPush"
  EventMerge _ -> "EventMerge"
  EventFan _ _ -> "EventFan"
  EventSwitch _ -> "EventSwitch"
  EventCoincidence _ -> "EventCoincidence"
  EventHold _ -> "EventHold"
  EventDyn _ -> "EventDyn"
  EventHoldIdentity _ -> "EventHoldIdentity"
  EventDynIdentity _ -> "EventDynIdentity"

#else

debugPropagate = False

debugInvalidateHeight = False

debugInvalidate = False

-- This must be inline, or error messages will cause memory leaks due to retaining the node in question
{-# INLINE showNodeId #-}
showNodeId :: a -> String
showNodeId _ = ""

#endif

#ifdef DEBUG_NODEIDS
{-# NOINLINE nextNodeIdRef #-}
nextNodeIdRef :: IORef Int
nextNodeIdRef = unsafePerformIO $ newIORef 1

newNodeId :: IO Int
newNodeId = atomicModifyIORef' nextNodeIdRef $ \n -> (succ n, n)

{-# NOINLINE unsafeNodeId #-}
unsafeNodeId :: a -> Int
unsafeNodeId a = unsafePerformIO $ do
  touch a
  newNodeId
#endif

--------------------------------------------------------------------------------
-- Event
--------------------------------------------------------------------------------

newtype Event x a = Event { unEvent :: Subscriber x a -> EventM x (EventSubscription x, Maybe a) }

{-# INLINE subscribeAndRead #-}
subscribeAndRead :: Event x a -> Subscriber x a -> EventM x (EventSubscription x, Maybe a)
subscribeAndRead = unEvent

{-# RULES
"cacheEvent/cacheEvent" forall e. cacheEvent (cacheEvent e) = cacheEvent e
"cacheEvent/pushCheap" forall f e. pushCheap f (cacheEvent e) = cacheEvent (pushCheap f e)
"hold/cacheEvent" forall f e. hold f (cacheEvent e) = hold f e
  #-}

-- | Construct an 'Event' equivalent to that constructed by 'push', but with no
-- caching; if the computation function is very cheap, this is (much) more
-- efficient than 'push'
{-# NOINLINE pushCheap #-}
pushCheap :: HasSpiderTimeline x => (a -> ComputeM x (Maybe b)) -> Event x a -> Event x b
pushCheap f e = Event $ \sub -> do
  (subscription, occ) <- subscribeAndRead e $ sub
    { subscriberPropagate = \a -> do
        mb <- runComputeM $ f a
        mapM_ (subscriberPropagate sub) mb
    }
  occ' <- join <$> mapM (runComputeM . f) occ
  return (subscription, occ')

data CacheSubscribed x a
   = CacheSubscribed { _cacheSubscribed_subscribers :: {-# UNPACK #-} !(WeakBag (Subscriber x a))
                     , _cacheSubscribed_weakSelf :: {-# UNPACK #-} !(IORef (Weak (CacheSubscribed x a)))
                     , _cacheSubscribed_parent :: {-# UNPACK #-} !(EventSubscription x)
                     , _cacheSubscribed_cached :: {-# UNPACK #-} !(IORef (Maybe (CacheSubscribed x a)))
                     , _cacheSubscribed_occurrence :: {-# UNPACK #-} !(IORef (Maybe a))
                     }

-- | Construct an 'Event' whose value is guaranteed not to be recomputed
-- repeatedly
--
--TODO: Try a caching strategy where we subscribe directly to the parent when
--there's only one subscriber, and then build our own WeakBag only when a second
--subscriber joins
{-# NOINLINE cacheEvent #-}
cacheEvent :: HasSpiderTimeline x => Event x a -> Event x a
cacheEvent e = Event $ \sub -> {-# SCC "cacheEvent" #-} do
  let !mSubscribedRef = unsafeNewIORef e Nothing
  mSubscribed <- liftIO $ readIORef mSubscribedRef
  case mSubscribed of
    Just subscribed -> liftIO $ do
      ticket <- WeakBag.insert sub (_cacheSubscribed_subscribers subscribed) (_cacheSubscribed_weakSelf subscribed) cleanupCacheSubscribed
      occ <- readIORef $ _cacheSubscribed_occurrence subscribed
      let es = EventSubscribed
            { eventSubscribedHeightRef = eventSubscribedHeightRef $ _eventSubscription_subscribed $ _cacheSubscribed_parent subscribed
            , eventSubscribedRetained = toAny subscribed
            }
      return (EventSubscription (WeakBag.remove ticket >> touch ticket) es, occ)
    Nothing -> do
      weakSelf <- liftIO $ newIORef $ error "cacheEvent: weakSelf not yet intialized"
      (subscribers, ticket) <- liftIO $ WeakBag.singleton sub weakSelf cleanupCacheSubscribed
      occRef <- liftIO $ newIORef undefined
      (parentSub, occ) <- subscribeAndRead e $ Subscriber
        { subscriberPropagate = \a -> do
            liftIO $ writeIORef occRef $ Just a
            scheduleClear occRef
            propagate a subscribers
        , subscriberInvalidateHeight = \old -> do
            WeakBag.traverse subscribers $ invalidateSubscriberHeight old
        , subscriberRecalculateHeight = \new -> do
            WeakBag.traverse subscribers $ recalculateSubscriberHeight new
        }
      liftIO $ writeIORef occRef occ
      when (isJust occ) $ scheduleClear occRef
      let !subscribed = CacheSubscribed
            { _cacheSubscribed_subscribers = subscribers
            , _cacheSubscribed_weakSelf = weakSelf
            , _cacheSubscribed_parent = parentSub
            , _cacheSubscribed_cached = mSubscribedRef
            , _cacheSubscribed_occurrence = occRef
            }
      liftIO $ writeIORef mSubscribedRef $ Just subscribed
      liftIO $ writeIORef weakSelf =<< evaluate =<< mkWeakPtrWithDebug subscribed "PushSubscribed"
      return (EventSubscription (WeakBag.remove ticket >> touch ticket) (EventSubscribed (eventSubscribedHeightRef $ _eventSubscription_subscribed parentSub) (toAny subscribed)), occ)

cleanupCacheSubscribed :: CacheSubscribed x a -> IO ()
cleanupCacheSubscribed self = do
  unsubscribe $ _cacheSubscribed_parent self
  writeIORef (_cacheSubscribed_cached self) Nothing

subscribe :: Event x a -> Subscriber x a -> EventM x (EventSubscription x)
subscribe e s = fst <$> subscribeAndRead e s

{-# INLINE wrap #-}
wrap :: MonadIO m => (t -> EventSubscribed x) -> (Subscriber x a -> m (WeakBagTicket, t, Maybe a)) -> Subscriber x a -> m (EventSubscription x, Maybe a)
wrap tag getSpecificSubscribed sub = do
  (sln, subd, occ) <- getSpecificSubscribed sub
  let es = tag subd
  return (EventSubscription (WeakBag.remove sln >> touch sln) es, occ)

eventRoot :: GCompare k => k a -> Root x k -> Event x a
eventRoot !k !r = Event $ wrap eventSubscribedRoot $ liftIO . getRootSubscribed k r

eventNever :: Event x a
eventNever = Event $ \_ -> return (EventSubscription (return ()) eventSubscribedNever, Nothing)

eventFan :: (GCompare k, HasSpiderTimeline x) => k a -> Fan x k -> Event x a
eventFan !k !f = Event $ wrap eventSubscribedFan $ getFanSubscribed k f

eventSwitch :: HasSpiderTimeline x => Switch x a -> Event x a
eventSwitch !s = Event $ wrap eventSubscribedSwitch $ getSwitchSubscribed s

eventCoincidence :: HasSpiderTimeline x => Coincidence x a -> Event x a
eventCoincidence !c = Event $ wrap eventSubscribedCoincidence $ getCoincidenceSubscribed c

eventHold :: HasSpiderTimeline x => Hold x p -> Event x p
eventHold !h = Event $ subscribeHoldEvent h

eventDyn :: (HasSpiderTimeline x, Patch p) => Dyn x p -> Event x p
eventDyn !j = Event $ \sub -> getDynHold j >>= \h -> subscribeHoldEvent h sub

{-# INLINE subscribeCoincidenceInner #-}
subscribeCoincidenceInner :: CanSubscribe x m => Event x a -> Height -> CoincidenceSubscribed x a -> m (Maybe a, Height, EventSubscribed x)
subscribeCoincidenceInner inner outerHeight subscribedUnsafe = do
  subInner <- liftIO $ newSubscriberCoincidenceInner subscribedUnsafe
  (subscription@(EventSubscription _ innerSubd), innerOcc) <- subscribeAndRead inner subInner
  innerHeight <- liftIO $ getEventSubscribedHeight innerSubd
  let height = max innerHeight outerHeight
  defer $ SomeResetCoincidence subscription $ if height > outerHeight then Just subscribedUnsafe else Nothing
  return (innerOcc, height, innerSubd)

--------------------------------------------------------------------------------
-- Subscriber
--------------------------------------------------------------------------------

data Subscriber x a = Subscriber
  { subscriberPropagate :: !(a -> EventM x ())
  , subscriberInvalidateHeight :: !(Height -> IO ())
  , subscriberRecalculateHeight :: !(Height -> IO ())
  }

--TODO: Move this comment to WeakBag
-- These function are constructor functions that are marked NOINLINE so they are
-- opaque to GHC. If we do not do this, then GHC will sometimes fuse the constructor away
-- so any weak references that are attached to the constructors will have their
-- finalizer run. Using the opaque constructor, does not see the
-- constructor application, so it behaves like an IORef and cannot be fused away.
--
-- The result is also evaluated to WHNF, since forcing a thunk invalidates
-- the weak pointer to it in some cases.

newSubscriberHold :: (HasSpiderTimeline x, Patch p) => Hold x p -> IO (Subscriber x p)
newSubscriberHold h = return $ Subscriber
  { subscriberPropagate = {-# SCC "traverseHold" #-} propagateSubscriberHold h
  , subscriberInvalidateHeight = \_ -> return ()
  , subscriberRecalculateHeight = \_ -> return ()
  }

newSubscriberFan :: forall x k. (HasSpiderTimeline x, GCompare k) => FanSubscribed x k -> IO (Subscriber x (DMap k Identity))
newSubscriberFan subscribed = return $ Subscriber
  { subscriberPropagate = \a -> {-# SCC "traverseFan" #-} do
      subs <- liftIO $ readIORef $ fanSubscribedSubscribers subscribed
      tracePropagate (Proxy :: Proxy x) $ "SubscriberFan" <> showNodeId subscribed <> ": " ++ show (DMap.size subs) ++ " keys subscribed, " ++ show (DMap.size a) ++ " keys firing"
      liftIO $ writeIORef (fanSubscribedOccurrence subscribed) $ Just a
      scheduleClear $ fanSubscribedOccurrence subscribed
      let f _ (Pair (Identity v) subsubs) = do
            propagate v $ _fanSubscribedChildren_list subsubs
            return $ Constant ()
      _ <- DMap.traverseWithKey f $ DMap.intersectionWithKey (\_ -> Pair) a subs --TODO: Would be nice to have DMap.traverse_
      return ()
  , subscriberInvalidateHeight = \old -> do
      when debugInvalidateHeight $ putStrLn $ "invalidateSubscriberHeight: SubscriberFan" <> showNodeId subscribed
      subscribers <- readIORef $ fanSubscribedSubscribers subscribed
      forM_ (DMap.toList subscribers) $ \(_ :=> v) -> WeakBag.traverse (_fanSubscribedChildren_list v) $ invalidateSubscriberHeight old
      when debugInvalidateHeight $ putStrLn $ "invalidateSubscriberHeight: SubscriberFan" <> showNodeId subscribed <> " done"
  , subscriberRecalculateHeight = \new -> do
      when debugInvalidateHeight $ putStrLn $ "recalculateSubscriberHeight: SubscriberFan" <> showNodeId subscribed
      subscribers <- readIORef $ fanSubscribedSubscribers subscribed
      forM_ (DMap.toList subscribers) $ \(_ :=> v) -> WeakBag.traverse (_fanSubscribedChildren_list v) $ recalculateSubscriberHeight new
      when debugInvalidateHeight $ putStrLn $ "recalculateSubscriberHeight: SubscriberFan" <> showNodeId subscribed <> " done"
  }

newSubscriberSwitch :: forall x a. HasSpiderTimeline x => SwitchSubscribed x a -> IO (Subscriber x a)
newSubscriberSwitch subscribed = return $ Subscriber
  { subscriberPropagate = \a -> {-# SCC "traverseSwitch" #-} do
      tracePropagate (Proxy :: Proxy x) $ "SubscriberSwitch" <> showNodeId subscribed
      liftIO $ writeIORef (switchSubscribedOccurrence subscribed) $ Just a
      scheduleClear $ switchSubscribedOccurrence subscribed
      propagate a $ switchSubscribedSubscribers subscribed
  , subscriberInvalidateHeight = \_ -> do
      when debugInvalidateHeight $ putStrLn $ "invalidateSubscriberHeight: SubscriberSwitch" <> showNodeId subscribed
      oldHeight <- readIORef $ switchSubscribedHeight subscribed
      when (oldHeight /= invalidHeight) $ do
        writeIORef (switchSubscribedHeight subscribed) $! invalidHeight
        WeakBag.traverse (switchSubscribedSubscribers subscribed) $ invalidateSubscriberHeight oldHeight
      when debugInvalidateHeight $ putStrLn $ "invalidateSubscriberHeight: SubscriberSwitch" <> showNodeId subscribed <> " done"
  , subscriberRecalculateHeight = \new -> do
      when debugInvalidateHeight $ putStrLn $ "recalculateSubscriberHeight: SubscriberSwitch" <> showNodeId subscribed
      updateSwitchHeight new subscribed
      when debugInvalidateHeight $ putStrLn $ "recalculateSubscriberHeight: SubscriberSwitch" <> showNodeId subscribed <> " done"
  }

newSubscriberCoincidenceOuter :: forall x b. HasSpiderTimeline x => CoincidenceSubscribed x b -> IO (Subscriber x (Event x b))
newSubscriberCoincidenceOuter subscribed = return $ Subscriber
  { subscriberPropagate = \a -> {-# SCC "traverseCoincidenceOuter" #-} do
      tracePropagate (Proxy :: Proxy x) $ "SubscriberCoincidenceOuter" <> showNodeId subscribed
      outerHeight <- liftIO $ readIORef $ coincidenceSubscribedHeight subscribed
      tracePropagate (Proxy :: Proxy x) $ "  outerHeight = " <> show outerHeight
      (occ, innerHeight, innerSubd) <- subscribeCoincidenceInner a outerHeight subscribed
      tracePropagate (Proxy :: Proxy x) $ "  isJust occ = " <> show (isJust occ)
      tracePropagate (Proxy :: Proxy x) $ "  innerHeight = " <> show innerHeight
      liftIO $ writeIORef (coincidenceSubscribedInnerParent subscribed) $ Just innerSubd
      scheduleClear $ coincidenceSubscribedInnerParent subscribed
      case occ of
        Nothing -> do
          when (innerHeight > outerHeight) $ liftIO $ do -- If the event fires, it will fire at a later height
            writeIORef (coincidenceSubscribedHeight subscribed) $! innerHeight
            WeakBag.traverse (coincidenceSubscribedSubscribers subscribed) $ invalidateSubscriberHeight outerHeight
            WeakBag.traverse (coincidenceSubscribedSubscribers subscribed) $ recalculateSubscriberHeight innerHeight
        Just o -> do -- Since it's already firing, no need to adjust height
          liftIO $ writeIORef (coincidenceSubscribedOccurrence subscribed) occ
          scheduleClear $ coincidenceSubscribedOccurrence subscribed
          propagate o $ coincidenceSubscribedSubscribers subscribed
  , subscriberInvalidateHeight = \_ -> do
      when debugInvalidateHeight $ putStrLn $ "invalidateSubscriberHeight: SubscriberCoincidenceOuter" <> showNodeId subscribed
      invalidateCoincidenceHeight subscribed
      when debugInvalidateHeight $ putStrLn $ "invalidateSubscriberHeight: SubscriberCoincidenceOuter" <> showNodeId subscribed <> " done"
  , subscriberRecalculateHeight = \_ -> do
      when debugInvalidateHeight $ putStrLn $ "recalculateSubscriberHeight: SubscriberCoincidenceOuter" <> showNodeId subscribed
      recalculateCoincidenceHeight subscribed
      when debugInvalidateHeight $ putStrLn $ "recalculateSubscriberHeight: SubscriberCoincidenceOuter" <> showNodeId subscribed <> " done"
  }

newSubscriberCoincidenceInner :: forall x a. HasSpiderTimeline x => CoincidenceSubscribed x a -> IO (Subscriber x a)
newSubscriberCoincidenceInner subscribed = return $ Subscriber
  { subscriberPropagate = \a -> {-# SCC "traverseCoincidenceInner" #-} do
      tracePropagate (Proxy :: Proxy x) $ "SubscriberCoincidenceInner" <> showNodeId subscribed
      occ <- liftIO $ readIORef $ coincidenceSubscribedOccurrence subscribed
      case occ of
        Just _ -> return () -- SubscriberCoincidenceOuter must have already propagated this event
        Nothing -> do
          liftIO $ writeIORef (coincidenceSubscribedOccurrence subscribed) $ Just a
          scheduleClear $ coincidenceSubscribedOccurrence subscribed
          propagate a $ coincidenceSubscribedSubscribers subscribed
  , subscriberInvalidateHeight = \_ -> do
      when debugInvalidateHeight $ putStrLn $ "invalidateSubscriberHeight: SubscriberCoincidenceInner" <> showNodeId subscribed
      invalidateCoincidenceHeight subscribed
      when debugInvalidateHeight $ putStrLn $ "invalidateSubscriberHeight: SubscriberCoincidenceInner" <> showNodeId subscribed <> " done"
  , subscriberRecalculateHeight = \_ -> do
      when debugInvalidateHeight $ putStrLn $ "recalculateSubscriberHeight: SubscriberCoincidenceInner" <> showNodeId subscribed
      recalculateCoincidenceHeight subscribed
      when debugInvalidateHeight $ putStrLn $ "recalculateSubscriberHeight: SubscriberCoincidenceInner" <> showNodeId subscribed <> " done"
  }

invalidateSubscriberHeight :: Height -> Subscriber x a -> IO ()
invalidateSubscriberHeight = flip subscriberInvalidateHeight

recalculateSubscriberHeight :: Height -> Subscriber x a -> IO ()
recalculateSubscriberHeight = flip subscriberRecalculateHeight

-- | Propagate everything at the current height
propagate :: a -> WeakBag (Subscriber x a) -> EventM x ()
propagate a subscribers = withIncreasedDepth $ do
  -- Note: in the following traversal, we do not visit nodes that are added to the list during our traversal; they are new events, which will necessarily have full information already, so there is no need to traverse them
  --TODO: Should we check if nodes already have their values before propagating?  Maybe we're re-doing work
  WeakBag.traverse subscribers $ \s -> subscriberPropagate s a

--------------------------------------------------------------------------------
-- EventSubscribed
--------------------------------------------------------------------------------

toAny :: a -> Any
toAny = unsafeCoerce

data EventSubscribed x = EventSubscribed
  { eventSubscribedHeightRef :: {-# UNPACK #-} !(IORef Height)
  , eventSubscribedRetained :: {-# NOUNPACK #-} !Any
#ifdef DEBUG_CYCLES
  , eventSubscribedGetParents :: !(IO [Some (EventSubscribed x)]) -- For debugging loops
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

eventSubscribedFan :: FanSubscribed x k -> EventSubscribed x
eventSubscribedFan !subscribed = EventSubscribed
  { eventSubscribedHeightRef = eventSubscribedHeightRef $ _eventSubscription_subscribed $ fanSubscribedParent subscribed
  , eventSubscribedRetained = toAny subscribed
#ifdef DEBUG_CYCLES
  , eventSubscribedGetParents = return [Some.This $ _eventSubscription_subscribed $ fanSubscribedParent subscribed]
  , eventSubscribedHasOwnHeightRef = False
  , eventSubscribedWhoCreated = whoCreatedIORef $ fanSubscribedCachedSubscribed subscribed
#endif
  }

eventSubscribedSwitch :: SwitchSubscribed x a -> EventSubscribed x
eventSubscribedSwitch !subscribed = EventSubscribed
  { eventSubscribedHeightRef = switchSubscribedHeight subscribed
  , eventSubscribedRetained = toAny subscribed
#ifdef DEBUG_CYCLES
  , eventSubscribedGetParents = do
      s <- readIORef $ switchSubscribedCurrentParent subscribed
      return [Some.This $ _eventSubscription_subscribed s]
  , eventSubscribedHasOwnHeightRef = True
  , eventSubscribedWhoCreated = whoCreatedIORef $ switchSubscribedCachedSubscribed subscribed
#endif
  }

eventSubscribedCoincidence :: CoincidenceSubscribed x a -> EventSubscribed x
eventSubscribedCoincidence !subscribed = EventSubscribed
  { eventSubscribedHeightRef = coincidenceSubscribedHeight subscribed
  , eventSubscribedRetained = toAny subscribed
#ifdef DEBUG_CYCLES
  , eventSubscribedGetParents = do
      innerSubscription <- readIORef $ coincidenceSubscribedInnerParent subscribed
      let outerParent = Some.This $ _eventSubscription_subscribed $ coincidenceSubscribedOuterParent subscribed
          innerParents = maybeToList $ fmap Some.This innerSubscription
      return $ outerParent : innerParents
  , eventSubscribedHasOwnHeightRef = True
  , eventSubscribedWhoCreated = whoCreatedIORef $ coincidenceSubscribedCachedSubscribed subscribed
#endif
  }

getEventSubscribedHeight :: EventSubscribed x -> IO Height
getEventSubscribedHeight es = readIORef $ eventSubscribedHeightRef es

#ifdef DEBUG_CYCLES
whoCreatedEventSubscribed :: EventSubscribed x -> IO [String]
whoCreatedEventSubscribed = eventSubscribedWhoCreated

walkInvalidHeightParents :: EventSubscribed x -> IO [Some (EventSubscribed x)]
walkInvalidHeightParents s0 = do
  subscribers <- flip execStateT mempty $ ($ Some.This s0) $ fix $ \loop (Some.This s) -> do
    h <- liftIO $ readIORef $ eventSubscribedHeightRef s
    when (h == invalidHeight) $ do
      when (eventSubscribedHasOwnHeightRef s) $ liftIO $ writeIORef (eventSubscribedHeightRef s) $! invalidHeightBeingTraversed
      modify (Some.This s :)
      mapM_ loop =<< liftIO (eventSubscribedGetParents s)
  forM_ subscribers $ \(Some.This s) -> writeIORef (eventSubscribedHeightRef s) $! invalidHeight
  return subscribers
#endif

{-# INLINE subscribeHoldEvent #-}
subscribeHoldEvent :: CanSubscribe x m => Hold x p -> Subscriber x p -> m (EventSubscription x, Maybe p)
subscribeHoldEvent = subscribeAndRead . holdEvent

--------------------------------------------------------------------------------
-- EventSubscription
--------------------------------------------------------------------------------

data EventSubscription x = EventSubscription
  { _eventSubscription_unsubscribe :: !(IO ())
  , _eventSubscription_subscribed :: {-# UNPACK #-} !(EventSubscribed x)
  }

unsubscribe :: EventSubscription x -> IO ()
unsubscribe (EventSubscription u _) = u

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
        askParentsRef >>= mapM_ (\r -> liftIO $ modifyIORef' r (SomeBehaviorSubscribed (BehaviorSubscribedPull subscribed) :))
        askInvalidator >>= mapM_ (\wi -> liftIO $ modifyIORef' (pullSubscribedInvalidators subscribed) (wi:))
        liftIO $ touch $ pullSubscribedOwnInvalidator subscribed
        return $ pullSubscribedValue subscribed
      Nothing -> do
        i <- liftIO $ newInvalidatorPull p
        wi <- liftIO $ mkWeakPtrWithDebug i "InvalidatorPull"
        parentsRef <- liftIO $ newIORef []
        holdInits <- askBehaviorHoldInits
        a <- liftIO $ runReaderT (unBehaviorM $ pullCompute p) (Just (wi, parentsRef), holdInits)
        invsRef <- liftIO . newIORef . maybeToList =<< askInvalidator
        parents <- liftIO $ readIORef parentsRef
        let subscribed = PullSubscribed
              { pullSubscribedValue = a
              , pullSubscribedInvalidators = invsRef
              , pullSubscribedOwnInvalidator = i
              , pullSubscribedParents = parents
              }
        liftIO $ writeIORef (pullValue p) $ Just subscribed
        askParentsRef >>= mapM_ (\r -> liftIO $ modifyIORef' r (SomeBehaviorSubscribed (BehaviorSubscribedPull subscribed) :))
        return a

behaviorDyn :: Patch p => Dyn x p -> Behavior x (PatchTarget p)
behaviorDyn !d = Behavior $ readHoldTracked =<< getDynHold d

{-# INLINE readHoldTracked #-}
readHoldTracked :: Hold x p -> BehaviorM x (PatchTarget p)
readHoldTracked h = do
  result <- liftIO $ readIORef $ holdValue h
  askInvalidator >>= mapM_ (\wi -> liftIO $ modifyIORef' (holdInvalidators h) (wi:))
  askParentsRef >>= mapM_ (\r -> liftIO $ modifyIORef' r (SomeBehaviorSubscribed (BehaviorSubscribedHold h) :))
  return result

{-# INLINABLE readBehaviorUntracked #-}
readBehaviorUntracked :: Defer (SomeHoldInit x) m => Behavior x a -> m a
readBehaviorUntracked b = do
  holdInits <- getDeferralQueue
  liftIO $ runBehaviorM (readBehaviorTracked b) Nothing holdInits --TODO: Specialize readBehaviorTracked to the Nothing and Just cases

--------------------------------------------------------------------------------
-- Dynamic
--------------------------------------------------------------------------------

data Dynamic x p = Dynamic
  { dynamicCurrent :: !(Behavior x (PatchTarget p))
  , dynamicUpdated :: !(Event x p)
  }

dynamicHold :: HasSpiderTimeline x => Hold x p -> Dynamic x p
dynamicHold !h = Dynamic
  { dynamicCurrent = behaviorHold h
  , dynamicUpdated = eventHold h
  }

dynamicHoldIdentity :: HasSpiderTimeline x => Hold x (Identity a) -> Dynamic x (Identity a)
dynamicHoldIdentity = dynamicHold

dynamicConst :: PatchTarget p -> Dynamic x p
dynamicConst !a = Dynamic
  { dynamicCurrent = behaviorConst a
  , dynamicUpdated = eventNever
  }

dynamicDyn :: (HasSpiderTimeline x, Patch p) => Dyn x p -> Dynamic x p
dynamicDyn !d = Dynamic
  { dynamicCurrent = behaviorDyn d
  , dynamicUpdated = eventDyn d
  }

dynamicDynIdentity :: HasSpiderTimeline x => Dyn x (Identity a) -> Dynamic x (Identity a)
dynamicDynIdentity = dynamicDyn

--------------------------------------------------------------------------------
-- Combinators
--------------------------------------------------------------------------------

--TODO: Figure out why certain things are not 'representational', then make them
--representational so we can use coerce

--type role Hold representational
data Hold x p
   = Hold { holdValue :: !(IORef (PatchTarget p))
          , holdInvalidators :: !(IORef [Weak (Invalidator x)])
          , holdEvent :: Event x p -- This must be lazy, or holds cannot be defined before their input Events
          , holdParent :: !(IORef (Maybe (EventSubscription x))) -- Keeps its parent alive (will be undefined until the hold is initialized) --TODO: Probably shouldn't be an IORef
#ifdef DEBUG_NODEIDS
          , holdNodeId :: Int
#endif
          }

-- | A statically allocated 'SpiderTimeline'; this is the only instance of
-- 'SpiderTimeline' that can used when using
-- -fspecialize-to-spidertimeline-global.
data Global

{-# NOINLINE globalSpiderTimelineEnv #-}
globalSpiderTimelineEnv :: SpiderTimelineEnv Global
globalSpiderTimelineEnv = unsafePerformIO unsafeNewSpiderTimelineEnv

-- | Stores all global data relevant to a particular Spider timeline; only one
-- value should exist for each type @x@
data SpiderTimelineEnv x = SpiderTimelineEnv
  { _spiderTimeline_lock :: {-# UNPACK #-} !(MVar ())
  , _spiderTimeline_eventEnv :: {-# UNPACK #-} !(EventEnv x)
#ifdef DEBUG
  , _spiderTimeline_depth :: {-# UNPACK #-} !(IORef Int)
#endif
  }
type role SpiderTimelineEnv nominal

instance GEq SpiderTimelineEnv where
  a `geq` b = if _spiderTimeline_lock a == _spiderTimeline_lock b
              then Just $ unsafeCoerce Refl -- This unsafeCoerce is safe because the same SpiderTimelineEnv can't have two different 'x' arguments
              else Nothing

data EventEnv x
   = EventEnv { eventEnvAssignments :: !(IORef [SomeAssignment x]) -- Needed for Subscribe
              , eventEnvHoldInits :: !(IORef [SomeHoldInit x]) -- Needed for Subscribe
              , eventEnvMergeUpdates :: !(IORef [SomeMergeUpdate x])
              , eventEnvMergeInits :: !(IORef [SomeMergeInit x]) -- Needed for Subscribe
              , eventEnvClears :: !(IORef [SomeClear]) -- Needed for Subscribe
              , eventEnvRootClears :: !(IORef [SomeRootClear])
              , eventEnvCurrentHeight :: !(IORef Height) -- Needed for Subscribe
              , eventEnvResetCoincidences :: !(IORef [SomeResetCoincidence x]) -- Needed for Subscribe
              , eventEnvDelayedMerges :: !(IORef (IntMap [EventM x ()]))
              }

{-# INLINE runEventM #-}
runEventM :: EventM x a -> IO a
runEventM = unEventM

asksEventEnv :: forall x a. HasSpiderTimeline x => (EventEnv x -> a) -> EventM x a
asksEventEnv f = return $ f $ _spiderTimeline_eventEnv (spiderTimeline :: SpiderTimelineEnv x)

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
    liftIO $ modifyIORef' delayedRef $ IntMap.insertWith (++) (unHeight height) [subscribed]

#ifdef SPECIALIZE_TO_SPIDERTIMELINE_GLOBAL
class x ~ Global => HasSpiderTimeline x where
#else
class HasSpiderTimeline x where
#endif
  -- | Retrieve the current SpiderTimelineEnv
  spiderTimeline :: SpiderTimelineEnv x

instance HasSpiderTimeline Global where
  spiderTimeline = globalSpiderTimelineEnv

putCurrentHeight :: HasSpiderTimeline x => Height -> EventM x ()
putCurrentHeight h = do
  heightRef <- asksEventEnv eventEnvCurrentHeight
  liftIO $ writeIORef heightRef $! h

instance HasSpiderTimeline x => Defer SomeClear (EventM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = asksEventEnv eventEnvClears

{-# INLINE scheduleClear #-}
scheduleClear :: Defer SomeClear m => IORef (Maybe a) -> m ()
scheduleClear r = defer $ SomeClear r

instance HasSpiderTimeline x => Defer SomeRootClear (EventM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = asksEventEnv eventEnvRootClears

{-# INLINE scheduleRootClear #-}
scheduleRootClear :: Defer SomeRootClear m => IORef (DMap k Identity) -> m ()
scheduleRootClear r = defer $ SomeRootClear r

instance HasSpiderTimeline x => Defer (SomeResetCoincidence x) (EventM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = asksEventEnv eventEnvResetCoincidences

-- Note: hold cannot examine its event until after the phase is over
{-# INLINE [1] hold #-}
hold :: (Patch p, Defer (SomeHoldInit x) m) => PatchTarget p -> Event x p -> m (Hold x p)
hold v0 e = do
  valRef <- liftIO $ newIORef v0
  invsRef <- liftIO $ newIORef []
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
getHoldEventSubscription :: forall m p x. (Defer (SomeAssignment x) m, Defer (SomeHoldInit x) m, Defer SomeClear m, Patch p, HasCurrentHeight x m, Defer (SomeMergeInit x) m, Defer (SomeResetCoincidence x) m, HasSpiderTimeline x, m ~ EventM x) => Hold x p -> m (EventSubscription x)
getHoldEventSubscription h = do
  ep <- liftIO $ readIORef $ holdParent h
  case ep of
    Just subd -> return subd
    Nothing -> do
      let e = holdEvent h
      subscriptionRef <- liftIO $ newIORef $ error "getHoldEventSubscription: subdRef uninitialized"
      (subscription@(EventSubscription _ _), occ) <- subscribeAndRead e =<< liftIO (newSubscriberHold h)
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

--type role BehaviorM representational
-- BehaviorM can sample behaviors
newtype BehaviorM x a = BehaviorM { unBehaviorM :: ReaderT (BehaviorEnv x) IO a } deriving (Functor, Applicative, MonadIO, MonadFix)

instance Monad (BehaviorM x) where
  {-# INLINE (>>=) #-}
  BehaviorM x >>= f = BehaviorM $ x >>= unBehaviorM . f
  {-# INLINE (>>) #-}
  BehaviorM x >> BehaviorM y = BehaviorM $ x >> y
  {-# INLINE return #-}
  return x = BehaviorM $ return x
  {-# INLINE fail #-}
  fail s = BehaviorM $ fail s

data BehaviorSubscribed x a
   = forall p. BehaviorSubscribedHold (Hold x p)
   | BehaviorSubscribedPull (PullSubscribed x a)

data SomeBehaviorSubscribed x = forall a. SomeBehaviorSubscribed (BehaviorSubscribed x a)

--type role PullSubscribed representational
data PullSubscribed x a
   = PullSubscribed { pullSubscribedValue :: !a
                    , pullSubscribedInvalidators :: !(IORef [Weak (Invalidator x)])
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

data Root x (k :: * -> *)
   = Root { rootOccurrence :: !(IORef (DMap k Identity)) -- The currently-firing occurrence of this event
          , rootSubscribed :: !(IORef (DMap k (RootSubscribed x)))
          , rootInit :: !(forall a. k a -> RootTrigger x a -> IO (IO ()))
          }

data SomeHoldInit x = forall p. Patch p => SomeHoldInit !(Hold x p)

data SomeMergeUpdate x = SomeMergeUpdate
  { _someMergeUpdate_update :: !(EventM x [EventSubscription x])
  , _someMergeUpdate_invalidateHeight :: !(IO ())
  , _someMergeUpdate_recalculateHeight :: !(IO ())
  }

newtype SomeMergeInit x = SomeMergeInit { unSomeMergeInit :: EventM x () }

-- EventM can do everything BehaviorM can, plus create holds
newtype EventM x a = EventM { unEventM :: IO a } deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadException, MonadAsyncException) -- The environment should be Nothing if we are not in a frame, and Just if we are - in which case it is a list of assignments to be done after the frame is over

newtype ComputeM x a = ComputeM (ReaderT (IORef [SomeHoldInit x]) IO a) deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

instance Defer (SomeHoldInit x) (ComputeM x) where
  getDeferralQueue = ComputeM ask

{-# INLINABLE runComputeM #-}
runComputeM :: Defer (SomeHoldInit x) m => ComputeM x a -> m a
runComputeM (ComputeM a) = liftIO . runReaderT a =<< getDeferralQueue

newtype MergeSubscribedParent x a = MergeSubscribedParent { unMergeSubscribedParent :: EventSubscription x }

data HeightBag = HeightBag
  { _heightBag_size :: {-# UNPACK #-} !Int
  , _heightBag_contents :: !(IntMap Word) -- Number of excess in each bucket
  }
  deriving (Show, Read, Eq, Ord)

heightBagEmpty :: HeightBag
heightBagEmpty = heightBagVerify $ HeightBag 0 IntMap.empty

heightBagSize :: HeightBag -> Int
heightBagSize = _heightBag_size

heightBagFromList :: [Height] -> HeightBag
heightBagFromList heights = heightBagVerify $ foldl' (flip heightBagAdd) heightBagEmpty heights

heightBagAdd :: Height -> HeightBag -> HeightBag
heightBagAdd (Height h) (HeightBag s c) = heightBagVerify $ HeightBag (succ s) $ IntMap.insertWithKey (\_ _ old -> succ old) h 0 c

heightBagRemove :: Height -> HeightBag -> HeightBag
heightBagRemove (Height h) b@(HeightBag s c) = heightBagVerify $ case IntMap.lookup h c of
  Nothing -> error $ "heightBagRemove: Height " <> show h <> " not present in bag " <> show b
  Just old -> HeightBag (pred s) $ case old of
    0 -> IntMap.delete h c
    _ -> IntMap.insert h (pred old) c

heightBagMax :: HeightBag -> Height
heightBagMax (HeightBag _ c) = case IntMap.maxViewWithKey c of
  Just ((h, _), _) -> Height h
  Nothing -> zeroHeight

heightBagVerify :: HeightBag -> HeightBag
#ifdef DEBUG
heightBagVerify b@(HeightBag s c) = if
  | s /= IntMap.size c + fromIntegral (sum (IntMap.elems c))
    -> error $ "heightBagVerify: size doesn't match: " <> show b
  | unHeight invalidHeight `IntMap.member` c
    -> error $ "heightBagVerify: contains invalid height: " <> show b
  | otherwise -> b
#else
heightBagVerify = id
#endif

data FanSubscribedChildren (x :: *) k a = FanSubscribedChildren
  { _fanSubscribedChildren_list :: !(WeakBag (Subscriber x a))
  , _fanSubscribedChildren_self :: {-# NOUNPACK #-} !(k a, FanSubscribed x k)
  , _fanSubscribedChildren_weakSelf :: !(IORef (Weak (k a, FanSubscribed x k)))
  }

data FanSubscribed (x :: *) k
   = FanSubscribed { fanSubscribedCachedSubscribed :: !(IORef (Maybe (FanSubscribed x k)))
                   , fanSubscribedOccurrence :: !(IORef (Maybe (DMap k Identity)))
                   , fanSubscribedSubscribers :: !(IORef (DMap k (FanSubscribedChildren x k))) -- This DMap should never be empty
                   , fanSubscribedParent :: !(EventSubscription x)
#ifdef DEBUG_NODEIDS
                   , fanSubscribedNodeId :: Int
#endif
                   }

data Fan x k
   = Fan { fanParent :: !(Event x (DMap k Identity))
         , fanSubscribed :: !(IORef (Maybe (FanSubscribed x k)))
         }

data SwitchSubscribed x a
   = SwitchSubscribed { switchSubscribedCachedSubscribed :: !(IORef (Maybe (SwitchSubscribed x a)))
                      , switchSubscribedOccurrence :: !(IORef (Maybe a))
                      , switchSubscribedHeight :: !(IORef Height)
                      , switchSubscribedSubscribers :: !(WeakBag (Subscriber x a))
                      , switchSubscribedOwnInvalidator :: {-# NOUNPACK #-} !(Invalidator x)
                      , switchSubscribedOwnWeakInvalidator :: !(IORef (Weak (Invalidator x)))
                      , switchSubscribedBehaviorParents :: !(IORef [SomeBehaviorSubscribed x])
                      , switchSubscribedParent :: !(Behavior x (Event x a))
                      , switchSubscribedCurrentParent :: !(IORef (EventSubscription x))
                      , switchSubscribedWeakSelf :: !(IORef (Weak (SwitchSubscribed x a)))
#ifdef DEBUG_NODEIDS
                      , switchSubscribedNodeId :: Int
#endif
                      }

data Switch x a
   = Switch { switchParent :: !(Behavior x (Event x a))
            , switchSubscribed :: !(IORef (Maybe (SwitchSubscribed x a)))
            }

{-# ANN CoincidenceSubscribed "HLint: ignore Redundant bracket" #-}
data CoincidenceSubscribed x a
   = CoincidenceSubscribed { coincidenceSubscribedCachedSubscribed :: !(IORef (Maybe (CoincidenceSubscribed x a)))
                           , coincidenceSubscribedOccurrence :: !(IORef (Maybe a))
                           , coincidenceSubscribedSubscribers :: !(WeakBag (Subscriber x a))
                           , coincidenceSubscribedHeight :: !(IORef Height)
                           , coincidenceSubscribedOuter :: {-# NOUNPACK #-} (Subscriber x (Event x a))
                           , coincidenceSubscribedOuterParent :: !(EventSubscription x)
                           , coincidenceSubscribedInnerParent :: !(IORef (Maybe (EventSubscribed x)))
                           , coincidenceSubscribedWeakSelf :: !(IORef (Weak (CoincidenceSubscribed x a)))
#ifdef DEBUG_NODEIDS
                           , coincidenceSubscribedNodeId :: Int
#endif
                           }

data Coincidence x a
   = Coincidence { coincidenceParent :: !(Event x (Event x a))
                 , coincidenceSubscribed :: !(IORef (Maybe (CoincidenceSubscribed x a)))
                 }

{-# NOINLINE newInvalidatorSwitch #-}
newInvalidatorSwitch :: SwitchSubscribed x a -> IO (Invalidator x)
newInvalidatorSwitch subd = return $! InvalidatorSwitch subd

{-# NOINLINE newInvalidatorPull #-}
newInvalidatorPull :: Pull x a -> IO (Invalidator x)
newInvalidatorPull p = return $! InvalidatorPull p

instance HasSpiderTimeline x => FunctorMaybe (Event x) where
  fmapMaybe f = push $ return . f

instance HasSpiderTimeline x => Align (Event x) where
  nil = eventNever
  align ea eb = fmapMaybe dmapToThese $ merge $ dynamicConst $ DMap.fromList [LeftTag :=> ea, RightTag :=> eb]

newtype Dyn x p = Dyn { unDyn :: IORef (Either (BehaviorM x (PatchTarget p), Event x p) (Hold x p)) }

newMapDyn :: HasSpiderTimeline x => (a -> b) -> Dynamic x (Identity a) -> Dynamic x (Identity b)
newMapDyn f d = dynamicDynIdentity $ unsafeDyn (fmap f $ readBehaviorTracked $ dynamicCurrent d) (Identity . f . runIdentity <$> dynamicUpdated d)

--TODO: Avoid the duplication between this and R.zipDynWith
zipDynWith :: HasSpiderTimeline x => (a -> b -> c) -> Dynamic x (Identity a) -> Dynamic x (Identity b) -> Dynamic x (Identity c)
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
  in dynamicDynIdentity $ unsafeDyn (f <$> readBehaviorUntracked (dynamicCurrent da) <*> readBehaviorUntracked (dynamicCurrent db)) ec

unsafeDyn :: BehaviorM x (PatchTarget p) -> Event x p -> Dyn x p
unsafeDyn readV0 v' = Dyn $ unsafeNewIORef x $ Left x
  where x = (readV0, v')

-- ResultM can read behaviors and events
type ResultM = EventM

{-# NOINLINE unsafeNewIORef #-}
unsafeNewIORef :: a -> b -> IORef b
unsafeNewIORef _ b = unsafePerformIO $ newIORef b

instance HasSpiderTimeline x => Functor (Event x) where
  fmap f = push $ return . Just . f

instance Functor (Behavior x) where
  fmap f = pull . fmap f . readBehaviorTracked

{-# INLINE push #-}
push :: HasSpiderTimeline x => (a -> ComputeM x (Maybe b)) -> Event x a -> Event x b
push f e = cacheEvent (pushCheap f e)

{-# INLINABLE pull #-}
pull :: BehaviorM x a -> Behavior x a
pull a = behaviorPull $ Pull
  { pullCompute = a
  , pullValue = unsafeNewIORef a Nothing
#ifdef DEBUG_NODEIDS
  , pullNodeId = unsafeNodeId a
#endif
  }

{-# INLINABLE switch #-}
switch :: HasSpiderTimeline x => Behavior x (Event x a) -> Event x a
switch a = eventSwitch $ Switch
  { switchParent = a
  , switchSubscribed = unsafeNewIORef a Nothing
  }

coincidence :: HasSpiderTimeline x => Event x (Event x a) -> Event x a
coincidence a = eventCoincidence $ Coincidence
  { coincidenceParent = a
  , coincidenceSubscribed = unsafeNewIORef a Nothing
  }

-- Propagate the given event occurrence; before cleaning up, run the given action, which may read the state of events and behaviors
run :: forall x b. HasSpiderTimeline x => [DSum (RootTrigger x) Identity] -> ResultM x b -> SpiderHost x b
run roots after = do
  tracePropagate (Proxy :: Proxy x) $ "Running an event frame with " <> show (length roots) <> " events"
  t <- SpiderHost ask
  result <- SpiderHost $ lift $ withMVar (_spiderTimeline_lock t) $ \_ -> flip runReaderT t $ unSpiderHost $ runFrame $ do
    rootsToPropagate <- forM roots $ \r@(RootTrigger (_, occRef, k) :=> a) -> do
      occBefore <- liftIO $ do
        occBefore <- readIORef occRef
        writeIORef occRef $! DMap.insert k a occBefore
        return occBefore
      if DMap.null occBefore
        then do scheduleRootClear occRef
                return $ Just r
        else return Nothing
    forM_ (catMaybes rootsToPropagate) $ \(RootTrigger (subscribersRef, _, _) :=> Identity a) -> do
      propagate a subscribersRef
    delayedRef <- asksEventEnv eventEnvDelayedMerges
    let go = do
          delayed <- liftIO $ readIORef delayedRef
          case IntMap.minViewWithKey delayed of
            Nothing -> return ()
            Just ((currentHeight, cur), future) -> do
              tracePropagate (Proxy :: Proxy x) $ "Running height " ++ show currentHeight
              putCurrentHeight $ Height currentHeight
              liftIO $ writeIORef delayedRef $! future
              sequence_ cur
              go
    go
    putCurrentHeight maxBound
    after
  tracePropagate (Proxy :: Proxy x) "Done running an event frame"
  return result

scheduleMerge' :: HasSpiderTimeline x => Height -> IORef Height -> EventM x () -> EventM x ()
scheduleMerge' initialHeight heightRef a = scheduleMerge initialHeight $ do
  height <- liftIO $ readIORef heightRef
  currentHeight <- getCurrentHeight
  case height `compare` currentHeight of
    LT -> error "Somehow a merge's height has been decreased after it was scheduled"
    GT -> scheduleMerge' height heightRef a -- The height has been increased (by a coincidence event; TODO: is this the only way?)
    EQ -> a

data SomeClear = forall a. SomeClear (IORef (Maybe a))

data SomeRootClear = forall k. SomeRootClear (IORef (DMap k Identity))

data SomeAssignment x = forall a. SomeAssignment {-# UNPACK #-} !(IORef a) {-# UNPACK #-} !(IORef [Weak (Invalidator x)]) a

debugFinalize :: Bool
debugFinalize = False

mkWeakPtrWithDebug :: a -> String -> IO (Weak a)
mkWeakPtrWithDebug x debugNote = do
  x' <- evaluate x
  mkWeakPtr x' $
    if debugFinalize
    then Just $ putStrLn $ "finalizing: " ++ debugNote
    else Nothing

type WeakList a = [Weak a]

{-# INLINE withIncreasedDepth #-}
#ifdef DEBUG
withIncreasedDepth :: CanTrace x m => m a -> m a
withIncreasedDepth a = do
  spiderTimeline <- askSpiderTimelineEnv
  liftIO $ modifyIORef' (_spiderTimeline_depth spiderTimeline) succ
  result <- a
  liftIO $ modifyIORef' (_spiderTimeline_depth spiderTimeline) pred
  return result
#else
withIncreasedDepth :: m a -> m a
withIncreasedDepth = id
#endif

type CanTrace x m = (HasSpiderTimeline x, MonadIO m)

{-# INLINE tracePropagate #-}
tracePropagate :: (CanTrace x m) => proxy x -> String -> m ()
tracePropagate p = traceWhen p debugPropagate

{-# INLINE traceInvalidate #-}
traceInvalidate :: String -> IO ()
traceInvalidate = when debugInvalidate . liftIO . putStrLn

{-# INLINE traceWhen #-}
traceWhen :: (CanTrace x m) => proxy x -> Bool -> String -> m ()
traceWhen p b message = traceMWhen p b $ return message

{-# INLINE traceMWhen #-}
traceMWhen :: (CanTrace x m) => proxy x -> Bool -> m String -> m ()
traceMWhen _ b getMessage = when b $ do
  message <- getMessage
#ifdef DEBUG
  spiderTimeline <- askSpiderTimelineEnv
  d <- liftIO $ readIORef $ _spiderTimeline_depth spiderTimeline
#else
  let d = 0
#endif
  liftIO $ putStrLn $ replicate d ' ' <> message

whoCreatedIORef :: IORef a -> IO [String]
whoCreatedIORef (IORef a) = whoCreated $! a

#ifdef DEBUG_CYCLES
groupByHead :: Eq a => [NonEmpty a] -> [(a, NonEmpty [a])]
groupByHead = \case
  [] -> []
  (x :| xs) : t -> case groupByHead t of
    [] -> [(x, xs :| [])]
    l@((y, yss) : t')
      | x == y -> (x, xs `NonEmpty.cons` yss) : t'
      | otherwise -> (x, xs :| []) : l

listsToForest :: Eq a => [[a]] -> Forest a
listsToForest l = fmap (\(a, l') -> Node a $ listsToForest $ toList l') $ groupByHead $ catMaybes $ fmap nonEmpty l
#endif

{-# INLINE propagateSubscriberHold #-}
propagateSubscriberHold :: forall x p. (HasSpiderTimeline x, Patch p) => Hold x p -> p -> EventM x ()
propagateSubscriberHold h a = do
  {-# SCC "trace" #-} traceMWhen (Proxy :: Proxy x) debugPropagate $ liftIO $ do
    invalidators <- liftIO $ readIORef $ holdInvalidators h
    return $ "SubscriberHold" <> showNodeId h <> ": " ++ show (length invalidators)
  v <- {-# SCC "read" #-} liftIO $ readIORef $ holdValue h
  case {-# SCC "apply" #-} apply a v of
    Nothing -> return ()
    Just v' -> do
      {-# SCC "trace2" #-} withIncreasedDepth $ tracePropagate (Proxy :: Proxy x) $ "propagateSubscriberHold: assigning Hold" <> showNodeId h
      vRef <- {-# SCC "vRef" #-} liftIO $ evaluate $ holdValue h
      iRef <- {-# SCC "iRef" #-} liftIO $ evaluate $ holdInvalidators h
      defer $ {-# SCC "assignment" #-} SomeAssignment vRef iRef v'

data SomeResetCoincidence x = forall a. SomeResetCoincidence !(EventSubscription x) !(Maybe (CoincidenceSubscribed x a)) -- The CoincidenceSubscriber will be present only if heights need to be reset

runBehaviorM :: BehaviorM x a -> Maybe (Weak (Invalidator x), IORef [SomeBehaviorSubscribed x]) -> IORef [SomeHoldInit x] -> IO a
runBehaviorM a mwi holdInits = runReaderT (unBehaviorM a) (mwi, holdInits)

askInvalidator :: BehaviorM x (Maybe (Weak (Invalidator x)))
askInvalidator = do
  (!m, _) <- BehaviorM ask
  case m of
    Nothing -> return Nothing
    Just (!wi, _) -> return $ Just wi

askParentsRef :: BehaviorM x (Maybe (IORef [SomeBehaviorSubscribed x]))
askParentsRef = do
  (!m, _) <- BehaviorM ask
  case m of
    Nothing -> return Nothing
    Just (_, !p) -> return $ Just p

askBehaviorHoldInits :: BehaviorM x (IORef [SomeHoldInit x])
askBehaviorHoldInits = do
  (_, !his) <- BehaviorM ask
  return his

{-# INLINE getDynHold #-}
getDynHold :: (Defer (SomeHoldInit x) m, Patch p) => Dyn x p -> m (Hold x p)
getDynHold d = do
  mh <- liftIO $ readIORef $ unDyn d
  case mh of
    Right h -> return h
    Left (readV0, v') -> do
      holdInits <- getDeferralQueue
      v0 <- liftIO $ runBehaviorM readV0 Nothing holdInits
      h <- hold v0 v'
      liftIO $ writeIORef (unDyn d) $ Right h
      return h

-- Always refers to 0
{-# NOINLINE zeroRef #-}
zeroRef :: IORef Height
zeroRef = unsafePerformIO $ newIORef zeroHeight

type CanSubscribe x m =
  ( HasSpiderTimeline x
  , HasCurrentHeight x m
  , Defer (SomeHoldInit x) m, Defer (SomeMergeInit x) m -- The things we need to lazily initialize state
  , Defer SomeClear m -- Wiping out event firings
  , Defer (SomeAssignment x) m -- Updating state
  , Defer (SomeResetCoincidence x) m
  , m ~ EventM x
  )

getRootSubscribed :: GCompare k => k a -> Root x k -> Subscriber x a -> IO (WeakBagTicket, RootSubscribed x a, Maybe a)
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
      when debugPropagate $ putStrLn $ "getRootSubscribed: calling rootInit"
      uninit <- rootInit r k $ RootTrigger (subs, rootOccurrence r, k)
      writeIORef uninitRef $! uninit
      let subscribed = RootSubscribed
            { rootSubscribedKey = k
            , rootSubscribedCachedSubscribed = cached
            , rootSubscribedOccurrence = getOcc
            , rootSubscribedSubscribers = subs
            , rootSubscribedUninit = uninit
            , rootSubscribedWeakSelf = weakSelf
#ifdef DEBUG_NODEIDS
            , rootSubscribedNodeId = unsafeNodeId (k, r, subs)
#endif
            }
      writeIORef weakSelf =<< evaluate =<< mkWeakPtrWithDebug subscribed "RootSubscribed"
      modifyIORef' (rootSubscribed r) $ DMap.insertWith (error $ "getRootSubscribed: duplicate key inserted into Root") k subscribed --TODO: I think we can just write back mSubscribed rather than re-reading it
      occ <- getOcc
      return (sln, subscribed, occ)

{-# ANN cleanupRootSubscribed "HLint: ignore Redundant bracket" #-}
cleanupRootSubscribed :: RootSubscribed x a -> IO ()
cleanupRootSubscribed self@(RootSubscribed { rootSubscribedKey = k, rootSubscribedCachedSubscribed = cached }) = do
  rootSubscribedUninit self
  modifyIORef' cached $ DMap.delete k

{-# INLINE subscribeRootSubscribed #-}
subscribeRootSubscribed :: RootSubscribed x a -> Subscriber x a -> IO WeakBagTicket
subscribeRootSubscribed subscribed sub = WeakBag.insert sub (rootSubscribedSubscribers subscribed) (rootSubscribedWeakSelf subscribed) cleanupRootSubscribed

{-# INLINABLE getFanSubscribed #-}
getFanSubscribed :: (Defer (SomeAssignment x) m, Defer (SomeHoldInit x) m, Defer (SomeMergeInit x) m, Defer SomeClear m, HasCurrentHeight x m, Defer (SomeResetCoincidence x) m, HasSpiderTimeline x, m ~ EventM x, GCompare k) => k a -> Fan x k -> Subscriber x a -> m (WeakBagTicket, FanSubscribed x k, Maybe a)
getFanSubscribed k f sub = do
  mSubscribed <- liftIO $ readIORef $ fanSubscribed f
  case mSubscribed of
    Just subscribed -> {-# SCC "hitFan" #-} liftIO $ do
      sln <- subscribeFanSubscribed k subscribed sub
      occ <- readIORef $ fanSubscribedOccurrence subscribed
      return (sln, subscribed, coerce $ DMap.lookup k =<< occ)
    Nothing -> {-# SCC "missFan" #-} do
      subscribedRef <- liftIO $ newIORef $ error "getFanSubscribed: subscribedRef not yet initialized"
      subscribedUnsafe <- liftIO $ unsafeInterleaveIO $ readIORef subscribedRef
      s <- liftIO $ newSubscriberFan subscribedUnsafe
      (subscription, parentOcc) <- subscribeAndRead (fanParent f) s
      weakSelf <- liftIO $ newIORef $ error "getFanSubscribed: weakSelf not yet initialized"
      (subsForK, slnForSub) <- liftIO $ WeakBag.singleton sub weakSelf cleanupFanSubscribed
      subscribersRef <- liftIO $ newIORef $ error "getFanSubscribed: subscribersRef not yet initialized"
      occRef <- liftIO $ newIORef parentOcc
      when (isJust parentOcc) $ scheduleClear occRef
      let subscribed = FanSubscribed
            { fanSubscribedCachedSubscribed = fanSubscribed f
            , fanSubscribedOccurrence = occRef
            , fanSubscribedParent = subscription
            , fanSubscribedSubscribers = subscribersRef
#ifdef DEBUG_NODEIDS
            , fanSubscribedNodeId = unsafeNodeId f
#endif
            }
      let !self = (k, subscribed)
      liftIO $ writeIORef subscribersRef $! DMap.singleton k $ FanSubscribedChildren subsForK self weakSelf
      liftIO $ writeIORef weakSelf =<< evaluate =<< mkWeakPtrWithDebug self "FanSubscribed"
      liftIO $ writeIORef subscribedRef $! subscribed
      liftIO $ writeIORef (fanSubscribed f) $ Just subscribed
      return (slnForSub, subscribed, coerce $ DMap.lookup k =<< parentOcc)

cleanupFanSubscribed :: GCompare k => (k a, FanSubscribed x k) -> IO ()
cleanupFanSubscribed (k, subscribed) = do
  subscribers <- readIORef $ fanSubscribedSubscribers subscribed
  let reducedSubscribers = DMap.delete k subscribers
  if DMap.null reducedSubscribers
    then do
      unsubscribe $ fanSubscribedParent subscribed
      -- Not necessary in this case, because this whole FanSubscribed is dead: writeIORef (fanSubscribedSubscribers subscribed) reducedSubscribers
      writeIORef (fanSubscribedCachedSubscribed subscribed) Nothing
    else writeIORef (fanSubscribedSubscribers subscribed) $! reducedSubscribers

{-# INLINE subscribeFanSubscribed #-}
subscribeFanSubscribed :: GCompare k => k a -> FanSubscribed x k -> Subscriber x a -> IO WeakBagTicket
subscribeFanSubscribed k subscribed sub = do
  subscribers <- readIORef $ fanSubscribedSubscribers subscribed
  case DMap.lookup k subscribers of
    Nothing -> {-# SCC "missSubscribeFanSubscribed" #-} do
      let !self = (k, subscribed)
      weakSelf <- newIORef =<< mkWeakPtrWithDebug self "FanSubscribed"
      (list, sln) <- WeakBag.singleton sub weakSelf cleanupFanSubscribed
      writeIORef (fanSubscribedSubscribers subscribed) $! DMap.insertWith (error "subscribeFanSubscribed: key that we just failed to find is present - should be impossible") k (FanSubscribedChildren list self weakSelf) subscribers
      return sln
    Just (FanSubscribedChildren list _ weakSelf) -> {-# SCC "hitSubscribeFanSubscribed" #-} WeakBag.insert sub list weakSelf cleanupFanSubscribed

{-# INLINABLE getSwitchSubscribed #-}
getSwitchSubscribed :: (Defer (SomeHoldInit x) m, Defer (SomeAssignment x) m, Defer (SomeMergeInit x) m, Defer SomeClear m, Defer (SomeResetCoincidence x) m, HasCurrentHeight x m, HasSpiderTimeline x, m ~ EventM x) => Switch x a -> Subscriber x a -> m (WeakBagTicket, SwitchSubscribed x a, Maybe a)
getSwitchSubscribed s sub = do
  mSubscribed <- liftIO $ readIORef $ switchSubscribed s
  case mSubscribed of
    Just subscribed -> {-# SCC "hitSwitch" #-} liftIO $ do
      sln <- subscribeSwitchSubscribed subscribed sub
      occ <- readIORef $ switchSubscribedOccurrence subscribed
      return (sln, subscribed, occ)
    Nothing -> {-# SCC "missSwitch" #-} do
      subscribedRef <- liftIO $ newIORef $ error "getSwitchSubscribed: subscribed has not yet been created"
      subscribedUnsafe <- liftIO $ unsafeInterleaveIO $ readIORef subscribedRef
      i <- liftIO $ newInvalidatorSwitch subscribedUnsafe
      mySub <- liftIO $ newSubscriberSwitch subscribedUnsafe
      wi <- liftIO $ mkWeakPtrWithDebug i "InvalidatorSwitch"
      wiRef <- liftIO $ newIORef wi
      parentsRef <- liftIO $ newIORef [] --TODO: This should be unnecessary, because it will always be filled with just the single parent behavior
      holdInits <- getDeferralQueue
      e <- liftIO $ runBehaviorM (readBehaviorTracked (switchParent s)) (Just (wi, parentsRef)) holdInits
      (subscription@(EventSubscription _ subd), parentOcc) <- subscribeAndRead e mySub
      heightRef <- liftIO $ newIORef =<< getEventSubscribedHeight subd
      subscriptionRef <- liftIO $ newIORef subscription
      occRef <- liftIO $ newIORef parentOcc
      when (isJust parentOcc) $ scheduleClear occRef
      weakSelf <- liftIO $ newIORef $ error "getSwitchSubscribed: weakSelf not yet initialized"
      (subs, slnForSub) <- liftIO $ WeakBag.singleton sub weakSelf cleanupSwitchSubscribed
      let !subscribed = SwitchSubscribed
            { switchSubscribedCachedSubscribed = switchSubscribed s
            , switchSubscribedOccurrence = occRef
            , switchSubscribedHeight = heightRef
            , switchSubscribedSubscribers = subs
            , switchSubscribedOwnInvalidator = i
            , switchSubscribedOwnWeakInvalidator = wiRef
            , switchSubscribedBehaviorParents = parentsRef
            , switchSubscribedParent = switchParent s
            , switchSubscribedCurrentParent = subscriptionRef
            , switchSubscribedWeakSelf = weakSelf
#ifdef DEBUG_NODEIDS
            , switchSubscribedNodeId = unsafeNodeId s
#endif
            }
      liftIO $ writeIORef weakSelf =<< evaluate =<< mkWeakPtrWithDebug subscribed "switchSubscribedWeakSelf"
      liftIO $ writeIORef subscribedRef $! subscribed
      liftIO $ writeIORef (switchSubscribed s) $ Just subscribed
      return (slnForSub, subscribed, parentOcc)

cleanupSwitchSubscribed :: SwitchSubscribed x a -> IO ()
cleanupSwitchSubscribed subscribed = do
  unsubscribe =<< readIORef (switchSubscribedCurrentParent subscribed)
  finalize =<< readIORef (switchSubscribedOwnWeakInvalidator subscribed) -- We don't need to get invalidated if we're dead
  writeIORef (switchSubscribedCachedSubscribed subscribed) Nothing

{-# INLINE subscribeSwitchSubscribed #-}
subscribeSwitchSubscribed :: SwitchSubscribed x a -> Subscriber x a -> IO WeakBagTicket
subscribeSwitchSubscribed subscribed sub = WeakBag.insert sub (switchSubscribedSubscribers subscribed) (switchSubscribedWeakSelf subscribed) cleanupSwitchSubscribed

{-# INLINABLE getCoincidenceSubscribed #-}
getCoincidenceSubscribed :: forall x a m. (Defer (SomeAssignment x) m, Defer (SomeHoldInit x) m, Defer (SomeMergeInit x) m, Defer SomeClear m, HasCurrentHeight x m, Defer (SomeResetCoincidence x) m, HasSpiderTimeline x, m ~ EventM x) => Coincidence x a -> Subscriber x a -> m (WeakBagTicket, CoincidenceSubscribed x a, Maybe a)
getCoincidenceSubscribed c sub = do
  mSubscribed <- liftIO $ readIORef $ coincidenceSubscribed c
  case mSubscribed of
    Just subscribed -> {-# SCC "hitCoincidence" #-} liftIO $ do
      sln <- subscribeCoincidenceSubscribed subscribed sub
      occ <- readIORef $ coincidenceSubscribedOccurrence subscribed
      return (sln, subscribed, occ)
    Nothing -> {-# SCC "missCoincidence" #-} do
      subscribedRef <- liftIO $ newIORef $ error "getCoincidenceSubscribed: subscribed has not yet been created"
      subscribedUnsafe <- liftIO $ unsafeInterleaveIO $ readIORef subscribedRef
      subOuter <- liftIO $ newSubscriberCoincidenceOuter subscribedUnsafe
      (outerSubscription@(EventSubscription _ outerSubd), outerOcc) <- subscribeAndRead (coincidenceParent c) subOuter
      outerHeight <- liftIO $ getEventSubscribedHeight outerSubd
      (occ, height, mInnerSubd) <- case outerOcc of
        Nothing -> return (Nothing, outerHeight, Nothing)
        Just o -> do
          (occ, height, innerSubd) <- subscribeCoincidenceInner o outerHeight subscribedUnsafe
          return (occ, height, Just innerSubd)
      occRef <- liftIO $ newIORef occ
      when (isJust occ) $ scheduleClear occRef
      heightRef <- liftIO $ newIORef height
      innerSubdRef <- liftIO $ newIORef mInnerSubd
      scheduleClear innerSubdRef
      weakSelf <- liftIO $ newIORef $ error "getCoincidenceSubscribed: weakSelf not yet implemented"
      (subs, slnForSub) <- liftIO $ WeakBag.singleton sub weakSelf cleanupCoincidenceSubscribed
      let subscribed = CoincidenceSubscribed
            { coincidenceSubscribedCachedSubscribed = coincidenceSubscribed c
            , coincidenceSubscribedOccurrence = occRef
            , coincidenceSubscribedHeight = heightRef
            , coincidenceSubscribedSubscribers = subs
            , coincidenceSubscribedOuter = subOuter
            , coincidenceSubscribedOuterParent = outerSubscription
            , coincidenceSubscribedInnerParent = innerSubdRef
            , coincidenceSubscribedWeakSelf = weakSelf
#ifdef DEBUG_NODEIDS
            , coincidenceSubscribedNodeId = unsafeNodeId c
#endif
            }
      liftIO $ writeIORef weakSelf =<< evaluate =<< mkWeakPtrWithDebug subscribed "CoincidenceSubscribed"
      liftIO $ writeIORef subscribedRef $! subscribed
      liftIO $ writeIORef (coincidenceSubscribed c) $ Just subscribed
      return (slnForSub, subscribed, occ)

cleanupCoincidenceSubscribed :: CoincidenceSubscribed x a -> IO ()
cleanupCoincidenceSubscribed subscribed = do
  unsubscribe $ coincidenceSubscribedOuterParent subscribed
  writeIORef (coincidenceSubscribedCachedSubscribed subscribed) Nothing

{-# INLINE subscribeCoincidenceSubscribed #-}
subscribeCoincidenceSubscribed :: CoincidenceSubscribed x a -> Subscriber x a -> IO WeakBagTicket
subscribeCoincidenceSubscribed subscribed sub = WeakBag.insert sub (coincidenceSubscribedSubscribers subscribed) (coincidenceSubscribedWeakSelf subscribed) cleanupCoincidenceSubscribed

{-# INLINE merge #-}
merge :: forall k x. (HasSpiderTimeline x, GCompare k) => Dynamic x (PatchDMap k (Event x)) -> Event x (DMap k Identity)
merge d = cacheEvent (mergeCheap d)

{-# INLINE [1] mergeCheap #-}
mergeCheap :: forall k x. (HasSpiderTimeline x, GCompare k) => Dynamic x (PatchDMap k (Event x)) -> Event x (DMap k Identity)
mergeCheap d = Event $ \sub -> do
  initialParents <- readBehaviorUntracked $ dynamicCurrent d
  accumRef <- liftIO $ newIORef $ error "merge: accumRef not yet initialized"
  heightRef <- liftIO $ newIORef $ error "merge: heightRef not yet initialized"
  heightBagRef <- liftIO $ newIORef $ error "merge: heightBagRef not yet initialized"
  parentsRef <- liftIO $ newIORef $ error "merge: parentsRef not yet initialized"
  let scheduleSelf height = scheduleMerge' height heightRef $ do
        m <- liftIO $ readIORef accumRef
        liftIO $ writeIORef accumRef $! DMap.empty
        --TODO: Assert that m is not empty
        subscriberPropagate sub m
      invalidateMyHeight = do
        oldHeight <- readIORef heightRef
        when (oldHeight /= invalidHeight) $ do -- If the height used to be valid, it must be invalid now; we should never have *more* heights than we have parents
          writeIORef heightRef $! invalidHeight
          subscriberInvalidateHeight sub oldHeight
      revalidateMyHeight = do
        currentHeight <- readIORef heightRef
        when (currentHeight == invalidHeight) $ do -- revalidateMergeHeight may be called multiple times; perhaps the's a way to finesse it to avoid this check
          heights <- readIORef heightBagRef
          parents <- readIORef parentsRef
          -- When the number of heights in the bag reaches the number of parents, we should have a valid height
          case heightBagSize heights `compare` DMap.size parents of
            LT -> return ()
            EQ -> do
              let height = succHeight $ heightBagMax heights
              when debugInvalidateHeight $ putStrLn $ "recalculateSubscriberHeight: height: " <> show height
              writeIORef heightRef $! height
              subscriberRecalculateHeight sub height
            GT -> error $ "revalidateMergeHeight: more heights than parents for Merge"
      subscriber :: forall a. k a -> Subscriber x a
      subscriber k = Subscriber
        { subscriberPropagate = \a -> do
            oldM <- liftIO $ readIORef accumRef
            let newM = DMap.insertWith (error $ "Same key fired multiple times for Merge") k (Identity a) oldM
            tracePropagate (Proxy :: Proxy x) $ "  DMap.size oldM = " <> show (DMap.size oldM) <> "; DMap.size newM = " <> show (DMap.size newM)
            liftIO $ writeIORef accumRef $! newM
            when (DMap.null oldM) $ do -- Only schedule the firing once
              height <- liftIO $ readIORef heightRef
              --TODO: assertions about height
              currentHeight <- getCurrentHeight
              when (height <= currentHeight) $ do
                if height /= invalidHeight
                  then do
                  myStack <- liftIO $ whoCreatedIORef undefined --TODO
                  error $ "Height (" ++ show height ++ ") is not greater than current height (" ++ show currentHeight ++ ")\n" ++ unlines (reverse myStack)
                  else liftIO $ do
#ifdef DEBUG_CYCLES
                  nodesInvolvedInCycle <- walkInvalidHeightParents $ eventSubscribedMerge subscribed
                  stacks <- forM nodesInvolvedInCycle $ \(Some.This es) -> whoCreatedEventSubscribed es
                  let cycleInfo = ":\n" <> drawForest (listsToForest stacks)
#else
                  let cycleInfo = ""
#endif
                  error $ "Causality loop found" <> cycleInfo
              scheduleSelf height
        , subscriberInvalidateHeight = \old -> do --TODO: When removing a parent doesn't actually change the height, maybe we can avoid invalidating
            modifyIORef' heightBagRef $ heightBagRemove old
            invalidateMyHeight
        , subscriberRecalculateHeight = \new -> do
            modifyIORef' heightBagRef $ heightBagAdd new
            revalidateMyHeight
        }
  subscribers :: [(Maybe (DSum k Identity), Height, DSum k (MergeSubscribedParent x))] <- forM (DMap.toList initialParents) $ \(k :=> e) -> do
    let s = subscriber k
    (subscription@(EventSubscription _ parentSubd), parentOcc) <- subscribeAndRead e s
    height <- liftIO $ getEventSubscribedHeight parentSubd
    return $ (fmap (\x -> k :=> Identity x) parentOcc, height, k :=> MergeSubscribedParent subscription)
  let dm = DMap.fromDistinctAscList $ mapMaybe (\(x, _, _) -> x) subscribers
      heights = fmap (\(_, h, _) -> h) subscribers --TODO: Assert that there's no invalidHeight in here
      myHeightBag = heightBagFromList $ filter (/= invalidHeight) heights
      myHeight = if invalidHeight `elem` heights
                 then invalidHeight
                 else succHeight $ heightBagMax myHeightBag
  currentHeight <- getCurrentHeight
  let (occ, accum) = if currentHeight >= myHeight -- If we should have fired by now
                     then (if DMap.null dm then Nothing else Just dm, DMap.empty)
                     else (Nothing, dm)
  unless (DMap.null accum) $ do
    scheduleSelf myHeight
  occRef <- liftIO $ newIORef occ
  when (isJust occ) $ scheduleClear occRef
  liftIO $ writeIORef accumRef $! accum
  liftIO $ writeIORef heightRef $! myHeight
  liftIO $ writeIORef heightBagRef $! myHeightBag
  changeSubdRef <- liftIO $ newIORef $ error "getMergeSubscribed: changeSubdRef not yet initialized"
  liftIO $ writeIORef parentsRef $! DMap.fromDistinctAscList $ map (\(_, _, x) -> x) subscribers
  let updateMe (PatchDMap p) = do
        oldParents <- liftIO $ readIORef parentsRef
        let f (subscriptionsToKill, ps) (k :=> ComposeMaybe me) = do
              (mOldSubd, newPs) <- case me of
                Nothing -> return $ DMap.updateLookupWithKey (\_ _ -> Nothing) k ps
                Just e -> do
                  let s = subscriber k
                  subscription@(EventSubscription _ subd) <- subscribe e s
                  newParentHeight <- liftIO $ getEventSubscribedHeight subd
                  let newParent = MergeSubscribedParent subscription
                  liftIO $ modifyIORef' heightBagRef $ heightBagAdd newParentHeight
                  return $ DMap.insertLookupWithKey' (\_ new _ -> new) k newParent ps
              forM_ mOldSubd $ \oldSubd -> do
                oldHeight <- liftIO $ getEventSubscribedHeight $ _eventSubscription_subscribed $ unMergeSubscribedParent oldSubd
                liftIO $ modifyIORef heightBagRef $ heightBagRemove oldHeight
              return (maybeToList (unMergeSubscribedParent <$> mOldSubd) ++ subscriptionsToKill, newPs)
        (subscriptionsToKill, newParents) <- foldM f ([], oldParents) $ DMap.toList p --TODO: I think this runEventM is OK, since no events are firing at this time, but it might not be
        liftIO $ writeIORef parentsRef $! newParents
        return subscriptionsToKill
  defer $ SomeMergeInit $ do
    let s = Subscriber
          { subscriberPropagate = \a -> {-# SCC "traverseMergeChange" #-} do
              tracePropagate (Proxy :: Proxy x) $ "SubscriberMerge/Change"
              defer $ SomeMergeUpdate (updateMe a) invalidateMyHeight revalidateMyHeight
          , subscriberInvalidateHeight = \_ -> return ()
          , subscriberRecalculateHeight = \_ -> return ()
          }
    (EventSubscription _ changeSubd, change) <- subscribeAndRead (dynamicUpdated d) s
    forM_ change $ \c -> defer $ SomeMergeUpdate (updateMe c) invalidateMyHeight revalidateMyHeight
    liftIO $ writeIORef changeSubdRef (s, changeSubd)
  let unsubscribeAll = do
        parents <- readIORef parentsRef
        forM_ (DMap.toList parents) $ \(_ :=> MergeSubscribedParent s) -> unsubscribe s
  return ( EventSubscription unsubscribeAll $ EventSubscribed heightRef $ toAny (parentsRef, changeSubdRef)
         , occ
         )

newtype EventSelector x k = EventSelector { select :: forall a. k a -> Event x a }

fan :: (HasSpiderTimeline x, GCompare k) => Event x (DMap k Identity) -> EventSelector x k
fan e =
  let f = Fan
        { fanParent = e
        , fanSubscribed = unsafeNewIORef e Nothing
        }
  in EventSelector $ \k -> eventFan k f

runHoldInits :: HasSpiderTimeline x => IORef [SomeHoldInit x] -> IORef [SomeMergeInit x] -> EventM x ()
runHoldInits holdInitRef mergeInitRef = do
  holdInits <- liftIO $ readIORef holdInitRef
  mergeInits <- liftIO $ readIORef mergeInitRef
  unless (null holdInits && null mergeInits) $ do
    liftIO $ writeIORef holdInitRef []
    liftIO $ writeIORef mergeInitRef []
    mapM_ initHold holdInits
    mapM_ unSomeMergeInit mergeInits
    runHoldInits holdInitRef mergeInitRef

initHold :: HasSpiderTimeline x => SomeHoldInit x -> EventM x ()
initHold (SomeHoldInit h) = void $ getHoldEventSubscription h

newEventEnv :: IO (EventEnv x)
newEventEnv = do
  toAssignRef <- newIORef [] -- This should only actually get used when events are firing
  holdInitRef <- newIORef []
  mergeUpdateRef <- newIORef []
  mergeInitRef <- newIORef []
  heightRef <- newIORef zeroHeight
  toClearRef <- newIORef []
  toClearRootRef <- newIORef []
  coincidenceInfosRef <- newIORef []
  delayedRef <- newIORef IntMap.empty
  return $ EventEnv toAssignRef holdInitRef mergeUpdateRef mergeInitRef toClearRef toClearRootRef heightRef coincidenceInfosRef delayedRef

clearEventEnv :: EventEnv x -> IO ()
clearEventEnv (EventEnv toAssignRef holdInitRef mergeUpdateRef mergeInitRef toClearRef toClearRootRef heightRef coincidenceInfosRef delayedRef) = do
  writeIORef toAssignRef []
  writeIORef holdInitRef []
  writeIORef mergeUpdateRef []
  writeIORef mergeInitRef []
  writeIORef heightRef zeroHeight
  writeIORef toClearRef []
  writeIORef toClearRootRef []
  writeIORef coincidenceInfosRef []
  writeIORef delayedRef IntMap.empty

-- | Run an event action outside of a frame
runFrame :: forall x a. HasSpiderTimeline x => EventM x a -> SpiderHost x a --TODO: This function also needs to hold the mutex
runFrame a = SpiderHost $ ask >>= \_ -> lift $ do
  let env = _spiderTimeline_eventEnv (spiderTimeline :: SpiderTimelineEnv x)
  let go = do
        result <- a
        runHoldInits (eventEnvHoldInits env) (eventEnvMergeInits env) -- This must happen before doing the assignments, in case subscribing a Hold causes existing Holds to be read by the newly-propagated events
        return result
  result <- runEventM go
  toClear <- readIORef $ eventEnvClears env
  forM_ toClear $ \(SomeClear ref) -> {-# SCC "clear" #-} writeIORef ref Nothing
  toClearRoot <- readIORef $ eventEnvRootClears env
  forM_ toClearRoot $ \(SomeRootClear ref) -> {-# SCC "rootClear" #-} writeIORef ref $! DMap.empty
  toAssign <- readIORef $ eventEnvAssignments env
  toReconnectRef <- newIORef []
  coincidenceInfos <- readIORef $ eventEnvResetCoincidences env
  forM_ toAssign $ \(SomeAssignment vRef iRef v) -> {-# SCC "assignment" #-} do
    writeIORef vRef v
    when debugInvalidate $ putStrLn $ "Invalidating Hold"
    writeIORef iRef =<< evaluate =<< invalidate toReconnectRef =<< readIORef iRef
  mergeUpdates <- readIORef $ eventEnvMergeUpdates env
  writeIORef (eventEnvMergeUpdates env) []
  when debugPropagate $ putStrLn "Updating merges"
  mergeSubscriptionsToKill <- runEventM $ concat <$> mapM _someMergeUpdate_update mergeUpdates
  when debugPropagate $ putStrLn "Updating merges done"
  toReconnect <- readIORef toReconnectRef
  clearEventEnv env
  switchSubscriptionsToKill <- forM toReconnect $ \(SomeSwitchSubscribed subscribed) -> {-# SCC "switchSubscribed" #-} do
    oldSubscription <- readIORef $ switchSubscribedCurrentParent subscribed
    wi <- readIORef $ switchSubscribedOwnWeakInvalidator subscribed
    when debugInvalidate $ putStrLn $ "Finalizing invalidator for Switch" <> showNodeId subscribed
    finalize wi
    i <- evaluate $ switchSubscribedOwnInvalidator subscribed
    wi' <- mkWeakPtrWithDebug i "wi'"
    writeIORef (switchSubscribedOwnWeakInvalidator subscribed) $! wi'
    writeIORef (switchSubscribedBehaviorParents subscribed) []
    writeIORef (eventEnvHoldInits env) [] --TODO: Should we reuse this?
    e <- runBehaviorM (readBehaviorTracked (switchSubscribedParent subscribed)) (Just (wi', switchSubscribedBehaviorParents subscribed)) $ eventEnvHoldInits env
    runEventM $ runHoldInits (eventEnvHoldInits env) (eventEnvMergeInits env) --TODO: Is this actually OK? It seems like it should be, since we know that no events are firing at this point, but it still seems inelegant
    --TODO: Make sure we touch the pieces of the SwitchSubscribed at the appropriate times
    sub <- newSubscriberSwitch subscribed
    subscription <- runReaderT (unSpiderHost (runFrame ({-# SCC "subscribeSwitch" #-} subscribe e sub))) spiderTimeline --TODO: Assert that the event isn't firing --TODO: This should not loop because none of the events should be firing, but still, it is inefficient
    {-
    stackTrace <- liftIO $ fmap renderStack $ ccsToStrings =<< (getCCSOf $! switchSubscribedParent subscribed)
    liftIO $ putStrLn $ (++stackTrace) $ "subd' subscribed to " ++ case e of
      EventRoot _ -> "EventRoot"
      EventNever -> "EventNever"
      _ -> "something else"
    -}
    writeIORef (switchSubscribedCurrentParent subscribed) $! subscription
    return oldSubscription
  liftIO $ mapM_ unsubscribe mergeSubscriptionsToKill
  liftIO $ mapM_ unsubscribe switchSubscriptionsToKill
  forM_ toReconnect $ \(SomeSwitchSubscribed subscribed) -> {-# SCC "switchSubscribed" #-} do
    EventSubscription _ subd' <- readIORef $ switchSubscribedCurrentParent subscribed
    parentHeight <- getEventSubscribedHeight subd'
    myHeight <- readIORef $ switchSubscribedHeight subscribed
    when (parentHeight /= myHeight) $ do
      writeIORef (switchSubscribedHeight subscribed) $! invalidHeight
      WeakBag.traverse (switchSubscribedSubscribers subscribed) $ invalidateSubscriberHeight myHeight
  mapM_ _someMergeUpdate_invalidateHeight mergeUpdates --TODO: In addition to when the patch is completely empty, we should also not run this if it has some Nothing values, but none of them have actually had any effect; potentially, we could even check for Just values with no effect (e.g. by comparing their IORefs and ignoring them if they are unchanged); actually, we could just check if the new height is different
  forM_ coincidenceInfos $ \(SomeResetCoincidence subscription mcs) -> do
    unsubscribe subscription
    mapM_ invalidateCoincidenceHeight mcs
  forM_ coincidenceInfos $ \(SomeResetCoincidence _ mcs) -> mapM_ recalculateCoincidenceHeight mcs
  mapM_ _someMergeUpdate_recalculateHeight mergeUpdates
  forM_ toReconnect $ \(SomeSwitchSubscribed subscribed) -> do
    height <- calculateSwitchHeight subscribed
    updateSwitchHeight height subscribed
  return result

newtype Height = Height { unHeight :: Int } deriving (Show, Read, Eq, Ord, Bounded)

{-# INLINE zeroHeight #-}
zeroHeight :: Height
zeroHeight = Height 0

{-# INLINE invalidHeight #-}
invalidHeight :: Height
invalidHeight = Height (-1000)

#ifdef DEBUG_CYCLES
-- | An invalid height that is currently being traversed, e.g. by walkInvalidHeightParents
{-# INLINE invalidHeightBeingTraversed #-}
invalidHeightBeingTraversed :: Height
invalidHeightBeingTraversed = Height (-1001)
#endif

{-# INLINE succHeight #-}
succHeight :: Height -> Height
succHeight h@(Height a) =
  if h == invalidHeight
  then invalidHeight
  else Height $ succ a

invalidateCoincidenceHeight :: CoincidenceSubscribed x a -> IO ()
invalidateCoincidenceHeight subscribed = do
  oldHeight <- readIORef $ coincidenceSubscribedHeight subscribed
  when (oldHeight /= invalidHeight) $ do
    writeIORef (coincidenceSubscribedHeight subscribed) $! invalidHeight
    WeakBag.traverse (coincidenceSubscribedSubscribers subscribed) $ invalidateSubscriberHeight oldHeight

updateSwitchHeight :: Height -> SwitchSubscribed x a -> IO ()
updateSwitchHeight new subscribed = do
  oldHeight <- readIORef $ switchSubscribedHeight subscribed
  when (oldHeight == invalidHeight) $ do --TODO: This 'when' should probably be an assertion
    when (new /= invalidHeight) $ do --TODO: This 'when' should probably be an assertion
      writeIORef (switchSubscribedHeight subscribed) $! new
      WeakBag.traverse (switchSubscribedSubscribers subscribed) $ recalculateSubscriberHeight new

recalculateCoincidenceHeight :: CoincidenceSubscribed x a -> IO ()
recalculateCoincidenceHeight subscribed = do
  oldHeight <- readIORef $ coincidenceSubscribedHeight subscribed
  when (oldHeight == invalidHeight) $ do --TODO: This 'when' should probably be an assertion
    height <- calculateCoincidenceHeight subscribed
    when (height /= invalidHeight) $ do
      writeIORef (coincidenceSubscribedHeight subscribed) $! height
      WeakBag.traverse (coincidenceSubscribedSubscribers subscribed) $ recalculateSubscriberHeight height

calculateSwitchHeight :: SwitchSubscribed x a -> IO Height
calculateSwitchHeight subscribed = getEventSubscribedHeight . _eventSubscription_subscribed =<< readIORef (switchSubscribedCurrentParent subscribed)

calculateCoincidenceHeight :: CoincidenceSubscribed x a -> IO Height
calculateCoincidenceHeight subscribed = do
  outerHeight <- getEventSubscribedHeight $ _eventSubscription_subscribed $ coincidenceSubscribedOuterParent subscribed
  innerHeight <- maybe (return zeroHeight) getEventSubscribedHeight =<< readIORef (coincidenceSubscribedInnerParent subscribed)
  return $ if outerHeight == invalidHeight || innerHeight == invalidHeight then invalidHeight else max outerHeight innerHeight

data SomeSwitchSubscribed x = forall a. SomeSwitchSubscribed {-# NOUNPACK #-} (SwitchSubscribed x a)

invalidate :: IORef [SomeSwitchSubscribed x] -> WeakList (Invalidator x) -> IO (WeakList (Invalidator x))
invalidate toReconnectRef wis = do
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
  return [] -- Since we always finalize everything, always return an empty list --TODO: There are some things that will need to be re-subscribed every time; we should try to avoid finalizing them

--------------------------------------------------------------------------------
-- Reflex integration
--------------------------------------------------------------------------------

#ifndef SPECIALIZE_TO_SPIDERTIMELINE_GLOBAL
-- When we're specializing, we include this from Reflex.Class instead, because
-- it needs access to these types; when we're not, we want them here, so our
-- instance isn't an orphan instance.
#include "SpiderTimeline.include.hs"
#include "SpiderTimelineHost.include.hs"
#endif

unsafeNewSpiderTimelineEnv :: forall x. IO (SpiderTimelineEnv x)
unsafeNewSpiderTimelineEnv = do
  lock <- newMVar ()
  env <- newEventEnv
#ifdef DEBUG
  depthRef <- newIORef 0
#endif
  return $ SpiderTimelineEnv
    { _spiderTimeline_lock = lock
    , _spiderTimeline_eventEnv = env
#ifdef DEBUG
    , _spiderTimeline_depth = depthRef
#endif
    }

#ifndef SPECIALIZE_TO_SPIDERTIMELINE_GLOBAL
-- | Create a new SpiderTimelineEnv
newSpiderTimeline :: IO (Some SpiderTimelineEnv)
newSpiderTimeline = withSpiderTimeline (pure . Some.This)

data LocalSpiderTimeline x s

instance Reifies s (SpiderTimelineEnv x) =>
         HasSpiderTimeline (LocalSpiderTimeline x s) where
  spiderTimeline = localSpiderTimeline (Proxy :: Proxy s) $ reflect (Proxy :: Proxy s)

localSpiderTimeline
  :: Proxy s
  -> SpiderTimelineEnv x
  -> SpiderTimelineEnv (LocalSpiderTimeline x s)
localSpiderTimeline _ = unsafeCoerce

-- | Pass a new timeline to the given function.
withSpiderTimeline :: (forall x. HasSpiderTimeline x => SpiderTimelineEnv x -> IO r) -> IO r
withSpiderTimeline k = do
  env <- unsafeNewSpiderTimelineEnv
  reify env $ \s -> k $ localSpiderTimeline s env
#endif

newtype SpiderPullM x a = SpiderPullM (BehaviorM x a) deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

newtype SpiderPushM x a = SpiderPushM (ComputeM x a) deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

#ifndef SPECIALIZE_TO_SPIDERTIMELINE_GLOBAL
instance HasSpiderTimeline x => R.Reflex (SpiderTimeline x) where
  {-# SPECIALIZE instance R.Reflex (SpiderTimeline Global) #-}
  newtype Behavior (SpiderTimeline x) a = SpiderBehavior { unSpiderBehavior :: Behavior x a }
  newtype Event (SpiderTimeline x) a = SpiderEvent { unSpiderEvent :: Event x a }
  newtype Dynamic (SpiderTimeline x) a = SpiderDynamic { unSpiderDynamic :: Dynamic x (Identity a) } -- deriving (Functor, Applicative, Monad)
  newtype Incremental (SpiderTimeline x) p = SpiderIncremental { unSpiderIncremental :: Dynamic x p }
  type PullM (SpiderTimeline x) = SpiderPullM x
  type PushM (SpiderTimeline x) = SpiderPushM x
  {-# INLINABLE never #-}
  never = SpiderEvent eventNever
  {-# INLINABLE constant #-}
  constant = SpiderBehavior . behaviorConst
  {-# INLINE push #-}
  push f = SpiderEvent . push (coerce f) . unSpiderEvent
  {-# INLINABLE pull #-}
  pull = SpiderBehavior . pull . coerce
  {-# INLINABLE merge #-}
  merge = SpiderEvent . merge . dynamicConst . (coerce :: DMap k (R.Event (SpiderTimeline x)) -> DMap k (Event x))
  {-# INLINABLE fan #-}
  fan e = R.EventSelector $ SpiderEvent . select (fan (unSpiderEvent e))
  {-# INLINABLE switch #-}
  switch = SpiderEvent . switch . (coerce :: Behavior x (R.Event (SpiderTimeline x) a) -> Behavior x (Event x a)) . unSpiderBehavior
  {-# INLINABLE coincidence #-}
  coincidence = SpiderEvent . coincidence . (coerce :: Event x (R.Event (SpiderTimeline x) a) -> Event x (Event x a)) . unSpiderEvent
  {-# INLINABLE current #-}
  current = SpiderBehavior . dynamicCurrent . unSpiderDynamic
  {-# INLINABLE updated #-}
  updated = SpiderEvent . fmap runIdentity . dynamicUpdated . unSpiderDynamic
  {-# INLINABLE unsafeBuildDynamic #-}
  unsafeBuildDynamic readV0 v' = SpiderDynamic $ dynamicDynIdentity $ unsafeDyn (coerce readV0) $ coerce $ unSpiderEvent v'
  {-# INLINABLE unsafeBuildIncremental #-}
  unsafeBuildIncremental readV0 dv = SpiderIncremental $ dynamicDyn $ unsafeDyn (coerce readV0) $ unSpiderEvent dv
  {-# INLINABLE mergeIncremental #-}
  mergeIncremental = SpiderEvent . merge . (unsafeCoerce :: Dynamic x (PatchDMap k (R.Event (SpiderTimeline x))) -> Dynamic x (PatchDMap k (Event x))) . unSpiderIncremental
  {-# INLINABLE currentIncremental #-}
  currentIncremental = SpiderBehavior . dynamicCurrent . unSpiderIncremental
  {-# INLINABLE updatedIncremental #-}
  updatedIncremental = SpiderEvent . dynamicUpdated . unSpiderIncremental
  {-# INLINABLE incrementalToDynamic #-}
  incrementalToDynamic (SpiderIncremental i) = SpiderDynamic $ dynamicDynIdentity $ unsafeDyn (readBehaviorUntracked $ dynamicCurrent i) $ flip push (dynamicUpdated i) $ \p -> do
    c <- readBehaviorUntracked $ dynamicCurrent i
    return $ Identity <$> apply p c --TODO: Avoid the redundant 'apply'
  eventCoercion Coercion = Coercion
  behaviorCoercion Coercion = Coercion
  dynamicCoercion = unsafeCoerce --TODO: How can we avoid this unsafeCoerce?  This is safe only because we know how Identity works as a Patch instance
#endif

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
newtype SpiderHost x a = SpiderHost { unSpiderHost :: ReaderT (SpiderTimelineEnv x) IO a } deriving (Functor, Applicative, MonadFix, MonadIO, MonadException, MonadAsyncException)

instance Monad (SpiderHost x) where
  {-# INLINABLE (>>=) #-}
  SpiderHost x >>= f = SpiderHost $ x >>= unSpiderHost . f
  {-# INLINABLE (>>) #-}
  SpiderHost x >> SpiderHost y = SpiderHost $ x >> y
  {-# INLINABLE return #-}
  return x = SpiderHost $ return x
  {-# INLINABLE fail #-}
  fail s = SpiderHost $ fail s

-- | Run an action affecting the global Spider timeline; this will be guarded by
-- a mutex for that timeline
runSpiderHost :: SpiderHost Global a -> IO a
runSpiderHost (SpiderHost a) = runReaderT a globalSpiderTimelineEnv

#ifndef SPECIALIZE_TO_SPIDERTIMELINE_GLOBAL
-- | Run an action affecting a given Spider timeline; this will be guarded by a
-- mutex for that timeline
runSpiderHostForTimeline :: SpiderHost x a -> SpiderTimelineEnv x -> IO a
runSpiderHostForTimeline (SpiderHost a) = runReaderT a
#endif

newtype SpiderHostFrame x a = SpiderHostFrame { runSpiderHostFrame :: EventM x a } deriving (Functor, Applicative, MonadFix, MonadIO, MonadException, MonadAsyncException)

instance Monad (SpiderHostFrame x) where
  {-# INLINABLE (>>=) #-}
  SpiderHostFrame x >>= f = SpiderHostFrame $ x >>= runSpiderHostFrame . f
  {-# INLINABLE (>>) #-}
  SpiderHostFrame x >> SpiderHostFrame y = SpiderHostFrame $ x >> y
  {-# INLINABLE return #-}
  return x = SpiderHostFrame $ return x
  {-# INLINABLE fail #-}
  fail s = SpiderHostFrame $ fail s

newEventWithTriggerIO :: (RootTrigger x a -> IO (IO ())) -> IO (Event x a)
newEventWithTriggerIO f = do
  es <- newFanEventWithTriggerIO $ \Refl -> f
  return $ select es Refl

newFanEventWithTriggerIO :: GCompare k => (forall a. k a -> RootTrigger x a -> IO (IO ())) -> IO (EventSelector x k)
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
