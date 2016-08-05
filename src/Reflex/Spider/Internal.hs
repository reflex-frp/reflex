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
-- | This module is the implementation of the 'Spider' 'Reflex' engine.  It uses
-- a graph traversal algorithm to propagate 'Event's and 'Behavior's.
module Reflex.Spider.Internal
       ( Spider
       , SpiderEnv (..)
       , Global
       , SpiderHost
       , runSpiderHost
       , newSpiderEnv
       , askSpiderEnv
       , SpiderEventHandle
       ) where

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
import Data.Functor.Compose
import Data.Functor.Constant
import Data.Functor.Product
import Data.GADT.Compare
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef
import Data.List hiding (foldl')
import Data.Maybe
import Data.Monoid ((<>))
import Data.Some (Some)
import qualified Data.Some as Some
import Data.These
import Data.Traversable
import Data.Word
import GHC.IORef (IORef (..))
import GHC.Stack
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
import qualified Reflex.Class as R
import qualified Reflex.Host.Class as R

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

instance HasNodeId (MergeSubscribed x a) where
  getNodeId = mergeSubscribedNodeId

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

newtype Event x a = Event { subscribeAndRead :: Subscriber x a -> EventM x (EventSubscription x a, Maybe a) }

subscribe :: Event x a -> Subscriber x a -> EventM x (EventSubscription x a)
subscribe e s = fst <$> subscribeAndRead e s

{-# INLINE wrap #-}
wrap :: MonadIO m => (t -> EventSubscribed x a) -> (Subscriber x a -> m (WeakBagTicket (Subscriber x a), t, Maybe a)) -> Subscriber x a -> m (EventSubscription x a, Maybe a)
wrap tag getSpecificSubscribed sub = do
  (sln, subd, occ) <- getSpecificSubscribed sub
  let es = tag subd
  return (EventSubscription (Just sln) es, occ)

eventRoot :: GCompare k => k a -> Root x k -> Event x a
eventRoot !k !r = Event $ wrap eventSubscribedRoot $ liftIO . getRootSubscribed k r

eventNever :: Event x a
eventNever = Event $ \_ -> return (EventSubscription Nothing eventSubscribedNever, Nothing)

eventPush :: Push x b a -> Event x a
eventPush !p = Event $ wrap eventSubscribedPush $ getPushSubscribed p

eventMerge :: (GCompare k, a ~ DMap k Identity) => Merge x k -> Event x a
eventMerge !m = Event $ wrap eventSubscribedMerge $ getMergeSubscribed m

eventFan :: GCompare k => k a -> Fan x k -> Event x a
eventFan !k !f = Event $ wrap eventSubscribedFan $ getFanSubscribed k f

eventSwitch :: Switch x a -> Event x a
eventSwitch !s = Event $ wrap eventSubscribedSwitch $ getSwitchSubscribed s

eventCoincidence :: Coincidence x a -> Event x a
eventCoincidence !c = Event $ wrap eventSubscribedCoincidence $ getCoincidenceSubscribed c

eventHold :: a ~ p b => Hold x p b -> Event x a
eventHold !h = Event $ subscribeHoldEvent h

eventDyn :: (a ~ p b, R.Patch p) => Dyn x p b -> Event x a
eventDyn !j = Event $ \sub -> getDynHold j >>= \h -> subscribeHoldEvent h sub

eventHoldIdentity :: (a ~ Identity b) => Hold x Identity b -> Event x a
eventHoldIdentity !h = Event $ subscribeHoldEvent h

eventDynIdentity :: (a ~ Identity b) => Dyn x Identity b -> Event x a
eventDynIdentity !j = Event $ \sub -> getDynHold j >>= \h -> subscribeHoldEvent h sub

{-# INLINE subscribeCoincidenceInner #-}
subscribeCoincidenceInner :: CanSubscribe x m => Event x a -> Height -> CoincidenceSubscribed x a -> m (Maybe a, Height, EventSubscribed x a)
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

newSubscriberPush :: (a -> ComputeM x (Maybe b)) -> PushSubscribed x a b -> IO (Subscriber x a)
newSubscriberPush compute subscribed = return $ Subscriber
  { subscriberPropagate = \a -> {-# SCC "traversePush" #-} do
      tracePropagate $ "SubscriberPush" <> showNodeId subscribed
      occ <- runComputeM $ compute a
      case occ of
        Nothing -> return () -- No need to write a Nothing back into the Ref
        Just o -> do
          liftIO $ writeIORef (pushSubscribedOccurrence subscribed) occ
          scheduleClear $ pushSubscribedOccurrence subscribed
          propagate o $ pushSubscribedSubscribers subscribed
  , subscriberInvalidateHeight = \old -> do
      when debugInvalidateHeight $ putStrLn $ "invalidateSubscriberHeight: SubscriberPush" <> showNodeId subscribed
      WeakBag.traverse (pushSubscribedSubscribers subscribed) $ invalidateSubscriberHeight old
      when debugInvalidateHeight $ putStrLn $ "invalidateSubscriberHeight: SubscriberPush" <> showNodeId subscribed <> " done"
  , subscriberRecalculateHeight = \new -> do
      when debugInvalidateHeight $ putStrLn $ "recalculateSubscriberHeight: SubscriberPush" <> showNodeId subscribed
      WeakBag.traverse (pushSubscribedSubscribers subscribed) $ recalculateSubscriberHeight new
      when debugInvalidateHeight $ putStrLn $ "recalculateSubscriberHeight: SubscriberPush" <> showNodeId subscribed <> " done"
  }

newSubscriberHold :: R.Patch p => Hold x p a -> IO (Subscriber x (p a))
newSubscriberHold h = return $ Subscriber
  { subscriberPropagate = {-# SCC "traverseHold" #-} propagateSubscriberHold h
  , subscriberInvalidateHeight = \_ -> return ()
  , subscriberRecalculateHeight = \_ -> return ()
  }

newSubscriberMerge :: GCompare k => k a -> MergeSubscribed x k -> IO (Subscriber x a)
newSubscriberMerge k subscribed = return $ Subscriber
  { subscriberPropagate = \a -> {-# SCC "traverseMerge" #-} do
      tracePropagate $ "SubscriberMerge" <> showNodeId subscribed
      oldM <- liftIO $ readIORef $ mergeSubscribedAccum subscribed
      let newM = DMap.insertWith (error $ "Same key fired multiple times for Merge" <> showNodeId subscribed) k (Identity a) oldM
      tracePropagate $ "  DMap.size oldM = " <> show (DMap.size oldM) <> "; DMap.size newM = " <> show (DMap.size newM)
      liftIO $ writeIORef (mergeSubscribedAccum subscribed) $! newM
      when (DMap.null oldM) $ do -- Only schedule the firing once
        height <- liftIO $ readIORef $ mergeSubscribedHeight subscribed
        --TODO: assertions about height
        currentHeight <- getCurrentHeight
        when (height <= currentHeight) $ do
          if height /= invalidHeight
            then do
            myStack <- liftIO $ whoCreatedIORef $ mergeSubscribedCachedSubscribed subscribed
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
        scheduleMerge height subscribed
  , subscriberInvalidateHeight = \old -> do
      when debugInvalidateHeight $ putStrLn $ "invalidateSubscriberHeight: SubscriberMerge" <> showNodeId subscribed
      removeMergeHeight old subscribed
      invalidateMergeHeight subscribed
      when debugInvalidateHeight $ putStrLn $ "invalidateSubscriberHeight: SubscriberMerge" <> showNodeId subscribed <> " done"
  , subscriberRecalculateHeight = \new -> do
      when debugInvalidateHeight $ putStrLn $ "recalculateSubscriberHeight: SubscriberMerge" <> showNodeId subscribed
      addMergeHeight new subscribed
      revalidateMergeHeight subscribed
      when debugInvalidateHeight $ putStrLn $ "recalculateSubscriberHeight: SubscriberMerge" <> showNodeId subscribed <> " done"
  }

newSubscriberMergeChange :: GCompare k => MergeSubscribed x k -> IO (Subscriber x (R.PatchDMap (DMap k (Event x))))
newSubscriberMergeChange subscribed = return $ Subscriber
  { subscriberPropagate = \a -> {-# SCC "traverseMergeChange" #-} do
      tracePropagate $ "SubscriberMerge" <> showNodeId subscribed
      defer $ SomeMergeUpdate a subscribed
  , subscriberInvalidateHeight = \_ -> return ()
  , subscriberRecalculateHeight = \_ -> return ()
  }

newSubscriberFan :: GCompare k => FanSubscribed x k -> IO (Subscriber x (DMap k Identity))
newSubscriberFan subscribed = return $ Subscriber
  { subscriberPropagate = \a -> {-# SCC "traverseFan" #-} do
      subs <- liftIO $ readIORef $ fanSubscribedSubscribers subscribed
      tracePropagate $ "SubscriberFan" <> showNodeId subscribed <> ": " ++ show (DMap.size subs) ++ " keys subscribed, " ++ show (DMap.size a) ++ " keys firing"
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

newSubscriberSwitch :: SwitchSubscribed x a -> IO (Subscriber x a)
newSubscriberSwitch subscribed = return $ Subscriber
  { subscriberPropagate = \a -> {-# SCC "traverseSwitch" #-} do
      tracePropagate $ "SubscriberSwitch" <> showNodeId subscribed
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

newSubscriberCoincidenceOuter :: CoincidenceSubscribed x b -> IO (Subscriber x (Event x b))
newSubscriberCoincidenceOuter subscribed = return $ Subscriber
  { subscriberPropagate = \a -> {-# SCC "traverseCoincidenceOuter" #-} do
      tracePropagate $ "SubscriberCoincidenceOuter" <> showNodeId subscribed
      outerHeight <- liftIO $ readIORef $ coincidenceSubscribedHeight subscribed
      tracePropagate $ "  outerHeight = " <> show outerHeight
      (occ, innerHeight, innerSubd) <- subscribeCoincidenceInner a outerHeight subscribed
      tracePropagate $ "  isJust occ = " <> show (isJust occ)
      tracePropagate $ "  innerHeight = " <> show innerHeight
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

newSubscriberCoincidenceInner :: CoincidenceSubscribed x a -> IO (Subscriber x a)
newSubscriberCoincidenceInner subscribed = return $ Subscriber
  { subscriberPropagate = \a -> {-# SCC "traverseCoincidenceInner" #-} do
      tracePropagate $ "SubscriberCoincidenceInner" <> showNodeId subscribed
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

data EventSubscribed x a = forall b. EventSubscribed
  { eventSubscribedHeightRef :: {-# UNPACK #-} !(IORef Height)
  , eventSubscribedRetained :: {-# NOUNPACK #-} !b
#ifdef DEBUG_CYCLES
  , eventSubscribedGetParents :: !(IO [Some (EventSubscribed x)]) -- For debugging loops
  , eventSubscribedHasOwnHeightRef :: !Bool
  , eventSubscribedWhoCreated :: !(IO [String])
#endif
  }

eventSubscribedRoot :: RootSubscribed x a -> EventSubscribed x a
eventSubscribedRoot !r = EventSubscribed
  { eventSubscribedHeightRef = zeroRef
  , eventSubscribedRetained = r
#ifdef DEBUG_CYCLES
  , eventSubscribedGetParents = return []
  , eventSubscribedHasOwnHeightRef = False
  , eventSubscribedWhoCreated = return ["root"]
#endif
  }

eventSubscribedNever :: EventSubscribed x a
eventSubscribedNever = EventSubscribed
  { eventSubscribedHeightRef = zeroRef
  , eventSubscribedRetained = ()
#ifdef DEBUG_CYCLES
  , eventSubscribedGetParents = return []
  , eventSubscribedHasOwnHeightRef = False
  , eventSubscribedWhoCreated = return ["never"]
#endif
  }

eventSubscribedPush :: PushSubscribed x b a -> EventSubscribed x a
eventSubscribedPush !subscribed = EventSubscribed
  { eventSubscribedHeightRef = pushSubscribedHeight subscribed
  , eventSubscribedRetained = subscribed
#ifdef DEBUG_CYCLES
  , eventSubscribedGetParents = return [Some.This $ _eventSubscription_subscribed $ pushSubscribedParent subscribed]
  , eventSubscribedHasOwnHeightRef = False
  , eventSubscribedWhoCreated = whoCreatedIORef $ pushSubscribedCachedSubscribed subscribed
#endif
  }

eventSubscribedMerge :: MergeSubscribed x k -> EventSubscribed x a
eventSubscribedMerge !subscribed = EventSubscribed
  { eventSubscribedHeightRef = mergeSubscribedHeight subscribed
  , eventSubscribedRetained = subscribed
#ifdef DEBUG_CYCLES
  , eventSubscribedGetParents = do
      ps <- readIORef $ mergeSubscribedParents subscribed
      return $ R.ffor (DMap.toList ps) $ \(_ :=> v) -> Some.This $ _eventSubscription_subscribed $ mergeSubscribedParentSubscription v
  , eventSubscribedHasOwnHeightRef = True
  , eventSubscribedWhoCreated = whoCreatedIORef $ mergeSubscribedCachedSubscribed subscribed
#endif
  }

eventSubscribedFan :: FanSubscribed x k -> EventSubscribed x a
eventSubscribedFan !subscribed = EventSubscribed
  { eventSubscribedHeightRef = eventSubscribedHeightRef $ _eventSubscription_subscribed $ fanSubscribedParent subscribed
  , eventSubscribedRetained = subscribed
#ifdef DEBUG_CYCLES
  , eventSubscribedGetParents = return [Some.This $ _eventSubscription_subscribed $ fanSubscribedParent subscribed]
  , eventSubscribedHasOwnHeightRef = False
  , eventSubscribedWhoCreated = whoCreatedIORef $ fanSubscribedCachedSubscribed subscribed
#endif
  }

eventSubscribedSwitch :: SwitchSubscribed x a -> EventSubscribed x a
eventSubscribedSwitch !subscribed = EventSubscribed
  { eventSubscribedHeightRef = switchSubscribedHeight subscribed
  , eventSubscribedRetained = subscribed
#ifdef DEBUG_CYCLES
  , eventSubscribedGetParents = do
      s <- readIORef $ switchSubscribedCurrentParent subscribed
      return [Some.This $ _eventSubscription_subscribed s]
  , eventSubscribedHasOwnHeightRef = True
  , eventSubscribedWhoCreated = whoCreatedIORef $ switchSubscribedCachedSubscribed subscribed
#endif
  }

eventSubscribedCoincidence :: CoincidenceSubscribed x a -> EventSubscribed x a
eventSubscribedCoincidence !subscribed = EventSubscribed
  { eventSubscribedHeightRef = coincidenceSubscribedHeight subscribed
  , eventSubscribedRetained = subscribed
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

getEventSubscribedHeight :: EventSubscribed x a -> IO Height
getEventSubscribedHeight es = readIORef $ eventSubscribedHeightRef es

#ifdef DEBUG_CYCLES
whoCreatedEventSubscribed :: EventSubscribed x a -> IO [String]
whoCreatedEventSubscribed = eventSubscribedWhoCreated

walkInvalidHeightParents :: EventSubscribed x a -> IO [Some (EventSubscribed x)]
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
subscribeHoldEvent :: CanSubscribe x m => Hold x p a -> Subscriber x (p a) -> m (EventSubscription x (p a), Maybe (p a))
subscribeHoldEvent = subscribeAndRead . holdEvent

--------------------------------------------------------------------------------
-- EventSubscription
--------------------------------------------------------------------------------

data EventSubscription x a = EventSubscription
  { _eventSubscription_ticket :: !(Maybe (WeakBagTicket (Subscriber x a)))
  , _eventSubscription_subscribed :: !(EventSubscribed x a)
  }

unsubscribe :: EventSubscription x a -> IO ()
unsubscribe (EventSubscription msln _) = forM_ msln $ \sln -> WeakBag.remove sln

--------------------------------------------------------------------------------
-- Behavior
--------------------------------------------------------------------------------

newtype Behavior x a = Behavior { readBehaviorTracked :: BehaviorM x a }

behaviorHold :: Hold x p a -> Behavior x a
behaviorHold !h = Behavior $ readHoldTracked h

behaviorHoldIdentity :: Hold x Identity a -> Behavior x a
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

behaviorDyn :: R.Patch p => Dyn x p a -> Behavior x a
behaviorDyn !d = Behavior $ readHoldTracked =<< getDynHold d

behaviorDynIdentity :: Dyn x Identity a -> Behavior x a
behaviorDynIdentity = behaviorDyn

{-# INLINE readHoldTracked #-}
readHoldTracked :: Hold x p a -> BehaviorM x a
readHoldTracked h = do
  result <- liftIO $ readIORef $ holdValue h
  askInvalidator >>= mapM_ (\wi -> liftIO $ modifyIORef' (holdInvalidators h) (wi:))
  askParentsRef >>= mapM_ (\r -> liftIO $ modifyIORef' r (SomeBehaviorSubscribed (BehaviorSubscribedHold h) :))
  return result

{-# SPECIALIZE readBehaviorUntracked :: Behavior x a -> BehaviorM x a #-}
{-# SPECIALIZE readBehaviorUntracked :: Behavior x a -> EventM x a #-}
readBehaviorUntracked :: Defer (SomeHoldInit x) m => Behavior x a -> m a
readBehaviorUntracked b = do
  holdInits <- getDeferralQueue
  liftIO $ runBehaviorM (readBehaviorTracked b) Nothing holdInits --TODO: Specialize readBehaviorTracked to the Nothing and Just cases

--------------------------------------------------------------------------------
-- Combinators
--------------------------------------------------------------------------------

--TODO: Figure out why certain things are not 'representational', then make them
--representational so we can use coerce

--type role Hold representational
data Hold x p a
   = Hold { holdValue :: !(IORef a)
          , holdInvalidators :: !(IORef [Weak (Invalidator x)])
          , holdEvent :: Event x (p a) -- This must be lazy, or holds cannot be defined before their input Events
          , holdParent :: !(IORef (Maybe (EventSubscription x (p a)))) -- Keeps its parent alive (will be undefined until the hold is initialized) --TODO: Probably shouldn't be an IORef
#ifdef DEBUG_NODEIDS
          , holdNodeId :: Int
#endif
          }

data EventEnv x
   = EventEnv { eventEnvSpiderEnv :: !(SpiderEnv x)
              , eventEnvAssignments :: !(IORef [SomeAssignment x]) -- Needed for Subscribe
              , eventEnvHoldInits :: !(IORef [SomeHoldInit x]) -- Needed for Subscribe
              , eventEnvMergeUpdates :: !(IORef [SomeMergeUpdate x])
              , eventEnvMergeInits :: !(IORef [SomeMergeInit x]) -- Needed for Subscribe
              , eventEnvClears :: !(IORef [SomeClear]) -- Needed for Subscribe
              , eventEnvRootClears :: !(IORef [SomeRootClear])
              , eventEnvCurrentHeight :: !(IORef Height) -- Needed for Subscribe
              , eventEnvResetCoincidences :: !(IORef [SomeResetCoincidence x]) -- Needed for Subscribe
              , eventEnvDelayedMerges :: !(IORef (IntMap [DelayedMerge x]))
              }

{-# INLINE runEventM #-}
runEventM :: EventM x a -> EventEnv x -> IO a
runEventM = runReaderT . unEventM

class MonadIO m => Defer a m where
  getDeferralQueue :: m (IORef [a])

{-# INLINE defer #-}
defer :: Defer a m => a -> m ()
defer a = do
  q <- getDeferralQueue
  liftIO $ modifyIORef' q (a:)

instance Defer (SomeAssignment x) (EventM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = EventM $ asks eventEnvAssignments

instance Defer (SomeHoldInit x) (EventM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = EventM $ asks eventEnvHoldInits

instance Defer (SomeHoldInit x) (BehaviorM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = BehaviorM $ asks snd

instance Defer (SomeMergeUpdate x) (EventM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = EventM $ asks eventEnvMergeUpdates

instance Defer (SomeMergeInit x) (EventM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = EventM $ asks eventEnvMergeInits

class HasCurrentHeight x m | m -> x where
  getCurrentHeight :: m Height
  scheduleMerge :: Height -> MergeSubscribed x a -> m ()

instance HasCurrentHeight x (EventM x) where
  {-# INLINE getCurrentHeight #-}
  getCurrentHeight = EventM $ do
    heightRef <- asks eventEnvCurrentHeight
    liftIO $ readIORef heightRef
  {-# INLINE scheduleMerge #-}
  scheduleMerge height subscribed = EventM $ do
    delayedRef <- asks eventEnvDelayedMerges
    liftIO $ modifyIORef' delayedRef $ IntMap.insertWith (++) (unHeight height) [DelayedMerge subscribed]

class HasSpiderEnv x m | m -> x where
  -- | Retrieve the current SpiderEnv
  askSpiderEnv :: m (SpiderEnv x)

instance HasSpiderEnv x (EventM x) where
  {-# INLINE askSpiderEnv #-}
  askSpiderEnv = EventM $ asks eventEnvSpiderEnv

instance HasSpiderEnv x (SpiderHost x) where
  {-# INLINE askSpiderEnv #-}
  askSpiderEnv = SpiderHost ask

putCurrentHeight :: Height -> EventM x ()
putCurrentHeight h = EventM $ do
  heightRef <- asks eventEnvCurrentHeight
  liftIO $ writeIORef heightRef $! h

instance Defer SomeClear (EventM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = EventM $ asks eventEnvClears

{-# INLINE scheduleClear #-}
scheduleClear :: Defer SomeClear m => IORef (Maybe a) -> m ()
scheduleClear r = defer $ SomeClear r

instance Defer SomeRootClear (EventM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = EventM $ asks eventEnvRootClears

{-# INLINE scheduleRootClear #-}
scheduleRootClear :: Defer SomeRootClear m => IORef (DMap k Identity) -> m ()
scheduleRootClear r = defer $ SomeRootClear r

instance Defer (SomeResetCoincidence x) (EventM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = EventM $ asks eventEnvResetCoincidences

-- Note: hold cannot examine its event until after the phase is over
{-# SPECIALIZE hold :: R.Patch p => a -> Event x (p a) -> EventM x (Hold x p a) #-}
{-# SPECIALIZE hold :: R.Patch p => a -> Event x (p a) -> ComputeM x (Hold x p a) #-}
hold :: (R.Patch p, Defer (SomeHoldInit x) m) => a -> Event x (p a) -> m (Hold x p a)
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
getHoldEventSubscription :: forall m p a x. (Defer (SomeAssignment x) m, Defer (SomeHoldInit x) m, Defer SomeClear m, R.Patch p, HasCurrentHeight x m, Defer (SomeMergeInit x) m, Defer (SomeResetCoincidence x) m, HasSpiderEnv x m, m ~ EventM x) => Hold x p a -> m (EventSubscription x (p a))
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
          case R.apply o old of
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
   = forall p. BehaviorSubscribedHold (Hold x p a)
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

data SomeHoldInit x = forall p a. R.Patch p => SomeHoldInit !(Hold x p a)

data SomeMergeUpdate x = forall k. GCompare k => SomeMergeUpdate (R.PatchDMap (DMap k (Event x))) (MergeSubscribed x k)

data SomeMergeInit x = forall k. GCompare k => SomeMergeInit !(MergeSubscribed x k) !(Event x (R.PatchDMap (DMap k (Event x))))

data SomeEventSubscription x = forall a. SomeEventSubscription (EventSubscription x a)

-- EventM can do everything BehaviorM can, plus create holds
newtype EventM x (a :: *) = EventM { unEventM :: ReaderT (EventEnv x) IO a } deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadException, MonadAsyncException) -- The environment should be Nothing if we are not in a frame, and Just if we are - in which case it is a list of assignments to be done after the frame is over

data PushSubscribed x a b
   = PushSubscribed { pushSubscribedCachedSubscribed :: !(IORef (Maybe (PushSubscribed x a b)))
                    , pushSubscribedOccurrence :: !(IORef (Maybe b)) -- If the current height is less than our height, this should always be Nothing; during our height, this will get filled in at some point, always before our children are notified; after our height, this will be filled in with the correct value (Nothing if we are not firing, Just if we are)
                    , pushSubscribedHeight :: !(IORef Height)
                    , pushSubscribedSubscribers :: !(WeakBag (Subscriber x b))
                    , pushSubscribedSelf :: !(Subscriber x a) -- Hold this in memory to ensure our WeakReferences don't die --TODO: Probably unneeded now
                    , pushSubscribedWeakSelf :: !(IORef (Weak (PushSubscribed x a b)))
                    , pushSubscribedParent :: !(EventSubscription x a)
#ifdef DEBUG_NODEIDS
                    , pushSubscribedNodeId :: Int
#endif
                    }

newtype ComputeM x a = ComputeM (ReaderT (IORef [SomeHoldInit x]) IO a) deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

instance Defer (SomeHoldInit x) (ComputeM x) where
  getDeferralQueue = ComputeM ask

{-# SPECIALIZE runComputeM :: ComputeM x a -> EventM x a #-}
runComputeM :: Defer (SomeHoldInit x) m => ComputeM x a -> m a
runComputeM (ComputeM a) = liftIO . runReaderT a =<< getDeferralQueue

data Push x a b
   = Push { pushCompute :: !(a -> ComputeM x (Maybe b)) -- Compute the current firing value; assumes that its parent has been computed --TODO: This only uses Defer SomeHoldInit EventM; perhaps only the hold init queue should be passed in (also, perhaps those hold inits can be accelerated - can there be a loop we would need to worry about?)
          , pushParent :: !(Event x a)
          , pushSubscribed :: !(IORef (Maybe (PushSubscribed x a b))) --TODO: Can we replace this with an unsafePerformIO thunk?
          }

data MergeSubscribedParent (x :: *) k a = MergeSubscribedParent !(EventSubscription x a) !(Subscriber x a)

mergeSubscribedParentSubscription :: MergeSubscribedParent x k a -> EventSubscription x a
mergeSubscribedParentSubscription (MergeSubscribedParent es _) = es

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

data MergeSubscribed x k
   = MergeSubscribed { mergeSubscribedCachedSubscribed :: !(IORef (Maybe (MergeSubscribed x k))) -- From the original merge
                     , mergeSubscribedOccurrence :: !(IORef (Maybe (DMap k Identity)))
                     , mergeSubscribedAccum :: !(IORef (DMap k Identity)) -- This will accumulate occurrences until our height is reached, at which point it will be transferred to mergeSubscribedOccurrence
                     , mergeSubscribedHeight :: !(IORef Height)
                     , mergeSubscribedHeightBag :: !(IORef HeightBag)
                     , mergeSubscribedSubscribers :: !(WeakBag (Subscriber x (DMap k Identity)))
                     , mergeSubscribedChange :: !(IORef (Subscriber x (R.PatchDMap (DMap k (Event x))), EventSubscribed x (R.PatchDMap (DMap k (Event x)))))
                     , mergeSubscribedParents :: !(IORef (DMap k (MergeSubscribedParent x k)))
                     , mergeSubscribedWeakSelf :: !(IORef (Weak (MergeSubscribed x k)))
#ifdef DEBUG_NODEIDS
                     , mergeSubscribedNodeId :: Int
#endif
                     }

--TODO: DMap sucks; we should really write something better (with a functor for the value as well as the key)
data Merge x k
   = Merge { mergeParents :: !(Dynamic x R.PatchDMap (DMap k (Event x)))
           , mergeSubscribed :: !(IORef (Maybe (MergeSubscribed x k)))
           }

data FanSubscribedChildren (x :: *) k a = FanSubscribedChildren
  { _fanSubscribedChildren_list :: !(WeakBag (Subscriber x a))
  , _fanSubscribedChildren_self :: {-# NOUNPACK #-} !(k a, FanSubscribed x k)
  , _fanSubscribedChildren_weakSelf :: !(IORef (Weak (k a, FanSubscribed x k)))
  }

data FanSubscribed (x :: *) k
   = FanSubscribed { fanSubscribedCachedSubscribed :: !(IORef (Maybe (FanSubscribed x k)))
                   , fanSubscribedOccurrence :: !(IORef (Maybe (DMap k Identity)))
                   , fanSubscribedSubscribers :: !(IORef (DMap k (FanSubscribedChildren x k))) -- This DMap should never be empty
                   , fanSubscribedParent :: !(EventSubscription x (DMap k Identity))
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
                      , switchSubscribedCurrentParent :: !(IORef (EventSubscription x a))
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
                           , coincidenceSubscribedOuterParent :: !(EventSubscription x (Event x a))
                           , coincidenceSubscribedInnerParent :: !(IORef (Maybe (EventSubscribed x a)))
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

data Dynamic x p a
   = DynamicHold !(Hold x p a)
   | p ~ Identity => DynamicHoldIdentity !(Hold x Identity a)
   | DynamicConst !a
   | DynamicDyn !(Dyn x p a)
   | p ~ Identity => DynamicDynIdentity !(Dyn x Identity a)

{-# INLINE current #-}
current :: R.Patch p => Dynamic x p a -> Behavior x a
current = \case
  DynamicHold h -> behaviorHold h
  DynamicHoldIdentity h -> behaviorHoldIdentity h
  DynamicConst a -> behaviorConst a
  DynamicDyn d -> behaviorDyn d
  DynamicDynIdentity d -> behaviorDynIdentity d

--TODO: If you only need updated, not current, can we avoid actually constructing the Hold?
{-# INLINE updated #-}
updated :: R.Patch p => Dynamic x p a -> Event x (p a)
updated = \case
  DynamicHold h -> eventHold h
  DynamicHoldIdentity h -> eventHoldIdentity h
  DynamicConst _ -> eventNever
  DynamicDyn d -> eventDyn d
  DynamicDynIdentity d -> eventDynIdentity d

newMapDyn :: (a -> b) -> Dynamic x Identity a -> Dynamic x Identity b
newMapDyn f d = DynamicDynIdentity $ unsafeDyn (fmap f $ readBehaviorTracked $ current d) (Identity . f . runIdentity <$> updated d)

instance Functor (Dynamic x Identity) where
  fmap = newMapDyn

instance Applicative (Dynamic x Identity) where
  pure = DynamicConst
  (<*>) = zipDynWith ($)
  _ *> b = b
  a <* _ = a

instance R.FunctorMaybe (Event x) where
  fmapMaybe f = push $ return . f

instance Align (Event x) where
  nil = eventNever
  align ea eb = R.fmapMaybe R.dmapToThese $ merge $ DynamicConst $ DMap.fromList [R.LeftTag :=> ea, R.RightTag :=> eb]

--TODO: Avoid the duplication between this and R.zipDynWith
zipDynWith :: (a -> b -> c) -> Dynamic x Identity a -> Dynamic x Identity b -> Dynamic x Identity c
zipDynWith f da db =
  let eab = align (updated da) (updated db)
      ec = flip push eab $ \o -> do
        (a, b) <- case o of
          This (Identity a) -> do
            b <- readBehaviorUntracked $ current db
            return (a, b)
          That (Identity b) -> do
            a <- readBehaviorUntracked $ current da
            return (a, b)
          These (Identity a) (Identity b) -> return (a, b)
        return $ Just $ Identity $ f a b
  in DynamicDynIdentity $ unsafeDyn (f <$> readBehaviorUntracked (current da) <*> readBehaviorUntracked (current db)) ec

instance Monad (Dynamic x Identity) where
  {-# INLINE return #-}
  return = pure
  {-# INLINE (>>=) #-}
  x >>= f = DynamicDynIdentity $ newJoinDyn $ newMapDyn f x
  {-# INLINE (>>) #-}
  _ >> y = y
  {-# INLINE fail #-}
  fail _ = error "Dynamic does not support 'fail'"

newtype Dyn x p a = Dyn { unDyn :: IORef (Either (BehaviorM x a, Event x (p a)) (Hold x p a)) }

unsafeDyn :: BehaviorM x a -> Event x (p a) -> Dyn x p a
unsafeDyn readV0 v' = Dyn $ unsafeNewIORef x $ Left x
  where x = (readV0, v')

newJoinDyn :: Dynamic x Identity (Dynamic x Identity a) -> Dyn x Identity a
newJoinDyn d =
  let readV0 = readBehaviorTracked . current =<< readBehaviorTracked (current d)
      eOuter = push (fmap (Just . Identity) . readBehaviorUntracked . current . runIdentity) $ updated d
      eInner = switch $ updated <$> current d
      eBoth = coincidence $ updated . runIdentity <$> updated d
      v' = unSpiderEvent $ R.leftmost $ map SpiderEvent [eBoth, eOuter, eInner]
  in unsafeDyn readV0 v'

-- ResultM can read behaviors and events
type ResultM = EventM

{-# NOINLINE unsafeNewIORef #-}
unsafeNewIORef :: a -> b -> IORef b
unsafeNewIORef _ b = unsafePerformIO $ newIORef b

instance Functor (Event x) where
  fmap f = push $ return . Just . f

instance Functor (Behavior x) where
  fmap f = pull . fmap f . readBehaviorTracked

{-# NOINLINE push #-} --TODO: If this is helpful, we can get rid of the unsafeNewIORef and use unsafePerformIO directly
push :: (a -> ComputeM x (Maybe b)) -> Event x a -> Event x b
push f e = eventPush $ Push
  { pushCompute = f
  , pushParent = e
  , pushSubscribed = unsafeNewIORef (f, e) Nothing --TODO: Does the use of the tuple here create unnecessary overhead?
  }
-- DISABLED: {- RULES "push/push" forall f g e. push f (push g e) = push (maybe (return Nothing) f <=< g) e #-}

{-# NOINLINE pull #-}
pull :: BehaviorM x a -> Behavior x a
pull a = behaviorPull $ Pull
  { pullCompute = a
  , pullValue = unsafeNewIORef a Nothing
#ifdef DEBUG_NODEIDS
  , pullNodeId = unsafeNodeId a
#endif
  }

{-# NOINLINE switch #-}
switch :: Behavior x (Event x a) -> Event x a
switch a = eventSwitch $ Switch
  { switchParent = a
  , switchSubscribed = unsafeNewIORef a Nothing
  }

coincidence :: Event x (Event x a) -> Event x a
coincidence a = eventCoincidence $ Coincidence
  { coincidenceParent = a
  , coincidenceSubscribed = unsafeNewIORef a Nothing
  }

-- Propagate the given event occurrence; before cleaning up, run the given action, which may read the state of events and behaviors
run :: [DSum (RootTrigger x) Identity] -> ResultM x b -> SpiderHost x b
run roots after = do
  tracePropagate $ "Running an event frame with " <> show (length roots) <> " events"
  spiderEnv <- SpiderHost ask
  result <- SpiderHost $ lift $ withMVar (_spiderEnv_lock spiderEnv) $ \_ -> flip runReaderT spiderEnv $ unSpiderHost $ runFrame $ do
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
    delayedRef <- EventM $ asks eventEnvDelayedMerges
    let go = do
          delayed <- liftIO $ readIORef delayedRef
          case IntMap.minViewWithKey delayed of
            Nothing -> return ()
            Just ((currentHeight, cur), future) -> do
              tracePropagate $ "Running height " ++ show currentHeight
              putCurrentHeight $ Height currentHeight
              liftIO $ writeIORef delayedRef $! future
              forM_ cur $ \d -> case d of
                DelayedMerge subscribed -> do
                  height <- liftIO $ readIORef $ mergeSubscribedHeight subscribed
                  case height `compare` Height currentHeight of
                    LT -> error "Somehow a merge's height has been decreased after it was scheduled"
                    GT -> scheduleMerge height subscribed -- The height has been increased (by a coincidence event; TODO: is this the only way?)
                    EQ -> do
                      m <- liftIO $ readIORef $ mergeSubscribedAccum subscribed
                      liftIO $ writeIORef (mergeSubscribedAccum subscribed) $! DMap.empty
                      --TODO: Assert that m is not empty
                      liftIO $ writeIORef (mergeSubscribedOccurrence subscribed) $ Just m
                      scheduleClear $ mergeSubscribedOccurrence subscribed
                      propagate m $ mergeSubscribedSubscribers subscribed
              go
    go
    putCurrentHeight maxBound
    after
  tracePropagate "Done running an event frame"
  return result

data SomeClear = forall a. SomeClear (IORef (Maybe a))

data SomeRootClear = forall k. SomeRootClear (IORef (DMap k Identity))

data SomeAssignment x = forall a. SomeAssignment {-# UNPACK #-} !(IORef a) {-# UNPACK #-} !(IORef [Weak (Invalidator x)]) a

data DelayedMerge x = forall k. DelayedMerge (MergeSubscribed x k)

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
  spiderEnv <- askSpiderEnv
  liftIO $ modifyIORef' (_spiderEnv_depth spiderEnv) succ
  result <- a
  liftIO $ modifyIORef' (_spiderEnv_depth spiderEnv) pred
  return result
#else
withIncreasedDepth :: m a -> m a
withIncreasedDepth = id
#endif

type CanTrace x m = (HasSpiderEnv x m, MonadIO m)

{-# INLINE tracePropagate #-}
tracePropagate :: (CanTrace x m) => String -> m ()
tracePropagate = traceWhen debugPropagate

{-# INLINE traceInvalidate #-}
traceInvalidate :: String -> IO ()
traceInvalidate = when debugInvalidate . liftIO . putStrLn

{-# INLINE traceWhen #-}
traceWhen :: (CanTrace x m) => Bool -> String -> m ()
traceWhen b message = traceMWhen b $ return message

{-# INLINE traceMWhen #-}
traceMWhen :: (CanTrace x m) => Bool -> m String -> m ()
traceMWhen b getMessage = when b $ do
  message <- getMessage
#ifdef DEBUG
  spiderEnv <- askSpiderEnv
  d <- liftIO $ readIORef $ _spiderEnv_depth spiderEnv
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
propagateSubscriberHold :: R.Patch p => Hold x p a -> p a -> EventM x ()
propagateSubscriberHold h a = do
  {-# SCC "trace" #-} traceMWhen debugPropagate $ liftIO $ do
    invalidators <- liftIO $ readIORef $ holdInvalidators h
    return $ "SubscriberHold" <> showNodeId h <> ": " ++ show (length invalidators)
  v <- {-# SCC "read" #-} liftIO $ readIORef $ holdValue h
  case {-# SCC "apply" #-} R.apply a v of
    Nothing -> return ()
    Just v' -> do
      {-# SCC "trace2" #-} withIncreasedDepth $ tracePropagate $ "propagateSubscriberHold: assigning Hold" <> showNodeId h
      vRef <- {-# SCC "vRef" #-} liftIO $ evaluate $ holdValue h
      iRef <- {-# SCC "iRef" #-} liftIO $ evaluate $ holdInvalidators h
      defer $ {-# SCC "assignment" #-} SomeAssignment vRef iRef v'

data SomeResetCoincidence x = forall a. SomeResetCoincidence !(EventSubscription x a) !(Maybe (CoincidenceSubscribed x a)) -- The CoincidenceSubscriber will be present only if heights need to be reset

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
getDynHold :: (Defer (SomeHoldInit x) m, R.Patch p) => Dyn x p a -> m (Hold x p a)
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
  ( HasSpiderEnv x m
  , HasCurrentHeight x m
  , Defer (SomeHoldInit x) m, Defer (SomeMergeInit x) m -- The things we need to lazily initialize state
  , Defer SomeClear m -- Wiping out event firings
  , Defer (SomeAssignment x) m -- Updating state
  , Defer (SomeResetCoincidence x) m
  , m ~ EventM x
  )

getRootSubscribed :: GCompare k => k a -> Root x k -> Subscriber x a -> IO (WeakBagTicket (Subscriber x a), RootSubscribed x a, Maybe a)
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
subscribeRootSubscribed :: RootSubscribed x a -> Subscriber x a -> IO (WeakBagTicket (Subscriber x a))
subscribeRootSubscribed subscribed sub = WeakBag.insert sub (rootSubscribedSubscribers subscribed) (rootSubscribedWeakSelf subscribed) cleanupRootSubscribed

-- When getPushSubscribed returns, the PushSubscribed returned will have a fully filled-in
{-# SPECIALIZE getPushSubscribed :: Push x a b -> Subscriber x b -> EventM x (WeakBagTicket (Subscriber x b), PushSubscribed x a b, Maybe b) #-}
getPushSubscribed :: CanSubscribe x m => Push x a b -> Subscriber x b -> m (WeakBagTicket (Subscriber x b), PushSubscribed x a b, Maybe b)
getPushSubscribed p sub = do
  mSubscribed <- liftIO $ readIORef $ pushSubscribed p
  case mSubscribed of
    Just subscribed -> {-# SCC "hitPush" #-} liftIO $ do
      sln <- subscribePushSubscribed subscribed sub
      occ <- readIORef $ pushSubscribedOccurrence subscribed
      return (sln, subscribed, occ)
    Nothing -> {-# SCC "missPush" #-} do -- Not yet subscribed
      subscribedRef <- liftIO $ newIORef $ error "getPushSubscribed: subscribed not yet intialized"
      subscribedUnsafe <- liftIO $ unsafeInterleaveIO $ readIORef subscribedRef
      s <- liftIO $ newSubscriberPush (pushCompute p) subscribedUnsafe
      (subscription@(EventSubscription _ subd), parentOcc) <- subscribeAndRead (pushParent p) s
      occ <- runComputeM $ join <$> mapM (pushCompute p) parentOcc
      occRef <- liftIO $ newIORef occ
      when (isJust occ) $ scheduleClear occRef
      weakSelf <- liftIO $ newIORef $ error "getPushSubscribed: weakSelf not initialized"
      let !cached = pushSubscribed p
      (subs, sln) <- liftIO $ WeakBag.singleton sub weakSelf cleanupPushSubscribed
      let subscribed = PushSubscribed
            { pushSubscribedCachedSubscribed = cached
            , pushSubscribedOccurrence = occRef
            , pushSubscribedHeight = eventSubscribedHeightRef subd -- Since pushes have the same height as their parents, share the ref
            , pushSubscribedSubscribers = subs
            , pushSubscribedSelf = s
            , pushSubscribedWeakSelf = weakSelf
            , pushSubscribedParent = subscription
#ifdef DEBUG_NODEIDS
            , pushSubscribedNodeId = unsafeNodeId p
#endif
            }
      liftIO $ writeIORef weakSelf =<< evaluate =<< mkWeakPtrWithDebug subscribed "PushSubscribed"
      liftIO $ writeIORef subscribedRef $! subscribed
      liftIO $ writeIORef (pushSubscribed p) $ Just subscribed
      return (sln, subscribed, occ)

cleanupPushSubscribed :: PushSubscribed x a b -> IO ()
cleanupPushSubscribed self = do
  unsubscribe $ pushSubscribedParent self
  writeIORef (pushSubscribedCachedSubscribed self) $ Nothing

{-# INLINE subscribePushSubscribed #-}
subscribePushSubscribed :: PushSubscribed x a b -> Subscriber x b -> IO (WeakBagTicket (Subscriber x b))
subscribePushSubscribed subscribed sub = WeakBag.insert sub (pushSubscribedSubscribers subscribed) (pushSubscribedWeakSelf subscribed) cleanupPushSubscribed

{-# SPECIALIZE getMergeSubscribed :: GCompare k => Merge x k -> Subscriber x (DMap k Identity) -> EventM x (WeakBagTicket (Subscriber x (DMap k Identity)), MergeSubscribed x k, Maybe (DMap k Identity)) #-}
getMergeSubscribed :: forall x k m. (CanSubscribe x m, GCompare k) => Merge x k -> Subscriber x (DMap k Identity) -> m (WeakBagTicket (Subscriber x (DMap k Identity)), MergeSubscribed x k, Maybe (DMap k Identity))
getMergeSubscribed m sub = do
  mSubscribed <- liftIO $ readIORef $ mergeSubscribed m
  case mSubscribed of
    Just subscribed -> {-# SCC "hitMerge" #-} liftIO $ do
      sln <- subscribeMergeSubscribed subscribed sub
      occ <- readIORef $ mergeSubscribedOccurrence subscribed
      return (sln, subscribed, occ)
    Nothing -> {-# SCC "missMerge" #-} do
      subscribedRef <- liftIO $ newIORef $ error "getMergeSubscribed: subscribedRef not yet initialized"
      subscribedUnsafe <- liftIO $ unsafeInterleaveIO $ readIORef subscribedRef
      initialParents <- readBehaviorUntracked $ current $ mergeParents m
      subscribers :: [(Maybe (DSum k Identity), Height, DSum k (MergeSubscribedParent x k))] <- forM (DMap.toList initialParents) $ \(k :=> e) -> do
        s <- liftIO $ newSubscriberMerge k subscribedUnsafe
        (subscription@(EventSubscription _ parentSubd), parentOcc) <- subscribeAndRead e s
        height <- liftIO $ getEventSubscribedHeight parentSubd
        return $ (fmap (\x -> k :=> Identity x) parentOcc, height, k :=> MergeSubscribedParent subscription s)
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
        scheduleMerge myHeight subscribedUnsafe
      occRef <- liftIO $ newIORef occ
      when (isJust occ) $ scheduleClear occRef
      accumRef <- liftIO $ newIORef accum
      heightRef <- liftIO $ newIORef myHeight
      heightMapRef <- liftIO $ newIORef myHeightBag
      weakSelf <- liftIO $ newIORef $ error "getMergeSubscribed: weakSelf not yet initialized"
      (subs, sln) <- liftIO $ WeakBag.singleton sub weakSelf cleanupMergeSubscribed
      changeSubdRef <- liftIO $ newIORef $ error "getMergeSubscribed: changeSubdRef not yet initialized"
      parentsRef <- liftIO $ newIORef $ DMap.fromDistinctAscList $ map (\(_, _, x) -> x) subscribers
      let !subscribed = MergeSubscribed
            { mergeSubscribedCachedSubscribed = mergeSubscribed m
            , mergeSubscribedOccurrence = occRef
            , mergeSubscribedAccum = accumRef
            , mergeSubscribedHeight = heightRef
            , mergeSubscribedHeightBag = heightMapRef
            , mergeSubscribedSubscribers = subs
            , mergeSubscribedChange = changeSubdRef
            , mergeSubscribedParents = parentsRef
            , mergeSubscribedWeakSelf = weakSelf
#ifdef DEBUG_NODEIDS
            , mergeSubscribedNodeId = unsafeNodeId m
#endif
            }
      defer $ SomeMergeInit subscribed $ updated $ mergeParents m
      liftIO $ writeIORef subscribedRef $! subscribed
      liftIO $ writeIORef weakSelf =<< evaluate =<< mkWeakPtrWithDebug subscribed "MergeSubscribed"
      liftIO $ writeIORef (mergeSubscribed m) $ Just subscribed
      return (sln, subscribed, occ)

cleanupMergeSubscribed :: MergeSubscribed x k -> IO ()
cleanupMergeSubscribed subscribed = do
  parents <- readIORef $ mergeSubscribedParents subscribed
  forM_ (DMap.toList parents) $ \(_ :=> msp) -> unsubscribe $ mergeSubscribedParentSubscription msp
  -- Not necessary, because this whole MergeSubscribed is dead: writeIORef (mergeSubscribedParents subscribed) DMap.empty
  writeIORef (mergeSubscribedCachedSubscribed subscribed) $ Nothing -- Get rid of the cached subscribed

{-# INLINE subscribeMergeSubscribed #-}
subscribeMergeSubscribed :: MergeSubscribed x k -> Subscriber x (DMap k Identity) -> IO (WeakBagTicket (Subscriber x (DMap k Identity)))
subscribeMergeSubscribed subscribed sub = WeakBag.insert sub (mergeSubscribedSubscribers subscribed) (mergeSubscribedWeakSelf subscribed) cleanupMergeSubscribed

{-# SPECIALIZE getFanSubscribed :: GCompare k => k a -> Fan x k -> Subscriber x a -> EventM x (WeakBagTicket (Subscriber x a), FanSubscribed x k, Maybe a) #-}
getFanSubscribed :: (Defer (SomeAssignment x) m, Defer (SomeHoldInit x) m, Defer (SomeMergeInit x) m, Defer SomeClear m, HasCurrentHeight x m, Defer (SomeResetCoincidence x) m, HasSpiderEnv x m, m ~ EventM x, GCompare k) => k a -> Fan x k -> Subscriber x a -> m (WeakBagTicket (Subscriber x a), FanSubscribed x k, Maybe a)
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
subscribeFanSubscribed :: GCompare k => k a -> FanSubscribed x k -> Subscriber x a -> IO (WeakBagTicket (Subscriber x a))
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

{-# SPECIALIZE getSwitchSubscribed :: Switch x a -> Subscriber x a -> EventM x (WeakBagTicket (Subscriber x a), SwitchSubscribed x a, Maybe a) #-}
getSwitchSubscribed :: (Defer (SomeHoldInit x) m, Defer (SomeAssignment x) m, Defer (SomeMergeInit x) m, Defer SomeClear m, Defer (SomeResetCoincidence x) m, HasCurrentHeight x m, HasSpiderEnv x m, m ~ EventM x) => Switch x a -> Subscriber x a -> m (WeakBagTicket (Subscriber x a), SwitchSubscribed x a, Maybe a)
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
subscribeSwitchSubscribed :: SwitchSubscribed x a -> Subscriber x a -> IO (WeakBagTicket (Subscriber x a))
subscribeSwitchSubscribed subscribed sub = WeakBag.insert sub (switchSubscribedSubscribers subscribed) (switchSubscribedWeakSelf subscribed) cleanupSwitchSubscribed

{-# SPECIALIZE getCoincidenceSubscribed :: Coincidence x a -> Subscriber x a -> EventM x (WeakBagTicket (Subscriber x a), CoincidenceSubscribed x a, Maybe a) #-}
getCoincidenceSubscribed :: forall x a m. (Defer (SomeAssignment x) m, Defer (SomeHoldInit x) m, Defer (SomeMergeInit x) m, Defer SomeClear m, HasCurrentHeight x m, Defer (SomeResetCoincidence x) m, HasSpiderEnv x m, m ~ EventM x) => Coincidence x a -> Subscriber x a -> m (WeakBagTicket (Subscriber x a), CoincidenceSubscribed x a, Maybe a)
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
subscribeCoincidenceSubscribed :: CoincidenceSubscribed x a -> Subscriber x a -> IO (WeakBagTicket (Subscriber x a))
subscribeCoincidenceSubscribed subscribed sub = WeakBag.insert sub (coincidenceSubscribedSubscribers subscribed) (coincidenceSubscribedWeakSelf subscribed) cleanupCoincidenceSubscribed

{-# INLINE merge #-}
merge :: GCompare k => Dynamic x R.PatchDMap (DMap k (Event x)) -> Event x (DMap k Identity)
merge m = eventMerge $ Merge
  { mergeParents = m
  , mergeSubscribed = unsafeNewIORef m Nothing
  }

newtype EventSelector x k = EventSelector { select :: forall a. k a -> Event x a }

fan :: GCompare k => Event x (DMap k Identity) -> EventSelector x k
fan e =
  let f = Fan
        { fanParent = e
        , fanSubscribed = unsafeNewIORef e Nothing
        }
  in EventSelector $ \k -> eventFan k f

runHoldInits :: IORef [SomeHoldInit x] -> IORef [SomeMergeInit x] -> EventM x ()
runHoldInits holdInitRef mergeInitRef = do
  holdInits <- liftIO $ readIORef holdInitRef
  mergeInits <- liftIO $ readIORef mergeInitRef
  unless (null holdInits && null mergeInits) $ do
    liftIO $ writeIORef holdInitRef []
    liftIO $ writeIORef mergeInitRef []
    mapM_ initHold holdInits
    mapM_ initMerge mergeInits
    runHoldInits holdInitRef mergeInitRef

initHold :: SomeHoldInit x -> EventM x ()
initHold (SomeHoldInit h) = void $ getHoldEventSubscription h

initMerge :: SomeMergeInit x -> EventM x ()
initMerge (SomeMergeInit subscribed changed) = do
  changeSub <- liftIO $ newSubscriberMergeChange subscribed
  (EventSubscription _ changeSubd, change) <- subscribeAndRead changed changeSub
  forM_ change $ \c -> defer $ SomeMergeUpdate c subscribed
  liftIO $ writeIORef (mergeSubscribedChange subscribed) (changeSub, changeSubd)

-- | Run an event action outside of a frame
runFrame :: forall x a. EventM x a -> SpiderHost x a --TODO: This function also needs to hold the mutex
runFrame a = SpiderHost $ ask >>= \spiderEnv -> lift $ do
  -- Clear out pending unsubscriptions; these will need to happen eventually anyway, and might cause us a lot of extra work during the frame, so get rid of them now
  pendingCleanups <- atomicModifyIORef (_spiderEnv_cleanups spiderEnv) $ \x -> ([], x)
  {-# SCC "cleanups" #-} sequence_ pendingCleanups
  toAssignRef <- newIORef [] -- This should only actually get used when events are firing
  holdInitRef <- newIORef []
  mergeUpdateRef <- newIORef []
  mergeInitRef <- newIORef []
  heightRef <- newIORef zeroHeight
  toClearRef <- newIORef []
  toClearRootRef <- newIORef []
  coincidenceInfosRef <- newIORef []
  delayedRef <- liftIO $ newIORef IntMap.empty
  let env = EventEnv spiderEnv toAssignRef holdInitRef mergeUpdateRef mergeInitRef toClearRef toClearRootRef heightRef coincidenceInfosRef delayedRef
  let go = do
        result <- a
        runHoldInits holdInitRef mergeInitRef -- This must happen before doing the assignments, in case subscribing a Hold causes existing Holds to be read by the newly-propagated events
        return result
  result <- runEventM go env
  toClear <- readIORef toClearRef
  forM_ toClear $ \(SomeClear ref) -> {-# SCC "clear" #-} writeIORef ref Nothing
  toClearRoot <- readIORef toClearRootRef
  forM_ toClearRoot $ \(SomeRootClear ref) -> {-# SCC "rootClear" #-} writeIORef ref $! DMap.empty
  toAssign <- readIORef toAssignRef
  toReconnectRef <- newIORef []
  coincidenceInfos <- readIORef coincidenceInfosRef
  forM_ toAssign $ \(SomeAssignment vRef iRef v) -> {-# SCC "assignment" #-} do
    writeIORef vRef v
    when debugInvalidate $ putStrLn $ "Invalidating Hold"
    writeIORef iRef =<< evaluate =<< invalidate toReconnectRef =<< readIORef iRef
  mergeUpdates <- readIORef mergeUpdateRef
  writeIORef mergeUpdateRef []
  let updateMerge :: SomeMergeUpdate x -> IO [SomeEventSubscription x]
      updateMerge (SomeMergeUpdate (R.PatchDMap p :: R.PatchDMap (DMap k (Event x))) m) = do
        oldParents <- readIORef $ mergeSubscribedParents m
        let f (subscriptionsToKill, ps) (k :=> Compose me) = do
              (mOldSubd, newPs) <- case me of
                Nothing -> return $ DMap.updateLookupWithKey (\_ _ -> Nothing) k ps
                Just e -> do
                  s <- liftIO $ newSubscriberMerge k m
                  subscription@(EventSubscription _ subd) <- subscribe e s
                  newParentHeight <- liftIO $ getEventSubscribedHeight subd
                  let newParent = MergeSubscribedParent subscription s
                  liftIO $ addMergeHeight newParentHeight m
                  return $ DMap.insertLookupWithKey' (\_ new _ -> new) k newParent ps
              forM_ mOldSubd $ \oldSubd -> do
                oldHeight <- liftIO $ getEventSubscribedHeight $ _eventSubscription_subscribed $ mergeSubscribedParentSubscription oldSubd
                liftIO $ removeMergeHeight oldHeight m
              return (maybeToList (SomeEventSubscription . mergeSubscribedParentSubscription <$> mOldSubd) ++ subscriptionsToKill, newPs)
        (subscriptionsToKill, newParents) <- flip runEventM env $ foldM f ([], oldParents) $ DMap.toList p --TODO: I think this runEventM is OK, since no events are firing at this time, but it might not be
        writeIORef (mergeSubscribedParents m) $! newParents
        return subscriptionsToKill
  when debugPropagate $ putStrLn "Updating merges"
  mergeSubscriptionsToKill <- concat <$> mapM updateMerge mergeUpdates
  when debugPropagate $ putStrLn "Updating merges done"
  toReconnect <- readIORef toReconnectRef
  switchSubscriptionsToKill <- forM toReconnect $ \(SomeSwitchSubscribed subscribed) -> {-# SCC "switchSubscribed" #-} do
    oldSubscription <- readIORef $ switchSubscribedCurrentParent subscribed
    wi <- readIORef $ switchSubscribedOwnWeakInvalidator subscribed
    when debugInvalidate $ putStrLn $ "Finalizing invalidator for Switch" <> showNodeId subscribed
    finalize wi
    i <- evaluate $ switchSubscribedOwnInvalidator subscribed
    wi' <- mkWeakPtrWithDebug i "wi'"
    writeIORef (switchSubscribedOwnWeakInvalidator subscribed) $! wi'
    writeIORef (switchSubscribedBehaviorParents subscribed) []
    writeIORef holdInitRef [] --TODO: Should we reuse this?
    e <- runBehaviorM (readBehaviorTracked (switchSubscribedParent subscribed)) (Just (wi', switchSubscribedBehaviorParents subscribed)) holdInitRef
    runEventM (runHoldInits holdInitRef mergeInitRef) env --TODO: Is this actually OK? It seems like it should be, since we know that no events are firing at this point, but it still seems inelegant
    --TODO: Make sure we touch the pieces of the SwitchSubscribed at the appropriate times
    sub <- newSubscriberSwitch subscribed
    subscription <- runReaderT (unSpiderHost (runFrame ({-# SCC "subscribeSwitch" #-} subscribe e sub))) spiderEnv --TODO: Assert that the event isn't firing --TODO: This should not loop because none of the events should be firing, but still, it is inefficient
    {-
    stackTrace <- liftIO $ fmap renderStack $ ccsToStrings =<< (getCCSOf $! switchSubscribedParent subscribed)
    liftIO $ putStrLn $ (++stackTrace) $ "subd' subscribed to " ++ case e of
      EventRoot _ -> "EventRoot"
      EventNever -> "EventNever"
      _ -> "something else"
    -}
    writeIORef (switchSubscribedCurrentParent subscribed) $! subscription
    return $ SomeEventSubscription oldSubscription
  forM_ mergeSubscriptionsToKill $ \(SomeEventSubscription oldSubscription) -> liftIO $ unsubscribe oldSubscription
  forM_ switchSubscriptionsToKill $ \(SomeEventSubscription oldSubscription) -> liftIO $ unsubscribe oldSubscription
  forM_ toReconnect $ \(SomeSwitchSubscribed subscribed) -> {-# SCC "switchSubscribed" #-} do
    EventSubscription _ subd' <- readIORef $ switchSubscribedCurrentParent subscribed
    parentHeight <- getEventSubscribedHeight subd'
    myHeight <- readIORef $ switchSubscribedHeight subscribed
    when (parentHeight /= myHeight) $ do
      writeIORef (switchSubscribedHeight subscribed) $! invalidHeight
      WeakBag.traverse (switchSubscribedSubscribers subscribed) $ invalidateSubscriberHeight myHeight
  forM_ mergeUpdates $ \(SomeMergeUpdate _ m) -> do
    invalidateMergeHeight m --TODO: In addition to when the patch is completely empty, we should also not run this if it has some Nothing values, but none of them have actually had any effect; potentially, we could even check for Just values with no effect (e.g. by comparing their IORefs and ignoring them if they are unchanged); actually, we could just check if the new height is different
  forM_ coincidenceInfos $ \(SomeResetCoincidence subscription mcs) -> do
    unsubscribe subscription
    mapM_ invalidateCoincidenceHeight mcs
  forM_ coincidenceInfos $ \(SomeResetCoincidence _ mcs) -> mapM_ recalculateCoincidenceHeight mcs
  forM_ mergeUpdates $ \(SomeMergeUpdate _ m) -> revalidateMergeHeight m
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

removeMergeHeight :: Height -> MergeSubscribed x k -> IO ()
removeMergeHeight oldParentHeight subscribed = do
  when debugInvalidateHeight $ putStrLn $ "removeMergeHeight: " <> show oldParentHeight <> " Merge" <> showNodeId subscribed
  oldHeights <- readIORef $ mergeSubscribedHeightBag subscribed
  let newHeights = heightBagRemove oldParentHeight oldHeights
  writeIORef (mergeSubscribedHeightBag subscribed) $! newHeights

invalidateMergeHeight :: MergeSubscribed x k -> IO ()
invalidateMergeHeight subscribed = do
  oldHeight <- readIORef $ mergeSubscribedHeight subscribed
  -- If the height used to be valid, it must be invalid now; we should never have *more* heights than we have parents
  when (oldHeight /= invalidHeight) $ do
    writeIORef (mergeSubscribedHeight subscribed) $! invalidHeight
    WeakBag.traverse (mergeSubscribedSubscribers subscribed) $ invalidateSubscriberHeight oldHeight

addMergeHeight :: Height -> MergeSubscribed x k -> IO ()
addMergeHeight newParentHeight subscribed = do
  when debugInvalidateHeight $ putStrLn $ "addMergeHeight: " <> show newParentHeight <> " Merge" <> showNodeId subscribed
  oldHeights <- readIORef $ mergeSubscribedHeightBag subscribed
  let newHeights = heightBagAdd newParentHeight oldHeights
  writeIORef (mergeSubscribedHeightBag subscribed) $! newHeights

revalidateMergeHeight :: MergeSubscribed x k -> IO ()
revalidateMergeHeight subscribed = do
  currentHeight <- readIORef $ mergeSubscribedHeight subscribed
  when (currentHeight == invalidHeight) $ do -- revalidateMergeHeight may be called multiple times; perhaps the's a way to finesse it to avoid this check
    heights <- readIORef $ mergeSubscribedHeightBag subscribed
    parents <- readIORef $ mergeSubscribedParents subscribed
    -- When the number of heights in the bag reaches the number of parents, we should have a valid height
    case heightBagSize heights `compare` DMap.size parents of
      LT -> return ()
      EQ -> do
        let height = succHeight $ heightBagMax heights
        when debugInvalidateHeight $ putStrLn $ "recalculateSubscriberHeight: height: " <> show height
        writeIORef (mergeSubscribedHeight subscribed) $! height
        WeakBag.traverse (mergeSubscribedSubscribers subscribed) $ recalculateSubscriberHeight height
      GT -> error $ "revalidateMergeHeight: more heights than parents for Merge" <> showNodeId subscribed

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

-- | The default, global Spider environment
type Spider = SpiderEnv Global

-- | Designates the default, global Spider timeline
data Global

{-# NOINLINE globalSpiderEnv #-}
globalSpiderEnv :: SpiderEnv Global
globalSpiderEnv = unsafePerformIO unsafeNewSpiderEnv

-- | Stores all global data relevant to a particular Spider timeline; only one
-- value should exist for each type @x@
data SpiderEnv x = SpiderEnv
  { _spiderEnv_toUnsubscribe :: !(IORef [SomeEventSubscription x])
  , _spiderEnv_lock :: !(MVar ())
  , _spiderEnv_cleanups :: !(IORef [IO ()])
#ifdef DEBUG
  , _spiderEnv_depth :: !(IORef Int)
#endif
  }
type role SpiderEnv nominal

instance GEq SpiderEnv where
  a `geq` b = if _spiderEnv_lock a == _spiderEnv_lock b
              then Just $ unsafeCoerce Refl -- This unsafeCoerce is safe because the same SpiderEnv can't have two different 'x' arguments
              else Nothing

unsafeNewSpiderEnv :: forall x. IO (SpiderEnv x)
unsafeNewSpiderEnv = do
  toUnsubscribe <- newIORef []
  lock <- newMVar ()
  cleanups <- newIORef []
#ifdef DEBUG
  depthRef <- newIORef 0
#endif
  return $ SpiderEnv
    { _spiderEnv_toUnsubscribe = toUnsubscribe
    , _spiderEnv_lock = lock
    , _spiderEnv_cleanups = cleanups
#ifdef DEBUG
    , _spiderEnv_depth = depthRef
#endif
    }

-- | Create a new SpiderEnv
newSpiderEnv :: IO (Some SpiderEnv)
newSpiderEnv = Some.This <$> unsafeNewSpiderEnv

newtype SpiderPullM x a = SpiderPullM (BehaviorM x a) deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

newtype SpiderPushM x a = SpiderPushM (ComputeM x a) deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

instance R.Reflex (SpiderEnv x) where
  newtype Behavior (SpiderEnv x) a = SpiderBehavior { unSpiderBehavior :: Behavior x a }
  newtype Event (SpiderEnv x) a = SpiderEvent { unSpiderEvent :: Event x a }
  newtype Dynamic (SpiderEnv x) a = SpiderDynamic { unSpiderDynamic :: Dynamic x Identity a } deriving (Functor, Applicative, Monad)
  newtype Incremental (SpiderEnv x) p a = SpiderIncremental { unSpiderIncremental :: Dynamic x p a }
  type PullM (SpiderEnv x) = SpiderPullM x
  type PushM (SpiderEnv x) = SpiderPushM x
  {-# INLINABLE never #-}
  never = SpiderEvent eventNever
  {-# INLINABLE constant #-}
  constant = SpiderBehavior . behaviorConst
  {-# INLINABLE push #-}
  push f = SpiderEvent . push (coerce f) . unSpiderEvent
  {-# INLINABLE pull #-}
  pull = SpiderBehavior . pull . coerce
  {-# INLINABLE merge #-}
  merge = SpiderEvent . merge . DynamicConst . (coerce :: DMap k (R.Event (SpiderEnv x)) -> DMap k (Event x))
  {-# INLINABLE fan #-}
  fan e = R.EventSelector $ SpiderEvent . select (fan (unSpiderEvent e))
  {-# INLINABLE switch #-}
  switch = SpiderEvent . switch . (coerce :: Behavior x (R.Event (SpiderEnv x) a) -> Behavior x (Event x a)) . unSpiderBehavior
  {-# INLINABLE coincidence #-}
  coincidence = SpiderEvent . coincidence . (coerce :: Event x (R.Event (SpiderEnv x) a) -> Event x (Event x a)) . unSpiderEvent
  {-# INLINABLE current #-}
  current = SpiderBehavior . current . unSpiderDynamic
  {-# INLINABLE updated #-}
  updated = SpiderEvent . fmap runIdentity . updated . unSpiderDynamic
  {-# INLINABLE unsafeBuildDynamic #-}
  unsafeBuildDynamic readV0 v' = SpiderDynamic $ DynamicDynIdentity $ unsafeDyn (coerce readV0) $ coerce $ unSpiderEvent v'
  {-# INLINABLE unsafeBuildIncremental #-}
  unsafeBuildIncremental readV0 dv = SpiderIncremental $ DynamicDyn $ unsafeDyn (coerce readV0) $ unSpiderEvent dv
  {-# INLINABLE mergeIncremental #-}
  mergeIncremental = SpiderEvent . merge . (unsafeCoerce :: Dynamic x R.PatchDMap (DMap k (R.Event (SpiderEnv x))) -> Dynamic x R.PatchDMap (DMap k (Event x))) . unSpiderIncremental
  {-# INLINABLE currentIncremental #-}
  currentIncremental = SpiderBehavior . current . unSpiderIncremental
  {-# INLINABLE updatedIncremental #-}
  updatedIncremental = SpiderEvent . updated . unSpiderIncremental
  {-# INLINABLE incrementalToDynamic #-}
  incrementalToDynamic (SpiderIncremental i) = SpiderDynamic $ DynamicDynIdentity $ unsafeDyn (readBehaviorUntracked $ current i) $ flip push (updated i) $ \p -> do
    c <- readBehaviorUntracked $ current i
    return $ Identity <$> R.apply p c --TODO: Avoid the redundant 'apply'

instance R.MonadSample (SpiderEnv x) (EventM x) where
  {-# INLINABLE sample #-}
  sample (SpiderBehavior b) = readBehaviorUntracked b

instance R.MonadHold (SpiderEnv x) (EventM x) where
  {-# INLINABLE hold #-}
  hold = holdSpiderEventM
  {-# INLINABLE holdDyn #-}
  holdDyn = holdDynSpiderEventM
  {-# INLINABLE holdIncremental #-}
  holdIncremental = holdIncrementalSpiderEventM

holdSpiderEventM :: a -> R.Event (SpiderEnv x) a -> EventM x (R.Behavior (SpiderEnv x) a)
holdSpiderEventM v0 e = fmap (SpiderBehavior . behaviorHoldIdentity) $ hold v0 $ coerce $ unSpiderEvent e

holdDynSpiderEventM :: a -> R.Event (SpiderEnv x) a -> EventM x (R.Dynamic (SpiderEnv x) a)
holdDynSpiderEventM v0 e = fmap (SpiderDynamic . DynamicHoldIdentity) $ hold v0 $ coerce $ unSpiderEvent e

holdIncrementalSpiderEventM :: R.Patch p => a -> R.Event (SpiderEnv x) (p a) -> EventM x (R.Incremental (SpiderEnv x) p a)
holdIncrementalSpiderEventM v0 e = fmap (SpiderIncremental . DynamicHold) $ hold v0 $ unSpiderEvent e

instance R.MonadHold (SpiderEnv x) (SpiderHost x) where
  {-# INLINABLE hold #-}
  hold v0 e = R.runHostFrame $ R.hold v0 e
  {-# INLINABLE holdDyn #-}
  holdDyn v0 e = R.runHostFrame $ R.holdDyn v0 e
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 e = R.runHostFrame $ R.holdIncremental v0 e

instance R.MonadSample (SpiderEnv x) (SpiderHost x) where
  {-# INLINABLE sample #-}
  sample = runFrame . readBehaviorUntracked . unSpiderBehavior

instance R.MonadSample (SpiderEnv x) (SpiderPullM x) where
  {-# INLINABLE sample #-}
  sample = coerce . readBehaviorTracked . unSpiderBehavior

instance R.MonadSample (SpiderEnv x) (SpiderPushM x) where
  {-# INLINABLE sample #-}
  sample (SpiderBehavior b) = SpiderPushM $ readBehaviorUntracked b

instance R.MonadHold (SpiderEnv x) (SpiderPushM x) where
  {-# INLINABLE hold #-}
  hold v0 e = R.current <$> R.holdDyn v0 e
  {-# INLINABLE holdDyn #-}
  holdDyn v0 (SpiderEvent e) = SpiderPushM $ fmap (SpiderDynamic . DynamicHoldIdentity) $ hold v0 $ coerce e
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 (SpiderEvent e) = SpiderPushM $ SpiderIncremental . DynamicHold <$> hold v0 e

data RootTrigger x a = forall k. GCompare k => RootTrigger (WeakBag (Subscriber x a), IORef (DMap k Identity), k a)

data SpiderEventHandle x a = SpiderEventHandle
  { spiderEventHandleSubscription :: EventSubscription x a
  , spiderEventHandleValue :: IORef (Maybe a)
  }

instance R.MonadSubscribeEvent (SpiderEnv x) (SpiderHostFrame x) where
  {-# INLINABLE subscribeEvent #-}
  subscribeEvent e = SpiderHostFrame $ do
    --TODO: Unsubscribe eventually (manually and/or with weak ref)
    val <- liftIO $ newIORef Nothing
    subscription <- subscribe (unSpiderEvent e) $ Subscriber
      { subscriberPropagate = \a -> do
          liftIO $ writeIORef val $ Just a
          scheduleClear val
      , subscriberInvalidateHeight = \_ -> return ()
      , subscriberRecalculateHeight = \_ -> return ()
      }
    return $ SpiderEventHandle
      { spiderEventHandleSubscription = subscription
      , spiderEventHandleValue = val
      }

instance R.ReflexHost (SpiderEnv x) where
  type EventTrigger (SpiderEnv x) = RootTrigger x
  type EventHandle (SpiderEnv x) = SpiderEventHandle x
  type HostFrame (SpiderEnv x) = SpiderHostFrame x

instance R.MonadReadEvent (SpiderEnv x) (ReadPhase x) where
  {-# NOINLINE readEvent #-}
  readEvent h = ReadPhase $ fmap (fmap return) $ liftIO $ do
    result <- readIORef $ spiderEventHandleValue h
    touch h
    return result

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
newtype SpiderHost x a = SpiderHost { unSpiderHost :: ReaderT (SpiderEnv x) IO a } deriving (Functor, Applicative, MonadFix, MonadIO, MonadException, MonadAsyncException)

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
runSpiderHost (SpiderHost a) = runReaderT a globalSpiderEnv

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

instance R.MonadSample (SpiderEnv x) (SpiderHostFrame x) where
  sample = SpiderHostFrame . readBehaviorUntracked . unSpiderBehavior --TODO: This can cause problems with laziness, so we should get rid of it if we can

instance R.MonadHold (SpiderEnv x) (SpiderHostFrame x) where
  {-# INLINABLE hold #-}
  hold v0 e = SpiderHostFrame $ fmap (SpiderBehavior . behaviorHoldIdentity) $ hold v0 $ coerce $ unSpiderEvent e
  {-# INLINABLE holdDyn #-}
  holdDyn v0 e = SpiderHostFrame $ fmap (SpiderDynamic . DynamicHoldIdentity) $ hold v0 $ coerce $ unSpiderEvent e
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 e = SpiderHostFrame $ fmap (SpiderIncremental . DynamicHold) $ hold v0 $ unSpiderEvent e

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

instance R.MonadReflexCreateTrigger (SpiderEnv x) (SpiderHost x) where
  newEventWithTrigger = SpiderHost . lift . fmap SpiderEvent . newEventWithTriggerIO
  newFanEventWithTrigger f = SpiderHost $ lift $ do
    es <- newFanEventWithTriggerIO f
    return $ R.EventSelector $ SpiderEvent . select es

instance R.MonadReflexCreateTrigger (SpiderEnv x) (SpiderHostFrame x) where
  newEventWithTrigger = SpiderHostFrame . EventM . liftIO . fmap SpiderEvent . newEventWithTriggerIO
  newFanEventWithTrigger f = SpiderHostFrame $ EventM $ liftIO $ do
    es <- newFanEventWithTriggerIO f
    return $ R.EventSelector $ SpiderEvent . select es

instance R.MonadSubscribeEvent (SpiderEnv x) (SpiderHost x) where
  {-# INLINABLE subscribeEvent #-}
  subscribeEvent = runFrame . runSpiderHostFrame . R.subscribeEvent

newtype ReadPhase x a = ReadPhase (ResultM x a) deriving (Functor, Applicative, Monad, MonadFix)

instance R.MonadSample (SpiderEnv x) (ReadPhase x) where
  {-# INLINABLE sample #-}
  sample = ReadPhase . R.sample

instance R.MonadHold (SpiderEnv x) (ReadPhase x) where
  {-# INLINABLE hold #-}
  hold v0 e = ReadPhase $ R.hold v0 e
  {-# INLINABLE holdDyn #-}
  holdDyn v0 e = ReadPhase $ R.holdDyn v0 e
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 e = ReadPhase $ R.holdIncremental v0 e

instance R.MonadReflexHost (SpiderEnv x) (SpiderHost x) where
  type ReadPhase (SpiderHost x) = ReadPhase x
  fireEventsAndRead es (ReadPhase a) = run es a
  runHostFrame = runFrame . runSpiderHostFrame

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
