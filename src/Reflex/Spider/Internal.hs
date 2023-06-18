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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
{-# OPTIONS_GHC -Wunused-binds #-}
-- | This module is the implementation of the 'Spider' 'Reflex' engine.  It uses
-- a graph traversal algorithm to propagate 'Event's and 'Behavior's.
module Reflex.Spider.Internal (module Reflex.Spider.Internal) where

#if MIN_VERSION_base(4,10,0)
import Control.Applicative (liftA2)
#endif
import Control.Concurrent
import Control.Exception
import Control.Monad hiding (forM, forM_, mapM, mapM_)
import Control.Monad.Exception
import Control.Monad.Identity hiding (forM, forM_, mapM, mapM_)
import Control.Monad.Primitive
import Control.Monad.Reader.Class
import Control.Monad.IO.Class
import Control.Monad.ReaderIO
import Control.Monad.Ref (MonadRef, MonadAtomicRef)
import qualified Control.Monad.Ref as MonadRef
import Control.Monad.Fail (MonadFail)
import qualified Control.Monad.Fail as MonadFail
import Data.Align
import Data.Coerce
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum (..))
import Data.FastMutableIntMap (FastMutableIntMap, PatchIntMap (..))
import qualified Data.FastMutableIntMap as FastMutableIntMap
import Data.Foldable hiding (concat, elem, sequence_)
import Data.Functor.Constant
import Data.Functor.Misc
import Data.Functor.Product
import Data.GADT.Compare
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef
import Data.Kind (Type)
import Data.Maybe hiding (mapMaybe)
import Data.Monoid (mempty, (<>))
import Data.Proxy
import Data.These
import Data.Traversable
import Data.Type.Equality ((:~:)(Refl))
import Data.Witherable (Filterable, mapMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Exts hiding (toList)
import GHC.IORef (IORef (..))
import Reflex.FastWeak
import System.IO.Unsafe
import System.Mem.Weak
import Unsafe.Coerce
import Data.List (intercalate)
import Data.Trie (Trie (..))
import qualified Data.Trie as Trie
import Data.Sequence ((<|))
import qualified Data.Sequence as Seq

import Reflex.Spider.Ref
#ifdef DEBUG_TRACE_REFS
import Reflex.Spider.Ref.Debug
#else
import Reflex.Spider.Ref.Normal
#endif

import Reflex.Spider.NodeInfo

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
import Control.Monad.RWS hiding (Any, forM, forM_, mapM, mapM_, sequence)
#endif

import Data.FastWeakBag (FastWeakBag, FastWeakBagTicket)
import qualified Data.FastWeakBag as FastWeakBag

import Data.Reflection
import Data.Some (Some(Some))
import Data.Type.Coercion
import Data.Profunctor.Unsafe (( #.), (.#))
import Data.WeakBag (WeakBag, WeakBagTicket, _weakBag_children)
import qualified Data.WeakBag as WeakBag
import qualified Reflex.Class
import qualified Reflex.Class as R
import qualified Reflex.Host.Class
import Reflex.NotReady.Class
import Data.Patch
import qualified Data.Patch.DMapWithMove as PatchDMapWithMove
import Reflex.PerformEvent.Base (PerformEventT)

#ifdef DEBUG

class HasNodeId a where
  type NodeIdX a :: *
  getNodeId :: a -> NodeId (NodeIdX a)

instance HasNodeId (EventSubscribed x) where
  type NodeIdX (EventSubscribed x) = x
  getNodeId = eventSubscribedNodeId

instance HasNodeId (CacheSubscribed x a) where
  type NodeIdX (CacheSubscribed x a) = x
  getNodeId = _cacheSubscribed_nodeId

instance HasNodeId (FanInt x a) where
  type NodeIdX (FanInt x a) = x
  getNodeId = _fanInt_nodeId

instance HasNodeId (Hold x p) where
  type NodeIdX (Hold x p) = x
  getNodeId = holdNodeId

instance HasNodeId (Switch x a) where
  type NodeIdX (Switch x a) = x
  getNodeId = switchNodeId

instance HasNodeId (Merge x k v s) where
  type NodeIdX (Merge x k v s) = x
  getNodeId = _merge_nodeId

instance HasNodeId (SwitchSubscribed x a) where
  type NodeIdX (SwitchSubscribed x a) = x
  getNodeId = switchSubscribedNodeId

instance HasNodeId (Fan x v a) where
  type NodeIdX (Fan x v a) = x
  getNodeId = fanNodeId

instance HasNodeId (FanSubscribed x v a) where
  type NodeIdX (FanSubscribed x v a) = x
  getNodeId = fanSubscribedNodeId

instance HasNodeId (Coincidence x a) where
  type NodeIdX (Coincidence x a) = x
  getNodeId = coincidenceNodeId

instance HasNodeId (CoincidenceSubscribed x a) where
  type NodeIdX (CoincidenceSubscribed x a) = x
  getNodeId = coincidenceSubscribedNodeId

instance HasNodeId (Root x a) where
  type NodeIdX (Root x a) = x
  getNodeId = rootNodeId

instance HasNodeId (RootSubscribed x a) where
  type NodeIdX (RootSubscribed x a) = x
  getNodeId = rootSubscribedNodeId

instance HasNodeId (Pull x a) where
  type NodeIdX (Pull x a) = x
  getNodeId = pullNodeId

{-# INLINE showNodeId #-}
showNodeId :: HasNodeId a => a -> String
showNodeId = showNodeId' . getNodeId

showNodeId' :: NodeId x -> String
showNodeId' = ("#"<>) . show . unNodeId


#else

newtype NodeId x = NodeId ()

getNodeId :: a -> NodeId x
getNodeId _ = NodeId ()

-- This must be inline, or error messages will cause memory leaks due to retaining the node in question
{-# INLINE showNodeId #-}
showNodeId :: a -> String
showNodeId _ = ""

{-# INLINE showNodeId' #-}
showNodeId' :: NodeId x -> String
showNodeId' _ = ""

#endif

{-# INLINE newRefI #-}
newRefI :: NodeId x -> String -> a -> IO (Ref x a)
newRefI nodeId name = newRefN (RefName (showNodeId' nodeId <> ":" <> name))

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
{-# INLINE [1] pushCheap #-}
pushCheap :: (a -> EventM x (Maybe b)) -> Event x a -> Event x b
pushCheap !f e = Event $ \sub -> do
  (subscription, occ) <- subscribeAndRead e $ sub
    { subscriberPropagate = \a -> do
        mb <- f a
        mapM_ (subscriberPropagate sub) mb
    }
  occ' <- join <$> mapM f occ
  return (subscription, occ')

data CacheSubscribed x a
   = CacheSubscribed { _cacheSubscribed_subscribers :: {-# UNPACK #-} !(FastWeakBag (Subscriber x a))
                     , _cacheSubscribed_parent :: {-# UNPACK #-} !(EventSubscription x)
                     , _cacheSubscribed_occurrence :: {-# UNPACK #-} !(Ref x (Maybe a))
                     , _cacheSubscribed_nodeId :: {-# UNPACK #-} !(NodeId x)
                     , _cacheSubscribed_stackInfo :: {-# UNPACK #-} !StackInfo
                     }

nowSpiderEventM :: (HasSpiderTimeline x) => EventM x (R.Event (SpiderTimeline x) ())
nowSpiderEventM =
  SpiderEvent <$> now

now :: forall x m. (HasSpiderTimeline x, MonadIO m, Defer (Some (Clear x)) m) => m (Event x ())
now = do
  nodeId <- newNodeId @x
  nowOrNot <- liftIO $ newRefI nodeId "nowOrNot" $ Just ()
  scheduleClear nowOrNot
  return . Event $ \_ -> do
    occ <- liftIO . readRef $ nowOrNot
    return ( EventSubscription (return ()) eventSubscribedNow
           , occ
           )

-- | Construct an 'Event' whose value is guaranteed not to be recomputed
-- repeatedly
--
--TODO: Try a caching strategy where we subscribe directly to the parent when
--there's only one subscriber, and then build our own FastWeakBag only when a second
--subscriber joins
{-# NOINLINE [0] cacheEvent #-}
cacheEvent :: forall x a. HasSpiderTimeline x => Event x a -> Event x a
cacheEvent e = withStackInfo e $ \stackInfo -> Event $
  unsafePerformIO $ do
    nodeId <- newNodeId @x
    mSubscribedRef :: Ref x (FastWeak (CacheSubscribed x a))
        <- newRefI nodeId "mSubscribedRef" emptyFastWeak
    pure $ \sub -> {-# SCC "cacheEvent" #-} frame @x ("cacheEvent" <> showNodeId' nodeId <> ": subscribe") $ do
          subscribedTicket <- liftIO (readRef mSubscribedRef >>= getFastWeakTicket) >>= \case
            Just subscribedTicket -> return subscribedTicket
            Nothing -> do
              subscribers <- liftIO FastWeakBag.empty
              occRef <- liftIO $ newRefI nodeId "occRef" Nothing -- This should never be read prior to being set below
              (parentSub, occ) <- subscribeAndRead e $ debugSubscriber' ("cacheEvent" <> showNodeId' nodeId) $ Subscriber
                  { subscriberPropagate = \a -> frame @x ("cacheEvent" <> showNodeId' nodeId <> ": subscriberPropagate") $ do
                      liftIO $ writeRef @x occRef (Just a)
                      scheduleClear occRef
                      propagateFast a subscribers
                  , subscriberInvalidateHeight = \oldHeight -> frame @x ("cacheEvent" <> showNodeId' nodeId <> ": subscriberInvalidateHeight") $ do
                      FastWeakBag.traverse_ subscribers $ invalidateSubscriberHeight oldHeight
                  , subscriberRecalculateHeight = \newHeight -> frame @x ("cacheEvent" <> showNodeId' nodeId <> ": subscriberRecalculateHeight") $ do
                      FastWeakBag.traverse_ subscribers $ recalculateSubscriberHeight newHeight
                  }
              when (isJust occ) $ do
                liftIO $ writeRef @x occRef occ -- Set the initial value of occRef; we don't need to do this if occ is Nothing
                scheduleClear occRef
              let !subscribed = CacheSubscribed
                    { _cacheSubscribed_subscribers = subscribers
                    , _cacheSubscribed_parent = parentSub
                    , _cacheSubscribed_occurrence = occRef
                    , _cacheSubscribed_nodeId = nodeId
                    , _cacheSubscribed_stackInfo = stackInfo
                    }
              subscribedTicket <- liftIO $ mkFastWeakTicket subscribed
              liftIO $ writeRef @x mSubscribedRef =<< getFastWeakTicketWeak subscribedTicket
              return subscribedTicket
          liftIO $ cacheSubscription sub mSubscribedRef subscribedTicket

cacheSubscription
  :: forall x a
  .  HasSpiderTimeline x
  => Subscriber x a
  -> Ref x (FastWeak (CacheSubscribed x a))
  -> FastWeakTicket (CacheSubscribed x a)
  -> IO (EventSubscription x, Maybe a)
cacheSubscription sub mSubscribedRef subscribedTicket = do
  subscribed <- getFastWeakTicketValue subscribedTicket
  ticket <- FastWeakBag.insert sub $ _cacheSubscribed_subscribers subscribed
  occ <- readRef $ _cacheSubscribed_occurrence subscribed

  let parentSub = _cacheSubscribed_parent subscribed
      es = EventSubscription
        { _eventSubscription_unsubscribe = do
          FastWeakBag.remove ticket

          isEmpty <- FastWeakBag.isEmpty $ _cacheSubscribed_subscribers subscribed
          when isEmpty $ do
            writeRef @x mSubscribedRef emptyFastWeak
            unsubscribe parentSub
          touch ticket
          touch subscribedTicket
        , _eventSubscription_subscribed = EventSubscribed
          { eventSubscribedHeightRef = eventSubscribedHeightRef $ _eventSubscription_subscribed parentSub
          , eventSubscribedRetained = toAny subscribedTicket
#ifdef DEBUG_CYCLES
          , eventSubscribedNodeId = getNodeId subscribed
          , eventSubscribedGetParents = return [_eventSubscription_subscribed parentSub]
          , eventSubscribedHasOwnHeightRef = False
          , eventSubscribedWhoCreated = stackInfoToStrings $ _cacheSubscribed_stackInfo subscribed
#endif
          }
        }
  return (es, occ)


subscribe :: Event x a -> Subscriber x a -> EventM x (EventSubscription x)
subscribe e s = fst <$> subscribeAndRead e s

{-# INLINE wrap #-}
wrap :: MonadIO m => (t -> EventSubscribed x) -> (Subscriber x a -> m (WeakBagTicket, t, Maybe a)) -> Subscriber x a -> m (EventSubscription x, Maybe a)
wrap tag getSpecificSubscribed sub = do
  (sln, subd, occ) <- getSpecificSubscribed sub
  let es = tag subd
  return (EventSubscription (WeakBag.remove sln >> touch sln) es, occ)

eventRoot :: (GCompare k, HasSpiderTimeline x) => k a -> Root x k -> Event x a
eventRoot !k !r = Event $ wrap eventSubscribedRoot $ liftIO . getRootSubscribed k r

subscribeAndReadNever :: EventM x (EventSubscription x, Maybe a)
subscribeAndReadNever = return (EventSubscription (return ()) eventSubscribedNever, Nothing)

eventNever :: Event x a
eventNever = Event $ const subscribeAndReadNever

eventFan :: (GCompare k, HasSpiderTimeline x) => k a -> Fan x k v -> Event x (v a)
eventFan !k !f = Event $ wrap eventSubscribedFan $ getFanSubscribed k f

eventSwitch :: HasSpiderTimeline x => Switch x a -> Event x a
eventSwitch !s = Event $ wrap eventSubscribedSwitch $ getSwitchSubscribed s

eventCoincidence :: HasSpiderTimeline x => Coincidence x a -> Event x a
eventCoincidence !c = Event $ wrap eventSubscribedCoincidence $ getCoincidenceSubscribed c

eventHold :: Hold x p -> Event x p
eventHold !h = Event $ subscribeHoldEvent h

eventDyn :: (HasSpiderTimeline x, Patch p) => Dyn x p -> Event x p
eventDyn !j = Event $ \sub -> getDynHold j >>= \h -> subscribeHoldEvent h sub

{-# INLINE subscribeCoincidenceInner #-}
subscribeCoincidenceInner :: HasSpiderTimeline x => Event x a -> Height -> CoincidenceSubscribed x a -> EventM x (Maybe a, Height, EventSubscribed x)
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

newSubscriberHold :: forall x p. (HasSpiderTimeline x, Patch p) => Hold x p -> IO (Subscriber x p)
newSubscriberHold h = debugSubscriber ("SubscriberHold" <> showNodeId h) $ Subscriber
  { subscriberPropagate = {-# SCC "traverseHold" #-} propagateSubscriberHold h
  , subscriberInvalidateHeight = \_ -> frame @x ("newSubscriberHold" <> showNodeId h <> ": subscriberPropagate") $ return ()
  , subscriberRecalculateHeight = \_ -> frame @x ("newSubscriberHold" <> showNodeId h <> ": subscriberPropagate") $ return ()
  }

newSubscriberFan :: forall x k v. (HasSpiderTimeline x, GCompare k) => FanSubscribed x k v -> IO (Subscriber x (DMap k v))
newSubscriberFan subscribed = debugSubscriber ("SubscriberFan " <> showNodeId subscribed) $ Subscriber
  { subscriberPropagate = \a -> {-# SCC "traverseFan" #-} do
      subs <- liftIO $ readRef $ fanSubscribedSubscribers subscribed
      liftIO $ writeRef @x (fanSubscribedOccurrence subscribed) $ Just a
      scheduleClear $ fanSubscribedOccurrence subscribed
      let f _ (Pair v subsubs) = do
            propagate v $ _fanSubscribedChildren_list subsubs
            return $ Constant ()
      _ <- DMap.traverseWithKey f $ DMap.intersectionWithKey (\_ -> Pair) a subs --TODO: Would be nice to have DMap.traverse_
      return ()
  , subscriberInvalidateHeight = \old -> do
      subscribers <- readRef $ fanSubscribedSubscribers subscribed
      forM_ (DMap.toList subscribers) $ \(_ :=> v) -> WeakBag.traverse_ (_fanSubscribedChildren_list v) $ invalidateSubscriberHeight old
  , subscriberRecalculateHeight = \new -> do
      subscribers <- readRef $ fanSubscribedSubscribers subscribed
      forM_ (DMap.toList subscribers) $ \(_ :=> v) -> WeakBag.traverse_ (_fanSubscribedChildren_list v) $ recalculateSubscriberHeight new
  }

newSubscriberSwitch :: forall x a. HasSpiderTimeline x => SwitchSubscribed x a -> IO (Subscriber x a)
newSubscriberSwitch subscribed = debugSubscriber ("SubscriberCoincidenceOuter" <> showNodeId subscribed) $ Subscriber
  { subscriberPropagate = \a -> {-# SCC "traverseSwitch" #-} do
      liftIO $ writeRef @x (switchSubscribedOccurrence subscribed) $ Just a
      scheduleClear $ switchSubscribedOccurrence subscribed
      propagate a $ switchSubscribedSubscribers subscribed
  , subscriberInvalidateHeight = \_ -> do
      oldHeight <- readRef $ switchSubscribedHeight subscribed
      when (oldHeight /= invalidHeight) $ do
        writeRef @x (switchSubscribedHeight subscribed) $! invalidHeight
        WeakBag.traverse_ (switchSubscribedSubscribers subscribed) $ invalidateSubscriberHeight oldHeight
  , subscriberRecalculateHeight = (`updateSwitchHeight` subscribed)
    }

newSubscriberCoincidenceOuter :: forall x b. HasSpiderTimeline x => CoincidenceSubscribed x b -> IO (Subscriber x (Event x b))
newSubscriberCoincidenceOuter subscribed = debugSubscriber ("SubscriberCoincidenceOuter" <> showNodeId subscribed) $ Subscriber
  { subscriberPropagate = \a -> {-# SCC "traverseCoincidenceOuter" #-} do
      outerHeight <- liftIO $ readRef $ coincidenceSubscribedHeight subscribed
      (occ, innerHeight, innerSubd) <- subscribeCoincidenceInner a outerHeight subscribed

      liftIO $ writeRef @x (coincidenceSubscribedInnerParent subscribed) $ Just innerSubd
      scheduleClear $ coincidenceSubscribedInnerParent subscribed
      case occ of
        Nothing ->
          when (innerHeight > outerHeight) $ liftIO $ do -- If the event fires, it will fire at a later height
            writeRef @x (coincidenceSubscribedHeight subscribed) $! invalidHeight
            WeakBag.traverse_ (coincidenceSubscribedSubscribers subscribed) $ invalidateSubscriberHeight outerHeight
            putStrLn $ "newSubscriberCoincidenceOuter: done invalidating"
            newInnerHeight <- liftIO $ readRef $ eventSubscribedHeightRef innerSubd
            when (newInnerHeight == invalidHeight) $ do
#ifdef DEBUG_CYCLES
              nodesInvolvedInCycle <- walkInvalidHeightParents innerSubd
              nodeInfos <- getNodeInfos nodesInvolvedInCycle
              throwIO (CausalityLoopException nodeInfos)
#else
              throwIO CausalityLoopException
#endif
            writeRef @x (coincidenceSubscribedHeight subscribed) $! innerHeight
            WeakBag.traverse_ (coincidenceSubscribedSubscribers subscribed) $ recalculateSubscriberHeight innerHeight
            putStrLn $ "newSubscriberCoincidenceOuter: done recalculating"
        Just o -> do -- Since it's already firing, no need to adjust height
          liftIO $ writeRef @x (coincidenceSubscribedOccurrence subscribed) occ
          scheduleClear $ coincidenceSubscribedOccurrence subscribed
          propagate o $ coincidenceSubscribedSubscribers subscribed
  , subscriberInvalidateHeight  = \_ -> invalidateCoincidenceHeight subscribed
  , subscriberRecalculateHeight = \_ -> recalculateCoincidenceHeight subscribed
  }

newSubscriberCoincidenceInner :: forall x a. HasSpiderTimeline x => CoincidenceSubscribed x a -> IO (Subscriber x a)
newSubscriberCoincidenceInner subscribed = debugSubscriber ("SubscriberCoincidenceInner" <> showNodeId subscribed) $ Subscriber
  { subscriberPropagate = \a -> {-# SCC "traverseCoincidenceInner" #-} do
      occ <- liftIO $ readRef $ coincidenceSubscribedOccurrence subscribed
      case occ of
        Just _ -> return () -- SubscriberCoincidenceOuter must have already propagated this event
        Nothing -> do
          liftIO $ writeRef @x (coincidenceSubscribedOccurrence subscribed) $ Just a
          scheduleClear $ coincidenceSubscribedOccurrence subscribed
          propagate a $ coincidenceSubscribedSubscribers subscribed
  , subscriberInvalidateHeight  = \_ -> invalidateCoincidenceHeight subscribed
  , subscriberRecalculateHeight = \_ -> recalculateCoincidenceHeight subscribed
  }

invalidateSubscriberHeight :: Height -> Subscriber x a -> IO ()
invalidateSubscriberHeight = flip subscriberInvalidateHeight

recalculateSubscriberHeight :: Height -> Subscriber x a -> IO ()
recalculateSubscriberHeight = flip subscriberRecalculateHeight

-- | Propagate everything at the current height
propagate :: forall x a. a -> WeakBag (Subscriber x a) -> EventM x ()
propagate a subscribers =
  -- Note: in the following traversal, we do not visit nodes that are added to the list during our traversal; they are new events, which will necessarily have full information already, so there is no need to traverse them
  --TODO: Should we check if nodes already have their values before propagating?  Maybe we're re-doing work
  WeakBag.traverse_ subscribers $ \s -> subscriberPropagate s a

-- | Propagate everything at the current height
propagateFast :: forall x a. a -> FastWeakBag (Subscriber x a) -> EventM x ()
propagateFast a subscribers =
  -- Note: in the following traversal, we do not visit nodes that are added to the list during our traversal; they are new events, which will necessarily have full information already, so there is no need to traverse them
  --TODO: Should we check if nodes already have their values before propagating?  Maybe we're re-doing work
  FastWeakBag.traverse_ subscribers $ \s -> subscriberPropagate s a

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
  { eventSubscribedHeightRef :: {-# UNPACK #-} !(Ref x Height)
  , eventSubscribedRetained :: {-# NOUNPACK #-} !Any
#ifdef DEBUG_CYCLES
  , eventSubscribedNodeId :: {-# UNPACK #-} !(NodeId x)
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
  , eventSubscribedNodeId = getNodeId r
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
  , eventSubscribedNodeId = neverNodeId
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
  , eventSubscribedNodeId = nowNodeId
  , eventSubscribedGetParents = return []
  , eventSubscribedHasOwnHeightRef = False
  , eventSubscribedWhoCreated = return ["now"]
#endif
  }

eventSubscribedFan :: FanSubscribed x k v -> EventSubscribed x
eventSubscribedFan !subscribed = EventSubscribed
  { eventSubscribedHeightRef = eventSubscribedHeightRef $ _eventSubscription_subscribed $ fanSubscribedParent subscribed
  , eventSubscribedRetained = toAny subscribed
#ifdef DEBUG_CYCLES
  , eventSubscribedNodeId = getNodeId subscribed
  , eventSubscribedGetParents = return [_eventSubscription_subscribed $ fanSubscribedParent subscribed]
  , eventSubscribedHasOwnHeightRef = False
  , eventSubscribedWhoCreated = stackInfoToStrings $ fanSubscribedCcs subscribed
#endif
  }

eventSubscribedSwitch :: SwitchSubscribed x a -> EventSubscribed x
eventSubscribedSwitch !subscribed = EventSubscribed
  { eventSubscribedHeightRef = switchSubscribedHeight subscribed
  , eventSubscribedRetained = toAny subscribed
#ifdef DEBUG_CYCLES
  , eventSubscribedNodeId = getNodeId subscribed
  , eventSubscribedGetParents = do
      s <- readRef $ switchSubscribedCurrentParent subscribed
      return [_eventSubscription_subscribed s]
  , eventSubscribedHasOwnHeightRef = True
  , eventSubscribedWhoCreated = stackInfoToStrings $ switchSubscribedCcs subscribed
#endif
  }

eventSubscribedCoincidence :: CoincidenceSubscribed x a -> EventSubscribed x
eventSubscribedCoincidence !subscribed = EventSubscribed
  { eventSubscribedHeightRef = coincidenceSubscribedHeight subscribed
  , eventSubscribedRetained = toAny subscribed
#ifdef DEBUG_CYCLES
  , eventSubscribedNodeId = getNodeId subscribed
  , eventSubscribedGetParents = do
      innerSubscription <- readRef $ coincidenceSubscribedInnerParent subscribed
      let outerParent = _eventSubscription_subscribed $ coincidenceSubscribedOuterParent subscribed
          innerParents = maybeToList $ innerSubscription
      return $ outerParent : innerParents
  , eventSubscribedHasOwnHeightRef = True
  , eventSubscribedWhoCreated = stackInfoToStrings $ coincidenceSubscribedCcs subscribed
#endif
  }

getEventSubscribedHeight :: EventSubscribed x -> IO Height
getEventSubscribedHeight es = readRef $ eventSubscribedHeightRef es

#ifdef DEBUG_CYCLES
whoCreatedEventSubscribed :: EventSubscribed x -> IO [String]
whoCreatedEventSubscribed = eventSubscribedWhoCreated

walkInvalidHeightParents :: forall x. HasSpiderTimeline x => EventSubscribed x -> IO [EventSubscribed x]
walkInvalidHeightParents s0 = do
  subscribers <- flip execStateT mempty $ ($ s0) $ fix $ \loop s -> do
    h <- liftIO $ readRef $ eventSubscribedHeightRef s
    when (h == invalidHeight) $ do
      when (eventSubscribedHasOwnHeightRef s) $ liftIO $ writeRef @x (eventSubscribedHeightRef s) $! invalidHeightBeingTraversed
      modify (s :)
      mapM_ loop =<< liftIO (eventSubscribedGetParents s)
  forM_ subscribers $ \s -> writeRef @x (eventSubscribedHeightRef s) $! invalidHeight
  return subscribers
#endif

{-# INLINE subscribeHoldEvent #-}
subscribeHoldEvent :: Hold x p -> Subscriber x p -> EventM x (EventSubscription x, Maybe p)
subscribeHoldEvent = subscribeAndRead . holdEvent

--------------------------------------------------------------------------------
-- Behavior
--------------------------------------------------------------------------------

newtype Behavior x a = Behavior { readBehaviorTracked :: BehaviorM x a }

behaviorHold :: HasSpiderTimeline x => Hold x p -> Behavior x (PatchTarget p)
behaviorHold !h = Behavior $ readHoldTracked h

behaviorHoldIdentity :: HasSpiderTimeline x => Hold x (Identity a) -> Behavior x a
behaviorHoldIdentity = behaviorHold

behaviorConst :: a -> Behavior x a
behaviorConst !a = Behavior $ return a

behaviorPull :: forall x a. HasSpiderTimeline x => Pull x a -> Behavior x a
behaviorPull !p = Behavior $ do
    val <- liftIO $ readRef $ pullValue p
    nodeId <- newNodeId @x
    case val of
      Just subscribed -> do
        askParentsRef >>= mapM_ (\r -> liftIO $ modifyRef' @x r (SomeBehaviorSubscribed (Some (BehaviorSubscribedPull subscribed)) :))
        askInvalidator >>= mapM_ (\wi -> liftIO $ modifyRef' @x (pullSubscribedInvalidators subscribed) (wi:))
        liftIO $ touch $ pullSubscribedOwnInvalidator subscribed
        return $ pullSubscribedValue subscribed
      Nothing -> do
        i <- liftIO $ newInvalidatorPull p
        wi <- liftIO $ mkWeakPtrWithDebug i "InvalidatorPull"
        parentsRef <- liftIO $ newRefI nodeId "parentsRef" []
        holdInits <- askBehaviorHoldInits
        a <- liftIO $ runReaderIO (unBehaviorM $ pullCompute p) (Just (wi, parentsRef), holdInits)
        invsRef <- liftIO . newRefI nodeId "invsRef" . maybeToList =<< askInvalidator
        parents <- liftIO $ readRef parentsRef
        let subscribed = PullSubscribed
              { pullSubscribedValue = a
              , pullSubscribedInvalidators = invsRef
              , pullSubscribedOwnInvalidator = i
              , pullSubscribedParents = parents
              }
        liftIO $ writeRef @x (pullValue p) $ Just subscribed
        askParentsRef >>= mapM_ (\r -> liftIO $ modifyRef' @x r (SomeBehaviorSubscribed (Some (BehaviorSubscribedPull subscribed)) :))
        return a

behaviorDyn :: (Patch p, HasSpiderTimeline x) => Dyn x p -> Behavior x (PatchTarget p)
behaviorDyn !d = Behavior $ readHoldTracked =<< getDynHold d

{-# INLINE readHoldTracked #-}
readHoldTracked :: forall x p. HasSpiderTimeline x => Hold x p -> BehaviorM x (PatchTarget p)
readHoldTracked h = do
  result <- liftIO $ readRef $ holdValue h
  askInvalidator >>= mapM_ (\wi -> liftIO $ modifyRef' @x (holdInvalidators h) (wi:))
  askParentsRef >>= mapM_ (\r -> liftIO $ modifyRef' @x r (SomeBehaviorSubscribed (Some (BehaviorSubscribedHold h)) :))
  liftIO $ touch h -- Otherwise, if this gets inlined enough, the hold's parent reference may get collected
  return result

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




dynamicHold :: HasSpiderTimeline x => Hold x p -> DynamicS x p
dynamicHold !h = Dynamic
  { dynamicCurrent = behaviorHold h
  , dynamicUpdated = eventHold h
  }

dynamicHoldIdentity :: HasSpiderTimeline x => Hold x (Identity a) -> DynamicS x (Identity a)
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
   = Hold { holdValue :: !(Ref x (PatchTarget p))
          , holdInvalidators :: !(Ref x [Weak (Invalidator x)])
          , holdEvent :: Event x p -- This must be lazy, or holds cannot be defined before their input Events
          , holdParent :: !(Ref x (Maybe (EventSubscription x))) -- Keeps its parent alive (will be undefined until the hold is initialized) --TODO: Probably shouldn't be an Ref
          , holdNodeId :: {-# UNPACK #-} !(NodeId x)
          }

-- | A statically allocated 'SpiderTimeline'
data GlobalS
data GlobalX

type Global = LocalSpiderTimeline GlobalX GlobalS

instance Reifies GlobalS (SpiderTimelineEnv GlobalX) where
  reflect _ = globalSpiderTimelineEnv

{-# NOINLINE globalSpiderTimelineEnv #-}
globalSpiderTimelineEnv :: SpiderTimelineEnv GlobalX
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
  , _spiderTimeline_stack :: {-# UNPACK #-} !(IORef (Int, [String]))
  , _spiderTimeline_nodeIdAllocator :: {-# UNPACK #-} !(NodeIdAllocator x)
#endif
  }

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
              , eventEnvClears :: !(IORef [Some (Clear x)]) -- Needed for Subscribe
              , eventEnvIntClears :: !(IORef [Some (IntClear x)])
              , eventEnvRootClears :: !(IORef [Some (RootClear x)])
              , eventEnvCurrentHeight :: !(Ref x Height) -- Needed for Subscribe
              , eventEnvResetCoincidences :: !(IORef [SomeResetCoincidence x]) -- Needed for Subscribe
              , eventEnvDelayedMerges :: !(IORef (IntMap [EventM x ()]))
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
    liftIO $ readRef heightRef
  {-# INLINE scheduleMerge #-}
  scheduleMerge height subscribed = do
    delayedRef <- asksEventEnv eventEnvDelayedMerges
    liftIO $ modifyIORef' delayedRef $ IntMap.insertWith (++) (unHeight height) [subscribed]

class HasSpiderTimeline x where
  -- | Retrieve the current SpiderTimelineEnv
  spiderTimeline :: SpiderTimelineEnv x

putCurrentHeight :: forall x. HasSpiderTimeline x => Height -> EventM x ()
putCurrentHeight h = do
  heightRef <- asksEventEnv eventEnvCurrentHeight
  liftIO $ writeRef @x heightRef $! h

instance HasSpiderTimeline x => Defer (Some (Clear x)) (EventM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = asksEventEnv eventEnvClears

{-# INLINE scheduleClear #-}
scheduleClear :: Defer (Some (Clear x)) m => Ref x (Maybe a) -> m ()
scheduleClear r = defer $ Some $ Clear r

instance HasSpiderTimeline x => Defer (Some (IntClear x)) (EventM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = asksEventEnv eventEnvIntClears

{-# INLINE scheduleIntClear #-}
scheduleIntClear :: Defer (Some (IntClear x)) m => Ref x (IntMap a) -> m ()
scheduleIntClear r = defer $ Some $ IntClear r

instance HasSpiderTimeline x => Defer (Some (RootClear x)) (EventM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = asksEventEnv eventEnvRootClears

{-# INLINE scheduleRootClear #-}
scheduleRootClear :: Defer (Some (RootClear x)) m => Ref x (DMap k Identity) -> m ()
scheduleRootClear r = defer $ Some $ RootClear r

instance HasSpiderTimeline x => Defer (SomeResetCoincidence x) (EventM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = asksEventEnv eventEnvResetCoincidences

-- Note: hold cannot examine its event until after the phase is over
{-# INLINE [1] hold #-}
hold :: forall x p m. (Patch p, Defer (SomeHoldInit x) m, HasSpiderTimeline x) => PatchTarget p -> Event x p -> m (Hold x p)
hold v0 e = do
  nodeId <- newNodeId @x
  valRef <- liftIO $ newRefI nodeId "valRef" v0
  invsRef <- liftIO $ newRefI nodeId "invsRef" []
  parentRef <- liftIO $ newRefI nodeId "parentRef" Nothing
  let h = Hold
        { holdValue = valRef
        , holdInvalidators = invsRef
        , holdEvent = e
        , holdParent = parentRef
        , holdNodeId = nodeId
        }
  defer $ SomeHoldInit h
  return h

{-# INLINE getHoldEventSubscription #-}
getHoldEventSubscription :: forall p x. (HasSpiderTimeline x, Patch p) => Hold x p -> EventM x (EventSubscription x)
getHoldEventSubscription h = do
  ep <- liftIO $ readRef $ holdParent h
  case ep of
    Just subd -> return subd
    Nothing -> do
      let e = holdEvent h
      subscriptionRef <- liftIO $ newRefI (getNodeId h) "subscriptionRef" $ error "getHoldEventSubscription: subdRef uninitialized"
      (subscription@(EventSubscription _ _), occ) <- subscribeAndRead e =<< liftIO (newSubscriberHold h)
      liftIO $ writeRef @x subscriptionRef $! subscription
      case occ of
        Nothing -> return ()
        Just o -> do
          old <- liftIO $ readRef $ holdValue h
          case apply o old of
            Nothing -> return ()
            Just new -> do
              -- Need to evaluate these so that we don't retain the Hold itself
              v <- liftIO $ evaluate $ holdValue h
              i <- liftIO $ evaluate $ holdInvalidators h
              defer $ SomeAssignment v i new
      liftIO $ writeRef @x (holdParent h) $ Just subscription
      return subscription

type BehaviorEnv x = (Maybe (Weak (Invalidator x), Ref x [SomeBehaviorSubscribed x]), IORef [SomeHoldInit x])

-- BehaviorM can sample behaviors
newtype BehaviorM x a = BehaviorM { unBehaviorM :: ReaderIO (BehaviorEnv x) a }
  deriving (Functor, Applicative, MonadIO, MonadFix, MonadReader (BehaviorEnv x))

instance Monad (BehaviorM x) where
  {-# INLINE (>>=) #-}
  BehaviorM x >>= f = BehaviorM $ x >>= unBehaviorM . f
  {-# INLINE (>>) #-}
  BehaviorM x >> BehaviorM y = BehaviorM $ x >> y
  {-# INLINE return #-}
  return x = BehaviorM $ return x
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
                    , pullSubscribedInvalidators :: !(Ref x [Weak (Invalidator x)])
                    , pullSubscribedOwnInvalidator :: !(Invalidator x)
                    , pullSubscribedParents :: ![SomeBehaviorSubscribed x] -- Need to keep parent behaviors alive, or they won't let us know when they're invalidated
                    }

--type role Pull representational
data Pull x a
   = Pull { pullValue :: !(Ref x (Maybe (PullSubscribed x a)))
          , pullCompute :: !(BehaviorM x a)
          , pullNodeId :: {-# UNPACK #-} !(NodeId x)
          }

data Invalidator x
   = forall a. InvalidatorPull (Pull x a)
   | forall a. InvalidatorSwitch (SwitchSubscribed x a)

data RootSubscribed x a = forall k. GCompare k => RootSubscribed
  { rootSubscribedKey :: !(k a)
  , rootSubscribedCachedSubscribed :: !(Ref x (DMap k (RootSubscribed x))) -- From the original Root
  , rootSubscribedSubscribers :: !(WeakBag (Subscriber x a))
  , rootSubscribedOccurrence :: !(IO (Maybe a)) -- Lookup from rootOccurrence
  , rootSubscribedUninit :: IO ()
  , rootSubscribedWeakSelf :: !(Ref x (Weak (RootSubscribed x a))) --TODO: Can we make this a lazy non-Ref and then force it manually to avoid an indirection each time we use it?
  , rootSubscribedNodeId :: {-# UNPACK #-} !(NodeId x)
  }

data Root x k
   = Root { rootOccurrence :: !(Ref x (DMap k Identity)) -- The currently-firing occurrence of this event
          , rootSubscribed :: !(Ref x (DMap k (RootSubscribed x)))
          , rootInit :: !(forall a. k a -> RootTrigger x a -> IO (IO ()))
          , rootNodeId :: {-# UNPACK #-} !(NodeId x)
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
newtype EventM x a = EventM { unEventM :: IO a } deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadException, MonadAsyncException)

newtype MergeSubscribedParent x a = MergeSubscribedParent { unMergeSubscribedParent :: EventSubscription x }

data MergeSubscribedParentWithMove x k a = MergeSubscribedParentWithMove
  { _mergeSubscribedParentWithMove_subscription :: !(EventSubscription x)
  , _mergeSubscribedParentWithMove_key :: !(Ref x (k a))
  }

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
heightBagAdd (Height h) (HeightBag s c) = heightBagVerify $ HeightBag (succ s) $
  IntMap.insertWithKey (\_ _ old -> succ old) h 0 c

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

data FanSubscribedChildren x k v a = FanSubscribedChildren
  { _fanSubscribedChildren_list :: !(WeakBag (Subscriber x (v a)))
  , _fanSubscribedChildren_self :: {-# NOUNPACK #-} !(k a, FanSubscribed x k v)
  , _fanSubscribedChildren_weakSelf :: !(Ref x (Weak (k a, FanSubscribed x k v)))
  }

data FanSubscribed x k v
   = FanSubscribed { fanSubscribedCachedSubscribed :: !(Ref x (Maybe (FanSubscribed x k v)))
                   , fanSubscribedOccurrence :: !(Ref x (Maybe (DMap k v)))
                   , fanSubscribedSubscribers :: !(Ref x (DMap k (FanSubscribedChildren x k v))) -- This DMap should never be empty
                   , fanSubscribedParent :: !(EventSubscription x)
                   , fanSubscribedNodeId :: {-# UNPACK #-} !(NodeId x)
                   , fanSubscribedCcs :: {-# UNPACK #-} !StackInfo
                   }

data Fan x k v
   = Fan { fanParent :: !(Event x (DMap k v))
         , fanSubscribed :: !(Ref x (Maybe (FanSubscribed x k v)))
         , fanNodeId :: {-# UNPACK #-} !(NodeId x)
         , fanCcs :: {-# UNPACK #-} !StackInfo
         }

data SwitchSubscribed x a
   = SwitchSubscribed { switchSubscribedCachedSubscribed :: !(Ref x (Maybe (SwitchSubscribed x a)))
                      , switchSubscribedOccurrence :: !(Ref x (Maybe a))
                      , switchSubscribedHeight :: !(Ref x Height)
                      , switchSubscribedSubscribers :: !(WeakBag (Subscriber x a))
                      , switchSubscribedOwnInvalidator :: {-# NOUNPACK #-} !(Invalidator x)
                      , switchSubscribedOwnWeakInvalidator :: !(Ref x (Weak (Invalidator x)))
                      , switchSubscribedBehaviorParents :: !(Ref x [SomeBehaviorSubscribed x])
                      , switchSubscribedParent :: !(Behavior x (Event x a))
                      , switchSubscribedCurrentParent :: !(Ref x (EventSubscription x))
                      , switchSubscribedWeakSelf :: !(Ref x (Weak (SwitchSubscribed x a)))
                      , switchSubscribedNodeId :: {-# UNPACK #-} !(NodeId x)
                      , switchSubscribedCcs :: {-# UNPACK #-} !StackInfo
                      }

data Switch x a
   = Switch { switchParent :: !(Behavior x (Event x a))
            , switchSubscribed :: !(Ref x (Maybe (SwitchSubscribed x a)))
            , switchNodeId :: {-# UNPACK #-} !(NodeId x)
            , switchCcs :: {-# UNPACK #-} !StackInfo
            }

#ifdef USE_TEMPLATE_HASKELL
{-# ANN CoincidenceSubscribed "HLint: ignore Redundant bracket" #-}
#endif
data CoincidenceSubscribed x a
   = CoincidenceSubscribed { coincidenceSubscribedCachedSubscribed :: !(Ref x (Maybe (CoincidenceSubscribed x a)))
                           , coincidenceSubscribedOccurrence :: !(Ref x (Maybe a))
                           , coincidenceSubscribedSubscribers :: !(WeakBag (Subscriber x a))
                           , coincidenceSubscribedHeight :: !(Ref x Height)
                           , coincidenceSubscribedOuter :: {-# NOUNPACK #-} (Subscriber x (Event x a))
                           , coincidenceSubscribedOuterParent :: !(EventSubscription x)
                           , coincidenceSubscribedInnerParent :: !(Ref x (Maybe (EventSubscribed x)))
                           , coincidenceSubscribedWeakSelf :: !(Ref x (Weak (CoincidenceSubscribed x a)))
                           , coincidenceSubscribedNodeId :: {-# UNPACK #-} !(NodeId x)
                           , coincidenceSubscribedCcs :: {-# UNPACK #-} !StackInfo
                           }

data Coincidence x a
   = Coincidence { coincidenceParent :: !(Event x (Event x a))
                 , coincidenceSubscribed :: !(Ref x (Maybe (CoincidenceSubscribed x a)))
                 , coincidenceNodeId :: {-# UNPACK #-} !(NodeId x)
                 , coincidenceCcs :: {-# UNPACK #-} !StackInfo
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

newtype Dyn (x :: Type) p = Dyn { unDyn :: Ref x (DynType x p) }

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

buildDynamic :: forall x m p. (Defer (SomeDynInit x) m, Patch p, HasSpiderTimeline x) => EventM x (PatchTarget p) -> Event x p -> m (Dyn x p)
buildDynamic readV0 v' = do
  nodeId <- newNodeId @x
  result <- liftIO $ newRefI nodeId "result" $ BuildDyn (readV0, v')
  let !d = Dyn result
  defer $ SomeDynInit d
  return d

unsafeBuildDynamic :: forall x p. HasSpiderTimeline x => BehaviorM x (PatchTarget p) -> Event x p -> Dyn x p
unsafeBuildDynamic readV0 v' = Dyn $ unsafePerformIO $ do
  nodeId <- newNodeId @x
  newRefI nodeId "dyn" $ UnsafeDyn (readV0, v')

-- ResultM can read behaviors and events
type ResultM = EventM

instance HasSpiderTimeline x => Functor (Event x) where
  fmap f = push $ return . Just . f

instance HasSpiderTimeline x => Functor (Behavior x) where
  fmap f = pull . fmap f . readBehaviorTracked

{-# INLINE push #-}
push :: HasSpiderTimeline x => (a -> EventM x (Maybe b)) -> Event x a -> Event x b
push f e = cacheEvent (pushCheap f e)

{-# INLINABLE pull #-}
pull :: forall x a. HasSpiderTimeline x => BehaviorM x a -> Behavior x a
pull a = unsafePerformIO $ do
  nodeId <- newNodeId @x
  ref <- newRefI nodeId "ref" Nothing
  pure $ behaviorPull $ Pull
    { pullCompute = a
    , pullValue = ref
    , pullNodeId = nodeId
    }

{-# INLINABLE switch #-}
switch :: forall x a. HasSpiderTimeline x => Behavior x (Event x a) -> Event x a
switch a = withStackInfo a $ \stackInfo -> unsafePerformIO $ do
  nodeId <- newNodeId @x
  ref <- newRefI nodeId "ref" Nothing
  pure $ eventSwitch $ Switch
    { switchParent = a
    , switchSubscribed = ref
    , switchNodeId = nodeId
    , switchCcs = stackInfo
    }

coincidence :: forall x a. HasSpiderTimeline x => Event x (Event x a) -> Event x a
coincidence a = withStackInfo a $ \stackInfo -> unsafePerformIO $ do
  nodeId <- newNodeId @x
  ref <- newRefI nodeId "ref" Nothing
  pure $ eventCoincidence $ Coincidence
    { coincidenceParent = a
    , coincidenceSubscribed = ref
    , coincidenceNodeId = nodeId
    , coincidenceCcs = stackInfo
    }

-- Propagate the given event occurrence; before cleaning up, run the given action, which may read the state of events and behaviors
run :: forall x b. HasSpiderTimeline x => [DSum (RootTrigger x) Identity] -> ResultM x b -> SpiderHost x b
run roots after = do
  let t = spiderTimeline :: SpiderTimelineEnv x
  SpiderHost $ withMVar (_spiderTimeline_lock (unSTE t)) $ \_ -> unSpiderHost $ runFrame $ do
    rootsToPropagate <- forM roots $ \r@(RootTrigger (_, occRef, k) :=> a) -> do
      occBefore <- liftIO $ do
        occBefore <- readRef occRef
        writeRef @x occRef $! DMap.insert k a occBefore
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
              putCurrentHeight $ Height currentHeight
              liftIO $ writeIORef delayedRef $! future
              sequence_ cur
              go
    go
    putCurrentHeight maxBound
    after

scheduleMerge' :: HasSpiderTimeline x => Height -> Ref x Height -> EventM x () -> EventM x ()
scheduleMerge' initialHeight heightRef a = scheduleMerge initialHeight $ do
  height <- liftIO $ readRef heightRef
  currentHeight <- getCurrentHeight
  case height `compare` currentHeight of
    LT -> error "Somehow a merge's height has been decreased after it was scheduled"
    GT -> scheduleMerge' height heightRef a -- The height has been increased (by a coincidence event; TODO: is this the only way?)
    EQ -> a

newtype Clear x a = Clear (Ref x (Maybe a))

newtype IntClear x a = IntClear (Ref x (IntMap a))

newtype RootClear x k = RootClear (Ref x (DMap k Identity))

data SomeAssignment x = forall a. SomeAssignment {-# UNPACK #-} !(Ref x a) {-# UNPACK #-} !(Ref x [Weak (Invalidator x)]) a

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

type CanTrace x m = (HasSpiderTimeline x, MonadIO m)




#ifdef DEBUG

debugSubscriber :: forall x a. HasSpiderTimeline x => String -> Subscriber x a -> IO (Subscriber x a)
debugSubscriber description = return . debugSubscriber' description

debugSubscriber' :: forall x a. HasSpiderTimeline x => String -> Subscriber x a -> Subscriber x a
debugSubscriber' description subscribed = Subscriber
  {
    subscriberPropagate = \m -> frame @x (description <> ": subscriberPropagate") $ do
      subscriberPropagate subscribed m
  , subscriberInvalidateHeight = \old -> frame @x (description <> ": subscriberInvalidateHeight, old = " <> show (unHeight old)) $ do
      subscriberInvalidateHeight subscribed old
  , subscriberRecalculateHeight = \new -> frame @x (description <> ": subscriberRecalculateHeight, new = " <> show (unHeight new)) $ do
      subscriberRecalculateHeight subscribed new
  }

{-# INLINE trace #-}
trace :: forall x m. CanTrace x m => String -> m ()
trace message = traceM @x $ return message

{-# INLINE traceM #-}
traceM :: forall x m. CanTrace x m => m String -> m ()
traceM getMessage = do
  message <- getMessage
  (d, _) <- liftIO $ readIORef $ _spiderTimeline_stack $ unSTE (spiderTimeline :: SpiderTimelineEnv x)
  liftIO $ putStrLn $ replicate d ' ' <> message

{-# INLINE frame #-}
frame :: forall x m a. CanTrace x m => String -> m a -> m a
frame name k = do
  trace @x $ "> " <> name
  liftIO $ atomicModifyIORef' (_spiderTimeline_stack $ unSTE (spiderTimeline :: SpiderTimelineEnv x)) $ \(oldDepth, oldStack) ->
    let !newDepth = succ oldDepth
        !newStack = name : oldStack
    in ((newDepth, newStack), ())
  result <- k
  liftIO $ atomicModifyIORef' (_spiderTimeline_stack $ unSTE (spiderTimeline :: SpiderTimelineEnv x)) $ \(oldDepth, oldStack) ->
    let !newDepth = pred oldDepth
        !newStack = drop 1 oldStack
    in ((newDepth, newStack), ())
  trace @x $ "< " <> name
  pure result

#else

{-# INLINE debugSubscriber #-}
debugSubscriber :: String -> Subscriber x a -> IO (Subscriber x a)
debugSubscriber _ = return

{-# INLINE debugSubscriber' #-}
debugSubscriber' :: String -> Subscriber x a -> Subscriber x a
debugSubscriber' _ = id



{-# INLINE trace #-}
trace :: forall x m. CanTrace x m => String -> m ()
trace _ = return ()

{-# INLINE traceM #-}
traceM :: forall x m. CanTrace x m => m String -> m ()
traceM _ = return ()

#endif

indent :: MonadWriter [String] m => m a -> m a
indent = censor (fmap ("  " <>))

trieToDot :: Trie String (Map Int (Set Int)) -> RWS () [String] Int ()
trieToDot (Trie prefix leaves children) = do
  myId <- get
  put $! succ myId
  tell ["subgraph cluster_" <> show myId <> " {"]
  indent $ do
    tell ["label = " <> show (intercalate "\n" $ reverse $ toList prefix) <> ";"]
    forM_ (maybe [] Map.toList leaves) $ \(nodeId, _) -> do
      tell ["n" <> show nodeId <> " [label=" <> show (showNodeId' (NodeId nodeId)) <> "];"]
    forM_ (Map.toList children) $ \(discriminatorStackFrame, Trie childStackFrames childLeaves childChildren) -> do
      trieToDot $ Trie (discriminatorStackFrame <| childStackFrames) childLeaves childChildren
  tell ["}"]

showDot :: [([String], (Int, Set Int))] -> String
showDot nodes = unlines $ snd $ execRWS graph () 1
  where
    includedNodes = Set.fromList $ fmap (\(_, (nodeId, _)) -> nodeId) nodes
    t = Trie.fromList $ (\(stack, (nodeId, parents)) -> (Seq.fromList stack, Map.singleton nodeId $ Set.intersection includedNodes parents)) <$> filter (\(_, (nodeId, _)) -> nodeId `Set.member` includedNodes) nodes
    edges = fmap (Set.intersection includedNodes) $ Map.fromList $ fmap snd nodes
    graph = do
      tell ["digraph {"]
      indent $ do
        tell ["labelloc=b;"]
        trieToDot t
        forM_ (Map.toList edges) $ \(nodeId, parents) -> do
          when (nodeId `Set.member` includedNodes) $ do
            tell ["{" <> intercalate ";" ((\parentId -> "n" <> show parentId) <$> Set.toList (Set.intersection includedNodes parents)) <> "} -> n" <> show nodeId <> ";"]
      tell ["}"]

#ifdef DEBUG_CYCLES

getNodeInfos :: [EventSubscribed x] -> IO [([String], (Int, Set Int))]
getNodeInfos nodes = forM nodes $ \subd -> do
  stack <- whoCreatedEventSubscribed subd
  parents <- eventSubscribedGetParents subd
  pure (stack, (unNodeId $ getNodeId subd, Set.fromList $ fmap (unNodeId . getNodeId) parents))

data CausalityLoopException = CausalityLoopException [([String], (Int, Set Int))]
instance Exception CausalityLoopException

instance Show CausalityLoopException where
  show (CausalityLoopException nodes) = unlines
    [ "causality loop detected:\n"
    , if all (\(stack, _) -> null stack) nodes
      then "  no location information; enable profiling for more info"
      else showDot nodes
    ]

#else

data CausalityLoopException = CausalityLoopException
instance Exception CausalityLoopException

instance Show CausalityLoopException where
  show CausalityLoopException = unlines
    [ "causality loop detected:"
    , "  compile reflex with flag 'debug-cycles' and enable profiling for more info"
    ]

#endif


{-# INLINE propagateSubscriberHold #-}
propagateSubscriberHold :: forall x p. (HasSpiderTimeline x, Patch p) => Hold x p -> p -> EventM x ()
propagateSubscriberHold h a = do
  v <- {-# SCC "read" #-} liftIO $ readRef $ holdValue h
  case {-# SCC "apply" #-} apply a v of
    Nothing -> return ()
    Just v' -> do
      vRef <- {-# SCC "vRef" #-} liftIO $ evaluate $ holdValue h
      iRef <- {-# SCC "iRef" #-} liftIO $ evaluate $ holdInvalidators h
      defer $ {-# SCC "assignment" #-} SomeAssignment vRef iRef v'

data SomeResetCoincidence x = forall a. SomeResetCoincidence !(EventSubscription x) !(Maybe (CoincidenceSubscribed x a)) -- The CoincidenceSubscriber will be present only if heights need to be reset

runBehaviorM :: BehaviorM x a -> Maybe (Weak (Invalidator x), Ref x [SomeBehaviorSubscribed x]) -> IORef [SomeHoldInit x] -> IO a
runBehaviorM a mwi holdInits = runReaderIO (unBehaviorM a) (mwi, holdInits)

askInvalidator :: BehaviorM x (Maybe (Weak (Invalidator x)))
askInvalidator = do
  (!m, _) <- ask
  case m of
    Nothing -> return Nothing
    Just (!wi, _) -> return $ Just wi

askParentsRef :: BehaviorM x (Maybe (Ref x [SomeBehaviorSubscribed x]))
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
getDynHold :: forall x m p. (Defer (SomeHoldInit x) m, Patch p, HasSpiderTimeline x) => Dyn x p -> m (Hold x p)
getDynHold d = do
  mh <- liftIO $ readRef $ unDyn d
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
      liftIO $ writeRef @x (unDyn d) $ HoldDyn h
      return h


-- Always refers to 0
{-# NOINLINE zeroRef #-}
zeroRef :: Ref x Height
zeroRef = unsafePerformIO $ newRefN (RefName "zeroRef") zeroHeight

getRootSubscribed :: forall k x a. (GCompare k, HasSpiderTimeline x) => k a -> Root x k -> Subscriber x a -> IO (WeakBagTicket, RootSubscribed x a, Maybe a)
getRootSubscribed k r sub = do
  let nodeId = getNodeId r
  mSubscribed <- readRef $ rootSubscribed r
  let getOcc = fmap (coerce . DMap.lookup k) $ readRef $ rootOccurrence r
  case DMap.lookup k mSubscribed of
    Just subscribed -> {-# SCC "hitRoot" #-} do
      sln <- subscribeRootSubscribed subscribed sub
      occ <- getOcc
      return (sln, subscribed, occ)
    Nothing -> {-# SCC "missRoot" #-} do
      weakSelf <- newRefI nodeId "weakSelf" $ error "getRootSubscribed: weakSelfRef not initialized"
      let !cached = rootSubscribed r
      uninitRef <- newRefI nodeId "uninitRef" $ error "getRootsubscribed: uninitRef not initialized"
      (subs, sln) <- WeakBag.singleton sub (toIORef weakSelf) cleanupRootSubscribed

      uninit <- rootInit r k $ RootTrigger (subs, rootOccurrence r, k)
      writeRef @x uninitRef $! uninit
      let !subscribed = RootSubscribed
            { rootSubscribedKey = k
            , rootSubscribedCachedSubscribed = cached
            , rootSubscribedOccurrence = getOcc
            , rootSubscribedSubscribers = subs
            , rootSubscribedUninit = uninit
            , rootSubscribedWeakSelf = weakSelf
            , rootSubscribedNodeId = nodeId
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
      writeRef @x weakSelf =<< evaluate =<< mkWeakPtr subscribed (Just finalCleanup)
      modifyRef' @x (rootSubscribed r) $ DMap.insertWith (error $ "getRootSubscribed: duplicate key inserted into Root") k subscribed --TODO: I think we can just write back mSubscribed rather than re-reading it
      occ <- getOcc
      return (sln, subscribed, occ)

#ifdef USE_TEMPLATE_HASKELL
{-# ANN cleanupRootSubscribed "HLint: ignore Redundant bracket" #-}
#endif
cleanupRootSubscribed :: forall x a. HasSpiderTimeline x => RootSubscribed x a -> IO ()
cleanupRootSubscribed self@RootSubscribed { rootSubscribedKey = k, rootSubscribedCachedSubscribed = cached } = do
  rootSubscribedUninit self
  modifyRef' @x cached $ DMap.delete k

{-# INLINE subscribeRootSubscribed #-}
subscribeRootSubscribed :: HasSpiderTimeline x => RootSubscribed x a -> Subscriber x a -> IO WeakBagTicket
subscribeRootSubscribed subscribed sub = WeakBag.insert sub (rootSubscribedSubscribers subscribed) (toIORef $ rootSubscribedWeakSelf subscribed) cleanupRootSubscribed

newtype EventSelectorInt x a = EventSelectorInt { selectInt :: Int -> Event x a }

data FanInt x a = FanInt
  { _fanInt_subscribers :: {-# UNPACK #-} !(FastMutableIntMap (FastWeakBag (Subscriber x a))) --TODO: Clean up the keys in here when their child weak bags get empty --TODO: Remove our own subscription when the subscribers list is completely empty
  , _fanInt_subscriptionRef :: {-# UNPACK #-} !(Ref x (EventSubscription x)) -- This should have a valid subscription iff subscribers is non-empty
  , _fanInt_occRef :: {-# UNPACK #-} !(Ref x (IntMap a))
  , _fanInt_nodeId :: {-# UNPACK #-} !(NodeId x)
  , _fanInt_stackInfo :: {-# UNPACK #-} !StackInfo
  }

newFanInt :: forall x a. HasSpiderTimeline x => StackInfo -> IO (FanInt x a)
newFanInt stackInfo = do
  nodeId <- newNodeId @x
  subscribers <- FastMutableIntMap.newEmpty --TODO: Clean up the keys in here when their child weak bags get empty --TODO: Remove our own subscription when the subscribers list is completely empty
  subscriptionRef <- newRefI nodeId "subscriptionRef" $ error "fanInt: no subscription"
  occRef <- newRefI nodeId "occRef" $ error "fanInt: no occurrence"
  return $ FanInt
    { _fanInt_subscribers = subscribers
    , _fanInt_subscriptionRef = subscriptionRef
    , _fanInt_occRef = occRef
    , _fanInt_nodeId = nodeId
    , _fanInt_stackInfo = stackInfo
    }

fanInt :: forall x a. HasSpiderTimeline x => Event x (IntMap a) -> EventSelectorInt x a
fanInt p = withStackInfo p $ \stackInfo -> unsafePerformIO $ do
  self <- newFanInt stackInfo
  pure $ EventSelectorInt $ \k -> Event $ \sub -> do
    isEmpty <- liftIO $ FastMutableIntMap.isEmpty (_fanInt_subscribers self)
    when isEmpty $ do -- This is the first subscriber, so we need to subscribe to our input
      let desc = "fanInt" <> showNodeId self <> ", k = "  <> show k
      (subscription, parentOcc) <- subscribeAndRead p $ debugSubscriber' desc $ Subscriber
        { subscriberPropagate = \m -> do
            liftIO $ writeRef @x (_fanInt_occRef self) m
            scheduleIntClear $ _fanInt_occRef self
            FastMutableIntMap.forIntersectionWithImmutable_ (_fanInt_subscribers self) m $ \b v ->  --TODO: Do we need to know that no subscribers are being added as we traverse?
              FastWeakBag.traverse_ b $ \s ->
                subscriberPropagate s v
        , subscriberInvalidateHeight = \old ->
            FastMutableIntMap.for_ (_fanInt_subscribers self) $ \b ->
              FastWeakBag.traverse_ b $ \s ->
                subscriberInvalidateHeight s old
        , subscriberRecalculateHeight = \new ->
            FastMutableIntMap.for_ (_fanInt_subscribers self) $ \b ->
              FastWeakBag.traverse_ b $ \s ->
                subscriberRecalculateHeight s new
        }
      liftIO $ do
        writeRef @x (_fanInt_subscriptionRef self) subscription
        writeRef @x (_fanInt_occRef self) $ fromMaybe IntMap.empty parentOcc
      scheduleIntClear $ _fanInt_occRef self
    liftIO $ do
      b <- FastMutableIntMap.lookup (_fanInt_subscribers self) k >>= \case
        Nothing -> do
          b <- FastWeakBag.empty
          FastMutableIntMap.insert (_fanInt_subscribers self) k b
          return b
        Just b -> return b
      ticket <- liftIO $ FastWeakBag.insert sub b
      currentOcc <- readRef (_fanInt_occRef self)

      subscribed <- fanIntSubscribed ticket self
      return (EventSubscription (FastWeakBag.remove ticket) subscribed, IntMap.lookup k currentOcc)

fanIntSubscribed :: FastWeakBagTicket k -> FanInt x a -> IO (EventSubscribed x)
fanIntSubscribed ticket self = do
  subscribedParent <- _eventSubscription_subscribed <$> readRef (_fanInt_subscriptionRef self)
  return $ EventSubscribed
    { eventSubscribedHeightRef = eventSubscribedHeightRef subscribedParent
    , eventSubscribedRetained = toAny (_fanInt_subscriptionRef self, ticket)
#ifdef DEBUG_CYCLES
    , eventSubscribedNodeId = getNodeId self
    , eventSubscribedGetParents = return [subscribedParent]
    , eventSubscribedHasOwnHeightRef = False
    , eventSubscribedWhoCreated = stackInfoToStrings $ _fanInt_stackInfo self
#endif
    }


{-# INLINABLE getFanSubscribed #-}
getFanSubscribed :: forall x k a v. (HasSpiderTimeline x, GCompare k) => k a -> Fan x k v -> Subscriber x (v a) -> EventM x (WeakBagTicket, FanSubscribed x k v, Maybe (v a))
getFanSubscribed k f sub = do
  let nodeId = getNodeId f
  mSubscribed <- liftIO $ readRef $ fanSubscribed f
  case mSubscribed of
    Just subscribed -> {-# SCC "hitFan" #-} liftIO $ do
      sln <- subscribeFanSubscribed k subscribed sub
      occ <- readRef $ fanSubscribedOccurrence subscribed
      return (sln, subscribed, coerce $ DMap.lookup k =<< occ)
    Nothing -> {-# SCC "missFan" #-} do
      subscribedRef <- liftIO $ newRefI nodeId "subscribedRef" $ error "getFanSubscribed: subscribedRef not yet initialized"
      subscribedUnsafe <- liftIO $ unsafeInterleaveIO $ readRef subscribedRef
      s <- liftIO $ newSubscriberFan subscribedUnsafe
      (subscription, parentOcc) <- subscribeAndRead (fanParent f) s
      weakSelf <- liftIO $ newRefI nodeId "weakSelf" $ error "getFanSubscribed: weakSelf not yet initialized"
      (subsForK, slnForSub) <- liftIO $ WeakBag.singleton sub (toIORef weakSelf) cleanupFanSubscribed
      subscribersRef <- liftIO $ newRefI nodeId "subscribersRef" $ error "getFanSubscribed: subscribersRef not yet initialized"
      occRef <- liftIO $ newRefI nodeId "occRef" parentOcc
      when (isJust parentOcc) $ scheduleClear occRef
      let subscribed = FanSubscribed
            { fanSubscribedCachedSubscribed = fanSubscribed f
            , fanSubscribedOccurrence = occRef
            , fanSubscribedParent = subscription
            , fanSubscribedSubscribers = subscribersRef
            , fanSubscribedNodeId = nodeId
            , fanSubscribedCcs = fanCcs f
            }
      let !self = (k, subscribed)
      liftIO $ writeRef @x subscribersRef $! DMap.singleton k $ FanSubscribedChildren subsForK self weakSelf
      liftIO $ writeRef @x weakSelf =<< evaluate =<< mkWeakPtrWithDebug self "FanSubscribed"
      liftIO $ writeRef @x subscribedRef $! subscribed
      liftIO $ writeRef @x (fanSubscribed f) $ Just subscribed
      return (slnForSub, subscribed, coerce $ DMap.lookup k =<< parentOcc)

cleanupFanSubscribed :: forall x k v a. (HasSpiderTimeline x, GCompare k) => (k a, FanSubscribed x k v) -> IO ()
cleanupFanSubscribed (k, subscribed) = do
  subscribers <- readRef $ fanSubscribedSubscribers subscribed
  let reducedSubscribers = DMap.delete k subscribers
  if DMap.null reducedSubscribers
    then do
      unsubscribe $ fanSubscribedParent subscribed
      -- Not necessary in this case, because this whole FanSubscribed is dead: writeRef (fanSubscribedSubscribers subscribed) reducedSubscribers
      writeRef @x (fanSubscribedCachedSubscribed subscribed) Nothing
    else writeRef @x (fanSubscribedSubscribers subscribed) $! reducedSubscribers

{-# INLINE subscribeFanSubscribed #-}
subscribeFanSubscribed :: forall x k v a. (HasSpiderTimeline x, GCompare k) => k a -> FanSubscribed x k v -> Subscriber x (v a) -> IO WeakBagTicket
subscribeFanSubscribed k subscribed sub = do
  let nodeId = getNodeId subscribed
  subscribers <- readRef $ fanSubscribedSubscribers subscribed
  case DMap.lookup k subscribers of
    Nothing -> {-# SCC "missSubscribeFanSubscribed" #-} do
      let !self = (k, subscribed)
      weakSelf <- newRefI nodeId "weakSelf" =<< mkWeakPtrWithDebug self "FanSubscribed"
      (list, sln) <- WeakBag.singleton sub (toIORef weakSelf) cleanupFanSubscribed
      writeRef @x (fanSubscribedSubscribers subscribed) $! DMap.insertWith (error "subscribeFanSubscribed: key that we just failed to find is present - should be impossible") k (FanSubscribedChildren list self weakSelf) subscribers
      return sln
    Just (FanSubscribedChildren list _ weakSelf) -> {-# SCC "hitSubscribeFanSubscribed" #-} WeakBag.insert sub list (toIORef weakSelf) cleanupFanSubscribed

{-# INLINABLE getSwitchSubscribed #-}
getSwitchSubscribed :: forall x a. HasSpiderTimeline x => Switch x a -> Subscriber x a -> EventM x (WeakBagTicket, SwitchSubscribed x a, Maybe a)
getSwitchSubscribed s sub = do
  let nodeId = getNodeId s
  mSubscribed <- liftIO $ readRef $ switchSubscribed s
  case mSubscribed of
    Just subscribed -> {-# SCC "hitSwitch" #-} liftIO $ do
      sln <- subscribeSwitchSubscribed subscribed sub
      occ <- readRef $ switchSubscribedOccurrence subscribed
      return (sln, subscribed, occ)
    Nothing -> {-# SCC "missSwitch" #-} do
      subscribedRef <- liftIO $ newRefI nodeId "subscribedRef" $ error "getSwitchSubscribed: subscribed has not yet been created"
      subscribedUnsafe <- liftIO $ unsafeInterleaveIO $ readRef subscribedRef
      i <- liftIO $ newInvalidatorSwitch subscribedUnsafe
      mySub <- liftIO $ newSubscriberSwitch subscribedUnsafe
      wi <- liftIO $ mkWeakPtrWithDebug i "InvalidatorSwitch"
      wiRef <- liftIO $ newRefI nodeId "wiRef" wi
      parentsRef <- liftIO $ newRefI nodeId "parentsRef" [] --TODO: This should be unnecessary, because it will always be filled with just the single parent behavior
      holdInits <- getDeferralQueue
      e <- liftIO $ runBehaviorM (readBehaviorTracked (switchParent s)) (Just (wi, parentsRef)) holdInits
      (subscription@(EventSubscription _ subd), parentOcc) <- subscribeAndRead e mySub
      heightRef <- liftIO $ newRefI nodeId "heightRef" =<< getEventSubscribedHeight subd
      subscriptionRef <- liftIO $ newRefI nodeId "subscriptionRef" subscription
      occRef <- liftIO $ newRefI nodeId "occRef" parentOcc
      when (isJust parentOcc) $ scheduleClear occRef
      weakSelf <- liftIO $ newRefI nodeId "weakSelf" $ error "getSwitchSubscribed: weakSelf not yet initialized"
      (subs, slnForSub) <- liftIO $ WeakBag.singleton sub (toIORef weakSelf) cleanupSwitchSubscribed
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
            , switchSubscribedNodeId = nodeId
            , switchSubscribedCcs = switchCcs s
            }
      liftIO $ writeRef @x weakSelf =<< evaluate =<< mkWeakPtrWithDebug subscribed "switchSubscribedWeakSelf"
      liftIO $ writeRef @x subscribedRef $! subscribed
      liftIO $ writeRef @x (switchSubscribed s) $ Just subscribed
      return (slnForSub, subscribed, parentOcc)

cleanupSwitchSubscribed :: forall x a. HasSpiderTimeline x => SwitchSubscribed x a -> IO ()
cleanupSwitchSubscribed subscribed = do
  unsubscribe =<< readRef (switchSubscribedCurrentParent subscribed)
  finalize =<< readRef (switchSubscribedOwnWeakInvalidator subscribed) -- We don't need to get invalidated if we're dead
  writeRef @x (switchSubscribedCachedSubscribed subscribed) Nothing

{-# INLINE subscribeSwitchSubscribed #-}
subscribeSwitchSubscribed :: HasSpiderTimeline x => SwitchSubscribed x a -> Subscriber x a -> IO WeakBagTicket
subscribeSwitchSubscribed subscribed sub = WeakBag.insert sub (switchSubscribedSubscribers subscribed) (toIORef $ switchSubscribedWeakSelf subscribed) cleanupSwitchSubscribed

{-# INLINABLE getCoincidenceSubscribed #-}
getCoincidenceSubscribed :: forall x a. HasSpiderTimeline x => Coincidence x a -> Subscriber x a -> EventM x (WeakBagTicket, CoincidenceSubscribed x a, Maybe a)
getCoincidenceSubscribed c sub = do
  let nodeId = getNodeId c
  mSubscribed <- liftIO $ readRef $ coincidenceSubscribed c
  case mSubscribed of
    Just subscribed -> {-# SCC "hitCoincidence" #-} liftIO $ do
      sln <- subscribeCoincidenceSubscribed subscribed sub
      occ <- readRef $ coincidenceSubscribedOccurrence subscribed
      return (sln, subscribed, occ)
    Nothing -> {-# SCC "missCoincidence" #-} do
      subscribedRef <- liftIO $ newRefI nodeId "subscribedRef" $ error "getCoincidenceSubscribed: subscribed has not yet been created"
      subscribedUnsafe <- liftIO $ unsafeInterleaveIO $ readRef subscribedRef
      subOuter <- liftIO $ newSubscriberCoincidenceOuter subscribedUnsafe
      (outerSubscription@(EventSubscription _ outerSubd), outerOcc) <- subscribeAndRead (coincidenceParent c) subOuter
      outerHeight <- liftIO $ getEventSubscribedHeight outerSubd
      (occ, height, mInnerSubd) <- case outerOcc of
        Nothing -> return (Nothing, outerHeight, Nothing)
        Just o -> do
          (occ, height, innerSubd) <- subscribeCoincidenceInner o outerHeight subscribedUnsafe
          return (occ, height, Just innerSubd)
      occRef <- liftIO $ newRefI nodeId "occRef" occ
      when (isJust occ) $ scheduleClear occRef
      heightRef <- liftIO $ newRefI nodeId "heightRef" height
      innerSubdRef <- liftIO $ newRefI nodeId "innerSubdRef" mInnerSubd
      scheduleClear innerSubdRef
      weakSelf <- liftIO $ newRefI nodeId "weakSelf" $ error "getCoincidenceSubscribed: weakSelf not yet implemented"
      (subs, slnForSub) <- liftIO $ WeakBag.singleton sub (toIORef weakSelf) cleanupCoincidenceSubscribed
      let subscribed = CoincidenceSubscribed
            { coincidenceSubscribedCachedSubscribed = coincidenceSubscribed c
            , coincidenceSubscribedOccurrence = occRef
            , coincidenceSubscribedHeight = heightRef
            , coincidenceSubscribedSubscribers = subs
            , coincidenceSubscribedOuter = subOuter
            , coincidenceSubscribedOuterParent = outerSubscription
            , coincidenceSubscribedInnerParent = innerSubdRef
            , coincidenceSubscribedWeakSelf = weakSelf
            , coincidenceSubscribedNodeId = nodeId
            , coincidenceSubscribedCcs = coincidenceCcs c
            }
      liftIO $ writeRef @x weakSelf =<< evaluate =<< mkWeakPtrWithDebug subscribed "CoincidenceSubscribed"
      liftIO $ writeRef @x subscribedRef $! subscribed
      liftIO $ writeRef @x (coincidenceSubscribed c) $ Just subscribed
      return (slnForSub, subscribed, occ)

cleanupCoincidenceSubscribed :: forall x a. HasSpiderTimeline x => CoincidenceSubscribed x a -> IO ()
cleanupCoincidenceSubscribed subscribed = do
  unsubscribe $ coincidenceSubscribedOuterParent subscribed
  writeRef @x (coincidenceSubscribedCachedSubscribed subscribed) Nothing

{-# INLINE subscribeCoincidenceSubscribed #-}
subscribeCoincidenceSubscribed :: HasSpiderTimeline x => CoincidenceSubscribed x a -> Subscriber x a -> IO WeakBagTicket
subscribeCoincidenceSubscribed subscribed sub = WeakBag.insert sub (coincidenceSubscribedSubscribers subscribed) (toIORef $ coincidenceSubscribedWeakSelf subscribed) cleanupCoincidenceSubscribed

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
mergeCheap nt = mergeGCheap' unMergeSubscribedParent getInitialSubscribers updateMe destroy
  where
      updateMe :: MergeUpdateFunc k v x (PatchDMap k q) (MergeSubscribedParent x)
      updateMe _ subscriber heightBagRef oldParents (PatchDMap p) = do
        let f (subscriptionsToKill, ps) (k :=> ComposeMaybe me) = do
              (mOldSubd, newPs) <- case me of
                Nothing -> return $ DMap.updateLookupWithKey (\_ _ -> Nothing) k ps
                Just e -> do
                  let s = subscriber $ return k
                  subscription@(EventSubscription _ subd) <- subscribe (nt e) s
                  newParentHeight <- liftIO $ getEventSubscribedHeight subd
                  let newParent = MergeSubscribedParent subscription
                  liftIO $ modifyRef' @x heightBagRef $ heightBagAdd newParentHeight
                  return $ DMap.insertLookupWithKey' (\_ new _ -> new) k newParent ps
              forM_ mOldSubd $ \oldSubd -> do
                oldHeight <- liftIO $ getEventSubscribedHeight $
                  _eventSubscription_subscribed $ unMergeSubscribedParent oldSubd

                liftIO $ modifyRef @x heightBagRef $ heightBagRemove oldHeight
              return (maybeToList (unMergeSubscribedParent <$> mOldSubd) ++ subscriptionsToKill, newPs)
        foldM f ([], oldParents) $ DMap.toList p

      getInitialSubscribers :: MergeInitFunc k v q x (MergeSubscribedParent x)
      getInitialSubscribers _ initialParents subscriber = do
        subscribers <- forM (DMap.toList initialParents) $ \(k :=> e) -> do
          let s = subscriber $ return k
          (subscription@(EventSubscription _ parentSubd), parentOcc) <- subscribeAndRead (nt e) s
          height <- liftIO $ getEventSubscribedHeight parentSubd
          return (fmap (k :=>) parentOcc, height, k :=> MergeSubscribedParent subscription)
        return ( DMap.fromDistinctAscList $ mapMaybe (\(x, _, _) -> x) subscribers
               , fmap (\(_, h, _) -> h) subscribers --TODO: Assert that there's no invalidHeight in here
               , DMap.fromDistinctAscList $ map (\(_, _, x) -> x) subscribers
               )

      destroy :: MergeDestroyFunc k (MergeSubscribedParent x)
      destroy s = forM_ (DMap.toList s) $ \(_ :=> MergeSubscribedParent sub) -> unsubscribe sub

{-# INLINE [1] mergeCheapWithMove #-}
mergeCheapWithMove :: forall k x v q. (HasSpiderTimeline x, GCompare k)
  => (forall a. q a -> Event x (v a))
  -> DynamicS x (PatchDMapWithMove k q)
  -> Event x (DMap k v)
mergeCheapWithMove nt = mergeGCheap' _mergeSubscribedParentWithMove_subscription getInitialSubscribers updateMe destroy
  where
      updateMe :: MergeUpdateFunc k v x (PatchDMapWithMove k q) (MergeSubscribedParentWithMove x k)
      updateMe nodeId subscriber heightBagRef oldParents p = do
        -- Prepare new parents for insertion
        let subscribeParent :: forall a. k a -> Event x (v a) -> EventM x (MergeSubscribedParentWithMove x k a)
            subscribeParent k e = do
              keyRef <- liftIO $ newRefI nodeId "keyRef" k
              let s = subscriber $ liftIO $ readRef keyRef
              subscription@(EventSubscription _ subd) <- subscribe e s
              liftIO $ do
                newParentHeight <- getEventSubscribedHeight subd
                modifyRef' @x heightBagRef $ heightBagAdd newParentHeight
                return $ MergeSubscribedParentWithMove subscription keyRef
        p' <- PatchDMapWithMove.traversePatchDMapWithMoveWithKey (\k q -> subscribeParent k (nt q)) p
        -- Collect old parents for deletion and update the keys of moved parents
        let moveOrDelete :: forall a. k a -> PatchDMapWithMove.NodeInfo k q a -> MergeSubscribedParentWithMove x k a -> Constant (EventM x (Maybe (EventSubscription x))) a
            moveOrDelete _ ni parent = Constant $ case getComposeMaybe $ PatchDMapWithMove._nodeInfo_to ni of
              Nothing -> do
                oldHeight <- liftIO $ getEventSubscribedHeight $ _eventSubscription_subscribed $
                  _mergeSubscribedParentWithMove_subscription parent

                liftIO $ putStrLn $ "Merge removing old height: " <> show oldHeight
                liftIO $ modifyRef @x heightBagRef $ heightBagRemove oldHeight
                return $ Just $ _mergeSubscribedParentWithMove_subscription parent
              Just toKey -> do
                liftIO $ writeRef @x (_mergeSubscribedParentWithMove_key parent) $! toKey
                return Nothing
        toDelete <- fmap catMaybes $ mapM (\(_ :=> v) -> getConstant v) $ DMap.toList $
          DMap.intersectionWithKey moveOrDelete (unPatchDMapWithMove p) oldParents

        return (toDelete, applyAlways p' oldParents)
      getInitialSubscribers :: MergeInitFunc k v q x (MergeSubscribedParentWithMove x k)
      getInitialSubscribers nodeId initialParents subscriber = do
        subscribers <- forM (DMap.toList initialParents) $ \(k :=> e) -> do
          keyRef <- liftIO $ newRefI nodeId "keyRef" k
          let s = subscriber $ liftIO $ readRef keyRef
          (subscription@(EventSubscription _ parentSubd), parentOcc) <- subscribeAndRead (nt e) s
          height <- liftIO $ getEventSubscribedHeight parentSubd
          return (fmap (k :=>) parentOcc, height, k :=> MergeSubscribedParentWithMove subscription keyRef)
        return ( DMap.fromDistinctAscList $ mapMaybe (\(x, _, _) -> x) subscribers
               , fmap (\(_, h, _) -> h) subscribers --TODO: Assert that there's no invalidHeight in here
               , DMap.fromDistinctAscList $ map (\(_, _, x) -> x) subscribers
               )

      destroy :: MergeDestroyFunc k (MergeSubscribedParentWithMove x k)
      destroy s = forM_ (DMap.toList s) $ \(_ :=> MergeSubscribedParentWithMove sub _) -> unsubscribe sub

type MergeUpdateFunc k v x p s
   = NodeId x
  -> (forall a. EventM x (k a) -> Subscriber x (v a))
  -> Ref x HeightBag
  -> DMap k s
  -> p
  -> EventM x ([EventSubscription x], DMap k s)

type MergeGetSubscription x s = forall a. s a -> EventSubscription x

type MergeInitFunc k v q x s
   = NodeId x
  -> DMap k q
  -> (forall a. EventM x (k a) -> Subscriber x (v a))
  -> EventM x (DMap k v, [Height], DMap k s)

type MergeDestroyFunc k s
   = DMap k s
  -> IO ()

data Merge x k v s = Merge
  { _merge_parentsRef :: {-# UNPACK #-} !(Ref x (DMap k s))
  , _merge_heightBagRef :: {-# UNPACK #-} !(Ref x HeightBag)
  , _merge_heightRef :: {-# UNPACK #-} !(Ref x Height)
  , _merge_sub :: {-# UNPACK #-} !(Subscriber x (DMap k v))
  , _merge_accumRef :: {-# UNPACK #-} !(Ref x (DMap k v))
  , _merge_nodeId :: {-# UNPACK #-} !(NodeId x)
  }

invalidateMergeHeight :: HasSpiderTimeline x => Merge x k v s -> IO ()
invalidateMergeHeight m = invalidateMergeHeight' (_merge_heightRef m) (_merge_sub m)

invalidateMergeHeight' :: forall x a. HasSpiderTimeline x => Ref x Height -> Subscriber x a -> IO ()
invalidateMergeHeight' heightRef sub = do
  oldHeight <- readRef heightRef
  -- If the height used to be valid, it must be invalid now; we should never have *more* heights than we have parents
  when (oldHeight /= invalidHeight) $ do
    writeRef @x heightRef $! invalidHeight
    subscriberInvalidateHeight sub oldHeight

revalidateMergeHeight :: forall x k v s. HasSpiderTimeline x => Merge x k v s -> IO ()
revalidateMergeHeight m = do
  currentHeight <- readRef $ _merge_heightRef m

  -- revalidateMergeHeight may be called multiple times; perhaps the's a way to finesse it to avoid this check
  when (currentHeight == invalidHeight) $ do
    heights <- readRef $ _merge_heightBagRef m
    parents <- readRef $ _merge_parentsRef m
    -- When the number of heights in the bag reaches the number of parents, we should have a valid height
    case heightBagSize heights `compare` DMap.size parents of
      LT -> return ()
      EQ -> do
        let height = succHeight $ heightBagMax heights
        writeRef @x (_merge_heightRef m) $! height
        subscriberRecalculateHeight (_merge_sub m) height
      GT -> error $ "revalidateMergeHeight: more heights (" <> show (heightBagSize heights) <> ") than parents (" <> show (DMap.size parents) <> ") for Merge"

scheduleMergeSelf :: forall x k v s. HasSpiderTimeline x => Merge x k v s -> Height -> EventM x ()
scheduleMergeSelf m height = scheduleMerge' height (_merge_heightRef m) $ do
  vals <- liftIO $ readRef $ _merge_accumRef m
  -- Once we're done with this, we can clear it immediately, because if there's a cacheEvent in front of us,
  -- it'll handle subsequent subscribers, and if not, we won't get subsequent subscribers
  liftIO $ writeRef @x (_merge_accumRef m) $! DMap.empty
  --TODO: Assert that m is not empty
  subscriberPropagate (_merge_sub m) vals

checkCycle :: HasSpiderTimeline x => EventSubscribed x -> EventM x ()
checkCycle subscribed = liftIO $ do
    height <- readRef (eventSubscribedHeightRef subscribed)

    when (height == invalidHeight) $ do
#ifdef DEBUG_CYCLES
      nodesInvolvedInCycle <- walkInvalidHeightParents subscribed
      nodeInfos <- getNodeInfos nodesInvolvedInCycle
      throwIO (CausalityLoopException nodeInfos)
#else
      throwIO CausalityLoopException
#endif


mergeSubscriber :: forall x k v s a. (HasSpiderTimeline x, GCompare k) => EventSubscribed x -> Merge x k v s -> EventM x (k a) -> Subscriber x (v a)
mergeSubscriber subscribed m getKey = Subscriber
  { subscriberPropagate = \a -> do
      oldM <- liftIO $ readRef $ _merge_accumRef m
      k <- getKey
      let newM = DMap.insertWith (error "Same key fired multiple times for Merge") k a oldM
      liftIO $ writeRef @x (_merge_accumRef m) $! newM
      when (DMap.null oldM) $ do -- Only schedule the firing once
        height <- liftIO $ readRef $ _merge_heightRef m
        checkCycle subscribed

        scheduleMergeSelf m height
  , subscriberInvalidateHeight = \old -> do --TODO: When removing a parent doesn't actually change the height, maybe we can avoid invalidating
      modifyRef' @x (_merge_heightBagRef m) $ heightBagRemove old
      invalidateMergeHeight m
  , subscriberRecalculateHeight = \new -> do
      modifyRef' @x (_merge_heightBagRef m) $ heightBagAdd new
      revalidateMergeHeight m
  }

--TODO: Be able to run as much of this as possible promptly
updateMerge :: forall x k v s p. (HasSpiderTimeline x, GCompare k) => EventSubscribed x -> Merge x k v s -> MergeUpdateFunc k v x p s -> p -> SomeMergeUpdate x
updateMerge subscribed m updateFunc p = SomeMergeUpdate updateMe (invalidateMergeHeight m) (revalidateMergeHeight m)
  where updateMe = do
          oldParents <- liftIO $ readRef $ _merge_parentsRef m
          (subscriptionsToKill, newParents) <- updateFunc (getNodeId m) (mergeSubscriber subscribed m) (_merge_heightBagRef m) oldParents p
          liftIO $ writeRef @x (_merge_parentsRef m) $! newParents
          return subscriptionsToKill

{-# INLINE mergeGCheap' #-}
mergeGCheap' :: forall k v x p s q. (HasSpiderTimeline x, GCompare k, PatchTarget p ~ DMap k q)
  => MergeGetSubscription x s -> MergeInitFunc k v q x s -> MergeUpdateFunc k v x p s -> MergeDestroyFunc k s -> DynamicS x p -> Event x (DMap k v)
mergeGCheap' getParent getInitialSubscribers updateFunc destroy d = withStackInfo d $ \stackInfo -> Event $ \sub -> do --TODO: is `d` enough of a dummy here for withStackInfo?
  nodeId <- newNodeId @x
  initialParents <- readBehaviorUntracked $ dynamicCurrent d
  accumRef <- liftIO $ newRefI nodeId "accumRef" $ error "merge: accumRef not yet initialized"
  heightRef <- liftIO $ newRefI nodeId "heightRef" $ error "merge: heightRef not yet initialized"
  heightBagRef <- liftIO $ newRefI nodeId "heightBagRef" $ error "merge: heightBagRef not yet initialized"
  parentsRef :: Ref x (DMap k s) <- liftIO $ newRefI nodeId "parentsRef" $ error "merge: parentsRef not yet initialized"
  changeSubdRef <- liftIO $ newRefI nodeId "changeSubdRef" $ error "getMergeSubscribed: changeSubdRef not yet initialized"

  let subscribed = EventSubscribed
        { eventSubscribedHeightRef = heightRef
        , eventSubscribedRetained = toAny (parentsRef, changeSubdRef)
#ifdef DEBUG_CYCLES
      , eventSubscribedNodeId = nodeId
      , eventSubscribedGetParents = do
          let getParent' (_ :=> v) = _eventSubscription_subscribed (getParent v)
          fmap getParent' . DMap.toList  <$> readRef parentsRef
      , eventSubscribedHasOwnHeightRef = False
      , eventSubscribedWhoCreated = stackInfoToStrings stackInfo
#endif
      }

      m = Merge
        { _merge_parentsRef = parentsRef
        , _merge_heightBagRef = heightBagRef
        , _merge_heightRef = heightRef
        , _merge_sub = sub
        , _merge_accumRef = accumRef
        , _merge_nodeId = nodeId
        }
  (dm, heights, initialParentState) <- getInitialSubscribers nodeId initialParents $ mergeSubscriber subscribed m
  let myHeightBag = heightBagFromList $ filter (/= invalidHeight) heights
      myHeight = if invalidHeight `elem` heights
                 then invalidHeight
                 else succHeight $ heightBagMax myHeightBag
  currentHeight <- getCurrentHeight
  let (occ, accum) = if currentHeight >= myHeight -- If we should have fired by now
                     then (if DMap.null dm then Nothing else Just dm, DMap.empty)
                     else (Nothing, dm)
  unless (DMap.null accum) $ scheduleMergeSelf m myHeight
  liftIO $ writeRef @x accumRef $! accum
  liftIO $ writeRef @x heightRef $! myHeight
  liftIO $ writeRef @x heightBagRef $! myHeightBag
  liftIO $ writeRef @x parentsRef $! initialParentState
  defer $ SomeMergeInit $ do
    let changeSubscriber = Subscriber
          { subscriberPropagate = \a -> {-# SCC "traverseMergeChange" #-} do
              defer $ updateMerge subscribed m updateFunc a
          , subscriberInvalidateHeight = \_ -> return ()
          , subscriberRecalculateHeight = \_ -> return ()
          }
    (changeSubscription, change) <- subscribeAndRead (dynamicUpdated d) changeSubscriber
    forM_ change $ \c -> defer $ updateMerge subscribed m updateFunc c
    -- We explicitly hold on to the unsubscribe function from subscribing to the update event.
    -- If we don't do this, there are certain cases where mergeCheap will fail to properly retain
    -- its subscription.
    liftIO $ writeRef @x changeSubdRef (changeSubscriber, changeSubscription)
  let unsubscribeAll = destroy =<< readRef parentsRef

  return (EventSubscription unsubscribeAll subscribed, occ)


mergeInt :: forall x a. (HasSpiderTimeline x) => DynamicS x (PatchIntMap (Event x a)) -> Event x (IntMap a)
mergeInt = cacheEvent . mergeIntCheap

{-# INLINABLE mergeIntCheap #-}
mergeIntCheap :: forall x a. (HasSpiderTimeline x) => DynamicS x (PatchIntMap (Event x a)) -> Event x (IntMap a)
mergeIntCheap d = withStackInfo d $ \stackInfo -> Event $ \sub -> do
  nodeId <- newNodeId @x
  initialParents <- readBehaviorUntracked $ dynamicCurrent d
  accum <- liftIO $ FastMutableIntMap.newEmpty
  heightRef <- liftIO $ newRefI nodeId "heightRef" zeroHeight
  heightBagRef <- liftIO $ newRefI nodeId "heightBagRef" heightBagEmpty
  parents <- liftIO $ FastMutableIntMap.newEmpty
  changeSubdRef <- liftIO $ newRefI nodeId "changeSubdRef" $ error "getMergeSubscribed: changeSubdRef not yet initialized"
  let subscribed = EventSubscribed
        { eventSubscribedHeightRef = heightRef
        , eventSubscribedRetained = toAny (parents, changeSubdRef)
#ifdef DEBUG_CYCLES
        , eventSubscribedNodeId = nodeId
        , eventSubscribedGetParents = fmap (_eventSubscription_subscribed . snd) <$> FastMutableIntMap.toList parents
        , eventSubscribedHasOwnHeightRef = False
        , eventSubscribedWhoCreated = stackInfoToStrings stackInfo
#endif
        }
  let scheduleSelf = do
        height <- liftIO $ readRef $ heightRef
        scheduleMerge' height heightRef $ do
          vals <- liftIO $ FastMutableIntMap.getFrozenAndClear accum
          subscriberPropagate sub vals
      invalidateMyHeight = invalidateMergeHeight' heightRef sub
      recalculateMyHeight = do
        currentHeight <- readRef heightRef
        when (currentHeight == invalidHeight) $ do --TODO: This will almost always be true; can we get rid of this check and just proceed to the next one always?
          heights <- readRef heightBagRef
          numParents <- FastMutableIntMap.size parents
          case heightBagSize heights `compare` numParents of
            LT -> return ()
            EQ -> do
              let height = succHeight $ heightBagMax heights
              writeRef @x heightRef $! height
              subscriberRecalculateHeight sub height
            GT -> error $ "revalidateMergeHeight: more heights (" <> show (heightBagSize heights) <> ") than parents (" <> show numParents <> ") for Merge"
      mySubscriber k = Subscriber
        { subscriberPropagate = \a -> do
            checkCycle subscribed

            wasEmpty <- liftIO $ FastMutableIntMap.isEmpty accum
            liftIO $ FastMutableIntMap.insert accum k a
            when wasEmpty scheduleSelf
        , subscriberInvalidateHeight = \old -> do
            modifyRef' @x heightBagRef $ heightBagRemove old
            invalidateMyHeight
        , subscriberRecalculateHeight = \new -> do
            modifyRef' @x heightBagRef $ heightBagAdd new
            recalculateMyHeight
        }
  forM_ (IntMap.toList initialParents) $ \(k, p) -> do
    (subscription@(EventSubscription _ parentSubd), parentOcc) <- subscribeAndRead p $ mySubscriber k
    liftIO $ do
      forM_ parentOcc $ FastMutableIntMap.insert accum k
      FastMutableIntMap.insert parents k subscription
      height <- getEventSubscribedHeight parentSubd
      if height == invalidHeight
        then writeRef @x heightRef invalidHeight
        else do
          modifyRef' @x heightBagRef $ heightBagAdd height
          modifyRef' @x heightRef $ \oldHeight ->
            if oldHeight == invalidHeight
            then invalidHeight
            else max (succHeight height) oldHeight
  myHeight <- liftIO $ readRef heightRef
  currentHeight <- getCurrentHeight
  isEmpty <- liftIO $ FastMutableIntMap.isEmpty accum
  occ <- if currentHeight >= myHeight -- If we should have fired by now
    then if isEmpty
         then return Nothing
         else liftIO $ Just <$> FastMutableIntMap.getFrozenAndClear accum
    else do when (not isEmpty) scheduleSelf -- We have things accumulated, but we shouldn't have fired them yet
            return Nothing
  defer $ SomeMergeInit $ do
    let updateMe a = SomeMergeUpdate u invalidateMyHeight recalculateMyHeight
          where
            u = do
              let f k newParent = do
                    subscription@(EventSubscription _ subd) <- subscribe newParent $ mySubscriber k
                    newParentHeight <- liftIO $ getEventSubscribedHeight subd
                    liftIO $ modifyRef' @x heightBagRef $ heightBagAdd newParentHeight
                    return subscription
              newSubscriptions <- FastMutableIntMap.traverseIntMapPatchWithKey f a
              oldParents <- liftIO $ FastMutableIntMap.applyPatch parents newSubscriptions
              liftIO $ for_ oldParents $ \oldParent -> do
                oldParentHeight <- getEventSubscribedHeight $ _eventSubscription_subscribed oldParent

                print ("updateMe", oldParentHeight)
                modifyRef' @x heightBagRef $ heightBagRemove oldParentHeight
              return $ IntMap.elems oldParents
    let changeSubscriber = Subscriber
          { subscriberPropagate = \a -> {-# SCC "traverseMergeChange" #-} do
              defer $ updateMe a
          , subscriberInvalidateHeight = \_ -> return ()
          , subscriberRecalculateHeight = \_ -> return ()
          }
    (changeSubscription, change) <- subscribeAndRead (dynamicUpdated d) changeSubscriber
    forM_ change $ \c -> defer $ updateMe c
    -- We explicitly hold on to the unsubscribe function from subscribing to the update event.
    -- If we don't do this, there are certain cases where mergeCheap will fail to properly retain
    -- its subscription.
    liftIO $ writeRef @x changeSubdRef (changeSubscriber, changeSubscription)
  let unsubscribeAll = traverse_ unsubscribe =<< FastMutableIntMap.getFrozenAndClear parents


  return (EventSubscription unsubscribeAll subscribed, occ)

newtype EventSelector x k = EventSelector { select :: forall a. k a -> Event x a }
newtype EventSelectorG x k v = EventSelectorG { selectG :: forall a. k a -> Event x (v a) }

fanG :: forall x k v. (HasSpiderTimeline x, GCompare k) => Event x (DMap k v) -> EventSelectorG x k v
fanG e = withStackInfo e $ \stackInfo -> unsafePerformIO $ do
  nodeId <- newNodeId @x
  ref <- newRefI nodeId "ref" Nothing
  let f = Fan
        { fanParent = e
        , fanSubscribed = ref
        , fanNodeId = nodeId
        , fanCcs = stackInfo
        }
  pure $ EventSelectorG $ \k -> eventFan k f

runHoldInits :: forall x. HasSpiderTimeline x => IORef [SomeHoldInit x] -> IORef [SomeDynInit x] -> IORef [SomeMergeInit x] -> EventM x ()
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
  heightRef <- newRefN (RefName "heightRef") zeroHeight
  toClearRef <- newIORef []
  toClearIntRef <- newIORef []
  toClearRootRef <- newIORef []
  coincidenceInfosRef <- newIORef []
  delayedRef <- newIORef IntMap.empty
  return $ EventEnv toAssignRef holdInitRef dynInitRef mergeUpdateRef mergeInitRef toClearRef toClearIntRef toClearRootRef heightRef coincidenceInfosRef delayedRef

clearEventEnv :: forall x. HasSpiderTimeline x => EventEnv x -> IO ()
clearEventEnv (EventEnv toAssignRef holdInitRef dynInitRef mergeUpdateRef mergeInitRef toClearRef toClearIntRef toClearRootRef heightRef coincidenceInfosRef delayedRef) = do
  writeIORef toAssignRef []
  writeIORef holdInitRef []
  writeIORef dynInitRef []
  writeIORef mergeUpdateRef []
  writeIORef mergeInitRef []
  writeRef @x heightRef zeroHeight
  writeIORef toClearRef []
  writeIORef toClearIntRef []
  writeIORef toClearRootRef []
  writeIORef coincidenceInfosRef []
  writeIORef delayedRef IntMap.empty

-- | Run an event action outside of a frame
runFrame :: forall x a. HasSpiderTimeline x => EventM x a -> SpiderHost x a --TODO: This function also needs to hold the mutex
runFrame a = SpiderHost $ do
  let env = _spiderTimeline_eventEnv $ unSTE (spiderTimeline :: SpiderTimelineEnv x)
  let go = do
        result <- a
        runHoldInits (eventEnvHoldInits env) (eventEnvDynInits env) (eventEnvMergeInits env) -- This must happen before doing the assignments, in case subscribing a Hold causes existing Holds to be read by the newly-propagated events
        return result
  result <- runEventM go
  frame @x "Clearing Maybes" $ do
    toClear <- readIORef $ eventEnvClears env
    forM_ toClear $ \(Some (Clear ref)) -> {-# SCC "clear" #-} writeRef @x ref Nothing
  frame @x "Clearing IntMaps" $ do
    toClearInt <- readIORef $ eventEnvIntClears env
    forM_ toClearInt $ \(Some (IntClear ref)) -> {-# SCC "intClear" #-} writeRef @x ref $! IntMap.empty
  frame @x "Clearing DMaps" $ do
    toClearRoot <- readIORef $ eventEnvRootClears env
    forM_ toClearRoot $ \(Some (RootClear ref)) -> {-# SCC "rootClear" #-} writeRef @x ref $! DMap.empty
  toAssign <- readIORef $ eventEnvAssignments env
  toReconnectRef <- newIORef []
  coincidenceInfos <- readIORef $ eventEnvResetCoincidences env --TODO: Why do we read this up here?
  frame @x "Invalidating Holds" $ do
    forM_ toAssign $ \(SomeAssignment vRef iRef v) -> {-# SCC "assignment" #-} do
      writeRef @x vRef v
      writeRef @x iRef =<< evaluate =<< invalidate toReconnectRef =<< readRef iRef
  mergeUpdates <- readIORef $ eventEnvMergeUpdates env
  mergeSubscriptionsToKill <- frame @x "Updating merges" $ do
    writeIORef (eventEnvMergeUpdates env) []
    runEventM $ concat <$> mapM _someMergeUpdate_update mergeUpdates
  toReconnect <- readIORef toReconnectRef
  clearEventEnv env
  switchSubscriptionsToKill <- frame @x "Switching Switches" $ forM toReconnect $ \(SomeSwitchSubscribed subscribed) -> {-# SCC "switchSubscribed" #-} do
    oldSubscription <- readRef $ switchSubscribedCurrentParent subscribed
    wi <- readRef $ switchSubscribedOwnWeakInvalidator subscribed
    trace @x $ "Finalizing invalidator for Switch" <> showNodeId subscribed
    finalize wi
    i <- evaluate $ switchSubscribedOwnInvalidator subscribed
    wi' <- mkWeakPtrWithDebug i "wi'"
    writeRef @x (switchSubscribedOwnWeakInvalidator subscribed) $! wi'
    writeRef @x (switchSubscribedBehaviorParents subscribed) []
    writeIORef (eventEnvHoldInits env) [] --TODO: Should we reuse this?
    e <- runBehaviorM (readBehaviorTracked (switchSubscribedParent subscribed)) (Just (wi', switchSubscribedBehaviorParents subscribed)) $ eventEnvHoldInits env
    runEventM $ runHoldInits (eventEnvHoldInits env) (eventEnvDynInits env) (eventEnvMergeInits env) --TODO: Is this actually OK? It seems like it should be, since we know that no events are firing at this point, but it still seems inelegant
    --TODO: Make sure we touch the pieces of the SwitchSubscribed at the appropriate times
    sub <- newSubscriberSwitch subscribed
    subscription <- unSpiderHost $ runFrame $ {-# SCC "subscribeSwitch" #-} subscribe e sub --TODO: Assert that the event isn't firing --TODO: This should not loop because none of the events should be firing, but still, it is inefficient
    writeRef @x (switchSubscribedCurrentParent subscribed) $! subscription
    return oldSubscription
  frame @x "Unsubscribing Merges" $ do
    liftIO $ mapM_ unsubscribe mergeSubscriptionsToKill
  frame @x "Unsubscribing Switches" $ do
    liftIO $ mapM_ unsubscribe switchSubscriptionsToKill
  forM_ toReconnect $ \(SomeSwitchSubscribed subscribed) -> {-# SCC "switchSubscribed" #-} do
    EventSubscription _ subd' <- readRef $ switchSubscribedCurrentParent subscribed
    parentHeight <- getEventSubscribedHeight subd'
    myHeight <- readRef $ switchSubscribedHeight subscribed
    when (parentHeight /= myHeight) $ do
      writeRef @x (switchSubscribedHeight subscribed) $! invalidHeight
      WeakBag.traverse_ (switchSubscribedSubscribers subscribed) $ invalidateSubscriberHeight myHeight
  frame @x "Invalidating Merges" $ do
    mapM_ _someMergeUpdate_invalidateHeight mergeUpdates --TODO: In addition to when the patch is completely empty, we should also not run this if it has some Nothing values, but none of them have actually had any effect; potentially, we could even check for Just values with no effect (e.g. by comparing their Refs and ignoring them if they are unchanged); actually, we could just check if the new height is different
  frame @x "Unsubscribing and Invalidating Coincidences" $ do
    forM_ coincidenceInfos $ \(SomeResetCoincidence subscription mcs) -> do
      unsubscribe subscription
      mapM_ invalidateCoincidenceHeight mcs
  frame @x "Recalculating Coincidences" $ do
    forM_ coincidenceInfos $ \(SomeResetCoincidence _ mcs) -> mapM_ recalculateCoincidenceHeight mcs
  frame @x "Recalculating Merges" $ do
    mapM_ _someMergeUpdate_recalculateHeight mergeUpdates
  frame @x "Reconnecting Switches" $ do
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

invalidateCoincidenceHeight :: forall x a. HasSpiderTimeline x => CoincidenceSubscribed x a -> IO ()
invalidateCoincidenceHeight subscribed = do
  oldHeight <- readRef $ coincidenceSubscribedHeight subscribed
  when (oldHeight /= invalidHeight) $ do
    writeRef @x (coincidenceSubscribedHeight subscribed) $! invalidHeight
    WeakBag.traverse_ (coincidenceSubscribedSubscribers subscribed) $ invalidateSubscriberHeight oldHeight

updateSwitchHeight :: forall x a. HasSpiderTimeline x => Height -> SwitchSubscribed x a -> IO ()
updateSwitchHeight new subscribed = do
  oldHeight <- readRef $ switchSubscribedHeight subscribed
  when (oldHeight == invalidHeight) $ do --TODO: This 'when' should probably be an assertion
    when (new /= invalidHeight) $ do --TODO: This 'when' should probably be an assertion
      writeRef @x (switchSubscribedHeight subscribed) $! new
      WeakBag.traverse_ (switchSubscribedSubscribers subscribed) $ recalculateSubscriberHeight new

recalculateCoincidenceHeight :: forall x a. HasSpiderTimeline x => CoincidenceSubscribed x a -> IO ()
recalculateCoincidenceHeight subscribed = do
  oldHeight <- readRef $ coincidenceSubscribedHeight subscribed
  when (oldHeight == invalidHeight) $ do --TODO: This 'when' should probably be an assertion
    height <- calculateCoincidenceHeight subscribed
    when (height /= invalidHeight) $ do
      writeRef @x (coincidenceSubscribedHeight subscribed) $! height
      WeakBag.traverse_ (coincidenceSubscribedSubscribers subscribed) $ recalculateSubscriberHeight height

calculateSwitchHeight :: SwitchSubscribed x a -> IO Height
calculateSwitchHeight subscribed = getEventSubscribedHeight . _eventSubscription_subscribed =<< readRef (switchSubscribedCurrentParent subscribed)

calculateCoincidenceHeight :: CoincidenceSubscribed x a -> IO Height
calculateCoincidenceHeight subscribed = do
  outerHeight <- getEventSubscribedHeight $ _eventSubscription_subscribed $ coincidenceSubscribedOuterParent subscribed
  innerHeight <- maybe (return zeroHeight) getEventSubscribedHeight =<< readRef (coincidenceSubscribedInnerParent subscribed)
  return $ if outerHeight == invalidHeight || innerHeight == invalidHeight then invalidHeight else max outerHeight innerHeight

data SomeSwitchSubscribed x = forall a. SomeSwitchSubscribed {-# NOUNPACK #-} (SwitchSubscribed x a)

invalidate :: forall x. HasSpiderTimeline x => IORef [SomeSwitchSubscribed x] -> WeakList (Invalidator x) -> IO (WeakList (Invalidator x))
invalidate toReconnectRef wis = do
  forM_ wis $ \wi -> do
    mi <- deRefWeak wi
    case mi of
      Nothing -> do
        return () --TODO: Should we clean this up here?
      Just i -> do
        finalize wi -- Once something's invalidated, it doesn't need to hang around; this will change when some things are strict
        case i of
          InvalidatorPull p -> do
            mVal <- readRef $ pullValue p
            forM_ mVal $ \val -> do
              writeRef @x (pullValue p) Nothing
              writeRef @x (pullSubscribedInvalidators val) =<< evaluate =<< invalidate toReconnectRef =<< readRef (pullSubscribedInvalidators val)
          InvalidatorSwitch subscribed -> do
            modifyIORef' toReconnectRef (SomeSwitchSubscribed subscribed :)
  return [] -- Since we always finalize everything, always return an empty list --TODO: There are some things that will need to be re-subscribed every time; we should try to avoid finalizing them

--------------------------------------------------------------------------------
-- Reflex integration
--------------------------------------------------------------------------------

-- | Designates the default, global Spider timeline
data SpiderTimeline x
type role SpiderTimeline nominal

-- | The default, global Spider environment
type Spider = SpiderTimeline Global

instance HasSpiderTimeline x => Reflex.Class.MonadSample (SpiderTimeline x) (EventM x) where
  {-# INLINABLE sample #-}
  sample (SpiderBehavior b) = readBehaviorUntracked b

instance HasSpiderTimeline x => Reflex.Class.MonadHold (SpiderTimeline x) (EventM x) where
  {-# INLINABLE hold #-}
  hold = holdSpiderEventM
  {-# INLINABLE holdDyn #-}
  holdDyn = holdDynSpiderEventM
  {-# INLINABLE holdIncremental #-}
  holdIncremental = holdIncrementalSpiderEventM
  {-# INLINABLE buildDynamic #-}
  buildDynamic = buildDynamicSpiderEventM
  {-# INLINABLE headE #-}
  headE = R.slowHeadE
--  headE (SpiderEvent e) = SpiderEvent <$> Reflex.Spider.Internal.headE e
  {-# INLINABLE now #-}
  now = nowSpiderEventM

instance Reflex.Class.MonadSample (SpiderTimeline x) (SpiderPullM x) where
  {-# INLINABLE sample #-}
  sample = coerce . readBehaviorTracked . unSpiderBehavior

instance HasSpiderTimeline x => Reflex.Class.MonadSample (SpiderTimeline x) (SpiderPushM x) where
  {-# INLINABLE sample #-}
  sample (SpiderBehavior b) = SpiderPushM $ readBehaviorUntracked b

instance HasSpiderTimeline x => Reflex.Class.MonadHold (SpiderTimeline x) (SpiderPushM x) where
  {-# INLINABLE hold #-}
  hold v0 e = Reflex.Class.current <$> Reflex.Class.holdDyn v0 e
  {-# INLINABLE holdDyn #-}
  holdDyn v0 (SpiderEvent e) = SpiderPushM $ fmap (SpiderDynamic . dynamicHoldIdentity) $ Reflex.Spider.Internal.hold v0 $ coerce e
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 (SpiderEvent e) = SpiderPushM $ SpiderIncremental . dynamicHold <$> Reflex.Spider.Internal.hold v0 e
  {-# INLINABLE buildDynamic #-}
  buildDynamic getV0 (SpiderEvent e) = SpiderPushM $ fmap (SpiderDynamic . dynamicDynIdentity) $ Reflex.Spider.Internal.buildDynamic (coerce getV0) $ coerce e
  {-# INLINABLE headE #-}
  headE = R.slowHeadE
--  headE (SpiderEvent e) = SpiderPushM $ SpiderEvent <$> Reflex.Spider.Internal.headE e
  {-# INLINABLE now #-}
  now = SpiderPushM nowSpiderEventM


instance HasSpiderTimeline x => Monad (Reflex.Class.Dynamic (SpiderTimeline x)) where
  {-# INLINE return #-}
  return = pure
  {-# INLINE (>>=) #-}
  x >>= f = SpiderDynamic $ dynamicDynIdentity $ newJoinDyn $ newMapDyn (unSpiderDynamic . f) $ unSpiderDynamic x
  {-# INLINE (>>) #-}
  (>>) = (*>)
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
#if MIN_VERSION_base(4,10,0)
  liftA2 f a b = SpiderDynamic $ Reflex.Spider.Internal.zipDynWith f (unSpiderDynamic a) (unSpiderDynamic b)
#endif
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

instance HasSpiderTimeline x => Reflex.Class.MonadHold (SpiderTimeline x) (SpiderHost x) where
  {-# INLINABLE hold #-}
  hold v0 e = runFrame . runSpiderHostFrame $ Reflex.Class.hold v0 e
  {-# INLINABLE holdDyn #-}
  holdDyn v0 e = runFrame . runSpiderHostFrame $ Reflex.Class.holdDyn v0 e
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 e = runFrame . runSpiderHostFrame $ Reflex.Class.holdIncremental v0 e
  {-# INLINABLE buildDynamic #-}
  buildDynamic getV0 e = runFrame . runSpiderHostFrame $ Reflex.Class.buildDynamic getV0 e
  {-# INLINABLE headE #-}
  headE e = runFrame . runSpiderHostFrame $ Reflex.Class.headE e
  {-# INLINABLE now #-}
  now = runFrame . runSpiderHostFrame $ Reflex.Class.now
  

instance HasSpiderTimeline x => Reflex.Class.MonadSample (SpiderTimeline x) (SpiderHostFrame x) where
  sample = SpiderHostFrame . readBehaviorUntracked . unSpiderBehavior --TODO: This can cause problems with laziness, so we should get rid of it if we can

instance HasSpiderTimeline x => Reflex.Class.MonadHold (SpiderTimeline x) (SpiderHostFrame x) where
  {-# INLINABLE hold #-}
  hold v0 e = SpiderHostFrame $ fmap (SpiderBehavior . behaviorHoldIdentity) $ Reflex.Spider.Internal.hold v0 $ coerce $ unSpiderEvent e
  {-# INLINABLE holdDyn #-}
  holdDyn v0 e = SpiderHostFrame $ fmap (SpiderDynamic . dynamicHoldIdentity) $ Reflex.Spider.Internal.hold v0 $ coerce $ unSpiderEvent e
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 e = SpiderHostFrame $ fmap (SpiderIncremental . dynamicHold) $ Reflex.Spider.Internal.hold v0 $ unSpiderEvent e
  {-# INLINABLE buildDynamic #-}
  buildDynamic getV0 e = SpiderHostFrame $ fmap (SpiderDynamic . dynamicDynIdentity) $ Reflex.Spider.Internal.buildDynamic (coerce getV0) $ coerce $ unSpiderEvent e
  {-# INLINABLE headE #-}
  headE = R.slowHeadE
--  headE (SpiderEvent e) = SpiderHostFrame $ SpiderEvent <$> Reflex.Spider.Internal.headE e
  {-# INLINABLE now #-}
  now = SpiderHostFrame Reflex.Class.now

instance HasSpiderTimeline x => Reflex.Class.MonadSample (SpiderTimeline x) (SpiderHost x) where
  {-# INLINABLE sample #-}
  sample = runFrame . readBehaviorUntracked . unSpiderBehavior

instance HasSpiderTimeline x => Reflex.Class.MonadSample (SpiderTimeline x) (Reflex.Spider.Internal.ReadPhase x) where
  {-# INLINABLE sample #-}
  sample = Reflex.Spider.Internal.ReadPhase . Reflex.Class.sample

instance HasSpiderTimeline x => Reflex.Class.MonadHold (SpiderTimeline x) (Reflex.Spider.Internal.ReadPhase x) where
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
    nodeId <- newNodeId @x
    val <- liftIO $ newRefI nodeId "val" Nothing
    subscription <- subscribe (unSpiderEvent e) $ Subscriber
      { subscriberPropagate = \a -> do
          liftIO $ writeRef @x val $ Just a
          scheduleClear val
      , subscriberInvalidateHeight = \_ -> return ()
      , subscriberRecalculateHeight = \_ -> return ()
      }
    return $ SpiderEventHandle
      { spiderEventHandleSubscription = subscription
      , spiderEventHandleValue = val
      }

instance HasSpiderTimeline x => Reflex.Host.Class.ReflexHost (SpiderTimeline x) where
  type EventTrigger (SpiderTimeline x) = RootTrigger x
  type EventHandle (SpiderTimeline x) = SpiderEventHandle x
  type HostFrame (SpiderTimeline x) = SpiderHostFrame x

instance HasSpiderTimeline x => Reflex.Host.Class.MonadReadEvent (SpiderTimeline x) (Reflex.Spider.Internal.ReadPhase x) where
  {-# NOINLINE readEvent #-}
  readEvent h = Reflex.Spider.Internal.ReadPhase $ fmap (fmap return) $ liftIO $ do
    result <- readRef $ spiderEventHandleValue h
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

instance HasSpiderTimeline x => Reflex.Host.Class.MonadSubscribeEvent (SpiderTimeline x) (SpiderHost x) where
  {-# INLINABLE subscribeEvent #-}
  subscribeEvent = runFrame . runSpiderHostFrame . Reflex.Host.Class.subscribeEvent

instance HasSpiderTimeline x => Reflex.Host.Class.MonadReflexHost (SpiderTimeline x) (SpiderHost x) where
  type ReadPhase (SpiderHost x) = Reflex.Spider.Internal.ReadPhase x
  fireEventsAndRead es (Reflex.Spider.Internal.ReadPhase a) = run es a
  runHostFrame = runFrame . runSpiderHostFrame

unsafeNewSpiderTimelineEnv :: forall x. IO (SpiderTimelineEnv x)
unsafeNewSpiderTimelineEnv = do
  lock <- newMVar ()
  env <- newEventEnv
  nodeIdAllocator <- newNodeIdAllocator
#ifdef DEBUG
  stackRef <- newIORef (0, [])
#endif
  return $ STE $ SpiderTimelineEnv
    { _spiderTimeline_lock = lock
    , _spiderTimeline_eventEnv = env
#ifdef DEBUG
    , _spiderTimeline_stack = stackRef
    , _spiderTimeline_nodeIdAllocator = nodeIdAllocator
#endif
    }

instance HasSpiderTimeline x => HasNodeIds x where
  getNodeIdAllocator = _spiderTimeline_nodeIdAllocator $ unSTE (spiderTimeline :: SpiderTimelineEnv x)

instance HasSpiderTimeline x => RefCtx x where
  newtype RefName x = RefName String
  traceRef (RefName name) action = trace @x $ show action <> " " <> name

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
localSpiderTimeline _ = unsafeCoerce

-- | Pass a new timeline to the given function.
withSpiderTimeline :: (forall x. HasSpiderTimeline x => SpiderTimelineEnv x -> IO r) -> IO r
withSpiderTimeline k = do
  env <- unsafeNewSpiderTimelineEnv
  reify env $ \s -> k $ localSpiderTimeline s env

newtype SpiderPullM (x :: Type) a = SpiderPullM (BehaviorM x a) deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

newtype SpiderPushM (x :: Type) a = SpiderPushM (EventM x a) deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

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

data RootTrigger x a = forall k. GCompare k => RootTrigger (WeakBag (Subscriber x a), Ref x (DMap k Identity), k a)

data SpiderEventHandle x a = SpiderEventHandle
  { spiderEventHandleSubscription :: EventSubscription x
  , spiderEventHandleValue :: Ref x (Maybe a)
  }

instance MonadRef (EventM x) where
  type Ref (EventM x) = MonadRef.Ref IO
  {-# INLINABLE newRef #-}
  {-# INLINABLE readRef #-}
  {-# INLINABLE writeRef #-}
  newRef = liftIO . MonadRef.newRef
  readRef = liftIO . MonadRef.readRef
  writeRef r a = liftIO $ MonadRef.writeRef r a

instance MonadAtomicRef (EventM x) where
  {-# INLINABLE atomicModifyRef #-}
  atomicModifyRef r f = liftIO $ MonadRef.atomicModifyRef r f

-- | The monad for actions that manipulate a Spider timeline identified by @x@
newtype SpiderHost (x :: Type) a = SpiderHost { unSpiderHost :: IO a } deriving (Functor, Applicative, MonadFix, MonadIO, MonadException, MonadAsyncException)

instance Monad (SpiderHost x) where
  {-# INLINABLE (>>=) #-}
  SpiderHost x >>= f = SpiderHost $ x >>= unSpiderHost . f
  {-# INLINABLE (>>) #-}
  SpiderHost x >> SpiderHost y = SpiderHost $ x >> y
  {-# INLINABLE return #-}
  return x = SpiderHost $ return x
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
  deriving (Functor, Applicative, MonadFix, MonadIO, MonadException, MonadAsyncException)

instance Monad (SpiderHostFrame x) where
  {-# INLINABLE (>>=) #-}
  SpiderHostFrame x >>= f = SpiderHostFrame $ x >>= runSpiderHostFrame . f
  {-# INLINABLE (>>) #-}
  SpiderHostFrame x >> SpiderHostFrame y = SpiderHostFrame $ x >> y
  {-# INLINABLE return #-}
  return x = SpiderHostFrame $ return x
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

newFanEventWithTriggerIO :: forall x k. (HasSpiderTimeline x, GCompare k) => (forall a. k a -> RootTrigger x a -> IO (IO ())) -> IO (EventSelector x k)
newFanEventWithTriggerIO f = do
  nodeId <- newNodeId @x
  occRef <- newRefI nodeId "occRef" DMap.empty
  subscribedRef <- newRefI nodeId "subscribedRef" DMap.empty
  let !r = Root
        { rootOccurrence = occRef
        , rootSubscribed = subscribedRef
        , rootInit = f
        , rootNodeId = nodeId
        }
  return $ EventSelector $ \k -> eventRoot k r

newtype ReadPhase x a = ReadPhase (ResultM x a) deriving (Functor, Applicative, Monad, MonadFix)

instance MonadRef (SpiderHost x) where
  type Ref (SpiderHost x) = MonadRef.Ref IO
  newRef = SpiderHost . MonadRef.newRef
  readRef = SpiderHost . MonadRef.readRef
  writeRef r = SpiderHost . MonadRef.writeRef r

instance MonadAtomicRef (SpiderHost x) where
  atomicModifyRef r = SpiderHost . MonadRef.atomicModifyRef r

instance MonadRef (SpiderHostFrame x) where
  type Ref (SpiderHostFrame x) = MonadRef.Ref IO
  newRef = SpiderHostFrame . MonadRef.newRef
  readRef = SpiderHostFrame . MonadRef.readRef
  writeRef r = SpiderHostFrame . MonadRef.writeRef r

instance MonadAtomicRef (SpiderHostFrame x) where
  atomicModifyRef r = SpiderHostFrame . MonadRef.atomicModifyRef r

instance PrimMonad (SpiderHostFrame x) where
  type PrimState (SpiderHostFrame x) = PrimState IO
  primitive = SpiderHostFrame . EventM . primitive

instance NotReady (SpiderTimeline x) (SpiderHost x) where
  notReadyUntil _ = return ()
  notReady = return ()

instance HasSpiderTimeline x => NotReady (SpiderTimeline x) (PerformEventT (SpiderTimeline x) (SpiderHost x)) where
  notReadyUntil _ = return ()
  notReady = return ()
