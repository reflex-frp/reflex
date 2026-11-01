{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
#if MIN_VERSION_base(4,13,0)
import GHC.IORef (atomicModifyIORef'_)
#endif
import GHC.Stack
import System.IO.Unsafe
import System.Mem.Weak
import Unsafe.Coerce
import Witherable (Filterable (..), mapMaybe)

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
import Data.WeakBag (WeakBag)
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

instance HasNodeId (FanSubscribed x map v) where
  getNodeId = fanSubscribedNodeId

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

-- Why do we use Any here, instead of just giving _eventSubscription_retained an
-- existential type? Sadly, GHC does not currently know how to unbox types
-- with existentially quantified fields. So instead we just coerce values
-- to type Any on the way in. Since we never coerce them back, this is
-- perfectly safe.
data EventSubscription x = EventSubscription
  { _eventSubscription_unsubscribe :: !(IO ())
  , _eventSubscription_retained :: {-# NOUNPACK #-} !Any
  , _eventSubscription_node :: {-# NOUNPACK #-} !(EventNode x)
  }

unsubscribe :: EventSubscription x -> IO ()
unsubscribe (EventSubscription u _ _) = u

--------------------------------------------------------------------------------
-- Event
--------------------------------------------------------------------------------

newtype Event x a = Event { subscribeAndRead :: Subscriber x a -> EventM x (SubscribeResult x a) }

data SubscribeResult x a = SubscribeResult
  { subscribeSubscription :: EventSubscription x
  , subscribeOccurrence :: Maybe a
  }


-- Rewrite rules for cached events
--
-- Before phase 1, the "expand" rules rewrite each cached primitive to
-- cacheEvent over its uncached event, and the other rules simplify the result.
-- From phase 1, the "contract" rules turn each remaining cacheEvent over an
-- uncached event back into the cached primitive, or into a fused primitive such
-- as pushSwitch when there is a pushCheap in between. cacheEvent, the uncached
-- events, the cached primitives and holdBehavior are not inlined before
-- phase 0, so that the rules can match them. fanG, fanInt and headE are only
-- in rules that are active before phase 1, so they are not inlined before
-- phase 1.
{-# RULES
"push/expand" [~1] forall f e. push f e = cacheEvent (pushCheap f e)
"switch/expand" [~1] forall b. switch b = cacheEvent (switchUncached b)
"coincidence/expand" [~1] forall e. coincidence e = cacheEvent (coincidenceUncached e)
"mergeInt/expand" [~1] forall d. mergeInt d = cacheEvent (mergeIntCheap d)
"mergeG/expand" [~1] forall (nt :: forall a. q a -> Event x (v a)) (d :: DynamicS x (PatchDMap k q)). mergeG nt d = cacheEvent (mergeCheap nt d)
"mergeWithMove/expand" [~1] forall (nt :: forall a. q a -> Event x (v a)) (d :: DynamicS x (PatchDMapWithMove k q)). mergeWithMove nt d = cacheEvent (mergeCheapWithMove nt d)

"cacheEvent/cacheEvent" [~1] forall e. cacheEvent (cacheEvent e) = cacheEvent e
"pushCheap/cacheEvent" [~1] forall f e. pushCheap f (cacheEvent e) = cacheEvent (pushCheap f e)
"pushCheap/pushCheap" [~1] forall f g e. pushCheap f (pushCheap g e) = pushCheap (composePush f g) e
"coincidenceUncached/cacheEvent" [~1] forall e. cacheEvent (coincidenceUncached (cacheEvent e)) = cacheEvent (coincidenceUncached e)
"pushCheap/coincidenceUncached/cacheEvent" [~1] forall f e. cacheEvent (pushCheap f (coincidenceUncached (cacheEvent e))) = cacheEvent (pushCheap f (coincidenceUncached e))
"fanG/cacheEvent" [~1] forall e. fanG (cacheEvent e) = fanG e
"fanInt/cacheEvent" [~1] forall e. fanInt (cacheEvent e) = fanInt e
"headE/cacheEvent" [~1] forall e. headE (cacheEvent e) = headE e
"holdBehavior/cacheEvent" [~1] forall v e. holdBehavior v (cacheEvent e) = holdBehavior v e

"push/contract" [1] forall f e. cacheEvent (pushCheap f e) = push f e
"switch/contract" [1] forall b. cacheEvent (switchUncached b) = switch b
"coincidence/contract" [1] forall e. cacheEvent (coincidenceUncached e) = coincidence e
"mergeInt/contract" [1] forall d. cacheEvent (mergeIntCheap d) = mergeInt d
"mergeG/contract" [1] forall (nt :: forall a. q a -> Event x (v a)) (d :: DynamicS x (PatchDMap k q)). cacheEvent (mergeCheap nt d) = mergeG nt d
"mergeWithMove/contract" [1] forall (nt :: forall a. q a -> Event x (v a)) (d :: DynamicS x (PatchDMapWithMove k q)). cacheEvent (mergeCheapWithMove nt d) = mergeWithMove nt d

"pushSwitch/contract" [1] forall f b. cacheEvent (pushCheap f (switchUncached b)) = pushSwitch f b
"pushCoincidence/contract" [1] forall f e. cacheEvent (pushCheap f (coincidenceUncached e)) = pushCoincidence f e
"pushMergeInt/contract" [1] forall f d. cacheEvent (pushCheap f (mergeIntCheap d)) = pushMergeInt f d
"pushMergeG/contract" [1] forall (f :: DMap k v -> EventM x (Maybe c)) (nt :: forall a. q a -> Event x (v a)) (d :: DynamicS x (PatchDMap k q)). cacheEvent (pushCheap f (mergeCheap nt d)) = pushMergeG f nt d
"pushMergeWithMove/contract" [1] forall (f :: DMap k v -> EventM x (Maybe c)) (nt :: forall a. q a -> Event x (v a)) (d :: DynamicS x (PatchDMapWithMove k q)). cacheEvent (pushCheap f (mergeCheapWithMove nt d)) = pushMergeWithMove f nt d
  #-}

-- | Used by the rule "pushCheap/pushCheap".
{-# INLINE composePush #-}
composePush :: (b -> EventM x (Maybe c)) -> (a -> EventM x (Maybe b)) -> a -> EventM x (Maybe c)
composePush f g a = g a >>= maybe (return Nothing) f

-- | Construct an 'Event' equivalent to that constructed by 'push', but with no
-- caching; if the computation function is very cheap, this is (much) more
-- efficient than 'push'
{-# INLINE [0] pushCheap #-}
pushCheap :: forall (x :: Type) a b. HasSpiderTimeline x => (a -> EventM x (Maybe b)) -> Event x a -> Event x b
pushCheap = pushCheapImpl

{-# INLINE pushCheapImpl #-}
pushCheapImpl :: forall (x :: Type) a b. HasSpiderTimeline x => (a -> EventM x (Maybe b)) -> Event x a -> Event x b
pushCheapImpl !f e = Event $ \sub -> do
  SubscribeResult subscription occ <- subscribeAndRead e $ debugSubscriber "push" $ sub
    { subscriberPropagate = \a -> do
        mb <- f a
        mapM_ (subscriberPropagate sub) mb
    }
  SubscribeResult subscription <$> maybe (return Nothing) f occ

-- | A subscriber that never triggers other 'Event's
{-# INLINE terminalSubscriber #-}
terminalSubscriber :: (a -> EventM x ()) -> Subscriber x a
terminalSubscriber p = Subscriber
  { subscriberPropagate = p
  , subscriberInvalidateHeight = \_ -> pure ()
  , subscriberRecalculateHeight = \_ -> pure ()
  }

{-# INLINE ownHeightSubscriber #-}
ownHeightSubscriber :: IORef Height -> (Height -> IO ()) -> (Height -> IO ()) -> (a -> EventM x ()) -> Subscriber x a
ownHeightSubscriber heightRef invalidateSubscribers recalculateSubscribers propagateOccurrence = Subscriber
  { subscriberPropagate = propagateOccurrence
  , subscriberInvalidateHeight = \_ -> invalidateOwnHeight heightRef invalidateSubscribers
  , subscriberRecalculateHeight = recalculateOwnHeight heightRef recalculateSubscribers . pure
  }

data HeadESubscribed x a = HeadESubscribed
  { headESubscribedSubscribers :: {-# UNPACK #-} !(WeakBag (Subscriber x a))
  , headESubscribedParent :: !(EventSubscription x)
  , headESubscribedOccurrence :: {-# UNPACK #-} !(IORef (Maybe a))
  , headESubscribedNode :: {-# NOUNPACK #-} !(EventNode x)
  }

-- | State of a 'headE' node, retains exactly what's needed for each phase.
data HeadEState x a
  = HeadEStateInitial (Event x a)
  | HeadEStateSubscribed !(HeadESubscribed x a)
  | HeadEStateOccurred {-# UNPACK #-} !(IORef (Maybe a))

-- | Specialized implementation of 'Reflex.Class.slowHeadE'.
{-# NOINLINE [1] headE #-}
headE :: forall x m a. (HasSpiderTimeline x, Defer (SomeMergeInit x) m) => Event x a -> m (Event x a)
headE originalE = do
  stateRef <- liftIO $ newIORef (HeadEStateInitial originalE :: HeadEState x a)
  let unsubscribeAfterOccurrence occRef = liftIO (readIORef stateRef) >>= \case
        HeadEStateSubscribed subscribed -> liftIO $ do
          writeIORef stateRef $ HeadEStateOccurred occRef
          unsubscribe $ headESubscribedParent subscribed
          deferRecalculateHeight @x $ SomeRecalculateHeight $ do
            let heightRef = eventNodeHeightRef $ headESubscribedNode subscribed
                subscribers = headESubscribedSubscribers subscribed
            invalidateOwnHeight heightRef $ invalidateBagHeight subscribers
            recalculateOwnHeight heightRef (recalculateBagHeight subscribers) (pure zeroHeight)
        _ -> pure ()
      subscribeUntilHead :: EventM x (HeadEState x a)
      subscribeUntilHead = liftIO (readIORef stateRef) >>= \case
        HeadEStateInitial parentEvent -> do
          subscribers <- liftIO WeakBag.empty
          occRef <- liftIO $ newIORef Nothing
          node <- mkEventNode zeroHeight
            (readIORef stateRef >>= \case
                HeadEStateSubscribed subscribed -> pure [_eventSubscription_node $ headESubscribedParent subscribed]
                _ -> pure [])
            (whoCreatedIORef stateRef)
          let heightRef = eventNodeHeightRef node
          SubscribeResult parentSubscription occ <- subscribeAndRead parentEvent $
            ownHeightSubscriber heightRef (invalidateBagHeight subscribers) (recalculateBagHeight subscribers) $ \a -> do
              setOccurrence "headE" occRef a
              -- Propagating before unsubscribing can help avoid
              -- teardown-then-revive when downstream would re-subscribe:
              propagate a subscribers
              unsubscribeAfterOccurrence occRef
          liftIO $ writeIORef heightRef =<< eventSubscriptionHeight parentSubscription
          let !subscribed = HeadESubscribed
                { headESubscribedSubscribers = subscribers
                , headESubscribedParent = parentSubscription
                , headESubscribedOccurrence = occRef
                , headESubscribedNode = node
                }
          liftIO $ writeIORef stateRef $ HeadEStateSubscribed subscribed
          case occ of
            Just a -> do
              setOccurrence "headE" occRef a
              unsubscribeAfterOccurrence occRef
              pure $ HeadEStateOccurred occRef
            Nothing -> pure $ HeadEStateSubscribed subscribed
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
        ticket <- WeakBag.insert sub $ headESubscribedSubscribers subscribed
        occ <- readIORef $ headESubscribedOccurrence subscribed
        pure $ SubscribeResult
              (EventSubscription
                   { _eventSubscription_unsubscribe = do
                       WeakBag.remove ticket
                       touch ticket
                   , _eventSubscription_retained = toAny stateRef
                   , _eventSubscription_node = headESubscribedNode subscribed
                   })
               occ
      HeadEStateOccurred occRef -> do
        occ <- liftIO $ readIORef occRef
        pure $ SubscribeResult eventSubscriptionNever occ

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
    SubscribeResult eventSubscriptionNever <$> (liftIO . readIORef $ nowOrNot)

-- | Construct an 'Event' whose value is guaranteed not to be recomputed
-- repeatedly
--
-- The cached primitives use cacheEventImpl and the Impl versions of the
-- uncached events, because the "push/contract" rule would rewrite the body of
-- push (@cacheEvent (pushCheap f e)@) into a call to push.
{-# NOINLINE [0] cacheEvent #-}
{-# SPECIALIZE cacheEvent :: Event Global a -> Event Global a #-}
cacheEvent :: forall x a. HasSpiderTimeline x => Event x a -> Event x a
cacheEvent = cacheEventImpl

{-# INLINE cacheEventImpl #-}
cacheEventImpl :: forall x a. HasSpiderTimeline x => Event x a -> Event x a
cacheEventImpl e =
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
                  $ debugSubscriber ("cacheEvent" <> showNodeId' nodeId)
#endif
                  $ Subscriber
                  { subscriberPropagate = \a -> do
                      setOccurrence "cacheEvent" occRef a
                      propagate a subscribers
                  , subscriberInvalidateHeight = invalidateBagHeight subscribers
                  , subscriberRecalculateHeight = recalculateBagHeight subscribers
                  }
              for_ occ $ setOccurrence "cacheEvent" occRef
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
          -- 'lazy' stops GHC <= 9.0 from reboxing subscribed, so that
          -- '_eventSubscription_retained' keeps the key of the weak pointer in
          -- 'mSubscribedRef' alive:
          let fields = lazy subscribed
          ticket <- liftIO $ WeakBag.insert sub $ _cacheSubscribed_subscribers fields
          occ <- liftIO $ readIORef $ _cacheSubscribed_occurrence fields
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
                , _eventSubscription_retained = toAny subscribed
                , _eventSubscription_node = _eventSubscription_node parentSub
                }
          pure $ SubscribeResult es occ

subscribe :: Event x a -> Subscriber x a -> EventM x (EventSubscription x)
subscribe e s = subscribeSubscription <$> subscribeAndRead e s

eventRoot :: (GCompare k, HasSpiderTimeline x) => k a -> Root x k -> Event x a
eventRoot !k !r = Event $ liftIO . subscribeAndReadRoot k r

{-# NOINLINE neverNode #-}
neverNode :: EventNode x
neverNode = unsafePerformIO $ mkEventNode zeroHeight (pure []) (pure ["never"])

eventSubscriptionNever :: EventSubscription x
eventSubscriptionNever = EventSubscription (pure ()) (toAny ()) neverNode

subscribeAndReadNever :: EventM x (SubscribeResult x a)
subscribeAndReadNever = pure $ SubscribeResult eventSubscriptionNever Nothing

eventNever :: Event x a
eventNever = Event $ const subscribeAndReadNever

#if !MIN_VERSION_base(4,13,0)
-- | 'GHC.IORef.atomicModifyIORef'_', which base < 4.13 does not have.
atomicModifyIORef'_ :: IORef a -> (a -> a) -> IO (a, a)
atomicModifyIORef'_ ref f = atomicModifyIORef' ref $ \old -> let !new = f old in (new, (old, new))
#endif

{-# INLINE eventFan #-}
eventFan :: forall x map v a. (HasSpiderTimeline x, IsMap map) => MapKey map a -> Event x (map v) -> Fan x map v -> Event x (v a)
eventFan !k p !(Fan weakMaybeFanSubscribed) = Event $ \sub -> do
  subscribed <- (liftIO $ readIORef weakMaybeFanSubscribed >>= maybe (pure Nothing) deRefWeak) >>= \case
    Just subscribed -> pure subscribed
    Nothing -> do
#ifdef DEBUG_NODEIDS
      nodeId <- liftIO newNodeId
#endif
      subscribersRef <- liftIO $ newIORef mapEmpty
      nextIdRef <- liftIO $ newIORef 0
      occRef <- liftIO $ newIORef Nothing
      SubscribeResult parentSub parentOcc <- subscribeAndRead p
#ifdef DEBUG_NODEIDS
          $ debugSubscriber ("SubscriberFan " <> showNodeId' nodeId)
#endif
          $ Subscriber
        { subscriberPropagate = \a -> {-# SCC "traverseFan" #-} do
            subs <- liftIO $ readIORef subscribersRef
            tracePropagate (Proxy :: Proxy x) $ show (mapSize subs) <> " keys subscribed, " <> show (mapSize a) <> " keys firing"
            setOccurrence "fan" occRef a
            mapForIntersection_ subs a $ \(FanSubscribers s) v ->
              withIncreasedDepth (Proxy :: Proxy x) $ WeakBag.traverseWeakIntMap_ s $ \c -> subscriberPropagate c v
        , subscriberInvalidateHeight = \old -> do
            subs <- readIORef subscribersRef
            mapFor_ subs $ \(FanSubscribers s) ->
              WeakBag.traverseWeakIntMap_ s $ \c -> subscriberInvalidateHeight c old
        , subscriberRecalculateHeight = \new -> do
            subs <- readIORef subscribersRef
            mapFor_ subs $ \(FanSubscribers s) ->
              WeakBag.traverseWeakIntMap_ s $ \c -> subscriberRecalculateHeight c new
        }
      for_ parentOcc $ setOccurrence "fan" occRef
      let !subscribed = FanSubscribed
            { fanSubscribedSubscribers = subscribersRef
            , fanSubscribedNextId = nextIdRef
            , fanSubscribedOccurrence = occRef
            , fanSubscribedParent = parentSub
#ifdef DEBUG_NODEIDS
            , fanSubscribedNodeId = nodeId
#endif
            }
      liftIO $ writeIORef weakMaybeFanSubscribed . Just =<< mkWeakPtrWithDebug subscribed "FanSubscribed"
      pure subscribed
  liftIO $ do
    let fields = lazy subscribed -- see 'cacheEventImpl' for 'lazy' reason
        subscribersRef = fanSubscribedSubscribers fields
    sub' <- evaluate sub
    -- Unlike the key map, the finalizer never changes fanSubscribedNextId, so a
    -- plain read and write are enough.
    i <- readIORef $ fanSubscribedNextId fields
    writeIORef (fanSubscribedNextId fields) $! succ i
    let removeSubscriber (FanSubscribers s) =
          let remaining = IntMap.delete i s
          in if IntMap.null remaining then Nothing else Just (FanSubscribers remaining)
    w <- mkWeakPtr sub' $ Just $ void $ atomicModifyIORef'_ subscribersRef $ mapUpdate removeSubscriber k
    let !onlySubscriber = FanSubscribers $ IntMap.singleton i w
    void $ atomicModifyIORef'_ subscribersRef $ mapInsertWith (\_ (FanSubscribers s) -> FanSubscribers $ IntMap.insert i w s) k onlySubscriber
    currentOcc <- readIORef $ fanSubscribedOccurrence fields
    let unsubscribeKey = do
          finalize w
          noKeys <- mapNull <$> readIORef subscribersRef
          when noKeys $ do
            slotSubscribed <- readIORef weakMaybeFanSubscribed >>= maybe (pure Nothing) deRefWeak
            for_ slotSubscribed $ \current -> when (fanSubscribedSubscribers current == subscribersRef) $ do
              writeIORef weakMaybeFanSubscribed Nothing
              unsubscribe $ fanSubscribedParent fields
          touch sub'
    pure $ SubscribeResult (EventSubscription unsubscribeKey (toAny subscribed) (_eventSubscription_node $ fanSubscribedParent fields)) $ mapLookup k =<< currentOcc

eventHold :: Hold x p -> Event x p
eventHold !h = Event $ subscribeHoldEvent h

eventDyn :: (HasSpiderTimeline x, Patch p) => Dyn x p -> Event x p
eventDyn !j = Event $ \sub -> getDynHold j >>= \h -> subscribeHoldEvent h sub

{-# INLINE subscribeCoincidenceInner #-}
-- | Returns Maybe Height where Nothing means the height was not raised to the inner's height.
subscribeCoincidenceInner :: HasSpiderTimeline x => Event x a -> Height -> CoincidenceSubscribed x a -> EventM x (Maybe a, Maybe Height, EventNode x)
subscribeCoincidenceInner inner outerHeight subscribed = do
  SubscribeResult subscription@(EventSubscription _ _ innerSubd) innerOcc <- subscribeAndRead inner $ newSubscriberCoincidenceInner subscribed
  innerHeight <- liftIO $ getEventNodeHeight innerSubd
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

{-# INLINE newSubscriberCoincidenceOuter #-}
newSubscriberCoincidenceOuter :: forall x b. HasSpiderTimeline x => CoincidenceSubscribed x b -> Subscriber x (Event x b)
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

{-# INLINE newSubscriberCoincidenceInner #-}
newSubscriberCoincidenceInner :: forall x a. HasSpiderTimeline x => CoincidenceSubscribed x a -> Subscriber x a
newSubscriberCoincidenceInner subscribed = debugSubscriber ("SubscriberCoincidenceInner" <> showNodeId subscribed) $ (coincidenceSubscribedSubscriber subscribed)
  { subscriberInvalidateHeight  = \_ -> invalidateCoincidenceHeight subscribed
  , subscriberRecalculateHeight = \_ -> recalculateCoincidenceHeight subscribed
  }

{-# INLINE invalidateBagHeight #-}
invalidateBagHeight :: WeakBag (Subscriber x a) -> Height -> IO ()
invalidateBagHeight subscribers = \old -> WeakBag.traverse_ subscribers $ \sub -> subscriberInvalidateHeight sub old

{-# INLINE recalculateBagHeight #-}
recalculateBagHeight :: WeakBag (Subscriber x a) -> Height -> IO ()
recalculateBagHeight subscribers = \new -> WeakBag.traverse_ subscribers $ \sub -> subscriberRecalculateHeight sub new

-- | Propagate everything at the current height
propagate :: forall x a. HasSpiderTimeline x => a -> WeakBag (Subscriber x a) -> EventM x ()
propagate a subscribers = withIncreasedDepth (Proxy::Proxy x) $
  -- Note: in the following traversal, we do not visit nodes that are added to the list during our traversal; they are new events, which will necessarily have full information already, so there is no need to traverse them
  --TODO: Should we check if nodes already have their values before propagating?  Maybe we're re-doing work
  WeakBag.traverse_ subscribers $ \s -> subscriberPropagate s a

--------------------------------------------------------------------------------
-- EventNode
--------------------------------------------------------------------------------

toAny :: a -> Any
toAny = unsafeCoerce

-- | A node of the graph on which the height protocol operates. Not every
-- combinator is a node. A combinator such as 'cacheEvent' or 'fan' fires
-- exactly when its parent fires, so the subscriptions that it returns carry
-- the node of the parent instead of a node of its own.
data EventNode x = EventNode
  { eventNodeHeightRef :: {-# UNPACK #-} !(IORef Height)
  -- ^ The height of the node.
#ifdef DEBUG_CYCLES
  , eventNodeGetParents :: !(IO [EventNode x])
  , eventNodeWhoCreated :: !(IO [String])
#endif
  }

{-# INLINE mkEventNode #-}
mkEventNode
  :: MonadIO m
  => Height
  -> IO [EventNode x]
  -> IO [String]
  -> m (EventNode x)
mkEventNode initialHeight _getParents _whoCreatedNode = liftIO $ do
  heightRef <- newIORef initialHeight
  pure EventNode
    { eventNodeHeightRef = heightRef
#ifdef DEBUG_CYCLES
    , eventNodeGetParents = _getParents
    , eventNodeWhoCreated = _whoCreatedNode
#endif
    }

getEventNodeHeight :: EventNode x -> IO Height
getEventNodeHeight node = readIORef $ eventNodeHeightRef node

{-# INLINE eventSubscriptionHeightRef #-}
eventSubscriptionHeightRef :: EventSubscription x -> IORef Height
eventSubscriptionHeightRef = eventNodeHeightRef . _eventSubscription_node

{-# INLINE eventSubscriptionHeight #-}
eventSubscriptionHeight :: EventSubscription x -> IO Height
eventSubscriptionHeight = getEventNodeHeight . _eventSubscription_node

#ifdef DEBUG_CYCLES
walkInvalidHeightParents :: EventNode x -> IO [EventNode x]
walkInvalidHeightParents s0 = do
  traversedRef <- newIORef []
  let loop s = do
        h <- readIORef $ eventNodeHeightRef s
        when (isUnmarkedInvalidHeight h) $ do
          writeIORef (eventNodeHeightRef s) $! invalidHeightBeingTraversed
          modifyIORef' traversedRef (s :)
          mapM_ loop =<< eventNodeGetParents s
  loop s0
  subscribers <- readIORef traversedRef
  forM_ subscribers $ \s -> writeIORef (eventNodeHeightRef s) $! invalidHeight
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

-- | Unlike with hold, the Hold is the only subscriber of the input event, so
-- the rule "holdBehavior/cacheEvent" can drop a cacheEvent on it.
{-# INLINE [0] holdBehavior #-}
holdBehavior :: Defer (SomeHoldInit x) m => a -> Event x a -> m (Behavior x a)
holdBehavior v0 e = behaviorHold @_ @(Identity _) <$> hold v0 (coerce e)

behaviorConst :: a -> Behavior x a
behaviorConst !a = Behavior $ return a

behaviorPull :: Pull x a -> Behavior x a
behaviorPull !p = Behavior $ do
    val <- liftIO $ readIORef $ pullValue p
    case val of
      Just subscribed -> do
        trackRead (pullSubscribedInvalidators subscribed) $ BehaviorSubscribedPull subscribed
        liftIO $ touch $ pullSubscribedOwnInvalidator subscribed
        return $ pullSubscribedValue subscribed
      Nothing -> do
        i <- liftIO $ newInvalidatorPull p
        wi <- liftIO $ mkWeakPtrWithDebug i "InvalidatorPull"
        parentsRef <- liftIO $ newIORef []
        !holdInits <- asks behaviorEnvHoldInits
        a <- liftIO $ runBehaviorM (pullCompute p) $ BehaviorEnv (Just wi) (Just parentsRef) holdInits
        inv0 <- maybeToList <$> asks behaviorEnvInvalidator
        invsRef <- liftIO $ newIORef $! InvalidatorList (length inv0) invalidatorPruneThreshold inv0
        parents <- liftIO $ readIORef parentsRef
        let subscribed = PullSubscribed
              { pullSubscribedValue = a
              , pullSubscribedInvalidators = invsRef
              , pullSubscribedOwnInvalidator = i
              , pullSubscribedParents = parents
              }
        liftIO $ writeIORef (pullValue p) $ Just subscribed
        asks behaviorEnvParents >>= mapM_ (\r -> liftIO $ modifyIORef' r (BehaviorSubscribedPull subscribed :))
        return a

behaviorDyn :: Patch p => Dyn x p -> Behavior x (PatchTarget p)
behaviorDyn !d = Behavior $ readHoldTracked =<< getDynHold d

{-# INLINE readHoldTracked #-}
readHoldTracked :: Hold x p -> BehaviorM x (PatchTarget p)
readHoldTracked h = do
  result <- liftIO $ readIORef $ holdValue h
  trackRead (holdInvalidators h) $ BehaviorSubscribedHold h
  liftIO $ touch h -- Otherwise, if this gets inlined enough, the hold's parent reference may get collected
  return result

{-# INLINE trackRead #-}
trackRead :: IORef (InvalidatorList x) -> SomeBehaviorSubscribed x -> BehaviorM x ()
trackRead invalidatorsRef subscribed = do
  asks behaviorEnvInvalidator >>= mapM_ (liftIO . addInvalidatorAmortized invalidatorsRef)
  asks behaviorEnvParents >>= mapM_ (\r -> liftIO $ modifyIORef' r (subscribed :))

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
  liftIO $ runBehaviorM (readBehaviorTracked b) $ BehaviorEnv Nothing Nothing holdInits --TODO: Specialize readBehaviorTracked to the Nothing and Just cases

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

--------------------------------------------------------------------------------
-- Combinators
--------------------------------------------------------------------------------

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
              , eventEnvPendingRecalculations :: !(IORef [SomeRecalculateHeight x])
              , eventEnvDelayedMerges :: !(IORef (HeightQueue (EventM x ())))
              }

{-# INLINE runEventM #-}
runEventM :: EventM x a -> IO a
runEventM = unEventM

{-# INLINE spiderEventEnv #-}
spiderEventEnv :: forall x. HasSpiderTimeline x => EventEnv x
spiderEventEnv = _spiderTimeline_eventEnv (unSTE (spiderTimeline :: SpiderTimelineEnv x))

asksEventEnv :: forall x a. HasSpiderTimeline x => (EventEnv x -> a) -> EventM x a
asksEventEnv f = pure $ f $ spiderEventEnv @x

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
  getDeferralQueue = asks behaviorEnvHoldInits

instance HasSpiderTimeline x => Defer (SomeMergeUpdate x) (EventM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = asksEventEnv eventEnvMergeUpdates

instance HasSpiderTimeline x => Defer (SomeMergeInit x) (EventM x) where
  {-# INLINE getDeferralQueue #-}
  getDeferralQueue = asksEventEnv eventEnvMergeInits

{-# INLINE getCurrentHeight #-}
getCurrentHeight :: HasSpiderTimeline x => EventM x Height
getCurrentHeight = do
  heightRef <- asksEventEnv eventEnvCurrentHeight
  liftIO $ readIORef heightRef

{-# INLINE scheduleMerge #-}
scheduleMerge :: HasSpiderTimeline x => Height -> EventM x () -> EventM x ()
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

{-# INLINE setOccurrence #-}
setOccurrence :: HasSpiderTimeline x => String -> IORef (Maybe a) -> a -> EventM x ()
#ifdef DEBUG_FRAME
setOccurrence nodeName occRef a = do
  liftIO $ readIORef occRef >>= debugOccurrence (nodeName <> ": two occurrences in one frame")
#else
setOccurrence _ occRef a = do
#endif
  liftIO $ writeIORef occRef $ Just a
  scheduleClear occRef

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

data BehaviorEnv x = BehaviorEnv
  { behaviorEnvInvalidator :: !(Maybe (Weak (Invalidator x)))
  , behaviorEnvParents :: !(Maybe (IORef [SomeBehaviorSubscribed x]))
  , behaviorEnvHoldInits :: IORef [SomeHoldInit x]
  }

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

data SomeBehaviorSubscribed x
   = forall p. BehaviorSubscribedHold (Hold x p)
   | forall a. BehaviorSubscribedPull (PullSubscribed x a)

data PullSubscribed x a
   = PullSubscribed { pullSubscribedValue :: !a
                    , pullSubscribedInvalidators :: !(IORef (InvalidatorList x))
                    , pullSubscribedOwnInvalidator :: !(Invalidator x)
                    , pullSubscribedParents :: ![SomeBehaviorSubscribed x] -- Need to keep parent behaviors alive, or they won't let us know when they're invalidated
                    }

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

data RootSubscribed x a = RootSubscribed
  { rootSubscribedSubscribers :: {-# UNPACK #-} !(WeakBag (Subscriber x a))
#ifdef DEBUG_NODEIDS
  , rootSubscribedNodeId :: Int
#endif
  }

data RootKey x a = RootKey
  { rootKeySubscribed :: {-# UNPACK #-} !(Weak (RootSubscribed x a))
  , rootKeyTeardown :: {-# UNPACK #-} !(MVar (Maybe (IO ())))
  }

data Root x k
   = Root { rootOccurrence :: !(IORef (DMap k Identity)) -- The currently-firing occurrence of this event
          , rootSubscribed :: !(IORef (DMap k (RootKey x)))
          , rootInit :: !(forall a. k a -> RootTrigger x a -> IO (IO ()))
          , rootNode :: {-# NOUNPACK #-} !(EventNode x)
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
  mapInsertLookup :: MapKey map a -> v a -> map v -> (Maybe (v a), map v)
  mapDeleteLookup :: MapKey map a -> map v -> (Maybe (v a), map v)
  mapInsertWith :: (v a -> v a -> v a) -> MapKey map a -> v a -> map v -> map v
  mapUpdate :: (v a -> Maybe (v a)) -> MapKey map a -> map v -> map v
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
  mapInsertLookup k v m = (DMap.lookup k m, DMap.insert k v m)
  mapDeleteLookup k m = (DMap.lookup k m, DMap.delete k m)
  mapInsertWith = DMap.insertWith'
  mapUpdate = DMap.update
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
  mapInsertLookup (Const2 k) v (IntMapOf m) = IntMapOf <$> IntMap.insertLookupWithKey (\_ new _ -> new) k v m
  mapDeleteLookup (Const2 k) (IntMapOf m) = IntMapOf <$> IntMap.updateLookupWithKey (\_ _ -> Nothing) k m
  mapInsertWith f (Const2 k) v (IntMapOf m) = IntMapOf $ IntMap.insertWith f k v m
  mapUpdate f (Const2 k) (IntMapOf m) = IntMapOf $ IntMap.update f k m
  mapForIntersection_ (IntMapOf left) (IntMapOf right) f = traverse_ (uncurry f) $ IntMap.intersectionWith (,) left right
  mapFor_ (IntMapOf m) f = traverse_ f m
  mapToList (IntMapOf m) = (\(k, v) -> Const2 k :=> v) <$> IntMap.toList m
  mapFromDistinctAscList entries = IntMapOf $ IntMap.fromDistinctAscList $ fmap toPair entries
    where
      toPair :: forall b w. DSum (Const2 Int b) w -> (Int, w b)
      toPair (Const2 k :=> v) = (k, v)

newtype FanSubscribers x v a = FanSubscribers (IntMap (Weak (Subscriber x (v a))))

newtype Fan x map v = Fan (IORef (Maybe (Weak (FanSubscribed x map v))))

-- We don't re-use WeakBag in Fan so that we can get atomic modification of the entire 'fanSubscribedSubscribers':
data FanSubscribed x map v = FanSubscribed
  { fanSubscribedSubscribers :: {-# UNPACK #-} !(IORef (map (FanSubscribers x v)))
  , fanSubscribedNextId :: {-# UNPACK #-} !(IORef Int)
  , fanSubscribedOccurrence :: {-# UNPACK #-} !(IORef (Maybe (map v)))
  , fanSubscribedParent :: {-# UNPACK #-} !(EventSubscription x)
#ifdef DEBUG_NODEIDS
  , fanSubscribedNodeId :: {-# UNPACK #-} !Int
#endif
  }

{-# INLINE newFan #-}
newFan :: IO (Fan x map v)
newFan = Fan <$> newIORef Nothing

data SwitchSubscribed x a
   = SwitchSubscribed { switchSubscribedNode :: {-# NOUNPACK #-} !(EventNode x)
                      , switchSubscribedSubscriber :: !(Subscriber x a)
                      , switchSubscribedOwnInvalidator :: !(IORef (Invalidator x))
                      , switchSubscribedBehaviorParents :: !(IORef [SomeBehaviorSubscribed x])
                      , switchSubscribedParent :: !(Behavior x (Event x a))
                      , switchSubscribedCurrentParent :: !(IORef (SwitchParentSubscription x))
#ifdef DEBUG_NODEIDS
                      , switchSubscribedNodeId :: Int
#endif
                      }

switchSubscribedHeight :: SwitchSubscribed x a -> IORef Height
switchSubscribedHeight = eventNodeHeightRef . switchSubscribedNode

data SwitchParentSubscription x
  = SwitchParentSubscribed !(EventSubscription x)
  | SwitchParentUnsubscribed
data CoincidenceSubscribed x a
   = CoincidenceSubscribed { coincidenceSubscribedNode :: {-# NOUNPACK #-} !(EventNode x)
                           , coincidenceSubscribedSubscriber :: !(Subscriber x a)
                           , coincidenceSubscribedOuterParent :: !(IORef (EventSubscription x))
                           , coincidenceSubscribedInnerParent :: !(IORef (Maybe (EventNode x)))
#ifdef DEBUG_NODEIDS
                           , coincidenceSubscribedNodeId :: Int
#endif
                           }

coincidenceSubscribedHeight :: CoincidenceSubscribed x a -> IORef Height
coincidenceSubscribedHeight = eventNodeHeightRef . coincidenceSubscribedNode

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
                 | HoldDyn   {-# NOUNPACK #-} !(Hold x p)

newtype Dyn (x :: Type) p = Dyn { unDyn :: IORef (DynType x p) }

newMapDyn :: HasSpiderTimeline x => (a -> b) -> DynamicS x (Identity a) -> DynamicS x (Identity b)
newMapDyn f d = dynamicDyn $ unsafeBuildDynamic (fmap f $ readBehaviorTracked $ dynamicCurrent d) (Identity . f . runIdentity <$> dynamicUpdated d)

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
  in dynamicDyn $ unsafeBuildDynamic (f <$> readBehaviorUntracked (dynamicCurrent da) <*> readBehaviorUntracked (dynamicCurrent db)) ec

buildDynamic :: (Defer (SomeDynInit x) m, Patch p) => EventM x (PatchTarget p) -> Event x p -> m (Dyn x p)
buildDynamic readV0 v' = do
  result <- liftIO $ newIORef $ BuildDyn (readV0, v')
  let !d = Dyn result
  defer $ SomeDynInit d
  return d

unsafeBuildDynamic :: BehaviorM x (PatchTarget p) -> Event x p -> Dyn x p
unsafeBuildDynamic readV0 v' =
  Dyn $ unsafePerformIO $ newIORef $ UnsafeDyn (readV0, v')

instance HasSpiderTimeline x => Functor (Event x) where
  fmap f = push $ return . Just . f

instance Functor (Behavior x) where
  fmap f = pull . fmap f . readBehaviorTracked

-- | See the comment at cacheEvent for why this is not
-- @cacheEvent (pushCheap f e)@.
{-# NOINLINE [0] push #-}
{-# SPECIALIZE push :: (a -> EventM Global (Maybe b)) -> Event Global a -> Event Global b #-}
push :: HasSpiderTimeline x => (a -> EventM x (Maybe b)) -> Event x a -> Event x b
push f e = cacheEventImpl (pushCheapImpl f e)

{-# NOINLINE [0] pushSwitch #-}
{-# SPECIALIZE pushSwitch :: (a -> EventM Global (Maybe b)) -> Behavior Global (Event Global a) -> Event Global b #-}
pushSwitch :: HasSpiderTimeline x => (a -> EventM x (Maybe b)) -> Behavior x (Event x a) -> Event x b
pushSwitch f b = cacheEventImpl (pushCheapImpl f (switchUncachedImpl b))

{-# NOINLINE [0] pushCoincidence #-}
{-# SPECIALIZE pushCoincidence :: (a -> EventM Global (Maybe c)) -> Event Global (R.Event (SpiderTimeline Global) a) -> Event Global c #-}
pushCoincidence :: HasSpiderTimeline x => (a -> EventM x (Maybe c)) -> Event x (R.Event (SpiderTimeline x) a) -> Event x c
pushCoincidence f e = cacheEventImpl (pushCheapImpl f (coincidenceUncachedImpl e))

{-# NOINLINE [0] pushMergeInt #-}
{-# SPECIALIZE pushMergeInt :: (IntMap a -> EventM Global (Maybe b)) -> DynamicS Global (PatchIntMap (Event Global a)) -> Event Global b #-}
pushMergeInt :: HasSpiderTimeline x => (IntMap a -> EventM x (Maybe b)) -> DynamicS x (PatchIntMap (Event x a)) -> Event x b
pushMergeInt f d = cacheEventImpl (pushCheapImpl f (mergeIntCheapImpl d))

{-# INLINE [0] pushMergeG #-}
pushMergeG :: forall k q x v c. (HasSpiderTimeline x, GCompare k)
  => (DMap k v -> EventM x (Maybe c))
  -> (forall a. q a -> Event x (v a))
  -> DynamicS x (PatchDMap k q) -> Event x c
pushMergeG f nt d = cacheEventImpl (pushCheapImpl f (mergeCheapImpl nt d))

{-# INLINE [0] pushMergeWithMove #-}
pushMergeWithMove :: forall k q x v c. (HasSpiderTimeline x, GCompare k)
  => (DMap k v -> EventM x (Maybe c))
  -> (forall a. q a -> Event x (v a))
  -> DynamicS x (PatchDMapWithMove k q) -> Event x c
pushMergeWithMove f nt d = cacheEventImpl (pushCheapImpl f (mergeCheapWithMoveImpl nt d))

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

{-# NOINLINE [0] switch #-}
{-# SPECIALIZE switch :: Behavior Global (Event Global a) -> Event Global a #-}
switch :: HasSpiderTimeline x => Behavior x (Event x a) -> Event x a
switch b = cacheEventImpl (switchUncachedImpl b)

{-# NOINLINE [0] switchUncached #-}
{-# SPECIALIZE switchUncached :: Behavior Global (Event Global a) -> Event Global a #-}
switchUncached :: forall x a. HasSpiderTimeline x => Behavior x (Event x a) -> Event x a
switchUncached = switchUncachedImpl

{-# INLINE switchUncachedImpl #-}
switchUncachedImpl :: forall x a. HasSpiderTimeline x => Behavior x (Event x a) -> Event x a
switchUncachedImpl b = Event $ \sub -> do
  currentParentRef <- liftIO $ newIORef $ error "switchUncached: current parent not yet subscribed"
  node <- mkEventNode (error "switchUncached: height not yet known")
    (readIORef currentParentRef >>= \case
        SwitchParentSubscribed parent -> pure [_eventSubscription_node parent]
        SwitchParentUnsubscribed -> pure [])
    (whoCreatedIORef currentParentRef)
  let heightRef = eventNodeHeightRef node
  invalidatorRef <- liftIO $ newIORef $ error "switchUncached: invalidator not yet created"
  parentsRef <- liftIO $ newIORef [] --TODO: This should be unnecessary, because it will always be filled with just the single parent behavior
#ifdef DEBUG_NODEIDS
  nid <- liftIO newNodeId
#endif
  let !subscribed = SwitchSubscribed
        { switchSubscribedNode = node
        , switchSubscribedSubscriber = debugSubscriber ("SubscriberSwitch" <> showNodeId subscribed) $
            ownHeightSubscriber heightRef (subscriberInvalidateHeight sub) (subscriberRecalculateHeight sub) (subscriberPropagate sub)
        , switchSubscribedOwnInvalidator = invalidatorRef
        , switchSubscribedBehaviorParents = parentsRef
        , switchSubscribedParent = b
        , switchSubscribedCurrentParent = currentParentRef
#ifdef DEBUG_NODEIDS
        , switchSubscribedNodeId = nid
#endif
        }
  liftIO $ writeIORef invalidatorRef =<< newInvalidatorSwitch subscribed
  holdInits <- getDeferralQueue
  SubscribeResult subscription parentOcc <- liftIO $
    subscribeSwitchCurrentParent subscribed holdInits $ \parent subscriber -> runEventM $ subscribeAndRead parent subscriber
  liftIO $ writeIORef heightRef =<< eventSubscriptionHeight subscription
  pure $ SubscribeResult
    (EventSubscription
      { _eventSubscription_unsubscribe = do
          readIORef currentParentRef >>= \case
            SwitchParentSubscribed parent -> unsubscribe parent
            SwitchParentUnsubscribed -> pure ()
          writeIORef currentParentRef SwitchParentUnsubscribed
      , _eventSubscription_retained = toAny subscribed
      , _eventSubscription_node = node
      })
    parentOcc

{-# INLINE subscribeSwitchCurrentParent #-}
subscribeSwitchCurrentParent
  :: SwitchSubscribed x a
  -> IORef [SomeHoldInit x]
  -> (Event x a -> Subscriber x a -> IO (SubscribeResult x a))
  -> IO (SubscribeResult x a)
subscribeSwitchCurrentParent subscribed holdInits subscribeToParent = do
  i <- readIORef $ switchSubscribedOwnInvalidator subscribed
  wi <- mkWeakPtrWithDebug i "InvalidatorSwitch"
  e <- runBehaviorM (readBehaviorTracked (switchSubscribedParent subscribed)) $
    BehaviorEnv (Just wi) (Just $ switchSubscribedBehaviorParents subscribed) holdInits
  result <- subscribeToParent e $ switchSubscribedSubscriber subscribed
  writeIORef (switchSubscribedCurrentParent subscribed) $! SwitchParentSubscribed $ subscribeSubscription result
  pure result

-- The inner events on coincidence have the type of the Reflex instance, so that
-- the instance passes the outer event to coincidence without a coerce: the
-- rules that drop a cacheEvent on the outer event don't match through a cast.
{-# NOINLINE [0] coincidence #-}
{-# SPECIALIZE coincidence :: Event Global (R.Event (SpiderTimeline Global) a) -> Event Global a #-}
coincidence :: HasSpiderTimeline x => Event x (R.Event (SpiderTimeline x) a) -> Event x a
coincidence e = cacheEventImpl (coincidenceUncachedImpl e)

{-# NOINLINE [0] coincidenceUncached #-}
{-# SPECIALIZE coincidenceUncached :: Event Global (R.Event (SpiderTimeline Global) a) -> Event Global a #-}
coincidenceUncached :: forall x a. HasSpiderTimeline x => Event x (R.Event (SpiderTimeline x) a) -> Event x a
coincidenceUncached = coincidenceUncachedImpl

{-# INLINE coincidenceUncachedImpl #-}
coincidenceUncachedImpl :: forall x a. HasSpiderTimeline x => Event x (R.Event (SpiderTimeline x) a) -> Event x a
coincidenceUncachedImpl outer = Event $ \sub -> do
  outerParentRef <- liftIO $ newIORef $ error "coincidenceUncached: outer parent not yet subscribed"
  innerParentRef <- liftIO $ newIORef Nothing
  node <- mkEventNode (error "coincidenceUncached: height not yet known")
    (do
        outerParent <- _eventSubscription_node <$> readIORef outerParentRef
        innerParents <- maybeToList <$> readIORef innerParentRef
        pure $ outerParent : innerParents)
    (whoCreatedIORef outerParentRef)
  let heightRef = eventNodeHeightRef node
#ifdef DEBUG_NODEIDS
  nid <- liftIO newNodeId
#endif
  let !subscribed = CoincidenceSubscribed
        { coincidenceSubscribedNode = node
        , coincidenceSubscribedSubscriber = sub
        , coincidenceSubscribedOuterParent = outerParentRef
        , coincidenceSubscribedInnerParent = innerParentRef
#ifdef DEBUG_NODEIDS
        , coincidenceSubscribedNodeId = nid
#endif
        }
  SubscribeResult outerSubscription outerOcc <- subscribeAndRead outer $ coerce $ newSubscriberCoincidenceOuter subscribed
  liftIO $ writeIORef outerParentRef outerSubscription
  outerHeight <- liftIO $ eventSubscriptionHeight outerSubscription
  (occ, height, mInnerSubd) <- case outerOcc of
    Nothing -> return (Nothing, outerHeight, Nothing)
    Just o -> do
      (occ, maybeRaisedToInnerHeight, innerSubd) <- subscribeCoincidenceInner (unSpiderEvent o) outerHeight subscribed
      return (occ, fromMaybe outerHeight maybeRaisedToInnerHeight, Just innerSubd)
  liftIO $ writeIORef heightRef $! height
  liftIO $ writeIORef innerParentRef mInnerSubd
  when (isJust mInnerSubd) $ scheduleClear innerParentRef
  pure $ SubscribeResult
    (EventSubscription
      { _eventSubscription_unsubscribe = unsubscribe =<< readIORef outerParentRef
      , _eventSubscription_retained = toAny subscribed
      , _eventSubscription_node = node
      })
    occ

-- | Propagate root triggers and process delayed merges, then run the
-- given read action. Runs inside the single 'runFrame' of
-- 'runHostFrameFireAndRead', after the build and hold-init phases.
propagateAndRead :: forall x b. HasSpiderTimeline x => [DSum (RootTrigger x) Identity] -> EventM x b -> EventM x b
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

debugSubscriber :: forall x a. HasSpiderTimeline x => String -> Subscriber x a -> Subscriber x a
debugSubscriber description subscribed = Subscriber
  {
    subscriberPropagate = \m -> do
      tracePropagate (Proxy :: Proxy x) ("subscriberPropagate: " <> description)
      subscriberPropagate subscribed m
  , subscriberInvalidateHeight = \old -> do
      traceInvalidateHeight $ "subscriberInvalidateHeight: " <> description <> ", old = " <> show old
      subscriberInvalidateHeight subscribed old
      traceInvalidateHeight $ "subscriberInvalidateHeight: " <> description <> ", done"
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
debugSubscriber :: String -> Subscriber x a -> Subscriber x a
debugSubscriber _ = id



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
  show (EventLoopException stacks) = "causality loop detected:\n" <> if all null stacks
    then "no location information, compile with profiling enabled for stack tree"
    else showStacks stacks

#else

data EventLoopException = EventLoopException
instance Exception EventLoopException

instance Show EventLoopException where
  show EventLoopException = "causality loop detected: \n" <>
    "compile reflex with flag 'debug-cycles' and compile with profiling enabled for stack tree"

#endif

-- | 'CoincidenceSubscribed' is present only when the coincidence raised its height while subscribing to the inner event.
data SomeResetCoincidence x = forall a. SomeResetCoincidence !(EventSubscription x) !(Maybe (CoincidenceSubscribed x a))

newtype SomeRecalculateHeight x = SomeRecalculateHeight (IO ())

runBehaviorM :: BehaviorM x a -> BehaviorEnv x -> IO a
runBehaviorM a = runReaderIO (unBehaviorM a)

{-# INLINE getDynHold #-}
getDynHold :: (Defer (SomeHoldInit x) m, Patch p) => Dyn x p -> m (Hold x p)
getDynHold d = do
  mh <- liftIO $ readIORef $ unDyn d
  case mh of
    HoldDyn h -> return h
    UnsafeDyn (readV0, v') -> do
      holdInits <- getDeferralQueue
      v0 <- liftIO $ runBehaviorM readV0 $ BehaviorEnv Nothing Nothing holdInits
      hold' v0 v'
    BuildDyn (readV0, v') -> do
      v0 <- liftIO $ runEventM readV0
      hold' v0 v'
  where
    hold' v0 v' = do
      h <- hold v0 v'
      liftIO $ writeIORef (unDyn d) $ HoldDyn h
      return h


subscribeAndReadRoot :: forall k x a. (GCompare k, HasSpiderTimeline x) => k a -> Root x k -> Subscriber x a -> IO (SubscribeResult x a)
subscribeAndReadRoot k r sub = do
  entry <- DMap.lookup k <$> readIORef (rootSubscribed r)
  live <- case entry of
    Just RootKey { rootKeySubscribed = w } -> fmap ((,) w) <$> deRefWeak w
    Nothing -> pure Nothing
  (weakSubscribed, subscribed) <- case live of
    Just found -> {-# SCC "hitRoot" #-} pure found
    Nothing -> {-# SCC "missRoot" #-} do
      for_ entry $ runRootKeyTeardown (rootSubscribed r) . rootKeyTeardown
      subscribers <- WeakBag.empty
      tracePropagate (Proxy::Proxy x) $  "subscribeAndReadRoot: calling rootInit"
      teardown <- rootInit r k $ RootTrigger (subscribers, rootOccurrence r, k)
#ifdef DEBUG_NODEIDS
      nid <- newNodeId
#endif
      let !subscribed = RootSubscribed
            { rootSubscribedSubscribers = subscribers
#ifdef DEBUG_NODEIDS
            , rootSubscribedNodeId = nid
#endif
            }
      teardownVar <- newMVar $ Just teardown
      weakSubscribed <- mkWeakPtr subscribed $ Just $ runRootKeyTeardown (rootSubscribed r) teardownVar
      void $ atomicModifyIORef'_ (rootSubscribed r) $ DMap.insert k $ RootKey weakSubscribed teardownVar
      pure (weakSubscribed, lazy subscribed)
  let fields = lazy subscribed -- 'lazy' used as in 'cacheEventImpl'
      subscribers = rootSubscribedSubscribers fields
  ticket <- WeakBag.insert sub subscribers
  occ <- coerce . DMap.lookup k <$> readIORef (rootOccurrence r)
  let unsubscribeRoot = do
        WeakBag.remove ticket
        noSubscribers <- WeakBag.isEmpty subscribers
        when noSubscribers $ finalize weakSubscribed
        touch ticket
  return $ SubscribeResult (EventSubscription unsubscribeRoot (toAny subscribed) (rootNode r)) occ
  where
    runRootKeyTeardown :: IORef (DMap k (RootKey x)) -> MVar (Maybe (IO ())) -> IO ()
    runRootKeyTeardown rootMap teardownVar = Control.Exception.mask $ \restore -> do
      teardown <- takeMVar teardownVar
      restore (sequence_ teardown) `Control.Exception.finally` (do
        putMVar teardownVar Nothing
        void $ atomicModifyIORef'_ rootMap $
          DMap.update (\e -> if rootKeyTeardown e == teardownVar then Nothing else Just e) k)

newtype EventSelectorInt x a = EventSelectorInt { selectInt :: Int -> Event x a }

{-# INLINE [0] mergeG #-}
mergeG :: forall k q x v. (HasSpiderTimeline x, GCompare k)
  => (forall a. q a -> Event x (v a))
  -> DynamicS x (PatchDMap k q) -> Event x (DMap k v)
mergeG nt d = cacheEventImpl (mergeCheapImpl nt d)

{-# INLINE [0] mergeWithMove #-}
mergeWithMove :: forall k v q x. (HasSpiderTimeline x, GCompare k)
  => (forall a. q a -> Event x (v a))
  -> DynamicS x (PatchDMapWithMove k q) -> Event x (DMap k v)
mergeWithMove nt d = cacheEventImpl (mergeCheapWithMoveImpl nt d)

{-# INLINE [0] mergeCheap #-}
mergeCheap
  :: forall k x q v. (HasSpiderTimeline x, GCompare k)
  => (forall a. q a -> Event x (v a))
  -> DynamicS x (PatchDMap k q)
  -> Event x (DMap k v)
mergeCheap = mergeCheapImpl

{-# INLINE mergeCheapImpl #-}
mergeCheapImpl
  :: forall k x q v. (HasSpiderTimeline x, GCompare k)
  => (forall a. q a -> Event x (v a))
  -> DynamicS x (PatchDMap k q)
  -> Event x (DMap k v)
mergeCheapImpl nt = genericMergeCheap unMergeSubscribedParent getInitialSubscribers updateMe
  where
      updateMe :: MergeUpdateFunc (DMap k) v x (PatchDMap k q) (MergeSubscribedParent x)
      updateMe subscriber heightBagRef oldParents (PatchDMap p) =
        updateMergeParents nt subscriber heightBagRef oldParents p

      getInitialSubscribers :: MergeInitFunc (DMap k) v (DMap k q) x (MergeSubscribedParent x)
      getInitialSubscribers = initMergeParents nt (\k -> pure (pure k, MergeSubscribedParent))

{-# INLINE [0] mergeCheapWithMove #-}
mergeCheapWithMove :: forall k x v q. (HasSpiderTimeline x, GCompare k)
  => (forall a. q a -> Event x (v a))
  -> DynamicS x (PatchDMapWithMove k q)
  -> Event x (DMap k v)
mergeCheapWithMove = mergeCheapWithMoveImpl

{-# INLINE mergeCheapWithMoveImpl #-}
mergeCheapWithMoveImpl :: forall k x v q. (HasSpiderTimeline x, GCompare k)
  => (forall a. q a -> Event x (v a))
  -> DynamicS x (PatchDMapWithMove k q)
  -> Event x (DMap k v)
mergeCheapWithMoveImpl nt = genericMergeCheap _mergeSubscribedParentWithMove_subscription getInitialSubscribers updateMe
  where
      updateMe :: MergeUpdateFunc (DMap k) v x (PatchDMapWithMove k q) (MergeSubscribedParentWithMove x k)
      updateMe subscriber heightBagRef oldParents p = do
        -- Prepare new parents for insertion
        let subscribeParent :: forall a. k a -> Event x (v a) -> EventM x (MergeSubscribedParentWithMove x k a)
            subscribeParent k e = do
              keyRef <- liftIO $ newIORef k
              (`MergeSubscribedParentWithMove` keyRef) . subscribeSubscription
                <$> subscribeMergeParent heightBagRef e (subscriber $ liftIO $ readIORef keyRef)
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
  -> IORef HeightBag
  -> EventM x (map v, map s)

data MergeChangeSubscription x
  = MergeChangeNotSubscribed
  | MergeChangeSubscribed !(EventSubscription x)
  | MergeChangeUnsubscribed

{-# INLINE initMergeParents #-}
initMergeParents
  :: forall map q v x s. IsMap map
  => (forall a. q a -> Event x (v a))
  -> (forall a. MapKey map a -> EventM x (EventM x (MapKey map a), EventSubscription x -> s a))
  -> MergeInitFunc map v (map q) x s
initMergeParents nt parentAt initialParents subscriber heightBagRef = do
  occurrencesRef <- liftIO $ newIORef mapEmpty
  parents <- forM (mapToList initialParents) $ \(k :=> e) -> do
    (getKey, toParent) <- parentAt k
    SubscribeResult subscription parentOcc <- subscribeMergeParent heightBagRef (nt e) (subscriber getKey)
    liftIO $ for_ parentOcc $ modifyIORef' occurrencesRef . mapInsert k
    pure $ k :=> toParent subscription
  occurrences <- liftIO $ readIORef occurrencesRef
  pure (occurrences, mapFromDistinctAscList parents)

{-# INLINE updateMergeParents #-}
updateMergeParents
  :: forall map q v x. IsMap map
  => (forall a. q a -> Event x (v a))
  -> MergeUpdateFunc map v x (map (ComposeMaybe q)) (MergeSubscribedParent x)
updateMergeParents nt subscriber heightBagRef oldParents patch =
  foldM updateParent ([], oldParents) $ mapToList patch
  where
    updateParent (subscriptionsToKill, parents) (k :=> ComposeMaybe maybeEvent) = do
      (maybeOldSubscribed, newParents) <- case maybeEvent of
        Nothing -> pure $ mapDeleteLookup k parents
        Just e -> do
          newParent <- MergeSubscribedParent . subscribeSubscription <$> subscribeMergeParent heightBagRef (nt e) (subscriber $ pure k)
          pure $ mapInsertLookup k newParent parents
      for_ maybeOldSubscribed $
        liftIO . forgetMergeParentHeight heightBagRef . unMergeSubscribedParent
      pure ( maybeToList (unMergeSubscribedParent <$> maybeOldSubscribed) ++ subscriptionsToKill
           , newParents
           )

subscribeMergeParent :: IORef HeightBag -> Event x a -> Subscriber x a -> EventM x (SubscribeResult x a)
subscribeMergeParent heightBagRef e sub = do
  result <- subscribeAndRead e sub
  liftIO $ modifyIORef' heightBagRef . heightBagAddParent =<< eventSubscriptionHeight (subscribeSubscription result)
  pure result

forgetMergeParentHeight :: IORef HeightBag -> EventSubscription x -> IO ()
forgetMergeParentHeight heightBagRef subscription =
  modifyIORef' heightBagRef . heightBagRemoveParent =<< eventSubscriptionHeight subscription

checkCycle :: HasSpiderTimeline x => EventNode x -> EventM x ()
checkCycle subscribed = do
    height <- liftIO $ readIORef (eventNodeHeightRef subscribed)
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
            stacks <- forM nodesInvolvedInCycle eventNodeWhoCreated
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
  heightBagRef <- liftIO $ newIORef heightBagEmpty
  parentsRef :: IORef (map s) <- liftIO $ newIORef $ error "merge: parentsRef not yet initialized"
  changeSubdRef <- liftIO $ newIORef MergeChangeNotSubscribed
  subscribed <- mkEventNode (error "merge: heightRef not yet initialized")
    (fmap (\(_ :=> parent) -> _eventSubscription_node (getParent parent)) . mapToList <$> readIORef parentsRef)
    (whoCreatedIORef changeSubdRef)
  let heightRef = eventNodeHeightRef subscribed

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

#ifdef DEBUG
      checkParentCount :: IO ()
      checkParentCount = do
        parents <- readIORef parentsRef
        heights <- readIORef heightBagRef
        when (mapSize parents /= heightBagParentCount heights) $
          error $ "merge: " <> show (mapSize parents) <> " parents, but the height bag has " <> show (heightBagParentCount heights)
#endif

      recalculateMyHeight = recalculateOwnHeight heightRef (subscriberRecalculateHeight sub) $ do
#ifdef DEBUG
        checkParentCount
#endif
        heightBagHeight <$> readIORef heightBagRef

      invalidateMyHeightOnParentChange = do
        own <- readIORef heightRef
        case own of
          ValidHeight -> do
            heights <- readIORef heightBagRef
            case heightBagHeight heights of
              new@ValidHeight | compareHeight own new == EQ -> pure ()
              _ -> invalidateMyHeight
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
            let heights' = heightBagInvalidateParent old heights
            writeIORef heightBagRef $! heights'
            own <- readIORef heightRef
            case own of
              ValidHeight ->
                when (compareHeight (heightBagSuccHeight heights') (heightBagSuccHeight heights) == LT)
                  invalidateMyHeight
              InvalidHeight -> pure ()
        , subscriberRecalculateHeight = \new -> do
            modifyIORef' heightBagRef $ heightBagRecalculateParent new
            own <- readIORef heightRef
            case own of
              ValidHeight -> when (compareHeight own new /= GT) invalidateMyHeight
              InvalidHeight -> pure ()
            recalculateMyHeight
        }

      --TODO: Be able to run as much of this as possible promptly
      updateParents patch = liftIO (readIORef changeSubdRef) >>= \case
        MergeChangeUnsubscribed -> return []
        _ -> do
          oldParents <- liftIO $ readIORef parentsRef
          (subscriptionsToKill, newParents) <- updateFunc parentSubscriber heightBagRef oldParents patch
          liftIO $ writeIORef parentsRef $! newParents
#ifdef DEBUG
          liftIO checkParentCount
#endif
          return subscriptionsToKill

      deferParentUpdate patch =
        defer $ SomeMergeUpdate (updateParents patch) invalidateMyHeightOnParentChange recalculateMyHeight

  (dm, initialParentState) <- getInitialSubscribers initialParents parentSubscriber heightBagRef
  myHeight <- liftIO $ heightBagHeight <$> readIORef heightBagRef
  currentHeight <- getCurrentHeight
  let (occ, accum) = if heightSlotPassed currentHeight myHeight -- The merge would have fired already in this frame
                     then (if mapNull dm then Nothing else Just dm, mapEmpty)
                     else (Nothing, dm)
  unless (mapNull accum) $ scheduleSelf myHeight
  liftIO $ writeIORef accumRef $! accum
  liftIO $ writeIORef heightRef $! myHeight
  liftIO $ writeIORef parentsRef $! initialParentState
#ifdef DEBUG
  liftIO checkParentCount
#endif
  defer $ SomeMergeInit $ liftIO (readIORef changeSubdRef) >>= \case
    MergeChangeNotSubscribed -> do
      let changeSubscriber = terminalSubscriber $ \patch -> {-# SCC "traverseMergeChange" #-} do
            tracePropagate (Proxy :: Proxy x) "SubscriberMerge/Change"
            deferParentUpdate patch
      SubscribeResult changeSubscription change <- subscribeAndRead (dynamicUpdated d) changeSubscriber
      for_ change deferParentUpdate
      liftIO $ writeIORef changeSubdRef $ MergeChangeSubscribed changeSubscription
    _ -> pure ()
  let unsubscribeAll = do
        parents <- readIORef parentsRef
        mapFor_ parents $ unsubscribe . getParent
        readIORef changeSubdRef >>= \case
          MergeChangeSubscribed changeSubscription -> unsubscribe changeSubscription
          _ -> pure ()
        writeIORef changeSubdRef MergeChangeUnsubscribed
  pure $ SubscribeResult (EventSubscription unsubscribeAll (toAny (parentsRef, changeSubdRef)) subscribed) occ


{-# NOINLINE [0] mergeInt #-}
{-# SPECIALIZE mergeInt :: DynamicS Global (PatchIntMap (Event Global a)) -> Event Global (IntMap a) #-}
mergeInt :: forall x a. (HasSpiderTimeline x) => DynamicS x (PatchIntMap (Event x a)) -> Event x (IntMap a)
mergeInt d = cacheEventImpl (mergeIntCheapImpl d)

{-# NOINLINE [0] mergeIntCheap #-}
{-# SPECIALIZE mergeIntCheap :: DynamicS Global (PatchIntMap (Event Global a)) -> Event Global (IntMap a) #-}
mergeIntCheap :: forall x a. (HasSpiderTimeline x) => DynamicS x (PatchIntMap (Event x a)) -> Event x (IntMap a)
mergeIntCheap = mergeIntCheapImpl

{-# INLINE mergeIntCheapImpl #-}
mergeIntCheapImpl :: forall x a. (HasSpiderTimeline x) => DynamicS x (PatchIntMap (Event x a)) -> Event x (IntMap a)
mergeIntCheapImpl = coerceEvent . genericMergeCheap unMergeSubscribedParent getInitialSubscribers updateMe
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
      getInitialSubscribers initialParents subscriber heightBagRef = do
        occurrencesRef <- liftIO $ newIORef IntMap.empty
        parentsRef <- liftIO $ newIORef IntMap.empty
        forM_ (IntMap.toList initialParents) $ \(k, e) -> do
          SubscribeResult subscription parentOcc <- subscribeMergeParent heightBagRef (eventOfIdentity e) (subscriber $ pure $ Const2 k)
          liftIO $ do
            for_ parentOcc $ \occurrence -> modifyIORef' occurrencesRef $ IntMap.insert k occurrence
            modifyIORef' parentsRef $ IntMap.insert k $ MergeSubscribedParent subscription
        occurrences <- liftIO $ readIORef occurrencesRef
        parents <- liftIO $ readIORef parentsRef
        pure (IntMapOf occurrences, IntMapOf parents)

newtype EventSelector x k = EventSelector { select :: forall a. k a -> Event x a }
newtype EventSelectorG x k v = EventSelectorG { selectG :: forall a. k a -> Event x (v a) }

{-# NOINLINE [1] fanG #-}
fanG :: (HasSpiderTimeline x, GCompare k) => Event x (DMap k v) -> EventSelectorG x k v
fanG e = unsafePerformIO $ do
  self <- newFan
  pure $ EventSelectorG $ \k -> eventFan k e self

{-# NOINLINE [1] fanInt #-}
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

initHold :: forall x. HasSpiderTimeline x => SomeHoldInit x -> EventM x ()
initHold (SomeHoldInit h) = do
  (liftIO $ readIORef $ holdParent h) >>= \case
    Just _ -> pure ()
    Nothing -> do
      -- Need to evaluate these so that we don't retain the Hold itself
      valueRef <- liftIO $ evaluate $ holdValue h
      invalidators <- liftIO $ evaluate $ holdInvalidators h
      let applyPatch patch = do
            old <- {-# SCC "read" #-} liftIO $ readIORef valueRef
            for_ ({-# SCC "apply" #-} apply patch old) $ \new -> do
              withIncreasedDepth (Proxy :: Proxy x) $
                tracePropagate (Proxy :: Proxy x) ("initHold: assigning Hold" <> showNodeId h)
              defer $ {-# SCC "assignment" #-} SomeAssignment valueRef invalidators new
      SubscribeResult subscription occ <- subscribeAndRead (holdEvent h) $ terminalSubscriber $ \a -> {-# SCC "traverseHold" #-} do
        when debugPropagate $ traceM (Proxy :: Proxy x) $ liftIO $ do
          InvalidatorList n _ _ <- readIORef invalidators
          return $ "SubscriberHold" <> showNodeId h <> ": " ++ show n
        applyPatch a
      for_ occ applyPatch
      liftIO $ writeIORef (holdParent h) $ Just subscription

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
#ifdef DEBUG_FRAME
  do toClear <- readIORef toClearRef
     toClearRoot <- readIORef toClearRootRef
     unless (null toClear && null toClearRoot) $
       error "clearEventEnv: a clear was scheduled after clearOccurrences"
#endif
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
  switchSubscriptionsToKill <- fmap catMaybes . forM toReconnect $ \(SomeSwitchSubscribed subscribed) -> {-# SCC "switchSubscribed" #-}
    readIORef (switchSubscribedCurrentParent subscribed) >>= \case
      SwitchParentUnsubscribed -> pure Nothing
      SwitchParentSubscribed oldSubscription -> do
        writeIORef (switchSubscribedBehaviorParents subscribed) []
        _ <- subscribeSwitchCurrentParent subscribed (eventEnvHoldInits env) subscribeReconnect
        return $ Just oldSubscription
  liftIO $ mapM_ unsubscribe mergeSubscriptionsToKill
  liftIO $ mapM_ unsubscribe switchSubscriptionsToKill
  forM_ toReconnect $ \(SomeSwitchSubscribed subscribed) -> {-# SCC "switchSubscribed" #-}
    readIORef (switchSubscribedCurrentParent subscribed) >>= \case
      SwitchParentUnsubscribed -> pure ()
      SwitchParentSubscribed parent -> do
        parentHeight <- eventSubscriptionHeight parent
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
  forM_ toReconnect $ \(SomeSwitchSubscribed subscribed) ->
    readIORef (switchSubscribedCurrentParent subscribed) >>= \case
      SwitchParentUnsubscribed -> pure ()
      SwitchParentSubscribed parent ->
        subscriberRecalculateHeight (switchSubscribedSubscriber subscribed) =<< eventSubscriptionHeight parent
#ifdef DEBUG_FRAME
  do leftover <- readIORef $ eventEnvPendingRecalculations env
     unless (null leftover) $
       error "runFrame: pending recalculations at frame end"
#endif
  return result
  where
    env = spiderEventEnv @x

    clearOccurrences :: IO ()
    clearOccurrences = do
      toClear <- readIORef $ eventEnvClears env
      forM_ toClear $ \(Some (Clear ref)) -> {-# SCC "clear" #-} writeIORef ref Nothing
      toClearRoot <- readIORef $ eventEnvRootClears env
      forM_ toClearRoot $ \(Some (RootClear ref)) -> {-# SCC "rootClear" #-} writeIORef ref $! DMap.empty
#ifdef DEBUG_FRAME
      -- Empty the clear lists, so that the DEBUG_FRAME check can verify that no
      -- clear was scheduled that would have been dropped silently.
      writeIORef (eventEnvClears env) []
      writeIORef (eventEnvRootClears env) []
#endif

    subscribeReconnect :: forall b. Event x b -> Subscriber x b -> IO (SubscribeResult x b)
    subscribeReconnect e sub = {-# SCC "subscribeSwitch" #-} do
#ifdef DEBUG
      pendingRecalculationsBefore <- length <$> readIORef (eventEnvPendingRecalculations env)
#endif
      result <- runEventM $
        subscribeAndRead e sub <* runHoldInits (eventEnvHoldInits env) (eventEnvDynInits env) (eventEnvMergeInits env)
      clearOccurrences
#ifdef DEBUG
      when (isJust $ subscribeOccurrence result) $ error "subscribeReconnect: the new parent has an occurrence, which should be impossible when no event is firing"
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

-- | Requests a height recalculation at the end of the current frame.
--
-- Call this from the node that invalidated its own height. Every invalidation
-- requires exactly one recalculation. Otherwise the height bags of the
-- subscribers stay one entry short and never complete again.
{-# INLINE deferRecalculateHeight #-}
deferRecalculateHeight :: forall x. HasSpiderTimeline x => SomeRecalculateHeight x -> IO ()
deferRecalculateHeight recalculate = modifyIORef' (eventEnvPendingRecalculations (spiderEventEnv @x)) (recalculate :)

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

calculateCoincidenceHeight :: CoincidenceSubscribed x a -> IO Height
calculateCoincidenceHeight subscribed = do
  outerHeight <- eventSubscriptionHeight =<< readIORef (coincidenceSubscribedOuterParent subscribed)
  innerHeight <- maybe (return zeroHeight) getEventNodeHeight =<< readIORef (coincidenceSubscribedInnerParent subscribed)
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
  -- hold and headE are INLINE in all four MonadHold instances, for the reason
  -- in the Reflex instance.
  {-# INLINE hold #-}
  hold = holdSpiderEventM
  {-# INLINABLE holdDyn #-}
  holdDyn = holdDynSpiderEventM
  {-# INLINABLE holdIncremental #-}
  holdIncremental = holdIncrementalSpiderEventM
  {-# INLINABLE buildDynamic #-}
  buildDynamic = buildDynamicSpiderEventM
  {-# INLINE headE #-}
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
  {-# INLINE hold #-}
  hold v0 (SpiderEvent e) = SpiderPushM $ SpiderBehavior <$> holdBehavior v0 e
  {-# INLINABLE holdDyn #-}
  holdDyn v0 (SpiderEvent e) = SpiderPushM $ fmap (SpiderDynamic . dynamicHold) $ Reflex.Spider.Internal.hold v0 $ coerce e
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 (SpiderEvent e) = SpiderPushM $ SpiderIncremental . dynamicHold <$> Reflex.Spider.Internal.hold v0 e
  {-# INLINABLE buildDynamic #-}
  buildDynamic getV0 (SpiderEvent e) = SpiderPushM $ fmap (SpiderDynamic . dynamicDyn) $ Reflex.Spider.Internal.buildDynamic (coerce getV0) $ coerce e
  {-# INLINE headE #-}
  headE (SpiderEvent e) = SpiderPushM $ SpiderEvent <$> Reflex.Spider.Internal.headE e
  {-# INLINABLE now #-}
  now = SpiderPushM nowSpiderEventM


instance HasSpiderTimeline x => Monad (Reflex.Class.Dynamic (SpiderTimeline x)) where
  {-# INLINE (>>=) #-}
  x >>= f = SpiderDynamic $ dynamicDyn $ newJoinDyn $ newMapDyn (unSpiderDynamic . f) $ unSpiderDynamic x
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
      eBoth = Reflex.Spider.Internal.coincidence $ SpiderEvent . dynamicUpdated . runIdentity <$> dynamicUpdated d
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

{-# INLINE holdSpiderEventM #-}
holdSpiderEventM :: HasSpiderTimeline x => a -> Reflex.Class.Event (SpiderTimeline x) a -> EventM x (Reflex.Class.Behavior (SpiderTimeline x) a)
holdSpiderEventM v0 e = fmap SpiderBehavior $ holdBehavior v0 $ unSpiderEvent e

holdDynSpiderEventM :: HasSpiderTimeline x => a -> Reflex.Class.Event (SpiderTimeline x) a -> EventM x (Reflex.Class.Dynamic (SpiderTimeline x) a)
holdDynSpiderEventM v0 e = fmap (SpiderDynamic . dynamicHold) $ Reflex.Spider.Internal.hold v0 $ coerce $ unSpiderEvent e

holdIncrementalSpiderEventM :: (HasSpiderTimeline x, Patch p) => PatchTarget p -> Reflex.Class.Event (SpiderTimeline x) p -> EventM x (Reflex.Class.Incremental (SpiderTimeline x) p)
holdIncrementalSpiderEventM v0 e = fmap (SpiderIncremental . dynamicHold) $ Reflex.Spider.Internal.hold v0 $ unSpiderEvent e

buildDynamicSpiderEventM :: HasSpiderTimeline x => SpiderPushM x a -> Reflex.Class.Event (SpiderTimeline x) a -> EventM x (Reflex.Class.Dynamic (SpiderTimeline x) a)
buildDynamicSpiderEventM getV0 e = fmap (SpiderDynamic . dynamicDyn) $ Reflex.Spider.Internal.buildDynamic (coerce getV0) $ coerce $ unSpiderEvent e

instance HasSpiderTimeline x => Reflex.Class.MonadSample (SpiderTimeline x) (SpiderHostFrame x) where
  {-# SPECIALIZE instance Reflex.Class.MonadSample (SpiderTimeline Global) (SpiderHostFrame Global) #-}
  sample = SpiderHostFrame . readBehaviorUntracked . unSpiderBehavior --TODO: This can cause problems with laziness, so we should get rid of it if we can

instance HasSpiderTimeline x => Reflex.Class.MonadHold (SpiderTimeline x) (SpiderHostFrame x) where
  {-# SPECIALIZE instance Reflex.Class.MonadHold (SpiderTimeline Global) (SpiderHostFrame Global) #-}
  {-# INLINE hold #-}
  hold v0 e = SpiderHostFrame $ fmap SpiderBehavior $ holdBehavior v0 $ unSpiderEvent e
  {-# INLINABLE holdDyn #-}
  holdDyn v0 e = SpiderHostFrame $ fmap (SpiderDynamic . dynamicHold) $ Reflex.Spider.Internal.hold v0 $ coerce $ unSpiderEvent e
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 e = SpiderHostFrame $ fmap (SpiderIncremental . dynamicHold) $ Reflex.Spider.Internal.hold v0 $ unSpiderEvent e
  {-# INLINABLE buildDynamic #-}
  buildDynamic getV0 e = SpiderHostFrame $ fmap (SpiderDynamic . dynamicDyn) $ Reflex.Spider.Internal.buildDynamic (coerce getV0) $ coerce $ unSpiderEvent e
  {-# INLINE headE #-}
  headE (SpiderEvent e) = SpiderHostFrame $ SpiderEvent <$> Reflex.Spider.Internal.headE e
  {-# INLINABLE now #-}
  now = SpiderHostFrame Reflex.Class.now

instance HasSpiderTimeline x => Reflex.Class.MonadSample (SpiderTimeline x) (Reflex.Spider.Internal.ReadPhase x) where
  {-# SPECIALIZE instance Reflex.Class.MonadSample (SpiderTimeline Global) (Reflex.Spider.Internal.ReadPhase Global) #-}
  {-# INLINABLE sample #-}
  sample = Reflex.Spider.Internal.ReadPhase . Reflex.Class.sample

instance HasSpiderTimeline x => Reflex.Class.MonadHold (SpiderTimeline x) (Reflex.Spider.Internal.ReadPhase x) where
  {-# SPECIALIZE instance Reflex.Class.MonadHold (SpiderTimeline Global) (Reflex.Spider.Internal.ReadPhase Global) #-}
  {-# INLINE hold #-}
  hold v0 e = Reflex.Spider.Internal.ReadPhase $ Reflex.Class.hold v0 e
  {-# INLINABLE holdDyn #-}
  holdDyn v0 e = Reflex.Spider.Internal.ReadPhase $ Reflex.Class.holdDyn v0 e
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 e = Reflex.Spider.Internal.ReadPhase $ Reflex.Class.holdIncremental v0 e
  {-# INLINABLE buildDynamic #-}
  buildDynamic getV0 e = Reflex.Spider.Internal.ReadPhase $ Reflex.Class.buildDynamic getV0 e
  {-# INLINE headE #-}
  headE e = Reflex.Spider.Internal.ReadPhase $ Reflex.Class.headE e
  {-# INLINABLE now #-}
  now = Reflex.Spider.Internal.ReadPhase Reflex.Class.now

instance HasSpiderTimeline x => Reflex.Host.Class.MonadSubscribeEvent (SpiderTimeline x) (SpiderHostFrame x) where
  {-# INLINABLE subscribeEvent #-}
  subscribeEvent e = SpiderHostFrame $ do
    --TODO: Unsubscribe eventually (manually and/or with weak ref)
    val <- liftIO $ newIORef Nothing
    let recordOccurrence = setOccurrence "subscribeEvent" val
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

runHostFrameFireAndRead :: forall x a b. HasSpiderTimeline x => EventM x a -> (a -> EventM x [DSum (RootTrigger x) Identity]) -> (a -> EventM x b) -> SpiderHost x b
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

newtype SpiderPushM (x :: Type) a = SpiderPushM (EventM x a) deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

instance HasSpiderTimeline x => R.Reflex (SpiderTimeline x) where
  {-# SPECIALIZE instance R.Reflex (SpiderTimeline Global) #-}
  -- The methods that build events are INLINE, because in the SPECIALIZE
  -- dictionary the cached primitives are already specialized and the rewrite
  -- rules don't match them.
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
  {-# INLINE fanG #-}
  fanG e = R.EventSelectorG $ SpiderEvent . selectG (fanG (unSpiderEvent e))
  {-# INLINE mergeG #-}
  mergeG
    :: forall k2 (k :: k2 -> Type) q (v :: k2 -> Type). GCompare k
    => (forall a. q a -> R.Event (SpiderTimeline x) (v a))
    -> DMap k q
    -> R.Event (SpiderTimeline x) (DMap k v)
  mergeG nt = SpiderEvent . mergeG (unSpiderEvent #. nt) . dynamicConst
  {-# INLINE switch #-}
  switch = SpiderEvent . switch . (coerce :: Behavior x (R.Event (SpiderTimeline x) a) -> Behavior x (Event x a)) . unSpiderBehavior
  {-# INLINE coincidence #-}
  coincidence = SpiderEvent . coincidence . unSpiderEvent
  {-# INLINABLE current #-}
  current = SpiderBehavior . dynamicCurrent . unSpiderDynamic
  {-# INLINE updated #-}
  updated = SpiderEvent #. dynamicUpdated .# fmap coerce . unSpiderDynamic
  {-# INLINABLE unsafeBuildDynamic #-}
  unsafeBuildDynamic readV0 v' = SpiderDynamic $ dynamicDyn $ unsafeBuildDynamic (coerce readV0) $ coerce $ unSpiderEvent v'
  {-# INLINABLE unsafeBuildIncremental #-}
  unsafeBuildIncremental readV0 dv = SpiderIncremental $ dynamicDyn $ unsafeBuildDynamic (coerce readV0) $ unSpiderEvent dv
  {-# INLINE mergeIncrementalG #-}
  mergeIncrementalG nt = SpiderEvent #. mergeG (coerce #. nt) .# unSpiderIncremental
  {-# INLINE mergeIncrementalWithMoveG #-}
  mergeIncrementalWithMoveG nt = SpiderEvent #. mergeWithMove (coerce #. nt) .# unSpiderIncremental
  {-# INLINABLE currentIncremental #-}
  currentIncremental = SpiderBehavior . dynamicCurrent . unSpiderIncremental
  {-# INLINE updatedIncremental #-}
  updatedIncremental = SpiderEvent . dynamicUpdated . unSpiderIncremental
  {-# INLINABLE incrementalToDynamic #-}
  incrementalToDynamic (SpiderIncremental i) = SpiderDynamic $ dynamicDyn $ unsafeBuildDynamic (readBehaviorUntracked $ dynamicCurrent i) $ flip push (dynamicUpdated i) $ \p -> do
    c <- readBehaviorUntracked $ dynamicCurrent i
    return $ Identity <$> apply p c --TODO: Avoid the redundant 'apply'
  eventCoercion Coercion = Coercion
  behaviorCoercion Coercion = Coercion
  dynamicCoercion Coercion = Coercion
  incrementalCoercion Coercion Coercion = Coercion
  {-# INLINE mergeIntIncremental #-}
  mergeIntIncremental = SpiderEvent . mergeInt . coerce
  {-# INLINE fanInt #-}
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
  node <- mkEventNode zeroHeight (pure []) (return ["root"])
  let !r = Root
        { rootOccurrence = occRef
        , rootSubscribed = subscribedRef
        , rootInit = f
        , rootNode = node
        }
  return $ EventSelector $ \k -> eventRoot k r

newtype ReadPhase x a = ReadPhase (EventM x a) deriving (Functor, Applicative, Monad, MonadFix)

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
