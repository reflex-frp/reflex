{-# LANGUAGE CPP, ExistentialQuantification, GADTs, ScopedTypeVariables, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, RankNTypes, BangPatterns, UndecidableInstances, EmptyDataDecls, RecursiveDo, RoleAnnotations, LambdaCase #-}
module Reflex.Spider.Internal where

import Prelude hiding (mapM, mapM_, any, sequence, concat)

import qualified Reflex.Class as R
import qualified Reflex.Host.Class as R

import Data.IORef
import System.Mem.Weak
import Data.Foldable
import Data.Traversable
import Control.Monad hiding (mapM, mapM_, forM_, forM, sequence)
import Control.Monad.Reader hiding (mapM, mapM_, forM_, forM, sequence)
import GHC.Exts
import Control.Applicative
import Data.Dependent.Map (DMap, DSum (..))
import qualified Data.Dependent.Map as DMap
import Data.GADT.Compare
import Data.Functor.Misc
import Data.Maybe
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Control.Lens hiding (coerce)
import Control.Monad.Ref
import Data.Monoid ((<>))

import System.IO.Unsafe
import Unsafe.Coerce
import Control.Monad.Primitive

debugPropagate :: Bool

debugInvalidateHeight :: Bool

#ifdef DEBUG

#define DEBUG_NODEIDS

debugPropagate = True

debugInvalidateHeight = True

class HasNodeId a where
  getNodeId :: a -> Int

instance HasNodeId (Hold a) where
  getNodeId = holdNodeId

instance HasNodeId (PushSubscribed a b) where
  getNodeId = pushSubscribedNodeId

instance HasNodeId (SwitchSubscribed a) where
  getNodeId = switchSubscribedNodeId

instance HasNodeId (MergeSubscribed a) where
  getNodeId = mergeSubscribedNodeId

instance HasNodeId (FanSubscribed a) where
  getNodeId = fanSubscribedNodeId

instance HasNodeId (CoincidenceSubscribed a) where
  getNodeId = coincidenceSubscribedNodeId

instance HasNodeId (RootSubscribed a) where
  getNodeId = rootSubscribedNodeId

showNodeId :: HasNodeId a => a -> String
showNodeId = ("#"<>) . show . getNodeId

#else

debugPropagate = False

debugInvalidateHeight = False

showNodeId :: a -> String
showNodeId = const ""

#endif

#ifdef DEBUG_NODEIDS
{-# NOINLINE nextNodeIdRef #-}
nextNodeIdRef :: IORef Int
nextNodeIdRef = unsafePerformIO $ newIORef 1

{-# NOINLINE unsafeNodeId #-}
unsafeNodeId :: a -> Int
unsafeNodeId a = unsafePerformIO $ do
  touch a
  atomicModifyIORef' nextNodeIdRef $ \n -> (succ n, n)
#endif

--TODO: Figure out why certain things are not 'representational', then make them representational so we can use coerce
--type role Hold representational
data Hold a
   = Hold { holdValue :: !(IORef a)
          , holdInvalidators :: !(IORef [Weak Invalidator])
            -- We need to use 'Any' for the next two things, because otherwise Hold inherits a nominal role for its 'a' parameter, and we want to be able to use 'coerce'
          , holdSubscriber :: !(IORef Any) -- Keeps its subscription alive; for some reason, a regular (or strict) reference to the Subscriber itself wasn't working, so had to use an IORef
          , holdParent :: !(IORef Any) -- Keeps its parent alive (will be undefined until the hold is initialized --TODO: Probably shouldn't be an IORef
#ifdef DEBUG_NODEIDS
          , holdNodeId :: Int
#endif
          }

data EventEnv
   = EventEnv { eventEnvAssignments :: !(IORef [SomeAssignment])
              , eventEnvHoldInits :: !(IORef [SomeHoldInit])
              , eventEnvClears :: !(IORef [SomeMaybeIORef])
              , eventEnvCurrentHeight :: !(IORef Int)
              , eventEnvCoincidenceInfos :: !(IORef [SomeCoincidenceInfo])
              , eventEnvDelayedMerges :: !(IORef (IntMap [DelayedMerge]))
              }

runEventM :: EventM a -> EventEnv -> IO a
runEventM = runReaderT . unEventM

askToAssignRef :: EventM (IORef [SomeAssignment])
askToAssignRef = EventM $ asks eventEnvAssignments

askHoldInitRef :: EventM (IORef [SomeHoldInit])
askHoldInitRef = EventM $ asks eventEnvHoldInits

getCurrentHeight :: EventM Int
getCurrentHeight = EventM $ do
  heightRef <- asks eventEnvCurrentHeight
  liftIO $ readIORef heightRef

putCurrentHeight :: Int -> EventM ()
putCurrentHeight h = EventM $ do
  heightRef <- asks eventEnvCurrentHeight
  liftIO $ writeIORef heightRef h

scheduleClear :: IORef (Maybe a) -> EventM ()
scheduleClear r = EventM $ do
  clears <- asks eventEnvClears
  liftIO $ modifyIORef' clears (SomeMaybeIORef r :)

scheduleMerge :: Int -> MergeSubscribed a -> EventM ()
scheduleMerge height subscribed = EventM $ do
  delayedRef <- asks eventEnvDelayedMerges
  liftIO $ modifyIORef' delayedRef $ IntMap.insertWith (++) height [DelayedMerge subscribed]

emitCoincidenceInfo :: SomeCoincidenceInfo -> EventM ()
emitCoincidenceInfo sci = EventM $ do
  ciRef <- asks eventEnvCoincidenceInfos
  liftIO $ modifyIORef' ciRef (sci:)

-- Note: hold cannot examine its event until after the phase is over
hold :: a -> Event a -> EventM (Behavior a)
hold v0 e = do
  holdInitRef <- askHoldInitRef
  liftIO $ do
    valRef <- newIORef v0
    invsRef <- newIORef []
    parentRef <- newIORef $ error "hold not yet initialized (parent)"
    subscriberRef <- newIORef $ error "hold not yet initialized (subscriber)"
    let h = Hold
          { holdValue = valRef
          , holdInvalidators = invsRef
          , holdSubscriber = subscriberRef
          , holdParent = parentRef
#ifdef DEBUG_NODEIDS
          , holdNodeId = unsafeNodeId (v0, e)
#endif
          }
        !s = SubscriberHold h
    writeIORef subscriberRef $ unsafeCoerce s
    modifyIORef' holdInitRef (SomeHoldInit e h :)
    return $ BehaviorHold h

subscribeHold :: Event a -> Hold a -> EventM ()
subscribeHold e h = do
  toAssignRef <- askToAssignRef
  !s <- liftIO $ liftM unsafeCoerce $ readIORef $ holdSubscriber h -- This must be performed strictly so that the weak pointer points at the actual item
  ws <- liftIO $ mkWeakPtrWithDebug s "holdSubscriber"
  subd <- subscribe e $ WeakSubscriberSimple ws
  liftIO $ writeIORef (holdParent h) $ unsafeCoerce subd
  occ <- liftIO $ getEventSubscribedOcc subd
  case occ of
    Nothing -> return ()
    Just o -> liftIO $ modifyIORef' toAssignRef (SomeAssignment h o :)

--type role BehaviorM representational
-- BehaviorM can sample behaviors
newtype BehaviorM a = BehaviorM { unBehaviorM :: ReaderT (Maybe (Weak Invalidator, IORef [SomeBehaviorSubscribed])) IO a } deriving (Functor, Applicative, Monad, MonadIO, MonadFix)

data BehaviorSubscribed a
   = BehaviorSubscribedHold (Hold a)
   | BehaviorSubscribedPull (PullSubscribed a)

data SomeBehaviorSubscribed = forall a. SomeBehaviorSubscribed (BehaviorSubscribed a)

--type role PullSubscribed representational
data PullSubscribed a
   = PullSubscribed { pullSubscribedValue :: !a
                    , pullSubscribedInvalidators :: !(IORef [Weak Invalidator])
                    , pullSubscribedOwnInvalidator :: !Invalidator
                    , pullSubscribedParents :: ![SomeBehaviorSubscribed] -- Need to keep parent behaviors alive, or they won't let us know when they're invalidated
                    }

--type role Pull representational
data Pull a
   = Pull { pullValue :: !(IORef (Maybe (PullSubscribed a)))
          , pullCompute :: !(BehaviorM a)
          }

data Invalidator
   = forall a. InvalidatorPull (Pull a)
   | forall a. InvalidatorSwitch (SwitchSubscribed a)

data RootSubscribed a
   = RootSubscribed { rootSubscribedSubscribers :: !(IORef [WeakSubscriber a])
                    , rootSubscribedOccurrence :: !(IORef (Maybe a)) -- Alias to rootOccurrence
                    }

data Root a
   = Root { rootOccurrence :: !(IORef (Maybe a)) -- The currently-firing occurrence of this event
          , rootSubscribed :: !(IORef (Maybe (RootSubscribed a)))
          , rootInit :: !(RootTrigger a -> IO (IO ()))
          }

data SomeHoldInit = forall a. SomeHoldInit (Event a) (Hold a)

-- EventM can do everything BehaviorM can, plus create holds
newtype EventM a = EventM { unEventM :: ReaderT EventEnv IO a } deriving (Functor, Applicative, Monad, MonadFix, MonadIO) -- The environment should be Nothing if we are not in a frame, and Just if we are - in which case it is a list of assignments to be done after the frame is over

data PushSubscribed a b
   = PushSubscribed { pushSubscribedOccurrence :: !(IORef (Maybe b)) -- If the current height is less than our height, this should always be Nothing; during our height, this will get filled in at some point, always before our children are notified; after our height, this will be filled in with the correct value (Nothing if we are not firing, Just if we are)
                    , pushSubscribedHeight :: !(IORef Int)
                    , pushSubscribedSubscribers :: !(IORef [WeakSubscriber b])
                    , pushSubscribedSelf :: !(Subscriber a) -- Hold this in memory to ensure our WeakReferences don't die
                    , pushSubscribedParent :: !(EventSubscribed a)
#ifdef DEBUG_NODEIDS
                    , pushSubscribedNodeId :: Int
#endif
                    }

data Push a b
   = Push { pushCompute :: !(a -> EventM (Maybe b)) -- Compute the current firing value; assumes that its parent has been computed
          , pushParent :: !(Event a)
          , pushSubscribed :: !(IORef (Maybe (PushSubscribed a b))) --TODO: Can we replace this with an unsafePerformIO thunk?
          }

data MergeSubscribed k
   = MergeSubscribed { mergeSubscribedOccurrence :: !(IORef (Maybe (DMap k)))
                     , mergeSubscribedAccum :: !(IORef (DMap k)) -- This will accumulate occurrences until our height is reached, at which point it will be transferred to mergeSubscribedOccurrence
                     , mergeSubscribedHeight :: !(IORef Int)
                     , mergeSubscribedSubscribers :: !(IORef [WeakSubscriber (DMap k)])
                     , mergeSubscribedSelf :: !Any -- Hold all our Subscribers in memory
                     , mergeSubscribedParents :: !(DMap (WrapArg EventSubscribed k))
#ifdef DEBUG_NODEIDS
                     , mergeSubscribedNodeId :: Int
#endif
                     }

--TODO: DMap sucks; we should really write something better (with a functor for the value as well as the key)
data Merge k
   = Merge { mergeParents :: !(DMap (WrapArg Event k))
           , mergeSubscribed :: !(IORef (Maybe (MergeSubscribed k))) --TODO: Can we replace this with an unsafePerformIO thunk?
           }

data FanSubscriberKey k a where
  FanSubscriberKey :: k a -> FanSubscriberKey k [WeakSubscriber a]

instance GEq k => GEq (FanSubscriberKey k) where
  geq (FanSubscriberKey a) (FanSubscriberKey b) = case geq a b of
    Nothing -> Nothing
    Just Refl -> Just Refl

instance GCompare k => GCompare (FanSubscriberKey k) where
  gcompare (FanSubscriberKey a) (FanSubscriberKey b) = case gcompare a b of
    GLT -> GLT
    GEQ -> GEQ
    GGT -> GGT

data FanSubscribed k
   = FanSubscribed { fanSubscribedSubscribers :: !(IORef (DMap (FanSubscriberKey k)))
                   , fanSubscribedParent :: !(EventSubscribed (DMap k))
                   , fanSubscribedSelf :: !(Subscriber (DMap k))
#ifdef DEBUG_NODEIDS
                   , fanSubscribedNodeId :: Int
#endif
                   }

data Fan k
   = Fan { fanParent :: !(Event (DMap k))
         , fanSubscribed :: !(IORef (Maybe (FanSubscribed k)))
         }

data SwitchSubscribed a
   = SwitchSubscribed { switchSubscribedOccurrence :: !(IORef (Maybe a))
                      , switchSubscribedHeight :: !(IORef Int)
                      , switchSubscribedSubscribers :: !(IORef [WeakSubscriber a])
                      , switchSubscribedSelf :: !(Subscriber a)
                      , switchSubscribedSelfWeak :: !(IORef (Weak (Subscriber a)))
                      , switchSubscribedOwnInvalidator :: !Invalidator
                      , switchSubscribedOwnWeakInvalidator :: !(IORef (Weak Invalidator))
                      , switchSubscribedBehaviorParents :: !(IORef [SomeBehaviorSubscribed])
                      , switchSubscribedParent :: !(Behavior (Event a))
                      , switchSubscribedCurrentParent :: !(IORef (EventSubscribed a))
#ifdef DEBUG_NODEIDS
                      , switchSubscribedNodeId :: Int
#endif
                      }

data Switch a
   = Switch { switchParent :: !(Behavior (Event a))
            , switchSubscribed :: !(IORef (Maybe (SwitchSubscribed a)))
            }

data CoincidenceSubscribed a
   = CoincidenceSubscribed { coincidenceSubscribedOccurrence :: !(IORef (Maybe a))
                           , coincidenceSubscribedSubscribers :: !(IORef [WeakSubscriber a])
                           , coincidenceSubscribedHeight :: !(IORef Int)
                           , coincidenceSubscribedOuter :: !(Subscriber (Event a))
                           , coincidenceSubscribedOuterParent :: !(EventSubscribed (Event a))
                           , coincidenceSubscribedInnerParent :: !(IORef (Maybe (EventSubscribed a)))
#ifdef DEBUG_NODEIDS
                           , coincidenceSubscribedNodeId :: Int
#endif
                           }

data Coincidence a
   = Coincidence { coincidenceParent :: !(Event (Event a))
                 , coincidenceSubscribed :: !(IORef (Maybe (CoincidenceSubscribed a)))
                 }

data Box a = Box { unBox :: a }

--type WeakSubscriber a = Weak (Subscriber a)
data WeakSubscriber a
   = forall k. GCompare k => WeakSubscriberMerge !(k a) !(Weak (Box (MergeSubscribed k))) --TODO: Can we inline the GCompare?
   | WeakSubscriberSimple !(Weak (Subscriber a))

showWeakSubscriberType :: WeakSubscriber a -> String
showWeakSubscriberType = \case
  WeakSubscriberMerge _ _ -> "WeakSubscriberMerge"
  WeakSubscriberSimple _ -> "WeakSubscriberSimple"

deRefWeakSubscriber :: WeakSubscriber a -> IO (Maybe (Subscriber a))
deRefWeakSubscriber ws = case ws of
  WeakSubscriberSimple w -> deRefWeak w
  WeakSubscriberMerge k w -> liftM (fmap $ SubscriberMerge k . unBox) $ deRefWeak w

data Subscriber a
   = forall b. SubscriberPush !(a -> EventM (Maybe b)) (PushSubscribed a b)
   | forall k. GCompare k => SubscriberMerge !(k a) (MergeSubscribed k) --TODO: Can we inline the GCompare?
   | forall k. (GCompare k, a ~ DMap k) => SubscriberFan (FanSubscribed k)
   | SubscriberHold !(Hold a)
   | SubscriberSwitch (SwitchSubscribed a)
   | forall b. a ~ Event b => SubscriberCoincidenceOuter (CoincidenceSubscribed b)
   | SubscriberCoincidenceInner (CoincidenceSubscribed a)

showSubscriberType :: Subscriber a -> String
showSubscriberType = \case
  SubscriberPush _ _ -> "SubscriberPush"
  SubscriberMerge _ _ -> "SubscriberMerge"
  SubscriberFan _ -> "SubscriberFan"
  SubscriberHold _ -> "SubscriberHold"
  SubscriberSwitch _ -> "SubscriberSwitch"
  SubscriberCoincidenceOuter _ -> "SubscriberCoincidenceOuter"
  SubscriberCoincidenceInner _ -> "SubscriberCoincidenceInner"

data Event a
   = EventRoot !(Root a)
   | EventNever
   | forall b. EventPush !(Push b a)
   | forall k. (GCompare k, a ~ DMap k) => EventMerge !(Merge k)
   | forall k. GCompare k => EventFan !(k a) !(Fan k)
   | EventSwitch !(Switch a)
   | EventCoincidence !(Coincidence a)

showEventType :: Event a -> String
showEventType = \case
  EventRoot _ -> "EventRoot"
  EventNever -> "EventNever"
  EventPush _ -> "EventPush"
  EventMerge _ -> "EventMerge"
  EventFan _ _ -> "EventFan"
  EventSwitch _ -> "EventSwitch"
  EventCoincidence _ -> "EventCoincidence"

data EventSubscribed a
   = EventSubscribedRoot !(RootSubscribed a)
   | EventSubscribedNever
   | forall b. EventSubscribedPush !(PushSubscribed b a)
   | forall k. (GCompare k, a ~ DMap k) => EventSubscribedMerge !(MergeSubscribed k)
   | forall k. GCompare k => EventSubscribedFan !(k a) !(FanSubscribed k)
   | EventSubscribedSwitch !(SwitchSubscribed a)
   | EventSubscribedCoincidence !(CoincidenceSubscribed a)

--type role Behavior representational
data Behavior a
   = BehaviorHold !(Hold a)
   | BehaviorConst !a
   | BehaviorPull !(Pull a)

-- ResultM can read behaviors and events
type ResultM = EventM

{-# NOINLINE unsafeNewIORef #-}
unsafeNewIORef :: a -> b -> IORef b
unsafeNewIORef _ b = unsafePerformIO $ newIORef b

instance Functor Event where
  fmap f = push $ return . Just . f

instance Functor Behavior where
  fmap f = pull . liftM f . readBehaviorTracked

{-# NOINLINE push #-} --TODO: If this is helpful, we can get rid of the unsafeNewIORef and use unsafePerformIO directly
push :: (a -> EventM (Maybe b)) -> Event a -> Event b
push f e = EventPush $ Push
  { pushCompute = f
  , pushParent = e
  , pushSubscribed = unsafeNewIORef (f, e) Nothing --TODO: Does the use of the tuple here create unnecessary overhead?
  }
{-# RULES "push/push" forall f g e. push f (push g e) = push (maybe (return Nothing) f <=< g) e #-}

{-# NOINLINE pull #-}
pull :: BehaviorM a -> Behavior a
pull a = BehaviorPull $ Pull
  { pullCompute = a
  , pullValue = unsafeNewIORef a Nothing
  }
{-# RULES "pull/pull" forall a. pull (readBehaviorTracked (pull a)) = pull a #-}

{-# NOINLINE switch #-}
switch :: Behavior (Event a) -> Event a
switch a = EventSwitch $ Switch
  { switchParent = a
  , switchSubscribed = unsafeNewIORef a Nothing
  }
{-# RULES "switch/constB" forall e. switch (BehaviorConst e) = e #-}

coincidence :: Event (Event a) -> Event a
coincidence a = EventCoincidence $ Coincidence
  { coincidenceParent = a
  , coincidenceSubscribed = unsafeNewIORef a Nothing
  }

newRoot :: IO (Root a)
newRoot = do
  occRef <- newIORef Nothing
  subscribedRef <- newIORef Nothing
  return $ Root
    { rootOccurrence = occRef
    , rootSubscribed = subscribedRef
    , rootInit = const $ return $ return ()
    }

propagateAndUpdateSubscribersRef :: IORef [WeakSubscriber a] -> a -> EventM ()
propagateAndUpdateSubscribersRef subscribersRef a = do
  subscribers <- liftIO $ readIORef subscribersRef
  liftIO $ writeIORef subscribersRef []
  stillAlive <- propagate a subscribers
  liftIO $ modifyIORef' subscribersRef (++stillAlive)

-- Propagate the given event occurrence; before cleaning up, run the given action, which may read the state of events and behaviors
run :: [DSum RootTrigger] -> ResultM b -> IO b
run roots after = do
  when debugPropagate $ putStrLn "Running an event frame"
  result <- runFrame $ do
    forM_ roots $ \(RootTrigger (_, occRef) :=> a) -> do
      liftIO $ writeIORef occRef $ Just a
      scheduleClear occRef
    forM_ roots $ \(RootTrigger (subscribersRef, _) :=> a) -> do
      propagateAndUpdateSubscribersRef subscribersRef a
    delayedRef <- EventM $ asks eventEnvDelayedMerges
    let go = do
          delayed <- liftIO $ readIORef delayedRef
          case IntMap.minViewWithKey delayed of
            Nothing -> return ()
            Just ((currentHeight, current), future) -> do
              when debugPropagate $ liftIO $ putStrLn $ "Running height " ++ show currentHeight
              putCurrentHeight currentHeight
              liftIO $ writeIORef delayedRef future
              forM_ current $ \d -> case d of
                DelayedMerge subscribed -> do
                  height <- liftIO $ readIORef $ mergeSubscribedHeight subscribed
                  case height `compare` currentHeight of
                    LT -> error "Somehow a merge's height has been decreased after it was scheduled"
                    GT -> scheduleMerge height subscribed -- The height has been increased (by a coincidence event; TODO: is this the only way?)
                    EQ -> do
                      m <- liftIO $ readIORef $ mergeSubscribedAccum subscribed
                      liftIO $ writeIORef (mergeSubscribedAccum subscribed) DMap.empty
                      --TODO: Assert that m is not empty
                      liftIO $ writeIORef (mergeSubscribedOccurrence subscribed) $ Just m
                      scheduleClear $ mergeSubscribedOccurrence subscribed
                      propagateAndUpdateSubscribersRef (mergeSubscribedSubscribers subscribed) m
              go
    go
    putCurrentHeight maxBound
    after
  when debugPropagate $ putStrLn "Done running an event frame"
  return result

data SomeMaybeIORef = forall a. SomeMaybeIORef (IORef (Maybe a))

data SomeAssignment = forall a. SomeAssignment (Hold a) a

data DelayedMerge = forall k. DelayedMerge (MergeSubscribed k)

debugFinalize :: Bool
debugFinalize = False

mkWeakPtrWithDebug :: a -> String -> IO (Weak a)
mkWeakPtrWithDebug x debugNote = mkWeakPtr x $
  if debugFinalize
  then Just $ putStrLn $ "finalizing: " ++ debugNote
  else Nothing

type WeakList a = [Weak a]

--TODO: Is it faster to clean up every time, or to occasionally go through and clean up as needed?
traverseAndCleanWeakList_ :: Monad m => (wa -> m (Maybe a)) -> [wa] -> (a -> m ()) -> m [wa]
traverseAndCleanWeakList_ deRef ws f = go ws
  where go [] = return []
        go (h:t) = do
          ma <- deRef h
          case ma of
            Just a -> do
              f a
              t' <- go t
              return $ h : t'
            Nothing -> go t

-- | Propagate everything at the current height
propagate :: a -> [WeakSubscriber a] -> EventM [WeakSubscriber a]
propagate a subscribers = do
  traverseAndCleanWeakList_ (liftIO . deRefWeakSubscriber) subscribers $ \s -> case s of
    SubscriberPush compute subscribed -> do
      when debugPropagate $ liftIO $ putStrLn $ "SubscriberPush" <> showNodeId subscribed
      occ <- compute a
      case occ of
        Nothing -> return () -- No need to write a Nothing back into the Ref
        Just o -> do
          liftIO $ writeIORef (pushSubscribedOccurrence subscribed) occ
          scheduleClear $ pushSubscribedOccurrence subscribed
          liftIO . writeIORef (pushSubscribedSubscribers subscribed) =<< propagate o =<< liftIO (readIORef (pushSubscribedSubscribers subscribed))
    SubscriberMerge k subscribed -> do
      when debugPropagate $ liftIO $ putStrLn $ "SubscriberMerge" <> showNodeId subscribed
      oldM <- liftIO $ readIORef $ mergeSubscribedAccum subscribed
      liftIO $ writeIORef (mergeSubscribedAccum subscribed) $ DMap.insertWith (error "Same key fired multiple times for") k a oldM
      when (DMap.null oldM) $ do -- Only schedule the firing once
        height <- liftIO $ readIORef $ mergeSubscribedHeight subscribed
        --TODO: assertions about height
        currentHeight <- getCurrentHeight
        when (height <= currentHeight) $ error $ "Height (" ++ show height ++ ") is not greater than current height (" ++ show currentHeight ++ ")"
        scheduleMerge height subscribed
    SubscriberFan subscribed -> do
      subs <- liftIO $ readIORef $ fanSubscribedSubscribers subscribed
      when debugPropagate $ liftIO $ putStrLn $ "SubscriberFan" <> showNodeId subscribed <> ": " ++ show (DMap.size subs) ++ " keys subscribed, " ++ show (DMap.size a) ++ " keys firing"
      --TODO: We need a better DMap intersection; here, we are assuming that the number of firing keys is small and the number of subscribers is large
      forM_ (DMap.toList a) $ \(k :=> v) -> case DMap.lookup (FanSubscriberKey k) subs of
        Nothing -> do
          when debugPropagate $ liftIO $ putStrLn "No subscriber for key"
          return ()
        Just subsubs -> do
          _ <- propagate v subsubs --TODO: use the value of this
          return ()
      --TODO: The following is way too slow to do all the time
      subs' <- liftIO $ forM (DMap.toList subs) $ ((\(FanSubscriberKey k :=> subsubs) -> do
        subsubs' <- traverseAndCleanWeakList_ (liftIO . deRefWeakSubscriber) subsubs (const $ return ())
        return $ if null subsubs' then Nothing else Just $ FanSubscriberKey k :=> subsubs') :: DSum (FanSubscriberKey k) -> IO (Maybe (DSum (FanSubscriberKey k))))
      liftIO $ writeIORef (fanSubscribedSubscribers subscribed) $ DMap.fromDistinctAscList $ catMaybes subs'
    SubscriberHold h -> do
      invalidators <- liftIO $ readIORef $ holdInvalidators h
      when debugPropagate $ liftIO $ putStrLn $ "SubscriberHold" <> showNodeId h <> ": " ++ show (length invalidators)
      toAssignRef <- askToAssignRef
      liftIO $ modifyIORef' toAssignRef (SomeAssignment h a :)
    SubscriberSwitch subscribed -> do
      when debugPropagate $ liftIO $ putStrLn $ "SubscriberSwitch" <> showNodeId subscribed
      liftIO $ writeIORef (switchSubscribedOccurrence subscribed) $ Just a
      scheduleClear $ switchSubscribedOccurrence subscribed
      subs <- liftIO $ readIORef $ switchSubscribedSubscribers subscribed
      liftIO . writeIORef (switchSubscribedSubscribers subscribed) =<< propagate a subs
    SubscriberCoincidenceOuter subscribed -> do
      when debugPropagate $ liftIO $ putStrLn $ "SubscriberCoincidenceOuter" <> showNodeId subscribed
      outerHeight <- liftIO $ readIORef $ coincidenceSubscribedHeight subscribed
      when debugPropagate $ liftIO $ putStrLn $ "  outerHeight = " <> show outerHeight
      (occ, innerHeight, innerSubd) <- subscribeCoincidenceInner a outerHeight subscribed
      when debugPropagate $ liftIO $ putStrLn $ "  isJust occ = " <> show (isJust occ)
      when debugPropagate $ liftIO $ putStrLn $ "  innerHeight = " <> show innerHeight
      liftIO $ writeIORef (coincidenceSubscribedInnerParent subscribed) $ Just innerSubd
      scheduleClear $ coincidenceSubscribedInnerParent subscribed
      case occ of
        Nothing -> do
          when (innerHeight > outerHeight) $ liftIO $ do -- If the event fires, it will fire at a later height
            writeIORef (coincidenceSubscribedHeight subscribed) innerHeight
            mapM_ invalidateSubscriberHeight =<< readIORef (coincidenceSubscribedSubscribers subscribed)
            mapM_ recalculateSubscriberHeight =<< readIORef (coincidenceSubscribedSubscribers subscribed)
        Just o -> do -- Since it's already firing, no need to adjust height
          liftIO $ writeIORef (coincidenceSubscribedOccurrence subscribed) occ
          scheduleClear $ coincidenceSubscribedOccurrence subscribed
          liftIO . writeIORef (coincidenceSubscribedSubscribers subscribed) =<< propagate o =<< liftIO (readIORef (coincidenceSubscribedSubscribers subscribed))
    SubscriberCoincidenceInner subscribed -> do
      when debugPropagate $ liftIO $ putStrLn $ "SubscriberCoincidenceInner" <> showNodeId subscribed
      liftIO $ writeIORef (coincidenceSubscribedOccurrence subscribed) $ Just a
      scheduleClear $ coincidenceSubscribedOccurrence subscribed
      liftIO . writeIORef (coincidenceSubscribedSubscribers subscribed) =<< propagate a =<< liftIO (readIORef (coincidenceSubscribedSubscribers subscribed))

data SomeCoincidenceInfo = forall a. SomeCoincidenceInfo (Weak (Subscriber a)) (Subscriber a) (Maybe (CoincidenceSubscribed a)) -- The CoincidenceSubscriber will be present only if heights need to be reset

subscribeCoincidenceInner :: Event a -> Int -> CoincidenceSubscribed a -> EventM (Maybe a, Int, EventSubscribed a)
subscribeCoincidenceInner o outerHeight subscribedUnsafe = do
  let !subInner = SubscriberCoincidenceInner subscribedUnsafe
  wsubInner <- liftIO $ mkWeakPtrWithDebug subInner "SubscriberCoincidenceInner"
  innerSubd <- {-# SCC "innerSubd" #-} (subscribe o $ WeakSubscriberSimple wsubInner)
  innerOcc <- liftIO $ getEventSubscribedOcc innerSubd
  innerHeight <- liftIO $ readIORef $ eventSubscribedHeightRef innerSubd
  let height = max innerHeight outerHeight
  emitCoincidenceInfo $ SomeCoincidenceInfo wsubInner subInner $ if height > outerHeight then Just subscribedUnsafe else Nothing
  return (innerOcc, height, innerSubd)

readBehavior :: Behavior a -> IO a
readBehavior b = runBehaviorM (readBehaviorTracked b) Nothing --TODO: Specialize readBehaviorTracked to the Nothing and Just cases

runBehaviorM :: BehaviorM a -> Maybe (Weak Invalidator, IORef [SomeBehaviorSubscribed]) -> IO a
runBehaviorM a mwi = runReaderT (unBehaviorM a) mwi

askInvalidator :: BehaviorM (Maybe (Weak Invalidator))
askInvalidator = liftM (fmap fst) $ BehaviorM ask

askParentsRef :: BehaviorM (Maybe (IORef [SomeBehaviorSubscribed]))
askParentsRef = liftM (fmap snd) $ BehaviorM ask

readBehaviorTracked :: Behavior a -> BehaviorM a
readBehaviorTracked b = case b of
  BehaviorHold h -> do
    result <- liftIO $ readIORef $ holdValue h
    askInvalidator >>= mapM_ (\wi -> liftIO $ modifyIORef' (holdInvalidators h) (wi:))
    askParentsRef >>= mapM_ (\r -> liftIO $ modifyIORef' r (SomeBehaviorSubscribed (BehaviorSubscribedHold h) :))
    liftIO $ touch $ holdSubscriber h
    return result
  BehaviorConst a -> return a
  BehaviorPull p -> do
    val <- liftIO $ readIORef $ pullValue p
    case val of
      Just subscribed -> do
        askParentsRef >>= mapM_ (\r -> liftIO $ modifyIORef' r (SomeBehaviorSubscribed (BehaviorSubscribedPull subscribed) :))
        askInvalidator >>= mapM_ (\wi -> liftIO $ modifyIORef' (pullSubscribedInvalidators subscribed) (wi:))
        liftIO $ touch $ pullSubscribedOwnInvalidator subscribed
        return $ pullSubscribedValue subscribed
      Nothing -> do
        let !i = InvalidatorPull p
        wi <- liftIO $ mkWeakPtrWithDebug i "InvalidatorPull"
        parentsRef <- liftIO $ newIORef []
        a <- liftIO $ runReaderT (unBehaviorM $ pullCompute p) $ Just (wi, parentsRef)
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

readEvent :: Event a -> ResultM (Maybe a)
readEvent e = case e of
  EventRoot r -> liftIO $ readIORef $ rootOccurrence r
  EventNever -> return Nothing
  EventPush p -> do
    subscribed <- getPushSubscribed p
    liftIO $ do
      result <- readIORef $ pushSubscribedOccurrence subscribed -- Since ResultM is always called after the final height is reached, this will always be valid
      touch $ pushSubscribedSelf subscribed
      return result
  EventMerge m -> do
    subscribed <- getMergeSubscribed m
    liftIO $ do
      result <- readIORef $ mergeSubscribedOccurrence subscribed
      touch $ mergeSubscribedSelf subscribed
      return result
  EventFan k f -> do
    parentOcc <- readEvent $ fanParent f
    return $ DMap.lookup k =<< parentOcc
  EventSwitch s -> do
    subscribed <- getSwitchSubscribed s
    liftIO $ do
      result <- readIORef $ switchSubscribedOccurrence subscribed
      touch $ switchSubscribedSelf subscribed
      touch $ switchSubscribedOwnInvalidator subscribed
      return result
  EventCoincidence c -> do
    subscribed <- getCoincidenceSubscribed c
    liftIO $ do
      result <- readIORef $ coincidenceSubscribedOccurrence subscribed
      touch $ coincidenceSubscribedOuter subscribed
      --TODO: do we need to touch the inner subscriber?
      return result

-- Always refers to 0
{-# NOINLINE zeroRef #-}
zeroRef :: IORef Int
zeroRef = unsafePerformIO $ newIORef 0

getEventSubscribed :: Event a -> EventM (EventSubscribed a)
getEventSubscribed e = case e of
  EventRoot r -> liftM EventSubscribedRoot $ getRootSubscribed r
  EventNever -> return EventSubscribedNever
  EventPush p -> liftM EventSubscribedPush $ getPushSubscribed p
  EventFan k f -> liftM (EventSubscribedFan k) $ getFanSubscribed f
  EventMerge m -> liftM EventSubscribedMerge $ getMergeSubscribed m
  EventSwitch s -> liftM EventSubscribedSwitch $ getSwitchSubscribed s
  EventCoincidence c -> liftM EventSubscribedCoincidence $ getCoincidenceSubscribed c

debugSubscribe :: Bool
debugSubscribe = False

subscribeEventSubscribed :: EventSubscribed a -> WeakSubscriber a -> IO ()
subscribeEventSubscribed es ws = case es of
  EventSubscribedRoot r -> do
    when debugSubscribe $ liftIO $ putStrLn $ "subscribeEventSubscribed Root"
    modifyIORef' (rootSubscribedSubscribers r) (ws:)
  EventSubscribedNever -> do
    when debugSubscribe $ liftIO $ putStrLn $ "subscribeEventSubscribed Never"
    return ()
  EventSubscribedPush subscribed -> do
    when debugSubscribe $ liftIO $ putStrLn $ "subscribeEventSubscribed Push"
    modifyIORef' (pushSubscribedSubscribers subscribed) (ws:)
  EventSubscribedFan k subscribed -> do
    when debugSubscribe $ liftIO $ putStrLn $ "subscribeEventSubscribed Fan"
    modifyIORef' (fanSubscribedSubscribers subscribed) $ DMap.insertWith (++) (FanSubscriberKey k) [ws]
  EventSubscribedMerge subscribed -> do
    when debugSubscribe $ liftIO $ putStrLn $ "subscribeEventSubscribed Merge"
    modifyIORef' (mergeSubscribedSubscribers subscribed) (ws:)
  EventSubscribedSwitch subscribed -> do
    when debugSubscribe $ liftIO $ putStrLn $ "subscribeEventSubscribed Switch"
    modifyIORef' (switchSubscribedSubscribers subscribed) (ws:)
  EventSubscribedCoincidence subscribed -> do
    when debugSubscribe $ liftIO $ putStrLn $ "subscribeEventSubscribed Coincidence"
    modifyIORef' (coincidenceSubscribedSubscribers subscribed) (ws:)

getEventSubscribedOcc :: EventSubscribed a -> IO (Maybe a)
getEventSubscribedOcc es = case es of
  EventSubscribedRoot r -> readIORef $ rootSubscribedOccurrence r
  EventSubscribedNever -> return Nothing
  EventSubscribedPush subscribed -> readIORef $ pushSubscribedOccurrence subscribed
  EventSubscribedFan k subscribed -> do
    parentOcc <- getEventSubscribedOcc $ fanSubscribedParent subscribed
    let occ = DMap.lookup k =<< parentOcc
    return occ
  EventSubscribedMerge subscribed -> readIORef $ mergeSubscribedOccurrence subscribed
  EventSubscribedSwitch subscribed -> readIORef $ switchSubscribedOccurrence subscribed
  EventSubscribedCoincidence subscribed -> readIORef $ coincidenceSubscribedOccurrence subscribed

eventSubscribedHeightRef :: EventSubscribed a -> IORef Int
eventSubscribedHeightRef es = case es of
  EventSubscribedRoot _ -> zeroRef
  EventSubscribedNever -> zeroRef
  EventSubscribedPush subscribed -> pushSubscribedHeight subscribed
  EventSubscribedFan _ subscribed -> eventSubscribedHeightRef $ fanSubscribedParent subscribed
  EventSubscribedMerge subscribed -> mergeSubscribedHeight subscribed
  EventSubscribedSwitch subscribed -> switchSubscribedHeight subscribed
  EventSubscribedCoincidence subscribed -> coincidenceSubscribedHeight subscribed

subscribe :: Event a -> WeakSubscriber a -> EventM (EventSubscribed a)
subscribe e ws = do
  subd <- getEventSubscribed e
  liftIO $ subscribeEventSubscribed subd ws
  return subd

getRootSubscribed :: Root a -> EventM (RootSubscribed a)
getRootSubscribed r = do
  mSubscribed <- liftIO $ readIORef $ rootSubscribed r
  case mSubscribed of
    Just subscribed -> return subscribed
    Nothing -> liftIO $ do
      subscribersRef <- newIORef []
      let !subscribed = RootSubscribed
            { rootSubscribedOccurrence = rootOccurrence r
            , rootSubscribedSubscribers = subscribersRef
            }
      -- Strangely, init needs the same stuff as a RootSubscribed has, but it must not be the same as the one that everyone's subscribing to, or it'll leak memory
      uninit <- rootInit r $ RootTrigger (subscribersRef, rootOccurrence r)
      addFinalizer subscribed $ do
--        putStrLn "Uninit root"
        uninit
      liftIO $ writeIORef (rootSubscribed r) $ Just subscribed
      return subscribed

-- When getPushSubscribed returns, the PushSubscribed returned will have a fully filled-in
getPushSubscribed :: Push a b -> EventM (PushSubscribed a b)
getPushSubscribed p = do
  mSubscribed <- liftIO $ readIORef $ pushSubscribed p
  case mSubscribed of
    Just subscribed -> return subscribed
    Nothing -> do -- Not yet subscribed
      subscribedUnsafe <- liftIO $ unsafeInterleaveIO $ liftM fromJust $ readIORef $ pushSubscribed p
      let !s = SubscriberPush (pushCompute p) subscribedUnsafe
      ws <- liftIO $ mkWeakPtrWithDebug s "SubscriberPush"
      subd <- subscribe (pushParent p) $ WeakSubscriberSimple ws
      parentOcc <- liftIO $ getEventSubscribedOcc subd
      occ <- liftM join $ mapM (pushCompute p) parentOcc
      occRef <- liftIO $ newIORef occ
      when (isJust occ) $ scheduleClear occRef
      subscribersRef <- liftIO $ newIORef []
      let subscribed = PushSubscribed
            { pushSubscribedOccurrence = occRef
            , pushSubscribedHeight = eventSubscribedHeightRef subd -- Since pushes have the same height as their parents, share the ref
            , pushSubscribedSubscribers = subscribersRef
            , pushSubscribedSelf = unsafeCoerce s
            , pushSubscribedParent = subd
#ifdef DEBUG_NODEIDS
            , pushSubscribedNodeId = unsafeNodeId p
#endif
            }
      liftIO $ writeIORef (pushSubscribed p) $ Just subscribed
      return subscribed

getMergeSubscribed :: forall k. GCompare k => Merge k -> EventM (MergeSubscribed k)
getMergeSubscribed m = {-# SCC "getMergeSubscribed.entire" #-} do
  mSubscribed <- liftIO $ readIORef $ mergeSubscribed m
  case mSubscribed of
    Just subscribed -> return subscribed
    Nothing -> if DMap.null $ mergeParents m then emptyMergeSubscribed else do
      subscribedRef <- liftIO $ newIORef $ error "getMergeSubscribed: subscribedRef not yet initialized"
      subscribedUnsafe <- liftIO $ unsafeInterleaveIO $ readIORef subscribedRef
      let !s = Box subscribedUnsafe
      ws <- liftIO $ mkWeakPtrWithDebug s "SubscriberMerge"
      subscribers :: [(Any, Maybe (DSum k), Int, DSum (WrapArg EventSubscribed k))] <- forM (DMap.toList $ mergeParents m) $ {-# SCC "getMergeSubscribed.a" #-} \(WrapArg k :=> e) -> {-# SCC "getMergeSubscribed.a1" #-} do
        parentSubd <- {-# SCC "getMergeSubscribed.a.parentSubd" #-} subscribe e $ WeakSubscriberMerge k ws
        parentOcc <- {-# SCC "getMergeSubscribed.a.parentOcc" #-} liftIO $ getEventSubscribedOcc parentSubd
        height <- {-# SCC "getMergeSubscribed.a.height" #-} liftIO $ readIORef $ eventSubscribedHeightRef parentSubd
        return $ {-# SCC "getMergeSubscribed.a.returnVal" #-} (unsafeCoerce s :: Any, fmap (k :=>) parentOcc, height, WrapArg k :=> parentSubd)
      let dm = DMap.fromDistinctAscList $ catMaybes $ map (^._2) subscribers
          subscriberHeights = map (^._3) subscribers
          myHeight =
            if any (==invalidHeight) subscriberHeights --TODO: Replace 'any' with invalidHeight-preserving 'maximum'
            then invalidHeight
            else succ $ Prelude.maximum subscriberHeights -- This is safe because the DMap will never be empty here
      currentHeight <- getCurrentHeight
      let (occ, accum) = if currentHeight >= myHeight -- If we should have fired by now
                         then (if DMap.null dm then Nothing else Just dm, DMap.empty)
                         else (Nothing, dm)
      when (not $ DMap.null accum) $ do
        scheduleMerge myHeight subscribedUnsafe
      occRef <- liftIO $ newIORef occ
      when (isJust occ) $ scheduleClear occRef
      accumRef <- liftIO $ newIORef accum
      heightRef <- liftIO $ newIORef myHeight
      subsRef <- liftIO $ newIORef []
      let subscribed = MergeSubscribed
            { mergeSubscribedOccurrence = occRef
            , mergeSubscribedAccum = accumRef
            , mergeSubscribedHeight = heightRef
            , mergeSubscribedSubscribers = subsRef
            , mergeSubscribedSelf = unsafeCoerce $ map (\(x, _, _, _) -> x) subscribers --TODO: Does lack of strictness make this leak?
            , mergeSubscribedParents = DMap.fromDistinctAscList $ map (^._4) subscribers
#ifdef DEBUG_NODEIDS
            , mergeSubscribedNodeId = unsafeNodeId m
#endif
            }
      liftIO $ writeIORef subscribedRef subscribed
      return subscribed
  where emptyMergeSubscribed = do --TODO: This should never happen
          occRef <- liftIO $ newIORef Nothing
          accumRef <- liftIO $ newIORef DMap.empty
          subsRef <- liftIO $ newIORef []
          return $ MergeSubscribed
            { mergeSubscribedOccurrence = occRef
            , mergeSubscribedAccum = accumRef
            , mergeSubscribedHeight = zeroRef
            , mergeSubscribedSubscribers = subsRef --TODO: This will definitely leak
            , mergeSubscribedSelf = unsafeCoerce ()
            , mergeSubscribedParents = DMap.empty
#ifdef DEBUG_NODEIDS
            , mergeSubscribedNodeId = -1
#endif
            }

getFanSubscribed :: GCompare k => Fan k -> EventM (FanSubscribed k)
getFanSubscribed f = do
  mSubscribed <- liftIO $ readIORef $ fanSubscribed f
  case mSubscribed of
    Just subscribed -> return subscribed
    Nothing -> do
      subscribedUnsafe <- liftIO $ unsafeInterleaveIO $ liftM fromJust $ readIORef $ fanSubscribed f
      let !sub = SubscriberFan subscribedUnsafe
      wsub <- liftIO $ mkWeakPtrWithDebug sub "SubscriberFan"
      subd <- subscribe (fanParent f) $ WeakSubscriberSimple wsub
      subscribersRef <- liftIO $ newIORef DMap.empty
      let subscribed = FanSubscribed
            { fanSubscribedParent = subd
            , fanSubscribedSubscribers = subscribersRef
            , fanSubscribedSelf = sub
#ifdef DEBUG_NODEIDS
            , fanSubscribedNodeId = unsafeNodeId f
#endif
            }
      liftIO $ writeIORef (fanSubscribed f) $ Just subscribed
      return subscribed

getSwitchSubscribed :: Switch a -> EventM (SwitchSubscribed a)
getSwitchSubscribed s = do
  mSubscribed <- liftIO $ readIORef $ switchSubscribed s
  case mSubscribed of
    Just subscribed -> return subscribed
    Nothing -> do
      subscribedRef <- liftIO $ newIORef $ error "getSwitchSubscribed: subscribed has not yet been created"
      subscribedUnsafe <- liftIO $ unsafeInterleaveIO $ readIORef subscribedRef
      let !i = InvalidatorSwitch subscribedUnsafe
          !sub = SubscriberSwitch subscribedUnsafe
      wi <- liftIO $ mkWeakPtrWithDebug i "InvalidatorSwitch"
      wiRef <- liftIO $ newIORef wi
      wsub <- liftIO $ mkWeakPtrWithDebug sub "SubscriberSwitch"
      selfWeakRef <- liftIO $ newIORef wsub
      parentsRef <- liftIO $ newIORef [] --TODO: This should be unnecessary, because it will always be filled with just the single parent behavior
      e <- liftIO $ runBehaviorM (readBehaviorTracked (switchParent s)) $ Just (wi, parentsRef)
      subd <- subscribe e $ WeakSubscriberSimple wsub
      subdRef <- liftIO $ newIORef subd
      parentOcc <- liftIO $ getEventSubscribedOcc subd
      occRef <- liftIO $ newIORef parentOcc
      when (isJust parentOcc) $ scheduleClear occRef
      heightRef <- liftIO $ newIORef =<< readIORef (eventSubscribedHeightRef subd)
      subscribersRef <- liftIO $ newIORef []
      let subscribed = SwitchSubscribed
            { switchSubscribedOccurrence = occRef
            , switchSubscribedHeight = heightRef
            , switchSubscribedSubscribers = subscribersRef
            , switchSubscribedSelf = sub
            , switchSubscribedSelfWeak = selfWeakRef
            , switchSubscribedOwnInvalidator = i
            , switchSubscribedOwnWeakInvalidator = wiRef
            , switchSubscribedBehaviorParents = parentsRef
            , switchSubscribedParent = switchParent s
            , switchSubscribedCurrentParent = subdRef
#ifdef DEBUG_NODEIDS
            , switchSubscribedNodeId = unsafeNodeId s
#endif
            }
      liftIO $ writeIORef subscribedRef subscribed
      liftIO $ writeIORef (switchSubscribed s) $ Just subscribed
      return subscribed

getCoincidenceSubscribed :: forall a. Coincidence a -> EventM (CoincidenceSubscribed a)
getCoincidenceSubscribed c = do
  mSubscribed <- liftIO $ readIORef $ coincidenceSubscribed c
  case mSubscribed of
    Just subscribed -> return subscribed
    Nothing -> do
      subscribedRef <- liftIO $ newIORef $ error "getCoincidenceSubscribed: subscribed has not yet been created"
      subscribedUnsafe <- liftIO $ unsafeInterleaveIO $ readIORef subscribedRef
      let !subOuter = SubscriberCoincidenceOuter subscribedUnsafe
      wsubOuter <- liftIO $ mkWeakPtrWithDebug subOuter "subOuter"
      outerSubd <- subscribe (coincidenceParent c) $ WeakSubscriberSimple wsubOuter
      outerOcc <- liftIO $ getEventSubscribedOcc outerSubd
      outerHeight <- liftIO $ readIORef $ eventSubscribedHeightRef outerSubd
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
      subscribersRef <- liftIO $ newIORef []
      let subscribed = CoincidenceSubscribed
            { coincidenceSubscribedOccurrence = occRef
            , coincidenceSubscribedHeight = heightRef
            , coincidenceSubscribedSubscribers = subscribersRef
            , coincidenceSubscribedOuter = subOuter
            , coincidenceSubscribedOuterParent = outerSubd
            , coincidenceSubscribedInnerParent = innerSubdRef
#ifdef DEBUG_NODEIDS
            , coincidenceSubscribedNodeId = unsafeNodeId c
#endif
            }
      liftIO $ writeIORef subscribedRef subscribed
      liftIO $ writeIORef (coincidenceSubscribed c) $ Just subscribed
      return subscribed

merge :: GCompare k => DMap (WrapArg Event k) -> Event (DMap k)
merge m = EventMerge $ Merge
  { mergeParents = m
  , mergeSubscribed = unsafeNewIORef m Nothing
  }

newtype EventSelector k = EventSelector { select :: forall a. k a -> Event a }

fan :: GCompare k => Event (DMap k) -> EventSelector k
fan e =
  let f = Fan
        { fanParent = e
        , fanSubscribed = unsafeNewIORef e Nothing
        }
  in EventSelector $ \k -> EventFan k f

-- | Run an event action outside of a frame
runFrame :: EventM a -> IO a
runFrame a = do
  toAssignRef <- newIORef [] -- This should only actually get used when events are firing
  holdInitRef <- newIORef []
  heightRef <- newIORef 0
  toClearRef <- newIORef []
  coincidenceInfosRef <- newIORef []
  delayedRef <- liftIO $ newIORef IntMap.empty
  result <- flip runEventM (EventEnv toAssignRef holdInitRef toClearRef heightRef coincidenceInfosRef delayedRef) $ do
    result <- a
    let runHoldInits = do
          holdInits <- liftIO $ readIORef holdInitRef
          if null holdInits then return () else do
            liftIO $ writeIORef holdInitRef []
            forM_ holdInits $ \(SomeHoldInit e h) -> subscribeHold e h
            runHoldInits
    runHoldInits -- This must happen before doing the assignments, in case subscribing a Hold causes existing Holds to be read by the newly-propagated events
    return result
  toClear <- readIORef toClearRef
  forM_ toClear $ \(SomeMaybeIORef ref) -> writeIORef ref Nothing
  toAssign <- readIORef toAssignRef
  toReconnectRef <- newIORef []
  forM_ toAssign $ \(SomeAssignment h v) -> do
    writeIORef (holdValue h) v
    writeIORef (holdInvalidators h) =<< invalidate toReconnectRef =<< readIORef (holdInvalidators h)
  coincidenceInfos <- readIORef coincidenceInfosRef
  forM_ coincidenceInfos $ \(SomeCoincidenceInfo wsubInner subInner mcs) -> do
    touch subInner
    finalize wsubInner
    mapM_ invalidateCoincidenceHeight mcs
  toReconnect <- readIORef toReconnectRef
  forM_ toReconnect $ \(SomeSwitchSubscribed subscribed) -> do
    wsub <- readIORef $ switchSubscribedSelfWeak subscribed
    finalize wsub
    wi <- readIORef $ switchSubscribedOwnWeakInvalidator subscribed
    finalize wi
    let !i = switchSubscribedOwnInvalidator subscribed
    wi' <- mkWeakPtrWithDebug i "wi'"
    writeIORef (switchSubscribedBehaviorParents subscribed) []
    e <- runBehaviorM (readBehaviorTracked (switchSubscribedParent subscribed)) (Just (wi', switchSubscribedBehaviorParents subscribed))
    --TODO: Make sure we touch the pieces of the SwitchSubscribed at the appropriate times
    let !sub = switchSubscribedSelf subscribed -- Must be done strictly, or the weak pointer will refer to a useless thunk
    wsub' <- mkWeakPtrWithDebug sub "wsub'"
    writeIORef (switchSubscribedSelfWeak subscribed) wsub'
    subd' <- runFrame $ subscribe e $ WeakSubscriberSimple wsub' --TODO: Assert that the event isn't firing --TODO: This should not loop because none of the events should be firing, but still, it is inefficient
    {-
    stackTrace <- liftIO $ liftM renderStack $ ccsToStrings =<< (getCCSOf $! switchSubscribedParent subscribed)
    liftIO $ putStrLn $ (++stackTrace) $ "subd' subscribed to " ++ case e of
      EventRoot _ -> "EventRoot"
      EventNever -> "EventNever"
      _ -> "something else"
    -}
    writeIORef (switchSubscribedCurrentParent subscribed) subd'
    parentHeight <- readIORef $ eventSubscribedHeightRef subd'
    myHeight <- readIORef $ switchSubscribedHeight subscribed
    if parentHeight == myHeight then return () else do
      writeIORef (switchSubscribedHeight subscribed) parentHeight
      mapM_ invalidateSubscriberHeight =<< readIORef (switchSubscribedSubscribers subscribed)
  forM_ coincidenceInfos $ \(SomeCoincidenceInfo _ _ mcs) -> mapM_ recalculateCoincidenceHeight mcs
  forM_ toReconnect $ \(SomeSwitchSubscribed subscribed) -> do
    mapM_ recalculateSubscriberHeight =<< readIORef (switchSubscribedSubscribers subscribed)
  return result

invalidHeight :: Int
invalidHeight = -1000

invalidateSubscriberHeight :: WeakSubscriber a -> IO ()
invalidateSubscriberHeight ws = do
  ms <- deRefWeakSubscriber ws
  case ms of
    Nothing -> return () --TODO: cleanup?
    Just s -> case s of
      SubscriberPush _ subscribed -> do
        when debugInvalidateHeight $ putStrLn $ "invalidateSubscriberHeight: SubscriberPush" <> showNodeId subscribed
        mapM_ invalidateSubscriberHeight =<< readIORef (pushSubscribedSubscribers subscribed)
        when debugInvalidateHeight $ putStrLn $ "invalidateSubscriberHeight: SubscriberPush" <> showNodeId subscribed <> " done"
      SubscriberMerge _ subscribed -> do
        when debugInvalidateHeight $ putStrLn $ "invalidateSubscriberHeight: SubscriberMerge" <> showNodeId subscribed
        oldHeight <- readIORef $ mergeSubscribedHeight subscribed
        when (oldHeight /= invalidHeight) $ do
          writeIORef (mergeSubscribedHeight subscribed) $ invalidHeight
          mapM_ invalidateSubscriberHeight =<< readIORef (mergeSubscribedSubscribers subscribed)
        when debugInvalidateHeight $ putStrLn $ "invalidateSubscriberHeight: SubscriberMerge" <> showNodeId subscribed <> " done"
      SubscriberFan subscribed -> do
        when debugInvalidateHeight $ putStrLn $ "invalidateSubscriberHeight: SubscriberFan" <> showNodeId subscribed
        subscribers <- readIORef $ fanSubscribedSubscribers subscribed
        forM_ (DMap.toList subscribers) $ ((\(FanSubscriberKey _ :=> v) -> mapM_ invalidateSubscriberHeight v) :: DSum (FanSubscriberKey k) -> IO ())
        when debugInvalidateHeight $ putStrLn $ "invalidateSubscriberHeight: SubscriberFan" <> showNodeId subscribed <> " done"
      SubscriberHold _ -> return ()
      SubscriberSwitch subscribed -> do
        when debugInvalidateHeight $ putStrLn $ "invalidateSubscriberHeight: SubscriberSwitch" <> showNodeId subscribed
        oldHeight <- readIORef $ switchSubscribedHeight subscribed
        when (oldHeight /= invalidHeight) $ do
          writeIORef (switchSubscribedHeight subscribed) $ invalidHeight
          mapM_ invalidateSubscriberHeight =<< readIORef (switchSubscribedSubscribers subscribed)
        when debugInvalidateHeight $ putStrLn $ "invalidateSubscriberHeight: SubscriberSwitch" <> showNodeId subscribed <> " done"
      SubscriberCoincidenceOuter subscribed -> do
        when debugInvalidateHeight $ putStrLn $ "invalidateSubscriberHeight: SubscriberCoincidenceOuter" <> showNodeId subscribed
        invalidateCoincidenceHeight subscribed
        when debugInvalidateHeight $ putStrLn $ "invalidateSubscriberHeight: SubscriberCoincidenceOuter" <> showNodeId subscribed <> " done"
      SubscriberCoincidenceInner subscribed -> do
        when debugInvalidateHeight $ putStrLn $ "invalidateSubscriberHeight: SubscriberCoincidenceInner" <> showNodeId subscribed
        invalidateCoincidenceHeight subscribed
        when debugInvalidateHeight $ putStrLn $ "invalidateSubscriberHeight: SubscriberCoincidenceInner" <> showNodeId subscribed <> " done"

invalidateCoincidenceHeight :: CoincidenceSubscribed a -> IO ()
invalidateCoincidenceHeight subscribed = do
  oldHeight <- readIORef $ coincidenceSubscribedHeight subscribed
  when (oldHeight /= invalidHeight) $ do
    writeIORef (coincidenceSubscribedHeight subscribed) $ invalidHeight
    mapM_ invalidateSubscriberHeight =<< readIORef (coincidenceSubscribedSubscribers subscribed)

--TODO: The recalculation algorithm seems a bit funky; make sure it doesn't miss stuff or hit stuff twice; also, it should probably be lazy

recalculateSubscriberHeight :: WeakSubscriber a -> IO ()
recalculateSubscriberHeight ws = do
  ms <- deRefWeakSubscriber ws
  case ms of
    Nothing -> return () --TODO: cleanup?
    Just s -> case s of
      SubscriberPush _ subscribed -> do
        when debugInvalidateHeight $ putStrLn $ "recalculateSubscriberHeight: SubscriberPush" <> showNodeId subscribed
        mapM_ recalculateSubscriberHeight =<< readIORef (pushSubscribedSubscribers subscribed)
        when debugInvalidateHeight $ putStrLn $ "recalculateSubscriberHeight: SubscriberPush" <> showNodeId subscribed <> " done"
      SubscriberMerge _ subscribed -> do
        when debugInvalidateHeight $ putStrLn $ "recalculateSubscriberHeight: SubscriberMerge" <> showNodeId subscribed
        oldHeight <- readIORef $ mergeSubscribedHeight subscribed
        when (oldHeight == invalidHeight) $ do
          height <- calculateMergeHeight subscribed
          when debugInvalidateHeight $ putStrLn $ "recalculateSubscriberHeight: height: " <> show height
          when (height /= invalidHeight) $ do -- If height == invalidHeight, that means some of the prereqs have not yet been recomputed; when they do recompute, they'll catch this node again --TODO: this is O(n*m), where n is the number of children of this noe and m is the number that have been invalidated
            writeIORef (mergeSubscribedHeight subscribed) height
            mapM_ recalculateSubscriberHeight =<< readIORef (mergeSubscribedSubscribers subscribed)
        when debugInvalidateHeight $ putStrLn $ "recalculateSubscriberHeight: SubscriberMerge" <> showNodeId subscribed <> " done"
      SubscriberFan subscribed -> do
        when debugInvalidateHeight $ putStrLn $ "recalculateSubscriberHeight: SubscriberFan" <> showNodeId subscribed
        subscribers <- readIORef $ fanSubscribedSubscribers subscribed
        forM_ (DMap.toList subscribers) $ ((\(FanSubscriberKey _ :=> v) -> mapM_ recalculateSubscriberHeight v) :: DSum (FanSubscriberKey k) -> IO ())
        when debugInvalidateHeight $ putStrLn $ "recalculateSubscriberHeight: SubscriberFan" <> showNodeId subscribed <> " done"
      SubscriberHold _ -> return ()
      SubscriberSwitch subscribed -> do
        when debugInvalidateHeight $ putStrLn $ "recalculateSubscriberHeight: SubscriberSwitch" <> showNodeId subscribed
        oldHeight <- readIORef $ switchSubscribedHeight subscribed
        when (oldHeight == invalidHeight) $ do
          height <- calculateSwitchHeight subscribed
          when (height /= invalidHeight) $ do
            writeIORef (switchSubscribedHeight subscribed) height
            mapM_ recalculateSubscriberHeight =<< readIORef (switchSubscribedSubscribers subscribed)
        when debugInvalidateHeight $ putStrLn $ "recalculateSubscriberHeight: SubscriberSwitch" <> showNodeId subscribed <> " done"
      SubscriberCoincidenceOuter subscribed -> do
        when debugInvalidateHeight $ putStrLn $ "recalculateSubscriberHeight: SubscriberCoincidenceOuter" <> showNodeId subscribed
        void $ recalculateCoincidenceHeight subscribed
        when debugInvalidateHeight $ putStrLn $ "recalculateSubscriberHeight: SubscriberCoincidenceOuter" <> showNodeId subscribed <> " done"
      SubscriberCoincidenceInner subscribed -> do
        when debugInvalidateHeight $ putStrLn $ "recalculateSubscriberHeight: SubscriberCoincidenceInner" <> showNodeId subscribed
        void $ recalculateCoincidenceHeight subscribed
        when debugInvalidateHeight $ putStrLn $ "recalculateSubscriberHeight: SubscriberCoincidenceInner" <> showNodeId subscribed <> " done"

recalculateCoincidenceHeight :: CoincidenceSubscribed a -> IO ()
recalculateCoincidenceHeight subscribed = do
  oldHeight <- readIORef $ coincidenceSubscribedHeight subscribed
  when (oldHeight == invalidHeight) $ do
    height <- calculateCoincidenceHeight subscribed
    when (height /= invalidHeight) $ do
      writeIORef (coincidenceSubscribedHeight subscribed) height
      mapM_ recalculateSubscriberHeight =<< readIORef (coincidenceSubscribedSubscribers subscribed) --TODO: This should probably be mandatory, just like with the merge and switch ones

calculateMergeHeight :: MergeSubscribed k -> IO Int
calculateMergeHeight subscribed = if DMap.null (mergeSubscribedParents subscribed) then return 0 else do
  heights <- forM (DMap.toList $ mergeSubscribedParents subscribed) $ \(WrapArg _ :=> es) -> do
    readIORef $ eventSubscribedHeightRef es
  return $ if any (== invalidHeight) heights then invalidHeight else succ $ Prelude.maximum heights --TODO: Replace 'any' with invalidHeight-preserving 'maximum'

calculateSwitchHeight :: SwitchSubscribed a -> IO Int
calculateSwitchHeight subscribed = readIORef . eventSubscribedHeightRef =<< readIORef (switchSubscribedCurrentParent subscribed)

calculateCoincidenceHeight :: CoincidenceSubscribed a -> IO Int
calculateCoincidenceHeight subscribed = do
  outerHeight <- readIORef $ eventSubscribedHeightRef $ coincidenceSubscribedOuterParent subscribed
  innerHeight <- maybe (return 0) (readIORef . eventSubscribedHeightRef) =<< readIORef (coincidenceSubscribedInnerParent subscribed)
  return $ if outerHeight == invalidHeight || innerHeight == invalidHeight then invalidHeight else max outerHeight innerHeight

{-
recalculateEventSubscribedHeight :: EventSubscribed a -> IO Int
recalculateEventSubscribedHeight es = case es of
  EventSubscribedRoot _ -> return 0
  EventSubscribedNever -> return 0
  EventSubscribedPush subscribed -> recalculateEventSubscribedHeight $ pushSubscribedParent subscribed
  EventSubscribedMerge subscribed -> do
    oldHeight <- readIORef $ mergeSubscribedHeight subscribed
    if oldHeight /= invalidHeight then return oldHeight else do
      height <- calculateMergeHeight subscribed
      writeIORef (mergeSubscribedHeight subscribed) height
      return height
  EventSubscribedFan _ subscribed -> recalculateEventSubscribedHeight $ fanSubscribedParent subscribed
  EventSubscribedSwitch subscribed -> do
    oldHeight <- readIORef $ switchSubscribedHeight subscribed
    if oldHeight /= invalidHeight then return oldHeight else do
      height <- calculateSwitchHeight subscribed
      writeIORef (switchSubscribedHeight subscribed) height
      return height
  EventSubscribedCoincidence subscribed -> recalculateCoincidenceHeight subscribed
-}

data SomeSwitchSubscribed = forall a. SomeSwitchSubscribed (SwitchSubscribed a)

debugInvalidate :: Bool
debugInvalidate = False

invalidate :: IORef [SomeSwitchSubscribed] -> WeakList Invalidator -> IO (WeakList Invalidator)
invalidate toReconnectRef wis = do
  forM_ wis $ \wi -> do
    mi <- deRefWeak wi
    case mi of
      Nothing -> do
        when debugInvalidate $ liftIO $ putStrLn "invalidate Dead"
        return () --TODO: Should we clean this up here?
      Just i -> do
        finalize wi -- Once something's invalidated, it doesn't need to hang around; this will change when some things are strict
        case i of
          InvalidatorPull p -> do
            when debugInvalidate $ liftIO $ putStrLn "invalidate Pull"
            mVal <- readIORef $ pullValue p
            forM_ mVal $ \val -> do
              writeIORef (pullValue p) Nothing
              writeIORef (pullSubscribedInvalidators val) =<< invalidate toReconnectRef =<< readIORef (pullSubscribedInvalidators val)
          InvalidatorSwitch subscribed -> do
            when debugInvalidate $ liftIO $ putStrLn "invalidate Switch"
            modifyIORef' toReconnectRef (SomeSwitchSubscribed subscribed :)
  return [] -- Since we always finalize everything, always return an empty list --TODO: There are some things that will need to be re-subscribed every time; we should try to avoid finalizing them

--------------------------------------------------------------------------------
-- Reflex integration
--------------------------------------------------------------------------------

data Spider

instance R.Reflex Spider where
  newtype Behavior Spider a = SpiderBehavior { unSpiderBehavior :: Behavior a }
  newtype Event Spider a = SpiderEvent { unSpiderEvent :: Event a }
  type PullM Spider = BehaviorM
  type PushM Spider = EventM
  {-# INLINE never #-}
  {-# INLINE constant #-}
  never = SpiderEvent EventNever
  constant = SpiderBehavior . BehaviorConst
  push f = SpiderEvent. push f . unSpiderEvent
  pull = SpiderBehavior . pull
  merge = SpiderEvent . merge . (unsafeCoerce :: DMap (WrapArg (R.Event Spider) k) -> DMap (WrapArg Event k))
  fan e = R.EventSelector $ SpiderEvent . select (fan (unSpiderEvent e))
  switch = SpiderEvent . switch . (unsafeCoerce :: Behavior (R.Event Spider a) -> Behavior (Event a)) . unSpiderBehavior
  coincidence = SpiderEvent . coincidence . (unsafeCoerce :: Event (R.Event Spider a) -> Event (Event a)) . unSpiderEvent

instance R.MonadSample Spider SpiderHost where
  {-# INLINE sample #-}
  sample = SpiderHost . readBehavior . unSpiderBehavior

instance R.MonadHold Spider SpiderHost where
  hold v0 = SpiderHost . liftM SpiderBehavior . runFrame . hold v0 . unSpiderEvent

instance R.MonadSample Spider BehaviorM where
  {-# INLINE sample #-}
  sample = readBehaviorTracked . unSpiderBehavior

instance R.MonadSample Spider EventM where
  {-# INLINE sample #-}
  sample = liftIO . readBehavior . unSpiderBehavior

instance R.MonadHold Spider EventM where
  {-# INLINE hold #-}
  hold v0 e = SpiderBehavior <$> hold v0 (unSpiderEvent e)

newtype RootTrigger a = RootTrigger (IORef [WeakSubscriber a], IORef (Maybe a))

instance R.ReflexHost Spider where
  type EventTrigger Spider = RootTrigger
  type EventHandle Spider = R.Event Spider
  type HostFrame Spider = SpiderHostFrame

instance R.MonadReadEvent Spider ResultM where
  {-# INLINE readEvent #-}
  readEvent = liftM (fmap return) . readEvent . unSpiderEvent

instance MonadRef EventM where
  type Ref EventM = Ref IO
  {-# INLINE newRef #-}
  {-# INLINE readRef #-}
  {-# INLINE writeRef #-}
  {-# INLINE atomicModifyRef #-}
  newRef = liftIO . newRef
  readRef = liftIO . readRef
  writeRef r a = liftIO $ writeRef r a
  atomicModifyRef r f = liftIO $ atomicModifyRef r f

newtype SpiderHost a = SpiderHost { runSpiderHost :: IO a } deriving (Functor, Applicative, Monad, MonadFix, MonadIO)

newtype SpiderHostFrame a = SpiderHostFrame { runSpiderHostFrame :: EventM a } deriving (Functor, Applicative, Monad, MonadFix, MonadIO)

instance R.MonadSample Spider SpiderHostFrame where
  sample = SpiderHostFrame . R.sample --TODO: This can cause problems with laziness, so we should get rid of it if we can
  
instance R.MonadHold Spider SpiderHostFrame where
  {-# INLINE hold #-}
  hold v0 e = SpiderHostFrame $ R.hold v0 e

newEventWithTriggerIO :: (RootTrigger a -> IO (IO ())) -> IO (R.Event Spider a)
newEventWithTriggerIO f = do
  occRef <- newIORef Nothing
  subscribedRef <- newIORef Nothing
  let !r = Root
        { rootOccurrence = occRef
        , rootSubscribed = subscribedRef
        , rootInit = f
        }
  return $ SpiderEvent $ EventRoot r

instance R.MonadReflexCreateTrigger Spider SpiderHost where
  newEventWithTrigger = SpiderHost . newEventWithTriggerIO

instance R.MonadReflexCreateTrigger Spider SpiderHostFrame where
  newEventWithTrigger = SpiderHostFrame . EventM . liftIO . newEventWithTriggerIO

instance R.MonadReflexHost Spider SpiderHost where
  fireEventsAndRead es a = SpiderHost $ run es a
  subscribeEvent e = SpiderHost $ do
    _ <- runFrame $ getEventSubscribed $ unSpiderEvent e --TODO: The result of this should actually be used
    return e
  runFrame = SpiderHost . runFrame
  runHostFrame = SpiderHost . runFrame . runSpiderHostFrame

instance MonadRef SpiderHost where
  type Ref SpiderHost = Ref IO
  newRef = SpiderHost . newRef
  readRef = SpiderHost . readRef
  writeRef r = SpiderHost . writeRef r
  atomicModifyRef r = SpiderHost . atomicModifyRef r

instance MonadRef SpiderHostFrame where
  type Ref SpiderHostFrame = Ref IO
  newRef = SpiderHostFrame . newRef
  readRef = SpiderHostFrame . readRef
  writeRef r = SpiderHostFrame . writeRef r
  atomicModifyRef r = SpiderHostFrame . atomicModifyRef r
