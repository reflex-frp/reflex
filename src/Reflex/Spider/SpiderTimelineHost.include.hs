instance HasSpiderTimeline x => Reflex.Host.Class.MonadSubscribeEvent (SpiderTimeline x) (SpiderHostFrame x) where
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

instance HasSpiderTimeline x => Reflex.Host.Class.ReflexHost (SpiderTimeline x) where
  type EventTrigger (SpiderTimeline x) = RootTrigger x
  type EventHandle (SpiderTimeline x) = SpiderEventHandle x
  type HostFrame (SpiderTimeline x) = SpiderHostFrame x
  newtype EventKey (SpiderTimeline x) s a = EventKey_SpiderTimeline { unEventKey_SpiderTimeline :: a -> EventM x () }
  newtype Fire (SpiderTimeline x) s a = Fire_SpiderTimeline { runFire_SpiderTimeline :: EventM x a }
  data FanThing (SpiderTimeline x) s = forall a. FanThing_SpiderTimeline
    { _fanThing_SpiderTimeline_subscription :: EventSubscription x
    , _fanThing_SpiderTimeline_go :: a -> EventM x ()
    , _fanThing_SpiderTimeline_subscribers :: FastWeakBag (Some (Subscriber x))
    }
  fire (EventKey_SpiderTimeline sub) a = Fire_SpiderTimeline $ sub a

instance HasSpiderTimeline x => Reflex.Host.Class.MonadReadEvent (SpiderTimeline x) (Reflex.Spider.Internal.ReadPhase x) where
  {-# NOINLINE readEvent #-}
  readEvent h = Reflex.Spider.Internal.ReadPhase $ fmap (fmap return) $ liftIO $ do
    result <- readIORef $ spiderEventHandleValue h
    touch h
    return result

instance ( HasSpiderTimeline x
#ifdef SPECIALIZE_TO_SPIDERTIMELINE_GLOBAL
         , x ~ Global
#endif
         ) => Reflex.Host.Class.MonadReflexCreateTrigger (SpiderTimeline x) (SpiderHost x) where
  newEventWithTrigger = SpiderHost . lift . fmap SpiderEvent . newEventWithTriggerIO
  newFanEventWithTrigger f = SpiderHost $ lift $ do
    es <- newFanEventWithTriggerIO f
    return $ Reflex.Class.EventSelector $ SpiderEvent . Reflex.Spider.Internal.select es
  newFanThing e f = do
    t <- SpiderHost ask
    runFrame $ runSpiderHostFrame $ Reflex.Host.Class.newFanThing e (SpiderHostFrame . liftIO . flip runSpiderHostForTimeline t . f)
  newEventKeyHost ft =
    runFrame $ runSpiderHostFrame $ Reflex.Host.Class.newEventKeyHost ft

instance ( HasSpiderTimeline x
#ifdef SPECIALIZE_TO_SPIDERTIMELINE_GLOBAL
         , x ~ Global
#endif
         ) => Reflex.Host.Class.MonadReflexCreateTrigger (SpiderTimeline x) (SpiderHostFrame x) where
  newEventWithTrigger = SpiderHostFrame . EventM . liftIO . fmap SpiderEvent . newEventWithTriggerIO
  newFanEventWithTrigger f = SpiderHostFrame $ EventM $ liftIO $ do
    es <- newFanEventWithTriggerIO f
    return $ Reflex.Class.EventSelector $ SpiderEvent . Reflex.Spider.Internal.select es
  newFanThing (SpiderEvent parent) f = SpiderHostFrame $ do
    subnRef <- liftIO $ newIORef $ error "newFanThing: not initialized" --TODO: Don't block inlining
    (go, b) <- runSpiderHostFrame $ f $ unsafeDupablePerformIO $ readIORef subnRef
    runThisFrame <- liftIO $ newIORef Nothing
    let goOncePerFrame a = do
          liftIO $ writeIORef runThisFrame $ Just ()
          scheduleClear runThisFrame
          coerce go a
    subscribers <- liftIO FastWeakBag.empty
    --TODO: should we ever call `unsubscribe`?
    (subn, parentOcc) <- subscribeAndRead parent $ Subscriber
      { subscriberPropagate = goOncePerFrame
      , subscriberInvalidateHeight = \old ->
          FastWeakBag.traverse subscribers $ \(Some.This sub) -> invalidateSubscriberHeight old sub
      , subscriberRecalculateHeight = \new ->
          FastWeakBag.traverse subscribers $ \(Some.This sub) -> recalculateSubscriberHeight new sub
      }
    mapM_ (runFire_SpiderTimeline . go) parentOcc
    liftIO $ writeIORef subnRef $ FanThing_SpiderTimeline
      { _fanThing_SpiderTimeline_subscription = subn
      , _fanThing_SpiderTimeline_go = goOncePerFrame
      , _fanThing_SpiderTimeline_subscribers = subscribers
      }
    return b
  newEventKeyHost (FanThing_SpiderTimeline parentSubn go outerSubscribers) = do
    occRef <- liftIO $ newIORef Nothing --TODO: What if we're currently firing? Is that possible?
    subscribers <- liftIO FastWeakBag.empty
    let e = Event $ \sub -> do
          outerTicket <- liftIO $ FastWeakBag.insert (Some.This sub) outerSubscribers
          innerTicket <- liftIO $ FastWeakBag.insert sub subscribers
          --mapM_ go parentFiring --TODO: How to determine if parent should be firing?
          --TODO: Can we relax the rules for returning the occurrence such that, instead of needing to force the parent to evaluate here, we can instead return 'Just' only if we're firing AND we won't be calling this subscriber this frame because it's too late for that
          occ <- liftIO $ readIORef occRef
          let EventSubscription _ (EventSubscribed !heightRef !retained) = parentSubn
          let subn = EventSubscription
                { _eventSubscription_unsubscribe = do
                    FastWeakBag.remove outerTicket
                    FastWeakBag.remove innerTicket
                , _eventSubscription_subscribed = EventSubscribed
                  { eventSubscribedHeightRef = heightRef
                  , eventSubscribedRetained = toAny (outerTicket, innerTicket, retained)
                  }
                }
          return (subn, occ)
        ek = EventKey_SpiderTimeline $ \a -> do
          liftIO (readIORef occRef) >>= \case
            Just _ -> return () -- Already fired; TODO: Should we do something else here?  Ideally, we could avoid this `readIORef` entirely
            Nothing -> do
              liftIO $ writeIORef occRef $ Just a
              scheduleClear occRef
              propagateFast a subscribers
    return (ek, SpiderEvent e)

instance HasSpiderTimeline x => Reflex.Host.Class.MonadSubscribeEvent (SpiderTimeline x) (SpiderHost x) where
  {-# INLINABLE subscribeEvent #-}
  subscribeEvent = runFrame . runSpiderHostFrame . Reflex.Host.Class.subscribeEvent

instance HasSpiderTimeline x => Reflex.Host.Class.MonadReflexHost (SpiderTimeline x) (SpiderHost x) where
  type ReadPhase (SpiderHost x) = Reflex.Spider.Internal.ReadPhase x
  fireEventsAndRead es (Reflex.Spider.Internal.ReadPhase a) = run es a
  runHostFrame = runFrame . runSpiderHostFrame
