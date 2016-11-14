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

instance (
#ifdef SPECIALIZE_TO_SPIDERTIMELINE_GLOBAL
           x ~ Global
#endif
         ) => Reflex.Class.MonadSample (SpiderTimeline x) (SpiderPullM x) where
  {-# INLINABLE sample #-}
  sample = coerce . readBehaviorTracked . unSpiderBehavior

instance (
#ifdef SPECIALIZE_TO_SPIDERTIMELINE_GLOBAL
           x ~ Global
#endif
         ) => Reflex.Class.MonadSample (SpiderTimeline x) (SpiderPushM x) where
  {-# INLINABLE sample #-}
  sample (SpiderBehavior b) = SpiderPushM $ readBehaviorUntracked b

instance HasSpiderTimeline x => Reflex.Class.MonadHold (SpiderTimeline x) (SpiderPushM x) where
  {-# INLINABLE hold #-}
  hold v0 e = Reflex.Class.current <$> Reflex.Class.holdDyn v0 e
  {-# INLINABLE holdDyn #-}
  holdDyn v0 (SpiderEvent e) = SpiderPushM $ fmap (SpiderDynamic . dynamicHoldIdentity) $ Reflex.Spider.Internal.hold v0 $ coerce e
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 (SpiderEvent e) = SpiderPushM $ SpiderIncremental . dynamicHold <$> Reflex.Spider.Internal.hold v0 e

instance HasSpiderTimeline x => Monad (Reflex.Class.Dynamic (SpiderTimeline x)) where
  {-# INLINE return #-}
  return = pure
  {-# INLINE (>>=) #-}
  x >>= f = SpiderDynamic $ dynamicDynIdentity $ newJoinDyn $ newMapDyn (unSpiderDynamic . f) $ unSpiderDynamic x
  {-# INLINE (>>) #-}
  _ >> y = y
  {-# INLINE fail #-}
  fail _ = error "Dynamic does not support 'fail'"

newJoinDyn :: HasSpiderTimeline x => Reflex.Spider.Internal.Dynamic x (Identity (Reflex.Spider.Internal.Dynamic x (Identity a))) -> Reflex.Spider.Internal.Dyn x (Identity a)
newJoinDyn d =
  let readV0 = readBehaviorTracked . dynamicCurrent =<< readBehaviorTracked (dynamicCurrent d)
      eOuter = Reflex.Spider.Internal.push (fmap (Just . Identity) . readBehaviorUntracked . dynamicCurrent . runIdentity) $ dynamicUpdated d
      eInner = Reflex.Spider.Internal.switch $ dynamicUpdated <$> dynamicCurrent d
      eBoth = Reflex.Spider.Internal.coincidence $ dynamicUpdated . runIdentity <$> dynamicUpdated d
      v' = unSpiderEvent $ Reflex.Class.leftmost $ map SpiderEvent [eBoth, eOuter, eInner]
  in unsafeDyn readV0 v'

instance HasSpiderTimeline x => Functor (Reflex.Class.Dynamic (SpiderTimeline x)) where
  fmap f = SpiderDynamic . newMapDyn f . unSpiderDynamic

instance HasSpiderTimeline x => Applicative (Reflex.Class.Dynamic (SpiderTimeline x)) where
  pure = SpiderDynamic . dynamicConst
  SpiderDynamic a <*> SpiderDynamic b = SpiderDynamic $ Reflex.Spider.Internal.zipDynWith ($) a b
  _ *> b = b
  a <* _ = a

holdSpiderEventM :: HasSpiderTimeline x => a -> Reflex.Class.Event (SpiderTimeline x) a -> EventM x (Reflex.Class.Behavior (SpiderTimeline x) a)
holdSpiderEventM v0 e = fmap (SpiderBehavior . behaviorHoldIdentity) $ Reflex.Spider.Internal.hold v0 $ coerce $ unSpiderEvent e

holdDynSpiderEventM :: HasSpiderTimeline x => a -> Reflex.Class.Event (SpiderTimeline x) a -> EventM x (Reflex.Class.Dynamic (SpiderTimeline x) a)
holdDynSpiderEventM v0 e = fmap (SpiderDynamic . dynamicHoldIdentity) $ Reflex.Spider.Internal.hold v0 $ coerce $ unSpiderEvent e

holdIncrementalSpiderEventM :: (HasSpiderTimeline x, Patch p) => PatchTarget p -> Reflex.Class.Event (SpiderTimeline x) p -> EventM x (Reflex.Class.Incremental (SpiderTimeline x) p)
holdIncrementalSpiderEventM v0 e = fmap (SpiderIncremental . dynamicHold) $ Reflex.Spider.Internal.hold v0 $ unSpiderEvent e

instance HasSpiderTimeline x => Reflex.Class.MonadHold (SpiderTimeline x) (SpiderHost x) where
  {-# INLINABLE hold #-}
  hold v0 e = runFrame . runSpiderHostFrame $ Reflex.Class.hold v0 e
  {-# INLINABLE holdDyn #-}
  holdDyn v0 e = runFrame . runSpiderHostFrame $ Reflex.Class.holdDyn v0 e
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 e = runFrame . runSpiderHostFrame $ Reflex.Class.holdIncremental v0 e

instance HasSpiderTimeline x => Reflex.Class.MonadSample (SpiderTimeline x) (SpiderHostFrame x) where
  sample = SpiderHostFrame . readBehaviorUntracked . unSpiderBehavior --TODO: This can cause problems with laziness, so we should get rid of it if we can

instance HasSpiderTimeline x => Reflex.Class.MonadHold (SpiderTimeline x) (SpiderHostFrame x) where
  {-# INLINABLE hold #-}
  hold v0 e = SpiderHostFrame $ fmap (SpiderBehavior . behaviorHoldIdentity) $ Reflex.Spider.Internal.hold v0 $ coerce $ unSpiderEvent e
  {-# INLINABLE holdDyn #-}
  holdDyn v0 e = SpiderHostFrame $ fmap (SpiderDynamic . dynamicHoldIdentity) $ Reflex.Spider.Internal.hold v0 $ coerce $ unSpiderEvent e
  {-# INLINABLE holdIncremental #-}
  holdIncremental v0 e = SpiderHostFrame $ fmap (SpiderIncremental . dynamicHold) $ Reflex.Spider.Internal.hold v0 $ unSpiderEvent e

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

--------------------------------------------------------------------------------
-- Deprecated items
--------------------------------------------------------------------------------

-- | 'SpiderEnv' is the old name for 'SpiderTimeline'
{-# DEPRECATED SpiderEnv "Use 'SpiderTimelineEnv' instead" #-}
type SpiderEnv = SpiderTimeline
