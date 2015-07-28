# Reflex Quick Reference

Note that function signatures have been simplified by removing most typeclass constraints.

Functions annotated with "[ ]" are *pure*:  They operate on Events, Behaviors, or Dynamics uniformly without regard to the "current time".

Other functions must operate in some monadic context, because they produce a result "as of now".  Functions annotated with "[H]" run in any monad supporting MonadHold.  For Reflex-Dom users, this will typically be MonadWidget.  Functions with "[S]" (of which there is only one) operate in MonadSample.  Again, for Reflex-Dom users this is likely MonadWidget (because any MonadHold context also supports MonadSample).

### Functions producing Event

```haskell
-- Trivial Event
[ ]   never :: Event t a
-- Extract Event from Dynamic
[ ]   updated :: Dynamic t a -> Event t a
-- Transform Event to Event using function
[ ]   fmap :: (a -> b) -> Event t a -> Event t b
[ ]   fmapMaybe :: (a -> Maybe b) -> Event t a -> Event t b
[ ]   ffor :: Event t a -> (a -> b) -> Event t b
[ ]   fforMaybe :: Event t a -> (a -> Maybe b) -> Event t b
[ ]   ffilter :: (a -> Bool) -> Event t a -> Event t a
[ ]   splitE :: Event t (a, b) -> (Event t a, Event t b)
-- Event to identical Event with debug trace.
[ ]   traceEvent :: Show a => String -> Event t a -> Event t a
[ ]   traceEventWith :: (a -> String) -> Event t a -> Event t a
-- Transform Event to Event by sampling Behavior or Dynamic
[ ]   tag :: Behavior t b -> Event t a -> Event t b
[ ]   tagDyn :: Dynamic t a -> Event t b -> Event t a
[ ]   gate :: Behavior t Bool -> Event t a -> Event t a
[ ]   attach :: Behavior t a -> Event t b -> Event t (a, b)
[ ]   attachDyn :: Dynamic t a -> Event t b -> Event t (a, b)
[ ]   attachWith :: (a -> b -> c) -> Behavior t a -> Event t b -> Event t c
[ ]   attachDynWith :: (a -> b -> c) -> Dynamic t a -> Event t b -> Event t c
[ ]   attachWithMaybe :: (a -> b -> Maybe c) -> Behavior t a -> Event t b -> Event t c
[ ]   attachDynWithMaybe :: (a -> b -> Maybe c) -> Dynamic t a -> Event t b -> Event t c
-- Combine multiple Events
[ ]   <> :: Monoid a => Event t a -> Event t a -> Event t a
[ ]   mergeWith :: (a -> a -> a) -> [Event t a] -> Event t a
[ ]   leftmost :: [Event t a] -> Event t a
[ ]   mergeList :: [Event t a] -> Event t (NonEmpty a)
[ ]   merge :: GCompare k => DMap (WrapArg (Event t) k) -> Event t (DMap k)
[ ]   mergeMap :: Ord k => Map k (Event t a) -> Event t (Map k a)
-- Unwrap Event-of-Event to Event
[ ]   coincidence :: Event t (Event t a) -> Event t a
-- Unwrap Behavior-of-Event to Event
[ ]   switch :: Behavior t (Event t a) -> Event t a
-- Unwrap Dyanmic-of-Event to Event
[ ]   switchPromptlyDyn :: Dynamic t (Event t a) -> Event t a
-- Efficient one-to-many fanout
[ ]   fan :: GCompare k => Event t (DMap k) -> EventSelector t k
[ ]   fanMap :: Ord k => Event t (Map k a) -> EventSelector t (Const2 k a)
[ ]   select :: EventSelector t k -> k a -> Event t a
-- Event to Event via monadic function
[ ]   push :: (a -> m (Maybe b)) -> Event t a -> Event t b
[ ]   pushAlways :: (a -> m b) -> Event t a -> Event t b
      -- Note supplied function operates in [H] context
-- Event to monadic Event
[H]   headE :: Event t a -> m (Event t a)
[H]   tailE :: Event t a -> m (Event t a)
[H]   headTailE :: Event t a -> m (Event t a, Event t a)
-- Event-of-Event to monadic Event
[H]   switchPromptly :: Event t a -> Event t (Event t a) -> m (Event t a)
```

### Functions producing Behavior

```haskell
-- Trivial Behavior
[ ]   constant :: a -> Behavior t a
-- Extract Behavior from Dynamic
[ ]   current :: Dynamic t a -> Behavior t a
-- Transform Behavior to Behavior using function
[ ]   fmap :: (a -> b) -> Behavior t a -> Behavior t b
[ ]   ffor :: Behavior t a -> (a -> b) -> Behavior t b
[ ]   <*> :: Behavior t (a -> b) -> Behavior t a -> Behavior t b
[ ]   >>= :: Behavior t a -> (a -> Behavior t b) -> Behavior t b
[ ]   <> :: Monoid a => Behavior t a -> Behavior t a -> Behavior t a
-- Monadic value to Behavior
[ ]   pull :: m a -> Behavior t a
      -- Note supplied value is in [S] context
-- Behavior to monadic value
[S]   sample :: Behavior t a -> m a
-- Event to monadic Behavior
[H]   hold :: a -> Event t a -> m (Behavior t a)
-- Unwrap Event-of-Behavior to monadic Behavior
[H]   switcher :: Behavior t a -> Event t (Behavior t a) -> m (Behavior t a)
```

### Functions producing Dynamic

```haskell
-- Trivial Dynamic
[ ]   constDyn :: a -> Dynamic t a
-- Construct monadic Dynamic from Event
[H]   holdDyn :: a -> Event t a -> m (Dynamic t a)
[H]   foldDyn :: (a -> b -> b) -> b -> Event t a -> m (Dynamic t b)
[H]   foldDynM :: (a -> b -> m' b) -> b -> Event t a -> m (Dynamic t b)
      -- Note supplied function operates in [H] context
[H]   count :: Num b => Event t a -> m (Dynamic t b)
[H]   toggle :: Bool -> Event t a -> m (Dynamic t Bool)
-- Transform Dynamic to monadic Dynamic using function
[H]   mapDyn :: (a -> b) -> Dynamic t a -> m (Dynamic t b)
[H]   forDyn :: Dynamic t a -> (a -> b) -> m (Dynamic t b)
[H]   mapDynM :: (a -> m' b) -> Dynamic t a -> m (Dynamic t b)
      -- Note supplied function runs in [S] context
[H]   splitDyn :: Dynamic t (a, b) -> m (Dynamic t a, Dynamic t b)
-- Combine multiple Dynamics
[H]   mconcatDyn :: Monoid a => [Dynamic t a] -> m (Dynamic t a)
[H]   distributeDMapOverDyn :: GCompare k => DMap (WrapArg (Dynamic t) k) -> m (Dynamic t (DMap k))
[H]   combineDyn :: (a -> b -> c) -> Dynamic t a -> Dynamic t b -> m (Dynamic t c)
-- Unwrap Dynamic-of-Dynamic to Dynamic
[ ]   joinDyn :: Dynamic t (Dynamic t a) -> Dynamic t a
[ ]   joinDynThroughMap :: Ord k => Dynamic t (Map k (Dynamic t a)) -> Dynamic t (Map k a)
-- Efficient one-to-many fanout
[ ]   demux :: Ord k => Dynamic t k -> Demux t k
[H]   getDemuxed :: Eq k => Demux t k -> k -> m (Dynamic t Bool)
-- Dynamic to Dynamic, removing Event occurrences w/o value change
[ ]   nubDyn :: Eq a => Dynamic t a -> Dynamic t a
-- Dynamic to identical Dynamic with debug trace.
[ ]   traceDyn :: Show a => String -> Dynamic t a -> Dynamic t a
[ ]   traceDynWith :: (a -> String) -> Dynamic t a -> Dynamic t a
```
