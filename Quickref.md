# Reflex Quick(ish) Reference

## Typeclasses

Many Reflex functions operate in monadic context `m a`, where the monadic type constructor 'm' supports various additional typeclasses such as MonadWidget, MonadHold, or MonadSample in addition to Monad itself.  The actual 'm' in use will be determined by the top-level entry point of the FRP host (such as Reflex-Dom -- see the bottom of the Reflex-Dom quick reference for details).

The function signatures here have been simplified by removing many typeclass constraints and adding simple annotations to each function.  Also the ubiquitous 't' type parameter has been removed.

Functions annotated with "[ ]" are *pure*:  They operate on Events, Behaviors, or Dynamics uniformly without regard to the "current time".

Other functions must operate in some monadic context, because they produce a result "as of now".  The annotations are:

```haskell
[ ]   -- Pure function
[S]   -- Function runs in any monad supporting MonadSample
[H]   -- Function runs in any monad supporting MonadHold
```

Since MonadHold depends on MonadSample, any [S] function also runs in [H] context.

## Functions producing Event

```haskell
-- Trivial Event
[ ]   never   ::              Event a

-- Extract Event from Dynamic
[ ]   updated :: Dynamic a -> Event a

-- Transform Event to Event using function
[ ]   fmap      :: (a ->       b) ->        Event a -> Event b
[ ]   fmapMaybe :: (a -> Maybe b) ->        Event a -> Event b
[ ]   ffilter   :: (a ->    Bool) ->        Event a -> Event a
[ ]   ffor      ::        Event a -> (a ->       b) -> Event b
[ ]   fforMaybe ::        Event a -> (a -> Maybe b) -> Event b

-- Event to identical Event with debug trace.
[ ]   traceEvent     :: Show a => String -> Event a -> Event a
[ ]   traceEventWith ::    (a -> String) -> Event a -> Event a

-- Transform Event to Event by sampling Behavior or Dynamic
[ ]   gate   ::                                 Behavior Bool -> Event a -> Event a
[ ]   tag    ::                                    Behavior a -> Event b -> Event a
[ ]   tagDyn ::                                     Dynamic a -> Event b -> Event a
[ ]   attach ::                                    Behavior a -> Event b -> Event (a, b)
[ ]   attachDyn ::                                  Dynamic a -> Event b -> Event (a, b)
[ ]   attachWith ::         (a -> b ->       c) -> Behavior a -> Event b -> Event c
[ ]   attachDynWith ::      (a -> b ->       c) ->  Dynamic a -> Event b -> Event c
[ ]   attachWithMaybe ::    (a -> b -> Maybe c) -> Behavior a -> Event b -> Event c
[ ]   attachDynWithMaybe :: (a -> b -> Maybe c) ->  Dynamic a -> Event b -> Event c

-- Combine multiple Events
[ ]   <> :: Monoid a => Event a -> Event a -> Event a
[ ]   mergeWith :: (a -> a -> a) -> [Event a] -> Event a
[ ]   leftmost :: [Event a] -> Event a
[ ]   mergeList :: [Event a] -> Event (NonEmpty a)
[ ]   merge :: GCompare k => DMap (WrapArg Event k) -> Event (DMap k)
[ ]   mergeMap :: Ord k => Map k (Event a) -> Event (Map k a)

-- Efficient one-to-many fanout
[ ]   fanMap ::      Ord k => Event (Map k a) -> EventSelector (Const2 k a)
[ ]   fan    :: GCompare k => Event  (DMap k) -> EventSelector k
[ ]   select ::                                  EventSelector k -> k a -> Event a

-- Event to Event via monadic function
[ ]   push       :: (a -> m (Maybe b)) -> Event a -> Event b
[ ]   pushAlways :: (a -> m        b ) -> Event a -> Event b
      -- Note supplied function operates in [H] context

-- Event to monadic Event
[H]   headE     :: Event a -> m (Event a)
[H]   tailE     :: Event a -> m (Event a)
[H]   headTailE :: Event a -> m (Event a, Event a)
```

## Functions producing Behavior

```haskell
-- Trivial Behavior
[ ]   constant ::              a ->    Behavior a

-- Extract Behavior from Dynamic
[ ]   current  ::      Dynamic a ->    Behavior a

-- Event to monadic Behavior
[H]   hold     :: a ->   Event a -> m (Behavior a)

-- Transform Behavior to Behavior using function
[ ]   fmap ::   (a -> b) -> Behavior a -> Behavior b
[ ]   ffor :: Behavior a ->   (a -> b) -> Behavior b
[ ]   <*> :: Behavior (a -> b) -> Behavior a -> Behavior b
[ ]   >>= :: Behavior a -> (a -> Behavior b) -> Behavior b
[ ]   <> :: Monoid a => Behavior a -> Behavior a -> Behavior a
-- ... plus many more due to typeclass membership

-- Behavior to Behavior via monadic value
[S]   sample :: Behavior a -> m a
[ ]   pull   ::               m a -> Behavior a
      -- Note supplied value is in [S] context
```

## Functions producing Dynamic

```haskell
-- Trivial Dynamic
[ ]   constDyn ::                     a            ->    Dynamic a

-- Construct Dynamic from Event
[H]   holdDyn  ::                     a -> Event a -> m (Dynamic a)
[H]   foldDyn  :: (a -> b ->    b) -> b -> Event a -> m (Dynamic b)
[H]   foldDynM :: (a -> b -> m' b) -> b -> Event a -> m (Dynamic b)
      -- Note supplied function operates in [H] context
[H]   count :: Num b => Event a -> m (Dynamic b)
[H]   toggle :: Bool -> Event a -> m (Dynamic Bool)

-- Transform Dynamic to Dynamic using function
[H]   forDyn  ::   Dynamic a ->  (a -> b) -> m (Dynamic b)
[H]   mapDyn  :: (a ->    b) -> Dynamic a -> m (Dynamic b)
[H]   mapDynM :: (a -> m' b) -> Dynamic a -> m (Dynamic b)
      -- Note supplied function runs in [S] context
[H]   splitDyn :: Dynamic (a, b) -> m (Dynamic a, Dynamic b)

-- Combine multiple Dynamics
[H]   mconcatDyn :: Monoid a => [Dynamic a] -> m (Dynamic a)
[H]   distributeDMapOverDyn :: GCompare k => DMap (WrapArg Dynamic k) -> m (Dynamic (DMap k))
[H]   combineDyn :: (a -> b -> c) -> Dynamic a -> Dynamic b -> m (Dynamic c)

-- Efficient one-to-many fanout
[ ]   demux      :: Ord k => Dynamic k -> Demux k
[H]   getDemuxed ::  Eq k =>              Demux k -> k -> m (Dynamic Bool)

-- Dynamic to Dynamic, removing updates w/o value change
[ ]   nubDyn :: Eq a => Dynamic a -> Dynamic a

-- Dynamic to identical Dynamic with debug trace.
[ ]   traceDyn     :: Show a => String -> Dynamic a -> Dynamic a
[ ]   traceDynWith ::    (a -> String) -> Dynamic a -> Dynamic a
```

## Flattening functions

These functions flatten nested types such as Event-of-Event, Behavior-of-Event, Event-of-Behavior etc, removing the outer wrapper.

For Events, the returned Event fires whenever the latest Event supplied by the wrapper fires.  There are differences in how the functions handle *coincidences* -- situations where the old or new Event fires at the same instant that we switch from old to new.  In some cases, the output Event tracks the old Event at the instant of switchover, while in other cases it tracks the new Event.

```haskell
-- Flatten Event-of-Event to monadic Event.  New Event is used immediately.
[H]   switchPromptly    ::       Event a ->    Event (Event a)  -> m (Event a)

-- Flatten Behavior-of-Event to Event.  Old Event is used during switchover.
[ ]   switch            ::                  Behavior (Event a)  ->    Event a

-- Flatten Dyanmic-of-Event to Event.  New Event is used immediately.
[ ]   switchPromptlyDyn ::                   Dynamic (Event a)  ->    Event a

-- Flatten Event-of-Event to Event that fires when both wrapper AND new Event fire.
[ ]   coincidence       ::                     Event (Event a)  ->    Event a

-- Flatten Dynamic-of-Dynamic to Dynamic.  New Dynamic is used immediately.
-- Output updated whenever inner OR outer Dynamic updates. 
[ ]   joinDyn           ::          Dynamic        (Dynamic a)  ->  Dynamic a
[ ]   joinDynThroughMap :: Ord k => Dynamic (Map k (Dynamic a)) ->  Dynamic (Map k a)

-- Flatten Event-of-Behavior to monadic Behavior
[H]   switcher          ::    Behavior a -> Event (Behavior a)  -> m (Behavior a)
```
