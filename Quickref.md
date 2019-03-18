# Reflex Quick(ish) Reference

## Typeclasses

Many Reflex functions operate in monadic context `m a`, where the monad 'm' supports various additional typeclasses such as MonadWidget, MonadHold, or MonadSample in addition to Monad itself.  The actual 'm' in use will be determined by the top-level entry point of the FRP host (such as Reflex-Dom -- see the bottom of the Reflex-Dom quick reference for details).

The function signatures here have been simplified by removing many typeclass constraints and adding simple annotations to each function.  Also the ubiquitous 't' type parameter has been removed.

Some of these functions are *pure*:  They operate on Events, Behaviors, or Dynamics uniformly without regard to the "current time".  Other functions must operate in some monadic context, because they produce a result "as of now"  (e.g., any function that takes an "initial" value, or returns a "current" value).  Annotations are used to distinguish these cases:

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
[ ]   <$        ::              a ->        Event b -> Event a

-- Event to identical Event with debug trace.  (Only prints if Event is ultimately used.)
[ ]   traceEvent     :: Show a => String -> Event a -> Event a
[ ]   traceEventWith ::    (a -> String) -> Event a -> Event a

-- Transform Event to Event by sampling Behavior or Dynamic
[ ]   gate                       ::                     Behavior Bool -> Event a -> Event a
[ ]   tag                        ::                        Behavior a -> Event b -> Event a
[ ]   tagPromptlyDyn             ::                         Dynamic a -> Event b -> Event a
[ ]   attach                     ::                        Behavior a -> Event b -> Event (a, b)
[ ]   attachPromptlyDyn          ::                         Dynamic a -> Event b -> Event (a, b)
[ ]   attachWith                 :: (a -> b ->       c) -> Behavior a -> Event b -> Event c
[ ]   attachPromptlyDynWith      :: (a -> b ->       c) ->  Dynamic a -> Event b -> Event c
[ ]   attachWithMaybe            :: (a -> b -> Maybe c) -> Behavior a -> Event b -> Event c
[ ]   attachPromptlyDynWithMaybe :: (a -> b -> Maybe c) ->  Dynamic a -> Event b -> Event c
[ ]   <@>                        ::                 Behavior (a -> b) -> Event a -> Event b
[ ]   <@                         ::                        Behavior a -> Event b -> Event a

-- Combine multiple Events
[ ]   <>         ::      Semigroup a => Event a -> Event a -> Event a
[ ]   difference ::                     Event a -> Event b -> Event a
[ ]   align      ::                     Event a -> Event b -> Event (These a b)
[ ]   alignWith  :: (These a b -> c) -> Event a -> Event b -> Event c
[ ]   mergeWith  :: (a -> a -> a) -> [Event a] -> Event a
[ ]   leftmost   :: [Event a] -> Event a
[ ]   mergeList  :: [Event a] -> Event (NonEmpty a)
[ ]   merge      :: GCompare k => DMap k Event -> Event (DMap k Identity)
[ ]   mergeMap   :: Ord k => Map k (Event a) -> Event (Map k a)

-- Efficient one-to-many fanout
[ ]   fanMap    :: Ord k      => Event (Map k a)         -> EventSelector (Const2 k a)
[ ]   fan       :: GCompare k => Event (DMap k Identity) -> EventSelector k
[ ]   select    ::                                          EventSelector k -> k a -> Event a
[ ]   fanEither :: Event (Either a b) -> (Event a, Event b)
[ ]   fanThese  :: Event (These a b)  -> (Event a, Event b)

-- Event to Event via function that can sample current values
[ ]   push       :: (a -> m (Maybe b)) -> Event a -> Event b
[ ]   pushAlways :: (a -> m        b ) -> Event a -> Event b
      -- Note supplied function operates in [H] context

-- Split Event into next occurrence and all other future occurrences, as of now.
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

-- Create Behavior with given initial value, updated when the Event fires.
[H]   hold     :: a ->   Event a -> m (Behavior a)

-- Transform Behavior to Behavior using function
[ ]   fmap ::               (a -> b) ->        Behavior a -> Behavior b
[ ]   ffor ::             Behavior a ->          (a -> b) -> Behavior b
[ ]   <*>  ::      Behavior (a -> b) ->        Behavior a -> Behavior b
[ ]   >>=  ::             Behavior a -> (a -> Behavior b) -> Behavior b
[ ]   <>   :: Monoid a => Behavior a ->        Behavior a -> Behavior a
-- ... plus many more due to typeclass membership

-- Combine multiple behaviors via applicative instance
[ ]   ffor2 :: Behavior a -> Behavior b ->               (a -> b -> c)      -> Behavior c
[ ]   ffor3 :: Behavior a -> Behavior b -> Behavior c -> (a -> b -> c -> d) -> Behavior d

-- Behavior to Behavior by sampling current values
[S]   sample :: Behavior a -> m a
[ ]   pull   ::               m a -> Behavior a
      -- Note supplied value is in [S] context
```

## Functions producing Dynamic

```haskell
-- Trivial Dynamic
[ ]   constDyn ::                                  a            ->    Dynamic a

-- Create Dynamic with given initial value, updated (various ways) when the Event fires.
[H]   holdDyn       ::                             a -> Event a -> m (Dynamic a)
[H]   foldDyn       :: (a -> b ->           b ) -> b -> Event a -> m (Dynamic b)
[H]   foldDynMaybe  :: (a -> b ->     Maybe b ) -> b -> Event a -> m (Dynamic b)
[H]   foldDynM      :: (a -> b -> m'        b ) -> b -> Event a -> m (Dynamic b)
[H]   foldDynMaybeM :: (a -> b -> m' (Maybe b)) -> b -> Event a -> m (Dynamic b)
      -- Note m' supplies [H] context

-- Initial value is 0; counts Event firings from now.
[H]   count :: Num b => Event a -> m (Dynamic b)
-- Initial Bool value is supplied; toggles at each Event firing.
[H]   toggle :: Bool -> Event a -> m (Dynamic Bool)

-- Transform Dynamic to Dynamic using function
[ ]   ffor         ::   Dynamic a ->  (a -> b) -> Dynamic b
[ ]   fmap         :: (a ->    b) -> Dynamic a -> Dynamic b
[ ]   splitDynPure ::           Dynamic (a, b) -> (Dynamic a, Dynamic b)

-- Combine multiple Dynamics
[ ]   mconcat                   :: Monoid a => [Dynamic a] -> Dynamic a
[ ]   distributeDMapOverDynPure :: GCompare k => DMap (WrapArg Dynamic k) -> Dynamic (DMap k)
[ ]   <*>                       ::           Dynamic (a -> b) ->        Dynamic a -> Dynamic b
[ ]   >>=                       ::                  Dynamic a -> (a -> Dynamic b) -> Dynamic b
[ ]   zipDynWith                :: (a -> b -> c) -> Dynamic a -> Dynamic b        -> Dynamic c

-- Combine multiple dynamics via applicative instance
[ ]   ffor2 :: Dynamic a -> Dynamic b ->              (a -> b -> c)      -> Dynamic c
[ ]   ffor3 :: Dynamic a -> Dynamic b -> Dynamic c -> (a -> b -> c -> d) -> Dynamic d

-- Efficient one-to-many fanout
[ ]   demux   :: Ord k => Dynamic k -> Demux k
[ ]   demuxed :: Eq k  =>              Demux k -> k -> Dynamic Bool

-- Dynamic to Dynamic, removing updates w/o value change
[H]   holdUniqDyn   :: Eq a => Dynamic a -> m (Dynamic a)
[H]   holdUniqDynBy :: (a -> a -> Bool) -> Dynamic t a -> m (Dynamic t a)

-- Dynamic to identical Dynamic with debug trace.  (Only prints if Dynamic is ultimately used.)
[ ]   traceDyn     :: Show a => String -> Dynamic a -> Dynamic a
[ ]   traceDynWith ::    (a -> String) -> Dynamic a -> Dynamic a
```

## Flattening functions

These functions flatten nested types such as Event-of-Event, Behavior-of-Event, Event-of-Behavior etc, removing the outer wrapper.

For Events, the returned Event fires whenever the latest Event supplied by the wrapper fires.  There are differences in how the functions handle *coincidences* -- situations where the old or new Event fires at the same instant that we switch from old to new.  In some cases, the output Event tracks the old Event at the instant of switchover, while in other cases it tracks the new Event.

```haskell
-- Flatten Behavior-of-Event to Event.  Old Event is used during switchover.
[ ]   switch            ::                  Behavior (Event a)  ->    Event a

-- Flatten Dyanmic-of-Event to Event.  New Event is used immediately.
[ ]   switchDyn         ::                   Dynamic (Event a)  ->    Event a

-- Flatten Event-of-Event to Event that fires when both wrapper AND new Event fire.
[ ]   coincidence       ::                     Event (Event a)  ->    Event a

-- Flatten Behavior-of-Behavior to Behavior. Behavior is an instance of Monad,
-- so the 'join' function from 'Control.Monad' suffices. The output of 'join' is
-- a Behavior whose value is equal to the current value of the current value of
-- the input.
[ ]   join              ::                Behavior (Behavior a) ->    Behavior a


-- Flatten Dynamic-of-Dynamic to Dynamic.  New Dynamic is used immediately.
-- Output updated whenever inner OR outer Dynamic updates.
[ ]   join              ::          Dynamic        (Dynamic a)  ->  Dynamic a
[ ]   joinDynThroughMap :: Ord k => Dynamic (Map k (Dynamic a)) ->  Dynamic (Map k a)

-- Analogous to 'hold':  Create a Behavior that is initially identical to the
-- supplied Behavior.  Updated to track a new Behavior whenever the Event fires.
[H]   switcher          ::    Behavior a -> Event (Behavior a)  -> m (Behavior a)

-- Similar to above, for Events.  Created Event initially tracks the first argument.
-- At switchover, the output Event immediately tracks the new Event.
[H]   switchHold        ::       Event a ->    Event (Event a)  -> m (Event a)
```

## Typeclasses to introspect and modify an FRP network.

The functions mentioned above are used to create a static FRP network.
There are additional typeclasses that can be used to modify the FRP network, to have it interact with IO action, or to introspect the building of the network.

Th typeclasses and their associated annotations include:

- `PostBuild`
    Fire an Event when an FRP network has been set up.
    ```haskell
    [B]   -- Function runs in any monad supporting PostBuild
    ```

- `Adjustable` 
    Use Events to add or remove pieces of an FRP network.
    ```haskell
    [A]   -- Function runs in any monad supporting Adjustable
    ```

- `TriggerEvent`
    Create new externally-fired Events from within an FRP network.
    ```haskell
    [T]   -- Function runs in any monad supporting TriggerEvent
    ```

- `PerformEvent`
    Use Events to trigger IO actions and to collect their results.
    ```haskell
    [P]   -- Function runs in any monad supporting PerformEvent
    ```

## Startup

```haskell
-- One-shot Event that is triggered once the FRP network has been built
[B]   getPostBuild :: m (Event ())
```

## Collection management functions

```haskell
-- Turn a Dynamic key/value map into a set of dynamically-changing widgets.
[H,A,B]   listWithKey :: Ord k =>
            Dynamic (Map k v) -> (k -> Dynamic v -> m        a ) -> m (Dynamic (Map k a))

-- Same as above where the widget constructor doesn't care about the key.
[H,A,B]   list        :: Ord k =>
            Dynamic (Map k v) -> (     Dynamic v -> m        a ) -> m (Dynamic (Map k a))

-- Even simpler version where there are no keys and we just use a list.
[H,A,B]   simpleList  ::
            Dynamic       [v] -> (     Dynamic v -> m        a ) -> m (Dynamic       [a])

-- Like listWithKey specialized for widgets returning (Event a).
[H,A,B]   listViewWithKey :: Ord k =>
            Dynamic (Map k v) -> (k -> Dynamic v -> m (Event a)) -> m (Event   (Map k a))

-- Create a dynamically-changing set of widgets, one of which is selected at any time.
[H,A,B]   selectViewListWithKey_ :: Ord k => Dynamic k ->
            Dynamic (Map k v) -> (k -> Dynamic v -> Dynamic Bool -> m (Event a)) -> m (Event k)

-- Same as listWithKey, but takes initial values and an updates Event instead of a Dynamic.
[H,A]     listWithKeyShallowDiff :: Ord k =>
            Map k v -> Event (Map k (Maybe v)) -> (k -> v -> Event v -> m a) -> m (Dynamic (Map k a))
```

## Creating new events from within an FRP network.

```haskell
-- Create a new Event and a function that will cause the Event to fire, from within an FRP network
[T]   newTriggerEvent      :: m (Event a, a -> IO ())
```

## Connecting to the real world (I/O)

```haskell
-- Run side-effecting actions in Event when it occurs; returned Event contains
-- results. Side effects run in the (Performable m) monad which is associated with
-- the (PerformEvent t m) typeclass constraint. 
-- This allows for working with IO when a ((MonadIO (Performable m)) constraint is available.
[P]   performEvent        :: Event (Performable m a )                 -> m (Event a)

-- Just run side-effects; no return Event
[P]   performEvent_       :: Event (Performable m ())                 -> m ()

-- Actions run asynchronously; actions are given a callback to send return values
[P,T]   performEventAsync :: Event ((a -> IO ()) -> Performable m ()) -> m (Event a)
```

### Time

```haskell
-- Create Event at given interval with given basis time.
[P,T]   tickLossy :: NominalDiffTime -> UTCTime -> m (Event t TickInfo)

-- Delay an Event's occurrences by a given amount in seconds.
[P,T]   delay :: NominalDiffTime -> Event t a -> m (Event t a)
```

## Networks

```haskell
-- Functions from Reflex.Network used to deal with Dynamics/Events carrying (m a)

-- Given a Dynamic of network-creating actions, create a network that is recreated whenever the Dynamic updates. 
-- The returned Event of network results occurs when the Dynamic does. Note: Often, the type a is an Event, 
-- in which case the return value is an Event-of-Events that would typically be flattened (via switchHold).
[P,A]   networkView :: Dynamic (m a) -> m (Event a) 

-- Given an initial network and an Event of network-creating actions, create a network that is recreated whenever the 
-- Event fires. The returned Dynamic of network results occurs when the Event does. Note: Often, the type a is an 
-- Event, in which case the return value is a Dynamic-of-Events that would typically be flattened.
[H,A]   networkHold :: m a -> Event (m a) -> m (Dynamic a) 

-- Render a placeholder network to be shown while another network is not yet done building
[P,A]   untilReady :: m a -> m b -> m (a, Event b) 
