# Reflex / Reflex-Dom Quick(ish) Reference

## Typeclasses

Many Reflex functions operate in monadic context `X a`, where the monadic type X supports various additional typeclasses such as MonadWidget, MonadHold, or MonadSample in addition to Monad itself.  For Reflex-Dom users, the type X will generally be `Widget Spider (Gui Spider (WithWebView SpiderHost) (HostFrame Spider))`, from the Reflex-Dom main entry point:

```haskell
mainWidget :: Widget Spider (Gui Spider (WithWebView SpiderHost) (HostFrame Spider)) () -> IO ()
```

The function signatures here have been simplified by removing many typeclass constraints and adding simple annotations to each function.  Also the ubiquitous 't' type parameter has been removed.

Functions annotated with "[ ]" are *pure*:  They operate on Events, Behaviors, or Dynamics uniformly without regard to the "current time".

Other functions must operate in some monadic context, because they produce a result "as of now".  The annotations are:

```haskell
[S]   -- Function runs in any monad supporting MonadSample
[H]   -- Function runs in any monad supporting MonadHold
[W]   -- Function runs in any monad supporting MonadWidget
```

Since MonadWidget depends on MonadHold and MonadHold depends on MonadSample, any [S] function also runs in [H] or [W] context, and any [H] function also runs in [W].

## Reflex

### Functions producing Event

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

### Functions producing Behavior

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

### Functions producing Dynamic

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

### Flattening functions

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

## Reflex-Dom

### Creating widgets (DOM elements hooked into the FRP system)

#### Basic widgets

These functions generally take an element tag (such as "div", "h1", etc) and a child widget, and produce a widget of the given type containing the child.  Some of these functions return data of type `El`, which is a handle to the created DOM element along with a set of potential Events that might occur on that element.

Widgets may return any type (this is 'a' in many of the functions below).  Often this will be an Event carrying user interactions with the widget.

```haskell
-- Simplest form.  Create a widget of given type containing the given child.
-- Return whatever the child returns.
[W]   el         :: String ->                                m a -> m a

-- This version returns the 'El' as well.
[W]   el'        :: String ->                                m a -> m (El, a)

-- These two additionally apply attributes to the element, such as ("class" =: "blah")
[W]   elAttr     :: String ->          Map String String  -> m a -> m a
[W]   elAttr'    :: String ->          Map String String  -> m a -> m (El, a)

-- As above, but now the attribute map is Dynamic
[W]   elDynAttr  :: String -> Dynamic (Map String String) -> m a -> m a
[W]   elDynAttr' :: String -> Dynamic (Map String String) -> m a -> m (El, a)

-- Shortcut for elAttr when you only want to set the "class" attribute.
[W]   elClass    :: String ->                      String -> m a -> m a

-- Even shorter-cut for above when element type is "div".  Create a div of given class.
[W]   divClass   ::                                String -> m a -> m a

-- Create a widget of given type with arbitrary, Dymamic HTML inside.
[W]   elDynHtml'     :: String ->                      Dynamic String -> m El
[W]   elDynHtmlAttr' :: String -> Map String String -> Dynamic String -> m El

-- Shortcut for elDynHtml' where the type is "div" and you don't need the 'El'.
[W]   dynHtml        ::                                Dynamic String -> m ()

-- Create a static text element
[W]   text    ::              String -> m ()

-- Create a dynamic text element
[W]   dynText ::      Dynamic String -> m ()
[W]   display :: Show a => Dynamic a -> m ()

-- Create a "button" element with given label, return onClick Event
[W]   button :: String -> m (Event ())

-- Empty widget
[W]   blank :: m ()
```

#### Dynamic widgets

In the Dynamic cases so far, the *content* of a widget is dynamic but the *definition* of the widget is static.  The functions below enable the definition and/or structure of the widget itself to change over time.

Note the "list" functions do not imply particular HTML tags (ul, li, etc), though the widgets they create can have those tags if you construct them appropriately.

```haskell
-- Given a Dynamic of widget-creating actions, create a widget that is redefined
-- whenever the Dynamic updates. The returned Event of widget results occurs when
-- the Dynamic does.  Note that if type 'a' is an Event, the return value is an
-- Event-of-Events that would typically be flattened.
[W]   dyn        ::        Dynamic (m a) -> m (Event a)

-- Same as dyn, but takes initial value and an update Event instead of a Dynamic.
[W]   widgetHold :: m a ->   Event (m a) -> m (Dynamic a)

-- Given a Dynamic key/value map and a function to create a widget for a given
-- key & Dynamic value, create a bunch of widgets.  Widgets will be created,
-- destroyed, and updated appropriately as the map changes.  Returns Dynamic map
-- from keys to widget results.
[W]   listWithKey :: Ord k =>
          Dynamic (Map k v) -> (k -> Dynamic v -> m        a ) -> m (Dynamic (Map k a))

-- Same as above where the widget constructor doesn't care about the key.
[W]   list        :: Ord k =>
          Dynamic (Map k v) -> (     Dynamic v -> m        a ) -> m (Dynamic (Map k a))

-- Even simpler version where there are no keys and we just use a list.
[W]   simpleList  ::
          Dynamic       [v] -> (     Dynamic v -> m        a ) -> m (Dynamic       [a])

-- Like listWithKey specialized for widgets returning (Event a).  listWithKey would
-- return 'Dynamic (Map k (Event a))' in this scenario, but listViewWithKey flattens
-- this to 'Event (Map k a)' via 'switch'.
[W]   listViewWithKey :: Ord k =>
          Dynamic (Map k v) -> (k -> Dynamic v -> m (Event a)) -> m (Event   (Map k a))

-- As above, but there is a "current key", and the widget constructor gets a
-- Dynamic Bool indicating if that widget is currently selected.  The returned
-- Event fires when any of the widgets' returned Events fire, and returns the key
-- of an arbitrary firing widget.
[W]   selectViewListWithKey_ :: Ord k => Dynamic k ->
          Dynamic (Map k v) -> (k -> Dynamic v -> Dynamic Bool -> m (Event a)) -> m (Event k)

-- Same as listWithKey, but takes initial values and an updates Event instead of a Dynamic.
[W]   listWithKey' :: Ord k =>
          Map k v -> Event (Map k (Maybe v)) -> (k -> v -> Event v -> m a) -> m (Dynamic (Map k a))

-- tableDynAttr
-- tabDisplay
-- workflow
-- workflowView
```

#### Utility widgets

These are useful widgets that are implemented (or could be implemented) in terms of the low-level widgets above.

Some of these widget builders take a configuration record and return a record containing Events or other useful data associated with the created widget (similar to 'El').  The configuration records have default values, so you can just supply 'def'.  See Reflex/Dom/Widget/Input.hs for record fields (Lenses are provided).

```haskell
-- Text input.
[W]   textInput :: TextInputConfig -> m TextInput
[ ]   textInputGetEnter :: TextInput -> Event ()
[W]   textArea :: TextAreaConfig -> m TextArea

-- Checkbox.  The Bool supplies the initial state.
[W]   checkbox :: Bool -> CheckboxConfig -> m Checkbox

-- Dropdown with Dynamic options.  First argument is initial state.
[W]   dropdown :: (Ord k, Show k, Read k) =>
          k -> Dynamic (Map k String) -> DropdownConfig k -> m (Dropdown k)

-- Widget to efficiently display long scrolling lists.  Dynamically control number of items in
-- list, current scroll position, attributes, and row content.  Returns current scroll position
-- and current selection.  Full documentation in Reflex/Dom/Widget/Lazy.hs
[W]   virtualListWithSelection :: Ord k => Int -> Int -> Dynamic Int -> Int -> Event Int ->
          String -> Dynamic (Map String String) -> String -> Dynamic (Map String String) ->
          (k -> Dynamic v -> m ()) -> Dynamic (Map k v) -> m (Dynamic (Int, Int), Event k)
```

### Connecting to the real world (I/O)

#### Connecting to DOM events

```haskell
-- Extract the specified Event from an 'El'.  The allowable event names, and their
-- corresponding data types, are listed in Reflex/Dom/Widget/Basic.hs
[ ]   domEvent :: EventName en -> El -> Event (EventResultType en)
```

#### Performing arbitrary I/O in response to Events

```haskell
-- Run side-effecting actions in Event when it occurs; returned Event contains
-- results. Side effects run in (WidgetHost m) monad, which includes [S] and [H]
-- and can also do I/O via liftIO
[W]   performEvent      :: Event (                WidgetHost m  a) -> m (Event a)

-- Just run side-effects; no return Event
[W]   performEvent_     :: Event (                WidgetHost m ()) -> m ()

-- Actions run asynchronously; actions are given a callback to send return values
[W]   performEventAsync :: Event ((a -> IO ()) -> WidgetHost m ()) -> m (Event a)
```

#### XMLHttpRequest

Convenience functions for XMLHttpRequest.  For configuration field details, see Reflex/Dom/Xhr.hs.

```haskell
-- Given method, URL, and config record (with default instance), construct a request.
[ ]   xhrRequest :: String -> String -> XhrRequestConfig -> XhrRequest

-- Given Event of requests, issue them and produce Event of responses.
[W]   performRequestAsync :: Event XhrRequest -> m (Event XhrResponse)

-- Issue a collection of requests, wait for them ALL to complete, return collected results.
[W]   performRequestsAsync :: Traversable f => Event (f XhrRequest) -> m (Event (f XhrResponse))

-- Convenience function to decode JSON-encoded response.
[ ]   decodeXhrResponse :: FromJSON a => XhrResponse -> Maybe a

-- Simplified interface to "GET" URLs and return decoded results.
[W]   getAndDecode :: FromJSON a => Event String -> m (Event (Maybe a))
```

#### Time

```haskell
-- Given a time interval in seconds and a basis time, create an Event that fires on the given
-- interval. The Event will report the number of time intervals elapsed since the basis time
-- (and other info, see Reflex/Dom/Time.hs).  High load will cause ticks to be dropped.
[W]   tickLossy :: NominalDiffTime -> UTCTime -> m (Event t TickInfo)

-- Delay an Event's occurences by a given amount in seconds.
[W]   delay :: NominalDiffTime -> Event t a -> m (Event t a)
```

### Startup

```haskell
-- Reflex-Dom entry point.  Takes a monadic widget-building action of lengthy
-- type and turns it into an IO action.
[I]   mainWidget ::
          Widget Spider (Gui Spider (WithWebView SpiderHost) (HostFrame Spider)) () -> IO ()
[I]   mainWidgetWithHead ::
          Widget Spider (Gui Spider (WithWebView SpiderHost) (HostFrame Spider)) () ->
          Widget Spider (Gui Spider (WithWebView SpiderHost) (HostFrame Spider)) () -> IO ()
[I]   mainWidgetWithCss ::
          ByteString ->
          Widget Spider (Gui Spider (WithWebView SpiderHost) (HostFrame Spider)) () -> IO ()

-- One-shot Event that is triggered once all initial widgets are built
[W]   getPostBuild :: m (Event ())
```
