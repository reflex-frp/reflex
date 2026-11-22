# Revision history for reflex

## Unreleased

The goal of this release is to bring hosts closer to Reflex's denotational model of time as implemented by `Reflex.Pure`. Host functions no longer add unintended instants of their own, and the denotationally meaningful `now` event replaces `getPostBuild`.

### Breaking changes

* `runQueryT`, `withQueryT`, and `dynWithQueryT` now require the `MonadHold t m` constraint instead of `MonadFix m`. This is due to [#530](https://github.com/reflex-frp/reflex/pull/530) fixing an implementation detail in `runQueryT`.
* `MonadReflexHost` has a single method now, `hostFrameAndRead`, which runs a `HostFrame` action, fires triggers and reads events at the same instant. `fireEventsAndRead` and `runHostFrame` are made regular functions. `ReflexHost` now requires `MonadIO (HostFrame t)` ([#547](https://github.com/reflex-frp/reflex/pull/547)).
* Each `sample`, `hold` or `subscribeEvent` in `SpiderHost` used to run at its own instant, so you could miss an occurrence of `now`. `SpiderHost` is no longer an instance of `MonadSample`, `MonadHold` or `MonadSubscribeEvent`, and `MonadReflexHost` no longer requires `MonadSubscribeEvent`. Call these functions inside `runHostFrame` instead, like `runHostFrame (subscribeEvent e)` ([#547](https://github.com/reflex-frp/reflex/pull/547)).
* `FireCommand`'s field is now `runFireCommandAndRead`. It no longer returns a list with one result per instant. Instead, its read callback runs at each instant: first the triggers' instant, then each instant where `performEvent` results occur. The callback passes an accumulator along and can return `Left` to stop. `runFireCommand` is now a function that fires triggers without reading ([#547](https://github.com/reflex-frp/reflex/pull/547)).
* `runPostBuildT` no longer takes a post-build event; it just unwraps `PostBuildT`. Also, `Reflex.PostBuild.Base` no longer exports `mapIntMapWithAdjustImpl` and `mapDMapWithAdjustImpl` ([#547](https://github.com/reflex-frp/reflex/pull/547)).
* `getPostBuild` is now simply `now`, and `PostBuild` has `MonadHold` as a superclass. Functions like `listWithKey`, `networkView`, `workflowView` and `tickLossy` need `MonadHold` instead of `PostBuild` ([#547](https://github.com/reflex-frp/reflex/pull/547)).
* `Data.FastMutableIntMap` is removed ([#556](https://github.com/reflex-frp/reflex/pull/556)).

### Deprecations

* The `PostBuild` class is deprecated. Use `now` of `MonadHold` instead ([#547](https://github.com/reflex-frp/reflex/pull/547)).
* The legacy `NotReady` class is deprecated and removed as a constraint from library functions. It is an artifact from early reflex development and no longer has a purpose.

### Fixes

* `runHeadlessApp`: `getPostBuild` used to occur one instant after the app started. It now occurs at the instant the app starts, since it's just `now` ([#547](https://github.com/reflex-frp/reflex/pull/547)).
* `runHeadlessApp` now returns at the instant the shutdown event occurs. It used to keep going until no `performEvent` actions were left ([#547](https://github.com/reflex-frp/reflex/pull/547)).
* `hostPerformEventT`: `performEvent` used to do nothing for occurrences at the instant `hostPerformEventT` runs its argument, like the occurrence of `now`. These actions now run, and their results occur at the next instant ([#547](https://github.com/reflex-frp/reflex/pull/547)).
* Spider's `subscribeEvent` used to miss an occurrence at the instant it subscribes, like the occurrence of `now` ([#547](https://github.com/reflex-frp/reflex/pull/547)).

### Improvements

* Add `hostPerformEventTAndRead`, a version of `hostPerformEventT` that also lets you read events at the instant it runs its argument and at each instant where `performEvent` results occur ([#547](https://github.com/reflex-frp/reflex/pull/547)).
* Add `MonadSample` and `MonadHold` instances for the lazy `StateT`.

## 0.10.0.0

### Breaking changes
Not expected to affect any user code:

* The modules `Reflex.FastWeak` and `Data.FastWeakBag` and the `fast-weak` cabal flag are removed. On GHCJS, Spider now always uses `System.Mem.Weak`.
* `Data.WeakBag`: `insert` no longer takes a weak reference and a callback, `singleton` and `_weakBag_children` are removed, and `isEmpty` and `traverseWeakIntMap_` are new.
* Many changes to `Reflex.Spider.Internal`. The height code moved to the new module `Reflex.Spider.Height`.
* `EventWriterT` now wraps `StateT [Event t w] m`.
* The deprecated type synonym `SpiderEnv` is removed. Use `SpiderTimeline` instead.

### Deprecations

* `mergeIntMap` and `mergeIntMapIncremental` are deprecated in favour of `mergeInt` and `mergeIntIncremental` ([#537](https://github.com/reflex-frp/reflex/pull/537)).

### Fixes

* Fix a space leak in `fanInt`: it stayed subscribed to its parent for as long as its `EventSelectorInt` was retained. `fan` and `fanInt` now share one implementation ([#554](https://github.com/reflex-frp/reflex/pull/554)).
* Fix `cacheEvent` storing a thunk instead of a weak reference, which kept its subscription alive until the thunk was evaluated ([#552](https://github.com/reflex-frp/reflex/pull/552)).
* `newFanEventWithTrigger` can now fire multiple keys in one frame ([#555](https://github.com/reflex-frp/reflex/pull/555)).
* Fix incremental merges not initializing the holds and merges that are created when a patch adds a parent ([#552](https://github.com/reflex-frp/reflex/pull/552)).
* `merge` and the other `DMap` merges now check for causality loops on every parent occurrence, not only on the first one in a frame ([#556](https://github.com/reflex-frp/reflex/pull/556)).
* Fix races between the finalizer thread and the frame thread in root events.
* Fix lost occurrences in `TriggerEventT`'s `newEventWithLazyTriggerWithOnComplete`: an occurrence queued before the event was torn down and set up again didn't reach the new subscribers.
* Fix a race between the finalizer thread and the frame thread in `fan`: the per-key subscriber map is now updated atomically.
* Fix a race in `switch`: if a finalizer unsubscribed a switch during a frame, the switch still subscribed to its new parent and unsubscribed from its old parent twice.
* Incremental merges no longer subscribe to new parents from patches after they are unsubscribed.
* Correct the `Dynamic` `Monad` instance in `Reflex.Pure` ([#549](https://github.com/reflex-frp/reflex/pull/549)).

### Improvements

* All event nodes in Spider are now garbage collected together with the last subscriber instead of being unsubscribed by a finalizer.
* Spider now has its own `headE` implementation instead of using `slowHeadE` ([#546](https://github.com/reflex-frp/reflex/pull/546)).
* Reconnecting switches at the end of a frame does less work ([#548](https://github.com/reflex-frp/reflex/pull/548)).
* `EventWriterT` now uses `mergeInt`, which is more efficient ([#539](https://github.com/reflex-frp/reflex/pull/539)).
* All Spider merges now share one implementation, and a merge only invalidates its subscribers' heights when its own height changes ([#556](https://github.com/reflex-frp/reflex/pull/556)).
* Wide int-based merges no longer take quadratic time when all their parents change height.
* Rewrite RULES are rewritten and more comprehensive ([#556](https://github.com/reflex-frp/reflex/pull/556)).
* Spider's `ReflexHost`, `MonadReflexHost`, `MonadSample` and `MonadHold` instances are now specialized at `Global` ([#555](https://github.com/reflex-frp/reflex/pull/555)).
* With GHC 9.12 and later, reflex is built with `-fexpose-overloaded-unfoldings`, so its overloaded functions can be specialized in user code ([#556](https://github.com/reflex-frp/reflex/pull/556)).
* Smaller optimizations in fans, holds, weak bags, height bags, merges and `cacheEvent`.
* New `debug-frame` cabal flag guarding frame invariants, for use during development or debugging, or simply as extra precaution ([#551](https://github.com/reflex-frp/reflex/pull/551)).
* Support for GHC 9.14 by @ymeister in [#528](https://github.com/reflex-frp/reflex/pull/528).

## 0.9.4.1

* Bug fix for `CausalityLoopException`s caused by incorrect coincidence and merge height invalidation by @parenthetical in [#536](https://github.com/reflex-frp/reflex/pull/536).
* Space leak fixed: Applicative combination of Behaviors, with at least one never changing, no longer causes unbounded memory growth by @parenthetical in [#535](https://github.com/reflex-frp/reflex/pull/535).
* Stray debug print removed from non-debug builds.

## 0.9.4.0

* Add note about `requesting` semantics by @LightAndLight in https://github.com/reflex-frp/reflex/pull/508
* Dropped lens by @ilyakooo0 in https://github.com/reflex-frp/reflex/pull/463
* Build with ghc(js) 9.8.2 + 9.10.1 + 9.12.2 by @ymeister in https://github.com/reflex-frp/reflex/pull/502
* headless: Add MonadThrow (Performable m) by @ali-abrar in https://github.com/reflex-frp/reflex/pull/523
* Don't check in newSubscriberCoincidenceInner whether occRef is already set by @parenthetical in https://github.com/reflex-frp/reflex/pull/496
* Always enable Template Haskell by @Ericson2314 in https://github.com/reflex-frp/reflex/pull/484

## 0.9.3.4

* Support random 1.3

## 0.9.3.3

* Add support for GHC 9.12
* Loosen version bounds

## 0.9.3.2

* Add support for witherable 0.5

## 0.9.3.1

* Add support for GHC 9.8 and 9.10

## 0.9.3.0

* Headless Host: Generalize to allow returning arbitrary types

## 0.9.2.0

* Add MonadMask, MonadCatch, MonadThrow instances

## 0.9.1.0

* Headless Host: Add some MonadSample, MonadHold, and MonadFix instances

## 0.9.0.1

* Add support for ghc-9.6

## 0.9.0.0

* Breaking Change: Filter updates to `listWithKey` child widgets so that changes to the input Map don't cause spurious updates to unaffected children. This imposes an `Eq` constraint on the child values.
* Expose all Requester internals in Reflex.Requester.Base.Internal
* [Add EventWriter instance for RequesterT #469](https://github.com/reflex-frp/reflex/pull/469)

## 0.8.2.2

* Require witherable >= 0.4 and, hence, a newer monoidal-containers
* Support newer constraints-extras (0.4)

## 0.8.2.1

* Fix build for GHC 9.2
* Require patch >= 0.0.7.0

## 0.8.2.0

* Add `matchResponseMapWithRequests`, which it similar to `matchResponsesWithRequests` but allows processing of multiple responses at once.

## 0.8.1.1

* Allow newer hlint for older GHCs, and add upper bound for newer GHCs

## 0.8.1.0

* Add support for GHC 8.10
* Drop support for GHC < 8.4

## 0.8.0.0

* Replace 0.7.2.0 with 0.8.0.0 to reflect the `MonadHold` interface change. Deprecates 0.7.2.0.

## 0.7.2.0 -- *Deprecated*

* ([#416](https://github.com/reflex-frp/reflex/pull/416)) Add `now :: m (Event t ())` to `MonadHold`.
* Extend some dependency version bounds
* Fix HLint 3 test

## 0.7.1.1

*Backport release*.
Changes do not carry forward to 0.7.2.0.

* Add support for GHC 8.10
* Drop support for GHC < 8.4
* Extend some dependency version bounds
* Fix HLint 3 test

## 0.7.1.0

* ([#413](https://github.com/reflex-frp/reflex/pull/413), [#417](https://github.com/reflex-frp/reflex/pull/417)) Add `Reflex.Host.Headless` module which provides `runHeadlessApp` as an easy way to run a Reflex network in a "headless" environment.
* ([#420](https://github.com/reflex-frp/reflex/pull/420)) Add a [`Data.Zip.Unzip`](https://hackage.haskell.org/package/semialign-1.1/docs/Data-Zip.html#t:Unzip) instance for `Event`.
* ([#419](https://github.com/reflex-frp/reflex/pull/419)) Add `distributeIntMapOverDynPure` and `joinDynThroughIntMap` as convenience functions for working with `Dynamic` `IntMap`s.


## 0.7.0.0

* Add lifting instances for most classes to `Reflex.Profiled.Profiled`. ([#398](https://github.com/reflex-frp/reflex/pull/398))
* Class `MonadQuery t q m` now has a `Monad m` superclass constraint. ([#400](https://github.com/reflex-frp/reflex/pull/400))
* **(Breaking change)** Rename class `MonadBehaviorWriter` -> `BehaviorWriter` for consistency with `EventWriter`/`DynamicWriter`. ([#401](https://github.com/reflex-frp/reflex/pull/401))
* Introduce deprecated alias `MonadBehaviorWriter = BehaviorWriter`. ([#401](https://github.com/reflex-frp/reflex/pull/401))
* Fix bug in spider where event subscriptions would be prematurely finalized due to over-aggressive inlining. ([#409](https://github.com/reflex-frp/reflex/pull/409))
* Add instances of `PerformEvent` and `TriggerEvent` for `MaybeT`. ([#395](https://github.com/reflex-frp/reflex/pull/395))

## 0.6.4.1

* Fix a bug in the Reflex Profiled transformer where
  `Reflex.Class.mergeIncrementalG` and
  `Reflex.Class.mergeIncrementalWithMoveG` implementations referenced
  itself instead of the inner transformed timeline, causing an
  infinite loop.

## 0.6.4

* Support GHC 8.8

* Add `Reflex.Query.Base.mapQueryT`. See that module for documentation

* The `Reflex.Patch.*` modules were moved to the `patch` library.
  They are `Data.Patch.*` there, but reexported under their old names for backwards compatability here.

* Additional instances for `Query` classes for basic types.

* Add cabal flags `debug-propagation` and `debug-event-cycles` to build in debugging
  code for performance and for cyclic dependencies between events

## 0.6.3

* `Data.WeakBag.traverse` and `Data.FastWeakBag.traverse` have been deprecated.
  They are replaced with `Data.WeakBag.traverse_` and `Data.FastWeakBag.traverse_`, respectively.

* Fixes a bug in `Reflex.Patch.MapWithMove.patchThatSortsMapWith` that was producing invalid `PatchMapWithMove`.

* Add missing `NotReady` instances:
   - `instance NotReady (SpiderTimeline x) (SpiderHost x)`
   - `instance HasSpiderTimeline x => NotReady (SpiderTimeline x) (PerformEventT (SpiderTimeline x) (SpiderHost x))`

## 0.6.2.4

* Update to monoidal-containers 0.6

## 0.6.2.3

* Add an upper-bound to witherable

## 0.6.2.2

* Support these >= 1. Add `split-these` flag to control whether to use new these/semialign combination or not.

* Update version bounds to fix some CI failures

* Add travis CI configuration

## 0.6.2.1

* Generalize `fan` to `fanG` to take a `DMap` with non-`Identity`
  functor:
    * `fan` to `fanG`
    * `EventSelectorG` for `fanG` result selector.

* Reduce the amount of unsafeCoerce in coercing newtypes under Event/Dynamic/Behavior.
    * Add fused ReaderIO for the purpose of coercion (ReaderT's third argument has nominal role preventing automated coerce)
    * Add incrementalCoercion/coerceIncremental to go with dynamicCoercion/coerceDynamic

* Generalize merging functions:
  `merge` to `mergeG`,
  `mergeIncremental` to `mergeIncrementalG`,
  `mergeIncrementalWithMove` to `mergeIncrementalWithMoveG`.

* Generalize distribute function:
    `distributeDMapOverDynPure` to `distributeDMapOverDynPureG`,

## 0.6.2.0

* Fix `holdDyn` so that it is lazy in its event argument
  These produce `DMap`s  whose values needn't be `Identity`.

* Stop using the now-deprecated `*Tag` classes (e.g., `ShowTag`).

* Fix `holdDyn` so that it is lazy in its event argument.

## 0.6.1.0

* Re-export all of `Data.Map.Monoidal`

* Fix `QueryT` and `RequesterT` tests

## 0.6.0.0 -- 2019-03-20

* Deprecate `FunctorMaybe` in favor of `Data.Witherable.Filterable`. We still export `fmapMaybe`, `ffilter`, etc., but they all rely on `Filterable` now.

* Rename `MonadDynamicWriter` to `DynamicWriter` and add a deprecation for the old name.

* Remove many deprecated functions.

* Add a `Num` instance for `Dynamic`.

* Add `matchRequestsWithResponses` to make it easier to use `Requester` with protocols that don't do this matching for you.

* Add `withRequesterT` to map functions over the request and response of a `RequesterT`.

* Suppress nil patches in `QueryT` as an optimization. The `Query` type must now have an `Eq` instance.

* Add `throttleBatchWithLag` to `Reflex.Time`. See that module for details.
