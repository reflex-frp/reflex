# Revision history for reflex

## Unreleased

* Support GHC 8.8

* Add `Reflex.Query.Base.mapQueryT`. See that module for documentation

* The `Reflex.Patch.*` modules were moved to the `patch` library.
  They are `Data.Patch.*` there, but reexported under their old names for backwards compatability here.

* Additional instances for `Query` classes for basic types.

* Refactor of `Reflex.Requester`:
    * Updated:
        * `RequesterData` to `RequestData`, `RequesterEnvelope`, and `ResponseData`
        * `singletonRequesterData` to `singletonRequestData` and `singletonResponseData`
        * `requesterDataToList` to `requestEnvelopesToDSums`
    * Removed:
        * `RequesterDataKey`
        * `multiEntry`
        * `unMultiEntry`
        * `withRequesterT`
        * `runWithReplaceRequesterTWith`
        * `requesting'`
        * `traverseIntMapWithKeyWithAdjustRequesterTWith`
        * `traverseDMapWithKeyWithAdjustRequesterTWith`

* Added `Data.List.Deferred` and `Data.List.NonEmpty.Deferred` for optimizing `<>` operations.

* Added `Data.TagMap`, `Reflex.FanTag`, and `Data.Unique.Tag.Local` to improve request and response tagging.

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
