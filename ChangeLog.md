# Revision history for reflex

## 0.6.0.0 -- 2019-03-20

* Deprecate FunctorMaybe in favor of Data.Witherable.Filterable. We still export fmapMaybe, ffilter, etc., but they all rely on Filterable now.
* Rename MonadDynamicWriter to DynamicWriter and add a deprecation for the old name.
* Remove many deprecated functions
* Add a Num instance for Dynamic
* Add matchRequestsWithResponses to make it easier to use Requester with protocols that don't do this matching for you
* Add withRequesterT to map functions over the request and response of a RequesterT
