-- | This module provides 'RequesterT', the standard implementation of
-- 'Requester'.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
module Reflex.Requester.Base
  ( RequesterT (..)
  , runRequesterT
  , runWithReplaceRequesterTWith
  , traverseIntMapWithKeyWithAdjustRequesterTWith
  , traverseDMapWithKeyWithAdjustRequesterTWith
  , RequesterData
  , RequesterDataKey
  , traverseRequesterData
  , requesterDataToList
  , singletonRequesterData
  ) where

import Reflex.Class
import Reflex.Dynamic
import Reflex.Host.Class
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class
import Reflex.Requester.Class
import Reflex.TriggerEvent.Class

import Control.Monad.Exception
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.State.Strict
import Data.Bits
import Data.Coerce
import Data.Dependent.Map (DMap, DSum (..))
import qualified Data.Dependent.Map as DMap
import qualified Data.Some as Some
import Data.FastMutableIntMap
import Data.Functor.Compose
import Data.Functor.Misc
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Unique.Tag
import Data.Some (Some)
import Data.Tuple
import Data.Type.Equality
import Data.Proxy

import GHC.Exts (Any)
import Unsafe.Coerce

newtype TagMap (f :: * -> *) = TagMap (IntMap Any)

newtype RequesterData f = RequesterData (TagMap (Entry f))

data RequesterDataKey a where
  RequesterDataKey_Single :: {-# UNPACK #-} !(MyTag (Single a)) -> RequesterDataKey a
  RequesterDataKey_Multi :: {-# UNPACK #-} !(MyTag Multi) -> !(RequesterDataKey a) -> RequesterDataKey a
  RequesterDataKey_Multi2 :: {-# UNPACK #-} !(MyTag (Multi2 k)) -> {-# UNPACK #-} !(Some k) -> !(RequesterDataKey a) -> RequesterDataKey a
  RequesterDataKey_Multi3 :: {-# UNPACK #-} !(MyTag Multi3) -> Int -> !(RequesterDataKey a) -> RequesterDataKey a

singletonRequesterData :: RequesterDataKey a -> f a -> RequesterData f
singletonRequesterData rdk v = case rdk of
  RequesterDataKey_Single k -> RequesterData $ singletonTagMap k $ Entry v
  RequesterDataKey_Multi k k' -> RequesterData $ singletonTagMap k $ Entry $ singletonRequesterData k' v
  RequesterDataKey_Multi2 k k' k'' -> RequesterData $ singletonTagMap k $ Entry $ Map.singleton k' $ singletonRequesterData k'' v
  RequesterDataKey_Multi3 k k' k'' -> RequesterData $ singletonTagMap k $ Entry $ IntMap.singleton k' $ singletonRequesterData k'' v

requesterDataToList :: RequesterData f -> [DSum RequesterDataKey f]
requesterDataToList (RequesterData m) = do
  k :=> Entry e <- tagMapToList m
  case myKeyType k of
    MyTagType_Single -> return $ RequesterDataKey_Single k :=> e
    MyTagType_Multi -> do
      k' :=> e' <- requesterDataToList e
      return $ RequesterDataKey_Multi k k' :=> e'
    MyTagType_Multi2 -> do
      (k', e') <- Map.toList e
      k'' :=> e'' <- requesterDataToList e'
      return $ RequesterDataKey_Multi2 k k' k'' :=> e''
    MyTagType_Multi3 -> do
      (k', e') <- IntMap.toList e
      k'' :=> e'' <- requesterDataToList e'
      return $ RequesterDataKey_Multi3 k k' k'' :=> e''

singletonTagMap :: forall f a. MyTag a -> f a -> TagMap f
singletonTagMap (MyTag k) v = TagMap $ IntMap.singleton k $ (unsafeCoerce :: f a -> Any) v

tagMapToList :: forall f. TagMap f -> [DSum MyTag f]
tagMapToList (TagMap m) = fmap f $ IntMap.toList m
  where f :: (Int, Any) -> DSum MyTag f
        f (k, v) = MyTag k :=> ((unsafeCoerce :: Any -> f a) v)

traverseTagMapWithKey :: forall t f g. Applicative t => (forall a. MyTag a -> f a -> t (g a)) -> TagMap f -> t (TagMap g)
traverseTagMapWithKey f (TagMap m) = TagMap <$> IntMap.traverseWithKey g m
  where
    g :: Int -> Any -> t Any
    g k v = (unsafeCoerce :: g a -> Any) <$> f (MyTag k) ((unsafeCoerce :: Any -> f a) v)

-- | Runs in reverse to accommodate for the fact that we accumulate it in reverse
traverseRequesterData :: forall m request response. Applicative m => (forall a. request a -> m (response a)) -> RequesterData request -> m (RequesterData response)
traverseRequesterData f (RequesterData m) = RequesterData <$> traverseTagMapWithKey go m --TODO: reverse this, since our tags are in reverse order
  where go :: forall x. MyTag x -> Entry request x -> m (Entry response x)
        go k (Entry request) = Entry <$> case myKeyType k of
          MyTagType_Single -> f request
          MyTagType_Multi -> traverseRequesterData f request
          MyTagType_Multi2 -> traverse (traverseRequesterData f) request
          MyTagType_Multi3 -> traverse (traverseRequesterData f) request

data MyTagType :: * -> * where
  MyTagType_Single :: MyTagType (Single a)
  MyTagType_Multi :: MyTagType Multi
  MyTagType_Multi2 :: MyTagType (Multi2 k)
  MyTagType_Multi3 :: MyTagType Multi3

myKeyType :: MyTag x -> MyTagType x
myKeyType (MyTag k) = case k .&. 0x3 of
  0x0 -> unsafeCoerce MyTagType_Single
  0x1 -> unsafeCoerce MyTagType_Multi
  0x2 -> unsafeCoerce MyTagType_Multi2
  0x3 -> unsafeCoerce MyTagType_Multi3
  t -> error $ "Reflex.Requester.Base.myKeyType: no such key type" <> show t

data Single a
data Multi
data Multi2 (k :: * -> *)
data Multi3

class MyTagTypeOffset x where
  myTagTypeOffset :: proxy x -> Int

instance MyTagTypeOffset (Single a) where
  myTagTypeOffset _ = 0x0

instance MyTagTypeOffset Multi where
  myTagTypeOffset _ = 0x1

instance MyTagTypeOffset (Multi2 k) where
  myTagTypeOffset _ = 0x2

instance MyTagTypeOffset Multi3 where
  myTagTypeOffset _ = 0x3

type family EntryContents request a where
  EntryContents request (Single a) = request a
  EntryContents request Multi = RequesterData request
  EntryContents request (Multi2 k) = Map (Some k) (RequesterData request)
  EntryContents request Multi3 = IntMap (RequesterData request)

newtype Entry request x = Entry { unEntry :: EntryContents request x }

{-# INLINE singleEntry #-}
singleEntry :: f a -> Entry f (Single a)
singleEntry = Entry

{-# INLINE multiEntry #-}
multiEntry :: RequesterData f -> Entry f Multi
multiEntry = Entry

{-# INLINE unMultiEntry #-}
unMultiEntry :: Entry f Multi -> RequesterData f
unMultiEntry = unEntry

-- | We use a hack here to pretend we have x ~ request a; we don't want to use a GADT, because GADTs (even with zero-size existential contexts) can't be newtypes
-- WARNING: This type should never be exposed.  In particular, this is extremely unsound if a MyTag from one run of runRequesterT is ever compared against a MyTag from another
newtype MyTag x = MyTag Int deriving (Show, Eq, Ord, Enum)

newtype MyTagWrap (f :: * -> *) x = MyTagWrap Int deriving (Show, Eq, Ord, Enum)

{-# INLINE castMyTagWrap #-}
castMyTagWrap :: MyTagWrap f (Entry f x) -> MyTagWrap g (Entry g x)
castMyTagWrap = coerce

instance GEq MyTag where
  (MyTag a) `geq` (MyTag b) =
    if a == b
    then Just $ unsafeCoerce Refl
    else Nothing

instance GCompare MyTag where
  (MyTag a) `gcompare` (MyTag b) =
    case a `compare` b of
      LT -> GLT
      EQ -> unsafeCoerce GEQ
      GT -> GGT

instance GEq (MyTagWrap f) where
  (MyTagWrap a) `geq` (MyTagWrap b) =
    if a == b
    then Just $ unsafeCoerce Refl
    else Nothing

instance GCompare (MyTagWrap f) where
  (MyTagWrap a) `gcompare` (MyTagWrap b) =
    case a `compare` b of
      LT -> GLT
      EQ -> unsafeCoerce GEQ
      GT -> GGT

data RequesterState t (request :: * -> *) = RequesterState
  { _requesterState_nextMyTag :: {-# UNPACK #-} !Int -- Starts at -4 and goes down by 4 each time, to accommodate two 'type' bits at the bottom
  , _requesterState_requests :: ![(Int, Event t Any)]
  }

-- | A basic implementation of 'Requester'.
newtype RequesterT t request (response :: * -> *) m a = RequesterT { unRequesterT :: StateT (RequesterState t request) (ReaderT (EventSelectorInt t Any) m) a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadException
-- MonadAsyncException can't be derived on ghc-8.0.1; we use base-4.9.1 as a proxy for ghc-8.0.2
#if MIN_VERSION_base(4,9,1)
           , MonadAsyncException
#endif
           )

deriving instance MonadSample t m => MonadSample t (RequesterT t request response m)
deriving instance MonadHold t m => MonadHold t (RequesterT t request response m)
deriving instance PostBuild t m => PostBuild t (RequesterT t request response m)
deriving instance TriggerEvent t m => TriggerEvent t (RequesterT t request response m)

-- | Run a 'RequesterT' action.  The resulting 'Event' will fire whenever
-- requests are made, and responses should be provided in the input 'Event'.
-- The 'Tag' keys will be used to return the responses to the same place the
-- requests were issued.

runRequesterT :: (Reflex t, Monad m)
              => RequesterT t request response m a
              -> Event t (RequesterData response) --TODO: This DMap will be in reverse order, so we need to make sure the caller traverses it in reverse
              -> m (a, Event t (RequesterData request)) --TODO: we need to hide these 'MyTag's here, because they're unsafe to mix in the wild
runRequesterT (RequesterT a) responses = do
  (result, s) <- runReaderT (runStateT a $ RequesterState (-4) []) $ fanInt $
    coerceEvent responses
  return (result, fmapCheap (RequesterData . TagMap) $ mergeInt $ IntMap.fromDistinctAscList $ _requesterState_requests s)

instance (Reflex t, Monad m) => Requester t (RequesterT t request response m) where
  type Request (RequesterT t request response m) = request
  type Response (RequesterT t request response m) = response
  requesting = fmap coerceEvent . responseFromTag . castMyTagWrap <=< tagRequest . (coerceEvent :: Event t (request a) -> Event t (Entry request (Single a)))
  requesting_ = void . tagRequest . fmapCheap singleEntry

{-# INLINE tagRequest #-}
tagRequest :: forall m x t request response. (Monad m, MyTagTypeOffset x) => Event t (Entry request x) -> RequesterT t request response m (MyTagWrap request (Entry request x))
tagRequest req = do
  old <- RequesterT get
  let n = _requesterState_nextMyTag old .|. myTagTypeOffset (Proxy :: Proxy x)
      t = MyTagWrap n
  RequesterT $ put $ RequesterState
    { _requesterState_nextMyTag = _requesterState_nextMyTag old - 0x4
    , _requesterState_requests = (n, (unsafeCoerce :: Event t (Entry request x) -> Event t Any) req) : _requesterState_requests old
    }
  return t

{-# INLINE responseFromTag #-}
responseFromTag :: (Monad m, Reflex t) => MyTagWrap response (Entry response x) -> RequesterT t request response m (Event t (Entry response x))
responseFromTag (MyTagWrap t) = do
  responses :: EventSelectorInt t Any <- RequesterT ask
  return $ (unsafeCoerce :: Event t Any -> Event t (Entry response x)) $ selectInt responses t

instance MonadTrans (RequesterT t request response) where
  lift = RequesterT . lift . lift

instance PerformEvent t m => PerformEvent t (RequesterT t request response m) where
  type Performable (RequesterT t request response m) = Performable m
  performEvent_ = lift . performEvent_
  performEvent = lift . performEvent

instance MonadRef m => MonadRef (RequesterT t request response m) where
  type Ref (RequesterT t request response m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (RequesterT t request response m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance MonadReader r m => MonadReader r (RequesterT t request response m) where
  ask = lift ask
  local f (RequesterT a) = RequesterT $ mapStateT (mapReaderT $ local f) a
  reader = lift . reader

instance (Reflex t, MonadAdjust t m, MonadHold t m, MonadFix m) => MonadAdjust t (RequesterT t request response m) where
  runWithReplace = runWithReplaceRequesterTWith $ \dm0 dm' -> lift $ runWithReplace dm0 dm'
  traverseIntMapWithKeyWithAdjust = traverseIntMapWithKeyWithAdjustRequesterTWith (\f dm0 dm' -> lift $ traverseIntMapWithKeyWithAdjust f dm0 dm') patchIntMapNewElementsMap mergeIntIncremental
  {-# INLINABLE traverseDMapWithKeyWithAdjust #-}
  traverseDMapWithKeyWithAdjust = traverseDMapWithKeyWithAdjustRequesterTWith (\f dm0 dm' -> lift $ traverseDMapWithKeyWithAdjust f dm0 dm') mapPatchDMap weakenPatchDMapWith patchMapNewElementsMap mergeMapIncremental
  traverseDMapWithKeyWithAdjustWithMove = traverseDMapWithKeyWithAdjustRequesterTWith (\f dm0 dm' -> lift $ traverseDMapWithKeyWithAdjustWithMove f dm0 dm') mapPatchDMapWithMove weakenPatchDMapWithMoveWith patchMapWithMoveNewElementsMap mergeMapIncrementalWithMove

requesting' :: (MyTagTypeOffset x, Monad m, Reflex t) => Event t (Entry request x) -> RequesterT t request response m (Event t (Entry response x))
requesting' = responseFromTag . castMyTagWrap <=< tagRequest

{-# INLINABLE runWithReplaceRequesterTWith #-}
runWithReplaceRequesterTWith :: forall m t request response a b. (Reflex t, MonadHold t m
                                                                 , MonadFix m
                                                                 )
                             => (forall a' b'. m a' -> Event t (m b') -> RequesterT t request response m (a', Event t b'))
                             -> RequesterT t request response m a
                             -> Event t (RequesterT t request response m b)
                             -> RequesterT t request response m (a, Event t b)
runWithReplaceRequesterTWith f a0 a' = do
  rec response <- fmap (fmapCheap unMultiEntry) $ requesting' $ fmapCheap multiEntry $ switchPromptlyDyn requests --TODO: Investigate whether we can really get rid of the prompt stuff here
      ((result0, requests0), v') <- f (runRequesterT a0 response) $ fmapCheap (`runRequesterT` response) a'
      requests <- holdDyn requests0 $ fmapCheap snd v'
  return (result0, fmapCheap fst v')

{-# INLINE traverseIntMapWithKeyWithAdjustRequesterTWith #-}
traverseIntMapWithKeyWithAdjustRequesterTWith :: forall t request response m v v' p.
                                        ( Reflex t
                                        , MonadHold t m
                                        , PatchTarget (p (Event t (RequesterData request))) ~ IntMap (Event t (RequesterData request))
                                        , Patch (p (Event t (RequesterData request)))
                                        , Functor p
                                        , MonadFix m
                                        )
                                     => (   (IntMap.Key -> v -> m (Event t (RequesterData request), v'))
                                         -> IntMap v
                                         -> Event t (p v)
                                         -> RequesterT t request response m (IntMap (Event t (RequesterData request), v'), Event t (p (Event t (RequesterData request), v')))
                                        )
                                     -> (p (Event t (RequesterData request)) -> IntMap (Event t (RequesterData request)))
                                     -> (Incremental t (p (Event t (RequesterData request))) -> Event t (IntMap (RequesterData request)))
                                     -> (IntMap.Key -> v -> RequesterT t request response m v')
                                     -> IntMap v
                                     -> Event t (p v)
                                     -> RequesterT t request response m (IntMap v', Event t (p v'))
traverseIntMapWithKeyWithAdjustRequesterTWith base patchNewElements mergePatchIncremental f dm0 dm' = do
  rec response <- requesting' $ fmapCheap pack $ promptRequests `mappend` mergePatchIncremental requests --TODO: Investigate whether we can really get rid of the prompt stuff here
      let responses :: EventSelectorInt t (RequesterData response)
          responses = fanInt $ fmapCheap unpack response
          unpack :: Entry response Multi3 -> IntMap (RequesterData response)
          unpack = unEntry
          pack :: IntMap (RequesterData request) -> Entry request Multi3
          pack = Entry
          f' :: IntMap.Key -> v -> m (Event t (RequesterData request), v')
          f' k v = fmap swap $ runRequesterT (f k v) $ selectInt responses k
      (children0, children') <- base f' dm0 dm'
      let result0 = fmap snd children0
          result' = fforCheap children' $ fmap snd
          requests0 :: IntMap (Event t (RequesterData request))
          requests0 = fmap fst children0
          requests' :: Event t (p (Event t (RequesterData request)))
          requests' = fforCheap children' $ fmap fst
          promptRequests :: Event t (IntMap (RequesterData request))
          promptRequests = coincidence $ fmapCheap (mergeInt . patchNewElements) requests' --TODO: Create a mergeIncrementalPromptly, and use that to eliminate this 'coincidence'
      requests <- holdIncremental requests0 requests'
  return (result0, result')

{-# INLINE traverseDMapWithKeyWithAdjustRequesterTWith #-}
traverseDMapWithKeyWithAdjustRequesterTWith :: forall k t request response m v v' p p'.
                                        ( GCompare k
                                        , Reflex t
                                        , MonadHold t m
                                        , PatchTarget (p' (Some k) (Event t (RequesterData request))) ~ Map (Some k) (Event t (RequesterData request))
                                        , Patch (p' (Some k) (Event t (RequesterData request)))
                                        , MonadFix m
                                        )
                                     => (forall k' v1 v2. GCompare k'
                                         => (forall a. k' a -> v1 a -> m (v2 a))
                                         -> DMap k' v1
                                         -> Event t (p k' v1)
                                         -> RequesterT t request response m (DMap k' v2, Event t (p k' v2))
                                        )
                                     -> (forall v1. (forall a. v1 a -> v' a) -> p k v1 -> p k v')
                                     -> (forall v1 v2. (forall a. v1 a -> v2) -> p k v1 -> p' (Some k) v2)
                                     -> (forall v2. p' (Some k) v2 -> Map (Some k) v2)
                                     -> (forall a. Incremental t (p' (Some k) (Event t a)) -> Event t (Map (Some k) a))
                                     -> (forall a. k a -> v a -> RequesterT t request response m (v' a))
                                     -> DMap k v
                                     -> Event t (p k v)
                                     -> RequesterT t request response m (DMap k v', Event t (p k v'))
traverseDMapWithKeyWithAdjustRequesterTWith base mapPatch weakenPatchWith patchNewElements mergePatchIncremental f dm0 dm' = do
  rec response <- requesting' $ fmapCheap pack $ promptRequests `mappend` mergePatchIncremental requests --TODO: Investigate whether we can really get rid of the prompt stuff here
      let responses :: EventSelector t (Const2 (Some k) (RequesterData response))
          responses = fanMap $ fmapCheap unpack response
          unpack :: Entry response (Multi2 k) -> Map (Some k) (RequesterData response)
          unpack = unEntry
          pack :: Map (Some k) (RequesterData request) -> Entry request (Multi2 k)
          pack = Entry
          f' :: forall a. k a -> v a -> m (Compose ((,) (Event t (RequesterData request))) v' a)
          f' k v = fmap (Compose . swap) $ runRequesterT (f k v) $ select responses (Const2 (Some.This k))
      (children0, children') <- base f' dm0 dm'
      let result0 = DMap.map (snd . getCompose) children0
          result' = fforCheap children' $ mapPatch $ snd . getCompose
          requests0 :: Map (Some k) (Event t (RequesterData request))
          requests0 = weakenDMapWith (fst . getCompose) children0
          requests' :: Event t (p' (Some k) (Event t (RequesterData request)))
          requests' = fforCheap children' $ weakenPatchWith $ fst . getCompose
          promptRequests :: Event t (Map (Some k) (RequesterData request))
          promptRequests = coincidence $ fmapCheap (mergeMap . patchNewElements) requests' --TODO: Create a mergeIncrementalPromptly, and use that to eliminate this 'coincidence'
      requests <- holdIncremental requests0 requests'
  return (result0, result')
