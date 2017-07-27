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
  , traverseDMapWithKeyWithAdjustRequesterTWith
  , RequesterData
  , traverseRequesterData
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
import Data.Functor.Compose
import Data.Functor.Misc
import Data.Int
import Data.Map (Map)
import Data.Monoid
import Data.Unique.Tag
import Data.Some (Some)
import Data.Tuple
import Data.Type.Equality

import Unsafe.Coerce

newtype RequesterData f = RequesterData { unRequesterData :: DMap MyTag (Entry f) }

-- | Runs in reverse to accommodate for the fact that we accumulate it in reverse
traverseRequesterData :: forall m request response. Applicative m => (forall a. request a -> m (response a)) -> RequesterData request -> m (RequesterData response)
traverseRequesterData f (RequesterData m) = RequesterData <$> DMap.traverseWithKey go m --TODO: reverse
  where go :: forall x. MyTag x -> Entry request x -> m (Entry response x)
        go k (Entry request) = Entry <$> case myKeyType k of
          MyTagType_Single -> f request
          MyTagType_Multi -> traverseRequesterData f request
          MyTagType_Multi2 -> traverse (traverseRequesterData f) request

data MyTagType :: * -> * where
  MyTagType_Single :: MyTagType (Single a)
  MyTagType_Multi :: MyTagType Multi
  MyTagType_Multi2 :: MyTagType (Multi2 k)

myKeyType :: MyTag x -> MyTagType x
myKeyType (MyTag k) = case k .&. 0x3 of
  0x0 -> unsafeCoerce MyTagType_Single
  0x1 -> unsafeCoerce MyTagType_Multi
  0x2 -> unsafeCoerce MyTagType_Multi2
  t -> error $ "Reflex.Requester.Base.myKeyType: no such key type" <> show t

data Single a
data Multi
data Multi2 (k :: * -> *)

type family EntryContents request a where
  EntryContents request (Single a) = request a
  EntryContents request Multi = RequesterData request
  EntryContents request (Multi2 k) = Map (Some k) (RequesterData request)

newtype Entry request x = Entry { unEntry :: EntryContents request x }

{-# INLINE singleEntry #-}
singleEntry :: f a -> Entry f (Single a)
singleEntry = Entry

{-# INLINE multiEntry #-}
multiEntry :: RequesterData f -> Entry f Multi
multiEntry = Entry

-- | We use a hack here to pretend we have x ~ request a; we don't want to use a GADT, because GADTs (even with zero-size existential contexts) can't be newtypes
-- WARNING: This type should never be exposed.  In particular, this is extremely unsound if a MyTag from one run of runRequesterT is ever compared against a MyTag from another
newtype MyTag x = MyTag Int32 deriving (Show, Eq, Ord, Enum)

newtype MyTagWrap (f :: * -> *) x = MyTagWrap Int32 deriving (Show, Eq, Ord, Enum)

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

data RequesterState t request = RequesterState
  { _requesterState_nextMyTag :: {-# UNPACK #-} !Int32 -- Starts at -4 and goes down by 4 each time, to accommodate two 'type' bits at the bottom
  , _requesterState_requests :: ![DSum (MyTagWrap request) (Event t)]
  }

-- | A basic implementation of 'Requester'.
newtype RequesterT t request response m a = RequesterT { unRequesterT :: StateT (RequesterState t request) (ReaderT (EventSelector t (MyTagWrap response)) m) a }
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

{-# INLINE wrapMyTags #-}
wrapMyTags :: DMap MyTag (Entry f) -> DMap (MyTagWrap f) Identity
wrapMyTags = unsafeCoerce

{-# INLINE unwrapMyTags #-}
unwrapMyTags :: DMap (MyTagWrap f) Identity -> DMap MyTag (Entry f)
unwrapMyTags = unsafeCoerce

-- | Run a 'RequesterT' action.  The resulting 'Event' will fire whenever
-- requests are made, and responses should be provided in the input 'Event'.
-- The 'Tag' keys will be used to return the responses to the same place the
-- requests were issued.

runRequesterT :: (Reflex t, Monad m)
              => RequesterT t request response m a
              -> Event t (RequesterData response) --TODO: This DMap will be in reverse order, so we need to make sure the caller traverses it in reverse
              -> m (a, Event t (RequesterData request)) --TODO: we need to hide these 'MyTag's here, because they're unsafe to mix in the wild
runRequesterT (RequesterT a) responses = do
  (result, s) <- runReaderT (runStateT a $ RequesterState (-4) []) $ fan $
    fmapCheap (wrapMyTags . unRequesterData) responses
  return (result, fmapCheap (RequesterData . unwrapMyTags) $ merge $ DMap.fromDistinctAscList $ _requesterState_requests s)

instance (Reflex t, Monad m) => Requester t (RequesterT t request response m) where
  type Request (RequesterT t request response m) = request
  type Response (RequesterT t request response m) = response
  requesting = fmap coerceEvent . responseFromTag . castMyTagWrap <=< tagRequest . (coerceEvent :: Event t (request a) -> Event t (Entry request (Single a)))
  requesting_ = void . tagRequest . fmapCheap singleEntry

{-# INLINE tagRequest #-}
tagRequest :: Monad m => Event t (Entry request x) -> RequesterT t request response m (MyTagWrap request (Entry request x))
tagRequest req = do
  old <- RequesterT get
  let n = _requesterState_nextMyTag old
      t = MyTagWrap n
  RequesterT $ put $ RequesterState
    { _requesterState_nextMyTag = n - 0x4
    , _requesterState_requests = (t :=> req) : _requesterState_requests old
    }
  return t

{-# INLINE responseFromTag #-}
responseFromTag :: (Monad m, Reflex t) => MyTagWrap response (Entry response x) -> RequesterT t request response m (Event t (Entry response x))
responseFromTag t = do
  responses :: EventSelector t (MyTagWrap response) <- RequesterT ask
  return $ select responses t

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
  traverseDMapWithKeyWithAdjust = traverseDMapWithKeyWithAdjustRequesterTWith (\f dm0 dm' -> lift $ traverseDMapWithKeyWithAdjust f dm0 dm') mapPatchDMap weakenPatchDMapWith patchMapNewElementsMap mergeMapIncremental
  traverseDMapWithKeyWithAdjustWithMove = traverseDMapWithKeyWithAdjustRequesterTWith (\f dm0 dm' -> lift $ traverseDMapWithKeyWithAdjustWithMove f dm0 dm') mapPatchDMapWithMove weakenPatchDMapWithMoveWith patchMapWithMoveNewElementsMap mergeMapIncrementalWithMove

requesting' :: (Monad m, Reflex t) => Event t (Entry request x) -> RequesterT t request response m (Event t (Entry response x))
requesting' = responseFromTag . castMyTagWrap <=< tagRequest

runWithReplaceRequesterTWith :: forall m t request response a b. (Reflex t, MonadHold t m
                                                                 , MonadFix m
                                                                 )
                             => (forall a' b'. m a' -> Event t (m b') -> RequesterT t request response m (a', Event t b'))
                             -> RequesterT t request response m a
                             -> Event t (RequesterT t request response m b)
                             -> RequesterT t request response m (a, Event t b)
runWithReplaceRequesterTWith f a0 a' = do
  rec response <- fmap (fmapCheap unEntry) $ requesting' $ fmapCheap multiEntry $ switchPromptlyDyn requests --TODO: Investigate whether we can really get rid of the prompt stuff here
      ((result0, requests0), v') <- f (runRequesterT a0 response) $ fmapCheap (`runRequesterT` response) a'
      requests <- holdDyn requests0 $ fmapCheap snd v'
  return (result0, fmapCheap fst v')

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
