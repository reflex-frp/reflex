-- | This module provides 'RequesterT', the standard implementation of
-- 'Requester'.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif
module Reflex.Requester.Base
  ( RequesterT (..)
  , RequestItem (..)
  , ResponseItem (..)
  , runRequesterT'
  , runWithReplaceRequesterTWith
  , traverseIntMapWithKeyWithAdjustRequesterTWith
  , traverseDMapWithKeyWithAdjustRequesterTWith
  ) where

import Reflex.Adjustable.Class
import Reflex.Class
import Reflex.EventWriter.Base
import Reflex.EventWriter.Class
import Reflex.Host.Class
import Reflex.PerformEvent.Class
import Reflex.PostBuild.Class
import Reflex.Requester.Class
import Reflex.TriggerEvent.Class

import Control.Applicative (liftA2)
import Control.Monad.Exception
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Ref
import Data.Dependent.Map (DMap)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map (Map)
import qualified Data.Semigroup as S
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Some (Some)
import Data.Unique.Tag

import GHC.TypeLits

-- | A basic implementation of 'Requester'.
newtype RequesterT t x (request :: * -> *) (response :: * -> *) m a = RequesterT { unRequesterT :: ReaderT (FanCell t x) (EventWriterT t (Seq (RequestItem t x request response)) m) a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadException
-- MonadAsyncException can't be derived on ghc-8.0.1; we use base-4.9.1 as a proxy for ghc-8.0.2
#if MIN_VERSION_base(4,9,1)
           , MonadAsyncException
#endif
           )

deriving instance MonadSample t m => MonadSample t (RequesterT t x request response m)
instance MonadHold t m => MonadHold t (RequesterT t x request response m)
deriving instance PostBuild t m => PostBuild t (RequesterT t x request response m)
deriving instance TriggerEvent t m => TriggerEvent t (RequesterT t x request response m)

-- TODO: Monoid and Semigroup can likely be derived once StateT has them.
instance (Monoid a, Monad m) => Monoid (RequesterT t x request response m a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance (S.Semigroup a, Monad m) => S.Semigroup (RequesterT t x request response m a) where
  (<>) = liftA2 (S.<>)


data RequestItem t x request response = forall a. RequestItem
  { _requestItem_trigger :: !(Maybe (CellTrigger t (response a) x))
  , _requestItem_value :: request a
  }

data ResponseItem t x response = forall a. ResponseItem
  { _responseItem_trigger :: !(CellTrigger t (response a) x)
  , _responseItem_value :: response a
  }

runRequesterT'
  :: forall t m request response a
  .  ( Reflex t
     , Monad m
     , MonadFix m
     , MonadHold t m
     )
  => (forall x. Event t (Seq (RequestItem t x request response)) -> RequesterT t x request response m (Event t (Seq (ResponseItem t x response)), a))
  -> m a
runRequesterT' a = withHoldFanCell $ \(c :: FanCell t x) -> do
  let update :: Seq (ResponseItem t x response) -> CellM t x ()
      update responses = withMonadCellM @t @x $
        forM_ responses $ \(ResponseItem t v) ->
        fireCellEvent t v
  rec ((responses, result :: a), requests) <- runEventWriterT $ runReaderT (unRequesterT (a requests)) c
  pure (fmapCheap update responses, result)

instance (Reflex t, Monad m, MonadMutate t m) => Requester t (RequesterT t x request response m) where
  type Request (RequesterT t x request response m) = request
  type Response (RequesterT t x request response m) = response
  requesting request = RequesterT $ do
    c <- ask
    (t, response) <- lift $ lift $ mutateFanCell c newCellEvent
    tellEvent $ fforCheap request $ \req -> Seq.singleton $ RequestItem
      { _requestItem_trigger = Just t
      , _requestItem_value = req
      }
    pure response
  requesting_ request = RequesterT $ do
    tellEvent $ fforCheap request $ \req -> Seq.singleton $ RequestItem
      { _requestItem_trigger = Nothing
      , _requestItem_value = req
      }

instance MonadTrans (RequesterT t x request response) where
  lift = RequesterT . lift . lift

instance PerformEvent t m => PerformEvent t (RequesterT t x request response m) where
  type Performable (RequesterT t x request response m) = Performable m
  performEvent_ = lift . performEvent_
  performEvent = lift . performEvent

instance MonadRef m => MonadRef (RequesterT t x request response m) where
  type Ref (RequesterT t x request response m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (RequesterT t x request response m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance MonadReader r m => MonadReader r (RequesterT t x request response m) where
  ask = lift ask
  local f (RequesterT a) = RequesterT $ mapReaderT (local f) a
  reader = lift . reader

instance (Reflex t, Adjustable t m, MonadHold t m, MonadFix m) => Adjustable t (RequesterT t x request response m) where
  runWithReplace = runWithReplaceRequesterTWith $ \dm0 dm' -> lift $ runWithReplace dm0 dm'
  traverseIntMapWithKeyWithAdjust = traverseIntMapWithKeyWithAdjustRequesterTWith (\f dm0 dm' -> lift $ traverseIntMapWithKeyWithAdjust f dm0 dm') patchIntMapNewElementsMap mergeIntIncremental
  {-# INLINABLE traverseDMapWithKeyWithAdjust #-}
  traverseDMapWithKeyWithAdjust = traverseDMapWithKeyWithAdjustRequesterTWith (\f dm0 dm' -> lift $ traverseDMapWithKeyWithAdjust f dm0 dm') mapPatchDMap weakenPatchDMapWith patchMapNewElementsMap mergeMapIncremental
  traverseDMapWithKeyWithAdjustWithMove = traverseDMapWithKeyWithAdjustRequesterTWith (\f dm0 dm' -> lift $ traverseDMapWithKeyWithAdjustWithMove f dm0 dm') mapPatchDMapWithMove weakenPatchDMapWithMoveWith patchMapWithMoveNewElementsMap mergeMapIncrementalWithMove

{-# INLINABLE runWithReplaceRequesterTWith #-}
runWithReplaceRequesterTWith
  :: forall m t x request response a b
  .  ( Reflex t
     , MonadHold t m
     )
  => (forall a' b'. m a' -> Event t (m b') -> RequesterT t x request response m (a', Event t b'))
  -> RequesterT t x request response m a
  -> Event t (RequesterT t x request response m b)
  -> RequesterT t x request response m (a, Event t b)
runWithReplaceRequesterTWith f a0 a' = do
  env <- RequesterT ask
  let r :: forall a'. RequesterT t x request response m a' -> EventWriterT t (Seq (RequestItem t x request response)) m a'
      r (RequesterT a) = runReaderT a env
  RequesterT $ lift $ runWithReplaceEventWriterTWith (\x0 x' -> r $ f x0 x') (r a0) $ fmapCheap r a'

{-
data Vec (n :: Nat) a where
  Vec_Cons :: a -> Vec n a -> Vec (n + 1) a
  Vec_Nil :: Vec 0 a

newtype UpTo (n :: Nat) = UpTo Int

data Perm (n :: Nat) where
  Perm_Cons :: UpTo n -> Perm n -> Perm (n + 1)
  Perm_Nil :: Perm 0

data PatchList a (i :: Nat) (d :: Nat) (k :: Nat) where
  PatchList_Insert :: a -> PatchList a i d k -> PatchList a (i + 1) d k
  PatchList_Delete :: PatchList a i d k -> PatchList a i (d + 1) k
  PatchList_Keep :: PatchList a i d k -> PatchList a i d (k + 1)
  PatchList_Nil :: PatchList a 0 0 0

applyPatchList :: PatchList a i d k -> Vec (k + d) a -> Vec (k + i) a
applyPatchList = \case
--  PatchList_Insert a p -> \l -> a `Vec_Cons` applyPatchList p l
--  PatchList_Delete p -> \(_ : t) -> applyPatchList p t
  PatchList_Keep p -> \(Vec_Cons h t) -> h `Vec_Cons` applyPatchList p t

data PatchMapWithMove a i d k = PatchMapWithMove (Perm k) (PatchList a i d k)
-}

class X m where
  data ChildData m :: * -> *
  runChild :: m a -> m (ChildData m a) --TODO: Linear
  depositChildren :: Traversable t => t (ChildData m a) -> m (t a)

{-
data PatchList n m a where
  PatchList :: n + i - d ~ m => Perm n -> 
-}

{-# INLINE traverseIntMapWithKeyWithAdjustRequesterTWith #-}
traverseIntMapWithKeyWithAdjustRequesterTWith
  :: forall t x request response m v v' p rd
  .  ( Reflex t
     , MonadHold t m
--     , PatchTarget (p (Event t (IntMap (rd request)))) ~ IntMap (Event t (IntMap (rd request)))
--     , Patch (p (Event t (IntMap (rd request))))
     , Functor p
     , MonadFix m
     )
  => (forall v1 v2.
         (IntMap.Key -> v1 -> m v2)
      -> IntMap v1
      -> Event t (PatchIntMap v1)
      -> RequesterT t x request response m (IntMap v2, Event t (PatchIntMap v2))
     )
  -> (p (Event t (Seq (RequestItem t x request response))) -> IntMap (Event t (Seq (RequestItem t x request response))))
  -> (Incremental t (p (Event t (Seq (RequestItem t x request response)))) -> Event t (IntMap (Seq (RequestItem t x request response))))
  -> (IntMap.Key -> v -> RequesterT t x request response m v')
  -> IntMap v
  -> Event t (p v)
  -> RequesterT t x request response m (IntMap v', Event t (p v'))
traverseIntMapWithKeyWithAdjustRequesterTWith base patchNewElements mergePatchIncremental f dm0 dm' = do
  env <- RequesterT ask
  let r :: forall a'. RequesterT t x request response m a' -> EventWriterT t (Seq (RequestItem t x request response)) m a'
      r (RequesterT a) = runReaderT a env
  RequesterT $ lift $ sequenceIntMapWithAdjustEventWriterTWith (\f' x0 x' -> r $ base f' x0 x') patchNewElements mergePatchIncremental undefined (r a0) $ fmapCheap r a'

{-# INLINE traverseDMapWithKeyWithAdjustRequesterTWith #-}
traverseDMapWithKeyWithAdjustRequesterTWith
  :: forall k t x request response m v v' p p' rd
  .  ( GCompare k
     , Reflex t
     , MonadHold t m
--     , PatchTarget (p' (Some k) (Event t (Seq (RequestItem t x request response)))) ~ Map (Some k) (Event t (Seq (RequestItem t x request response)))
--     , Patch (p' (Some k) (Event t (Seq (RequestItem t x request response))))
     , MonadFix m
     )
  => (forall k' v1 v2. GCompare k'
      => (forall a. k' a -> v1 a -> m (v2 a))
      -> DMap k' v1
      -> Event t (p k' v1)
      -> RequesterT t x request response m (DMap k' v2, Event t (p k' v2))
     )
  -> (forall v1 v2. (forall a. v1 a -> v2 a) -> p k v1 -> p k v2)
  -> (forall v1 v2. (forall a. v1 a -> v2) -> p k v1 -> p' (Some k) v2)
  -> (forall v2. p' (Some k) v2 -> Map (Some k) v2)
  -> (forall a. Incremental t (p' (Some k) (Event t a)) -> Event t (Map (Some k) a))
  -> (forall a. k a -> v a -> RequesterT t x request response m (v' a))
  -> DMap k v
  -> Event t (p k v)
  -> RequesterT t x request response m (DMap k v', Event t (p k v'))
traverseDMapWithKeyWithAdjustRequesterTWith base mapPatch weakenPatchWith patchNewElements mergePatchIncremental f dm0 dm' = undefined
