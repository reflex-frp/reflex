{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
#ifdef USE_REFLEX_OPTIMIZER
{-# OPTIONS_GHC -fplugin=Reflex.Optimizer #-}
#endif

-- | This module contains the Reflex interface, as well as a variety of
-- convenience functions for working with 'Event's, 'Behavior's, and other
-- signals.
module Reflex.Class
  ( module Reflex.Patch
    -- * Primitives
  , Reflex (..)
  , mergeInt
  , coerceBehavior
  , coerceEvent
  , coerceDynamic
  , MonadSample (..)
  , MonadHold (..)
    -- ** 'fan'-related types
  , EventSelector (..)
  , EventSelectorInt (..)
    -- ** 'Incremental'-related types
    -- * Convenience functions
  , constDyn
  , pushAlways
    -- ** Combining 'Event's
  , leftmost
  , mergeMap
  , mergeMapIncremental
  , mergeMapIncrementalWithMove
  , mergeIntMapIncremental
  , mergeList
  , mergeWith
  , difference
  , alignEventWithMaybe
    -- ** Breaking up 'Event's
  , splitE
  , fanEither
  , fanThese
  , fanMap
  , dmapToThese
  , EitherTag (..)
  , eitherToDSum
  , dsumToEither
    -- ** Collapsing 'Event . Event'
  , switchHold
  , switchHoldPromptly
  , switchHoldPromptOnly
    -- ** Using 'Event's to sample 'Behavior's
  , tag
  , attach
  , attachWith
  , attachWithMaybe
    -- ** Blocking an 'Event' based on a 'Behavior'
  , gate
    -- ** Combining 'Dynamic's
  , distributeDMapOverDynPure
  , distributeListOverDyn
  , distributeListOverDynWith
  , zipDyn
  , zipDynWith
    -- ** Accumulating state
  , Accumulator (..)
  , accumDyn
  , accumMDyn
  , accumMaybeDyn
  , accumMaybeMDyn
  , mapAccumDyn
  , mapAccumMDyn
  , mapAccumMaybeDyn
  , mapAccumMaybeMDyn
  , accumB
  , accumMB
  , accumMaybeB
  , accumMaybeMB
  , mapAccumB
  , mapAccumMB
  , mapAccumMaybeB
  , mapAccumMaybeMB
  , mapAccum_
  , mapAccumM_
  , mapAccumMaybe_
  , mapAccumMaybeM_
  , zipListWithEvent
  , numberOccurrences
  , numberOccurrencesFrom
  , numberOccurrencesFrom_
  , (<@>)
  , (<@)
  , tailE
  , headTailE
  , takeWhileE
  , switcher
    -- * Debugging functions
  , traceEvent
  , traceEventWith
    -- * Unsafe functions
  , unsafeDynamic
  , unsafeMapIncremental
    -- * 'FunctorMaybe'
  , FunctorMaybe (..)
  , fforMaybe
  , ffilter
  , filterLeft
  , filterRight
   -- * 'Adjustable'
  , Adjustable(..)
  , sequenceDMapWithAdjust
  , sequenceDMapWithAdjustWithMove
  , mapMapWithAdjustWithMove
    -- * Miscellaneous convenience functions
  , ffor
    -- * Deprecated functions
  , MonadAdjust
  , appendEvents
  , onceE
  , sequenceThese
  , fmapMaybeCheap
  , fmapCheap
  , fforCheap
  , fforMaybeCheap
  , pushAlwaysCheap
  , tagCheap
  , mergeWithCheap
  , mergeWithCheap'
  , switchPromptly
  , switchPromptOnly
    -- * Slow, but general, implementations
  , slowHeadE
  ) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.RWS (RWST)
import Control.Monad.Trans.Writer (WriterT)
import Data.Align
import Data.Bifunctor
import Data.Coerce
import Data.Default
import Data.Dependent.Map (DMap, DSum (..), GCompare (..))
import qualified Data.Dependent.Map as DMap
import Data.FastMutableIntMap (PatchIntMap)
import Data.Foldable
import Data.Functor.Bind
import Data.Functor.Constant
import Data.Functor.Misc
import Data.Functor.Plus
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Semigroup (Semigroup, sconcat, stimes, (<>))
import Data.String
import Data.These
import Data.Type.Coercion
import Reflex.FunctorMaybe
import Reflex.Patch

import Debug.Trace (trace)

-- | The 'Reflex' class contains all the primitive functionality needed for
-- Functional Reactive Programming (FRP).  The @t@ type parameter indicates
-- which "timeline" is in use.  Timelines are fully-independent FRP contexts,
-- and the type of the timeline determines the FRP engine to be used.  For most
-- purposes, the 'Reflex.Spider' implementation is recommended.
class ( MonadHold t (PushM t)
      , MonadSample t (PullM t)
      , MonadFix (PushM t)
      , Functor (Dynamic t)
      , Applicative (Dynamic t) -- Necessary for GHC <= 7.8
      , Monad (Dynamic t)
      ) => Reflex t where
  -- | A container for a value that can change over time.  'Behavior's can be
  -- sampled at will, but it is not possible to be notified when they change
  data Behavior t :: * -> *
  -- | A stream of occurrences.  During any given frame, an 'Event' is either
  -- occurring or not occurring; if it is occurring, it will contain a value of
  -- the given type (its "occurrence type")
  data Event t :: * -> *
  -- | A container for a value that can change over time and allows
  -- notifications on changes.  Basically a combination of a 'Behavior' and an
  -- 'Event', with a rule that the 'Behavior' will change if and only if the
  -- 'Event' fires.
  data Dynamic t :: * -> *
  -- | An 'Incremental' is a more general form of  a 'Dynamic'.
  -- Instead of always fully replacing the value, only parts of it can be patched.
  -- This is only needed for performance critical code via `mergeIncremental` to make small
  -- changes to large values.
  data Incremental t :: * -> *
  -- | A monad for doing complex push-based calculations efficiently
  type PushM t :: * -> *
  -- | A monad for doing complex pull-based calculations efficiently
  type PullM t :: * -> *
  -- | An 'Event' with no occurrences
  never :: Event t a
  -- | Create a 'Behavior' that always has the given value
  constant :: a -> Behavior t a --TODO: Refactor to use 'pure' from Applicative instead; however, we need to make sure that encouraging Applicative-style use of 'Behavior's doesn't have a negative performance impact
  -- | Create an 'Event' from another 'Event'; the provided function can sample
  -- 'Behavior's and hold 'Event's, and use the results to produce a occurring
  -- (Just) or non-occurring (Nothing) result
  push :: (a -> PushM t (Maybe b)) -> Event t a -> Event t b
  -- | Like 'push' but intended for functions that the implementation can consider cheap to compute for performance considerations. WARNING: The function passed to 'pushCheap' may be run multiple times without any caching.
  pushCheap :: (a -> PushM t (Maybe b)) -> Event t a -> Event t b
  -- | Create a 'Behavior' by reading from other 'Behavior's; the result will be
  -- recomputed whenever any of the read 'Behavior's changes
  pull :: PullM t a -> Behavior t a
  -- | Merge a collection of events; the resulting 'Event' will only occur if at
  -- least one input event is occuring, and will contain all of the input keys
  -- that are occurring simultaneously
  merge :: GCompare k => DMap k (Event t) -> Event t (DMap k Identity) --TODO: Generalize to get rid of DMap use --TODO: Provide a type-level guarantee that the result is not empty
  -- | Efficiently fan-out an event to many destinations.  This function should
  -- be partially applied, and then the result applied repeatedly to create
  -- child events
  fan :: GCompare k => Event t (DMap k Identity) -> EventSelector t k --TODO: Can we help enforce the partial application discipline here?  The combinator is worthless without it
  -- | Create an 'Event' that will occur whenever the currently-selected input
  -- 'Event' occurs
  switch :: Behavior t (Event t a) -> Event t a
  -- | Create an 'Event' that will occur whenever the input event is occurring
  -- and its occurrence value, another 'Event', is also occurring
  coincidence :: Event t (Event t a) -> Event t a
  -- | Extract the 'Behavior' of a 'Dynamic'.
  current :: Dynamic t a -> Behavior t a
  -- | Extract the 'Event' of the 'Dynamic'.
  updated :: Dynamic t a -> Event t a
  -- | Create a new 'Dynamic'.  The given 'PullM' must always return the most
  -- recent firing of the given 'Event', if any.
  unsafeBuildDynamic :: PullM t a -> Event t a -> Dynamic t a
  -- | Create a new 'Incremental'.  The given 'PullM''s value must always change
  -- in the same way that the accumulated application of patches would change
  -- that value.
  unsafeBuildIncremental :: Patch p => PullM t (PatchTarget p) -> Event t p -> Incremental t p
  -- | Create a merge whose parents can change over time
  mergeIncremental :: GCompare k => Incremental t (PatchDMap k (Event t)) -> Event t (DMap k Identity)
  -- | Experimental: Create a merge whose parents can change over time; changing the key of an Event is more efficient than with mergeIncremental
  mergeIncrementalWithMove :: GCompare k => Incremental t (PatchDMapWithMove k (Event t)) -> Event t (DMap k Identity)
  -- | Extract the 'Behavior' component of an 'Incremental'
  currentIncremental :: Patch p => Incremental t p -> Behavior t (PatchTarget p)
  -- | Extract the 'Event' component of an 'Incremental'
  updatedIncremental :: Patch p => Incremental t p -> Event t p
  -- | Convert an 'Incremental' to a 'Dynamic'
  incrementalToDynamic :: Patch p => Incremental t p -> Dynamic t (PatchTarget p)
  -- | Construct a 'Coercion' for a 'Behavior' given an 'Coercion' for its
  -- occurrence type
  behaviorCoercion :: Coercion a b -> Coercion (Behavior t a) (Behavior t b)
  -- | Construct a 'Coercion' for an 'Event' given an 'Coercion' for its
  -- occurrence type
  eventCoercion :: Coercion a b -> Coercion (Event t a) (Event t b)
  -- | Construct a 'Coercion' for a 'Dynamic' given an 'Coercion' for its
  -- occurrence type
  dynamicCoercion :: Coercion a b -> Coercion (Dynamic t a) (Dynamic t b)
  mergeIntIncremental :: Incremental t (PatchIntMap (Event t a)) -> Event t (IntMap a)
  fanInt :: Event t (IntMap a) -> EventSelectorInt t a

--TODO: Specialize this so that we can take advantage of knowing that there's no changing going on
mergeInt :: Reflex t => IntMap (Event t a) -> Event t (IntMap a)
mergeInt m = mergeIntIncremental $ unsafeBuildIncremental (return m) never

-- | Coerce a 'Behavior' between representationally-equivalent value types
coerceBehavior :: (Reflex t, Coercible a b) => Behavior t a -> Behavior t b
coerceBehavior = coerceWith $ behaviorCoercion Coercion

-- | Coerce an 'Event' between representationally-equivalent occurrence types
coerceEvent :: (Reflex t, Coercible a b) => Event t a -> Event t b
coerceEvent = coerceWith $ eventCoercion Coercion

-- | Coerce a 'Dynamic' between representationally-equivalent value types
coerceDynamic :: (Reflex t, Coercible a b) => Dynamic t a -> Dynamic t b
coerceDynamic = coerceWith $ dynamicCoercion Coercion

-- | Construct a 'Dynamic' from a 'Behavior' and an 'Event'.  The 'Behavior'
-- _must_ change when and only when the 'Event' fires, such that the
-- 'Behavior''s value is always equal to the most recent firing of the 'Event';
-- if this is not the case, the resulting 'Dynamic' will behave
-- nondeterministically.
unsafeDynamic :: Reflex t => Behavior t a -> Event t a -> Dynamic t a
unsafeDynamic = unsafeBuildDynamic . sample

-- | Construct a 'Dynamic' value that never changes
constDyn :: Reflex t => a -> Dynamic t a
constDyn = pure

instance (Reflex t, Default a) => Default (Dynamic t a) where
  def = pure def

-- | 'MonadSample' designates monads that can read the current value of a
-- 'Behavior'.  This includes both 'PullM' and 'PushM'.
class (Applicative m, Monad m) => MonadSample t m | m -> t where
  -- | Get the current value in the 'Behavior'
  sample :: Behavior t a -> m a

-- | 'MonadHold' designates monads that can create new 'Behavior's based on
-- 'Event's; usually this will be 'PushM' or a monad based on it.  'MonadHold'
-- is required to create any stateful computations with Reflex.
class MonadSample t m => MonadHold t m where
  -- | Create a new 'Behavior' whose value will initially be equal to the given
  -- value and will be updated whenever the given 'Event' occurs.  The update
  -- takes effect immediately after the 'Event' occurs; if the occurrence that
  -- sets the 'Behavior' (or one that is simultaneous with it) is used to sample
  -- the 'Behavior', it will see the _old_ value of the 'Behavior', not the new
  -- one.
  hold :: a -> Event t a -> m (Behavior t a)
  default hold :: (m ~ f m', MonadTrans f, MonadHold t m') => a -> Event t a -> m (Behavior t a)
  hold v0 = lift . hold v0
  -- | Create a 'Dynamic' value using the given initial value that changes every
  -- time the 'Event' occurs.
  holdDyn :: a -> Event t a -> m (Dynamic t a)
  default holdDyn :: (m ~ f m', MonadTrans f, MonadHold t m') => a -> Event t a -> m (Dynamic t a)
  holdDyn v0 = lift . holdDyn v0
  -- | Create an 'Incremental' value using the given initial value that changes
  -- every time the 'Event' occurs.
  holdIncremental :: Patch p => PatchTarget p -> Event t p -> m (Incremental t p)
  default holdIncremental :: (Patch p, m ~ f m', MonadTrans f, MonadHold t m') => PatchTarget p -> Event t p -> m (Incremental t p)
  holdIncremental v0 = lift . holdIncremental v0
  buildDynamic :: PullM t a -> Event t a -> m (Dynamic t a)
  {-
  default buildDynamic :: (m ~ f m', MonadTrans f, MonadHold t m') => PullM t a -> Event t a -> m (Dynamic t a)
  buildDynamic getV0 = lift . buildDynamic getV0
  -}
  -- | Create a new 'Event' that only occurs only once, on the first occurence of
  -- the supplied 'Event'.
  headE :: Event t a -> m (Event t a)


slowHeadE :: (Reflex t, MonadHold t m, MonadFix m) => Event t a -> m (Event t a)
slowHeadE e = do
  rec be <- hold e $ fmapCheap (const never) e'
      let e' = switch be
  return e'

-- | An 'EventSelector' allows you to efficiently 'select' an 'Event' based on a
-- key.  This is much more efficient than filtering for each key with
-- 'fmapMaybe'.
newtype EventSelector t k = EventSelector
  { -- | Retrieve the 'Event' for the given key.  The type of the 'Event' is
    -- determined by the type of the key, so this can be used to fan-out
    -- 'Event's whose sub-'Event's have different types.
    --
    -- Using 'EventSelector's and the 'fan' primitive is far more efficient than
    -- (but equivalent to) using 'fmapMaybe' to select only the relevant
    -- occurrences of an 'Event'.
    select :: forall a. k a -> Event t a
  }

newtype EventSelectorInt t a = EventSelectorInt { selectInt :: Int -> Event t a }

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance MonadSample t m => MonadSample t (ReaderT r m) where
  sample = lift . sample

instance MonadHold t m => MonadHold t (ReaderT r m) where
  hold a0 = lift . hold a0
  holdDyn a0 = lift . holdDyn a0
  holdIncremental a0 = lift . holdIncremental a0
  buildDynamic a0 = lift . buildDynamic a0
  headE = lift . headE

instance (MonadSample t m, Monoid r) => MonadSample t (WriterT r m) where
  sample = lift . sample

instance (MonadHold t m, Monoid r) => MonadHold t (WriterT r m) where
  hold a0 = lift . hold a0
  holdDyn a0 = lift . holdDyn a0
  holdIncremental a0 = lift . holdIncremental a0
  buildDynamic a0 = lift . buildDynamic a0
  headE = lift . headE

instance MonadSample t m => MonadSample t (StateT s m) where
  sample = lift . sample

instance MonadHold t m => MonadHold t (StateT s m) where
  hold a0 = lift . hold a0
  holdDyn a0 = lift . holdDyn a0
  holdIncremental a0 = lift . holdIncremental a0
  buildDynamic a0 = lift . buildDynamic a0
  headE = lift . headE

instance MonadSample t m => MonadSample t (ExceptT e m) where
  sample = lift . sample

instance MonadHold t m => MonadHold t (ExceptT e m) where
  hold a0 = lift . hold a0
  holdDyn a0 = lift . holdDyn a0
  holdIncremental a0 = lift . holdIncremental a0
  buildDynamic a0 = lift . buildDynamic a0
  headE = lift . headE

instance (MonadSample t m, Monoid w) => MonadSample t (RWST r w s m) where
  sample = lift . sample

instance (MonadHold t m, Monoid w) => MonadHold t (RWST r w s m) where
  hold a0 = lift . hold a0
  holdDyn a0 = lift . holdDyn a0
  holdIncremental a0 = lift . holdIncremental a0
  buildDynamic a0 = lift . buildDynamic a0
  headE = lift . headE

instance MonadSample t m => MonadSample t (ContT r m) where
  sample = lift . sample

instance MonadHold t m => MonadHold t (ContT r m) where
  hold a0 = lift . hold a0
  holdDyn a0 = lift . holdDyn a0
  holdIncremental a0 = lift . holdIncremental a0
  buildDynamic a0 = lift . buildDynamic a0
  headE = lift . headE

--------------------------------------------------------------------------------
-- Convenience functions
--------------------------------------------------------------------------------

-- | Create an 'Event' from another 'Event'.  The provided function can sample
-- 'Behavior's and hold 'Event's.
pushAlways :: Reflex t => (a -> PushM t b) -> Event t a -> Event t b
pushAlways f = push (fmap Just . f)

-- | Flipped version of 'fmap'.
ffor :: Functor f => f a -> (a -> b) -> f b
ffor = flip fmap

instance Reflex t => Applicative (Behavior t) where
  pure = constant
  f <*> x = pull $ sample f `ap` sample x
  _ *> b = b
  a <* _ = a

instance Reflex t => Apply (Behavior t) where
  (<.>) = (<*>)

instance Reflex t => Bind (Behavior t) where
  (>>-) = (>>=)

instance (Reflex t, Fractional a) => Fractional (Behavior t a) where
  (/) = liftA2 (/)
  fromRational = pure . fromRational
  recip = fmap recip

instance Reflex t => Functor (Behavior t) where
  fmap f = pull . fmap f . sample

instance (Reflex t, IsString a) => IsString (Behavior t a) where
  fromString = pure . fromString

instance Reflex t => Monad (Behavior t) where
  a >>= f = pull $ sample a >>= sample . f
  -- Note: it is tempting to write (_ >> b = b); however, this would result in (fail x >> return y) succeeding (returning y), which violates the law that (a >> b = a >>= \_ -> b), since the implementation of (>>=) above actually will fail.  Since we can't examine 'Behavior's other than by using sample, I don't think it's possible to write (>>) to be more efficient than the (>>=) above.
  return = constant
  fail = error "Monad (Behavior t) does not support fail"

instance (Reflex t, Monoid a) => Monoid (Behavior t a) where
  mempty = constant mempty
  mappend a b = pull $ liftM2 mappend (sample a) (sample b)
  mconcat = pull . fmap mconcat . mapM sample

instance (Reflex t, Num a) => Num (Behavior t a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  abs = fmap abs
  fromInteger = pure . fromInteger
  negate = fmap negate
  signum = fmap signum

instance (Reflex t, Semigroup a) => Semigroup (Behavior t a) where
  a <> b = pull $ liftM2 (<>) (sample a) (sample b)
  sconcat = pull . fmap sconcat . mapM sample
#if MIN_VERSION_semigroups(0,17,0)
  stimes n = fmap $ stimes n
#else
  times1p n = fmap $ times1p n
#endif

-- | Flipped version of 'fmapMaybe'.
fforMaybe :: FunctorMaybe f => f a -> (a -> Maybe b) -> f b
fforMaybe = flip fmapMaybe

-- | Filter 'f a' using the provided predicate.
-- Relies on 'fforMaybe'.
ffilter :: FunctorMaybe f => (a -> Bool) -> f a -> f a
ffilter f = fmapMaybe $ \x -> if f x then Just x else Nothing

-- | Filter 'Left's from 'f (Either a b)' into 'a'.
filterLeft :: FunctorMaybe f => f (Either a b) -> f a
filterLeft = fmapMaybe (either Just (const Nothing))

-- | Filter 'Right's from 'f (Either a b)' into 'b'.
filterRight :: FunctorMaybe f => f (Either a b) -> f b
filterRight = fmapMaybe (either (const Nothing) Just)

-- | Left-biased event union (prefers left event on simultaneous
-- occurrence).
instance Reflex t => Alt (Event t) where
  ev1 <!> ev2 = leftmost [ev1, ev2]

-- | 'Event' intersection (convenient interface to 'coincidence').
instance Reflex t => Apply (Event t) where
  evf <.> evx = coincidence (fmap (<$> evx) evf)

-- | 'Event' intersection (convenient interface to 'coincidence').
instance Reflex t => Bind (Event t) where
  evx >>- f = coincidence (f <$> evx)
  join = coincidence

instance Reflex t => Functor (Event t) where
  {-# INLINE fmap #-}
  fmap f = fmapMaybe $ Just . f
  {-# INLINE (<$) #-}
  x <$ e = fmapCheap (const x) e

instance Reflex t => FunctorMaybe (Event t) where
  {-# INLINE fmapMaybe #-}
  fmapMaybe f = push $ return . f

-- | Never: @'zero' = 'never'@.
instance Reflex t => Plus (Event t) where
  zero = never

-- | Replace each occurrence value of the 'Event' with the value of the
-- 'Behavior' at the time of that occurrence.
tag :: Reflex t => Behavior t b -> Event t a -> Event t b
tag b = pushAlways $ \_ -> sample b

-- | Create a new 'Event' that combines occurrences of supplied 'Event' with the
-- current value of the 'Behavior'.
attach :: Reflex t => Behavior t a -> Event t b -> Event t (a, b)
attach = attachWith (,)

-- | Create a new 'Event' that occurs when the supplied 'Event' occurs by
-- combining it with the current value of the 'Behavior'.
attachWith :: Reflex t => (a -> b -> c) -> Behavior t a -> Event t b -> Event t c
attachWith f = attachWithMaybe $ \a b -> Just $ f a b

-- | Create a new 'Event' by combining each occurrence with the current value of
-- the 'Behavior'. The occurrence is discarded if the combining function returns
-- Nothing
attachWithMaybe :: Reflex t => (a -> b -> Maybe c) -> Behavior t a -> Event t b -> Event t c
attachWithMaybe f b e = flip push e $ \o -> (`f` o) <$> sample b

-- | Create a new 'Event' that occurs on all but the first occurrence of the
-- supplied 'Event'.
tailE :: (Reflex t, MonadHold t m) => Event t a -> m (Event t a)
tailE e = snd <$> headTailE e

-- | Create a tuple of two 'Event's with the first one occurring only the first
-- time the supplied 'Event' occurs and the second occurring on all but the first
-- occurence.
headTailE :: (Reflex t, MonadHold t m) => Event t a -> m (Event t a, Event t a)
headTailE e = do
  eHead <- headE e
  be <- hold never $ fmap (const e) eHead
  return (eHead, switch be)

-- | Starting at the current time, fire all the occurrences of the 'Event' for
-- which the given predicate returns 'True'.  When 'False' is returned, do not
-- fire, and permanently stop firing, even if 'True' values are encountered
-- later.
takeWhileE :: forall t m a. (Reflex t, MonadFix m, MonadHold t m) => (a -> Bool) -> Event t a -> m (Event t a)
takeWhileE f e = do
  rec let (eFalse, eTrue) = fanEither $ ffor e' $ \a -> if f a
            then Right a
            else Left never
      be <- hold e eFalse
      let e' = switch be
  return eTrue

-- | Split the supplied 'Event' into two individual 'Event's occuring at the
-- same time with the respective values from the tuple.
splitE :: Reflex t => Event t (a, b) -> (Event t a, Event t b)
splitE e = (fmap fst e, fmap snd e)

-- | Print the supplied 'String' and the value of the 'Event' on each
-- occurence. This should /only/ be used for debugging.
--
-- Note: As with Debug.Trace.trace, the message will only be printed if the
-- 'Event' is actually used.
traceEvent :: (Reflex t, Show a) => String -> Event t a -> Event t a
traceEvent s = traceEventWith $ \x -> s <> ": " <> show x

-- | Print the output of the supplied function on each occurrence of the
-- 'Event'. This should /only/ be used for debugging.
--
-- Note: As with Debug.Trace.trace, the message will only be printed if the
-- 'Event' is actually used.
traceEventWith :: Reflex t => (a -> String) -> Event t a -> Event t a
traceEventWith f = push $ \x -> trace (f x) $ return $ Just x

instance (Semigroup a, Reflex t) => Semigroup (Event t a) where
  (<>) = alignWith (mergeThese (<>))
  sconcat = fmap sconcat . mergeList . toList
#if MIN_VERSION_semigroups(0,17,0)
  stimes n = fmap $ stimes n
#else
  times1p n = fmap $ times1p n
#endif

instance (Semigroup a, Reflex t) => Monoid (Event t a) where
  mempty = never
  mappend = (<>)
  mconcat = fmap sconcat . mergeList

-- | Create a new 'Event' that occurs if at least one of the 'Event's in the
-- list occurs. If multiple occur at the same time they are folded from the left
-- with the given function.
{-# INLINE mergeWith #-}
mergeWith :: Reflex t => (a -> a -> a) -> [Event t a] -> Event t a
mergeWith = mergeWith' id

{-# INLINE mergeWith' #-}
mergeWith' :: Reflex t => (a -> b) -> (b -> b -> b) -> [Event t a] -> Event t b
mergeWith' f g es = fmap (Prelude.foldl1 g . fmap f)
                  . mergeInt
                  . IntMap.fromDistinctAscList
                  $ zip [0 :: Int ..] es

-- | Create a new 'Event' that occurs if at least one of the 'Event's in the
-- list occurs. If multiple occur at the same time the value is the value of the
-- leftmost event.
{-# INLINE leftmost #-}
leftmost :: Reflex t => [Event t a] -> Event t a
leftmost = mergeWith const

-- | Create a new 'Event' that occurs if at least one of the 'Event's in the
-- list occurs and has a list of the values of all 'Event's occurring at that
-- time.
mergeList :: Reflex t => [Event t a] -> Event t (NonEmpty a)
mergeList [] = never
mergeList es = mergeWithFoldCheap' id es

unsafeMapIncremental :: (Reflex t, Patch p, Patch p') => (PatchTarget p -> PatchTarget p') -> (p -> p') -> Incremental t p -> Incremental t p'
unsafeMapIncremental f g a = unsafeBuildIncremental (fmap f $ sample $ currentIncremental a) $ g <$> updatedIncremental a

-- | Create a new 'Event' combining the map of 'Event's into an 'Event' that
-- occurs if at least one of them occurs and has a map of values of all 'Event's
-- occuring at that time.
mergeMap :: (Reflex t, Ord k) => Map k (Event t a) -> Event t (Map k a)
mergeMap = fmap dmapToMap . merge . mapWithFunctorToDMap

-- | Create a merge whose parents can change over time
mergeMapIncremental :: (Reflex t, Ord k) => Incremental t (PatchMap k (Event t a)) -> Event t (Map k a)
mergeMapIncremental = fmap dmapToMap . mergeIncremental . unsafeMapIncremental mapWithFunctorToDMap (const2PatchDMapWith id)

-- | Create a merge whose parents can change over time
mergeIntMapIncremental :: Reflex t => Incremental t (PatchIntMap (Event t a)) -> Event t (IntMap a)
mergeIntMapIncremental = fmap dmapToIntMap . mergeIncremental . unsafeMapIncremental intMapWithFunctorToDMap (const2IntPatchDMapWith id)

-- | Experimental: Create a merge whose parents can change over time; changing the key of an Event is more efficient than with mergeIncremental
mergeMapIncrementalWithMove :: (Reflex t, Ord k) => Incremental t (PatchMapWithMove k (Event t a)) -> Event t (Map k a)
mergeMapIncrementalWithMove = fmap dmapToMap . mergeIncrementalWithMove . unsafeMapIncremental mapWithFunctorToDMap (const2PatchDMapWithMoveWith id)

-- | Split the event into separate events for 'Left' and 'Right' values.
fanEither :: Reflex t => Event t (Either a b) -> (Event t a, Event t b)
fanEither e =
  let justLeft = either Just (const Nothing)
      justRight = either (const Nothing) Just
  in (fmapMaybe justLeft e, fmapMaybe justRight e)

-- | Split the event into separate events for 'This' and 'That' values,
-- allowing them to fire simultaneously when the input value is 'These'.
fanThese :: Reflex t => Event t (These a b) -> (Event t a, Event t b)
fanThese e =
  let this (This x) = Just x
      this (These x _) = Just x
      this _ = Nothing
      that (That y) = Just y
      that (These _ y) = Just y
      that _ = Nothing
  in (fmapMaybe this e, fmapMaybe that e)

-- | Split the event into an 'EventSelector' that allows efficient selection of
-- the individual 'Event's.
fanMap :: (Reflex t, Ord k) => Event t (Map k a) -> EventSelector t (Const2 k a)
fanMap = fan . fmap mapToDMap

-- | Switches to the new event whenever it receives one. Only the old event is
-- considered the moment a new one is switched in; the output event will fire at
-- that moment if only if the old event does.
--
-- Because the simultaneous firing case is irrelevant, this function imposes
-- laxer "timing requirements" on the overall circuit, avoiding many potential
-- cyclic dependency / metastability failures. It's also more performant. Use
-- this rather than 'switchHoldPromptly' and 'switchHoldPromptOnly' unless you
-- are absolutely sure you need to act on the new event in the coincidental
-- case.
switchHold :: (Reflex t, MonadHold t m) => Event t a -> Event t (Event t a) -> m (Event t a)
switchHold ea0 eea = switch <$> hold ea0 eea

-- | Switches to the new event whenever it receives one. Whenever a new event is
-- provided, if it is firing, its value will be the resulting event's value; if
-- it is not firing, but the old one is, the old one's value will be used.
--
-- 'switchHold', by always forwarding the old event the moment it is switched
-- out, avoids many potential cyclic dependency problems / metastability
-- problems. It's also more performant. Use it instead unless you are sure you
-- cannot.
switchHoldPromptly :: (Reflex t, MonadHold t m) => Event t a -> Event t (Event t a) -> m (Event t a)
switchHoldPromptly ea0 eea = do
  bea <- hold ea0 eea
  let eLag = switch bea
      eCoincidences = coincidence eea
  return $ leftmost [eCoincidences, eLag]

-- | switches to a new event whenever it receives one.  At the moment of
-- switching, the old event will be ignored if it fires, and the new one will be
-- used if it fires; this is the opposite of 'switch', which will use only the
-- old value.
--
-- 'switchHold', by always forwarding the old event the moment it is switched
-- out, avoids many potential cyclic dependency problems / metastability
-- problems. It's also more performant. Use it instead unless you are sure you
-- cannot.
switchHoldPromptOnly :: (Reflex t, MonadHold t m) => Event t a -> Event t (Event t a) -> m (Event t a)
switchHoldPromptOnly e0 e' = do
  eLag <- switch <$> hold e0 e'
  return $ coincidence $ leftmost [e', eLag <$ eLag]

instance Reflex t => Align (Event t) where
  nil = never
  align = alignEventWithMaybe Just

-- | Create a new 'Event' that only occurs if the supplied 'Event' occurs and
-- the 'Behavior' is true at the time of occurence.
gate :: Reflex t => Behavior t Bool -> Event t a -> Event t a
gate = attachWithMaybe $ \allow a -> if allow then Just a else Nothing

-- | Create a new behavior given a starting behavior and switch to a the behvior
--   carried by the event when it fires.
switcher :: (Reflex t, MonadHold t m)
        => Behavior t a -> Event t (Behavior t a) -> m (Behavior t a)
switcher b eb = pull . (sample <=< sample) <$> hold b eb

instance (Reflex t, IsString a) => IsString (Dynamic t a) where
  fromString = pure . fromString

-- | Combine two 'Dynamic's.  The result will change whenever either (or both)
-- input 'Dynamic' changes.  Equivalent to @zipDynWith (,)@.
zipDyn :: Reflex t => Dynamic t a -> Dynamic t b -> Dynamic t (a, b)
zipDyn = zipDynWith (,)

-- | Combine two 'Dynamic's with a combining function.  The result will change
-- whenever either (or both) input 'Dynamic' changes.
-- More efficient than liftA2.
zipDynWith :: Reflex t => (a -> b -> c) -> Dynamic t a -> Dynamic t b -> Dynamic t c
zipDynWith f da db =
  let eab = align (updated da) (updated db)
      ec = flip push eab $ \o -> do
        (a, b) <- case o of
          This a -> do
            b <- sample $ current db
            return (a, b)
          That b -> do
            a <- sample $ current da
            return (a, b)
          These a b -> return (a, b)
        return $ Just $ f a b
  in unsafeBuildDynamic (f <$> sample (current da) <*> sample (current db)) ec

instance (Reflex t, Semigroup a) => Semigroup (Dynamic t a) where
  (<>) = zipDynWith (<>)
#if MIN_VERSION_semigroups(0,17,0)
  stimes n = fmap $ stimes n
#else
  times1p n = fmap $ times1p n
#endif

instance (Reflex t, Monoid a) => Monoid (Dynamic t a) where
  mconcat = distributeListOverDynWith mconcat
  mempty = constDyn mempty
  mappend = zipDynWith mappend

-- | This function converts a 'DMap' whose elements are 'Dynamic's into a
-- 'Dynamic' 'DMap'.  Its implementation is more efficient than doing the same
-- through the use of multiple uses of 'zipWithDyn' or 'Applicative' operators.
distributeDMapOverDynPure :: forall t k. (Reflex t, GCompare k) => DMap k (Dynamic t) -> Dynamic t (DMap k Identity)
distributeDMapOverDynPure dm = case DMap.toList dm of
  [] -> constDyn DMap.empty
  [k :=> v] -> fmap (DMap.singleton k . Identity) v
  _ ->
    let getInitial = DMap.traverseWithKey (\_ -> fmap Identity . sample . current) dm
        edmPre = merge $ DMap.map updated dm
        result = unsafeBuildDynamic getInitial $ flip pushAlways edmPre $ \news -> do
          olds <- sample $ current result
          return $ DMap.unionWithKey (\_ _ new -> new) olds news
    in result

-- | Convert a list of 'Dynamic's into a 'Dynamic' list.
distributeListOverDyn :: Reflex t => [Dynamic t a] -> Dynamic t [a]
distributeListOverDyn = distributeListOverDynWith id

-- | Create a new 'Dynamic' by applying a combining function to a list of 'Dynamic's
distributeListOverDynWith :: Reflex t => ([a] -> b) -> [Dynamic t a] -> Dynamic t b
distributeListOverDynWith f = fmap (f . map (\(Const2 _ :=> Identity v) -> v) . DMap.toList) . distributeDMapOverDynPure . DMap.fromList . map (\(k, v) -> Const2 k :=> v) . zip [0 :: Int ..]

-- | Create a new 'Event' that occurs when the first supplied 'Event' occurs
-- unless the second supplied 'Event' occurs simultaneously.
difference :: Reflex t => Event t a -> Event t b -> Event t a
difference = alignEventWithMaybe $ \case
  This a -> Just a
  _      -> Nothing

alignEventWithMaybe :: Reflex t => (These a b -> Maybe c) -> Event t a -> Event t b -> Event t c
alignEventWithMaybe f ea eb =
  fmapMaybe (f <=< dmapToThese)
    $ merge
    $ DMap.fromList [LeftTag :=> ea, RightTag :=> eb]


--------------------------------------------------------------------------------
-- Accumulator
--------------------------------------------------------------------------------

-- | An 'Accumulator' type can be built by accumulating occurrences of an
-- 'Event'.
#if __GLASGOW_HASKELL__ < 802
{-# WARNING accum "ghc < 8.2.1 doesn't seem to be able to specialize functions in this class, which can lead to poor performance" #-}
{-# WARNING accumM "ghc < 8.2.1 doesn't seem to be able to specialize functions in this class, which can lead to poor performance" #-}
{-# WARNING accumMaybe "ghc < 8.2.1 doesn't seem to be able to specialize functions in this class, which can lead to poor performance" #-}
{-# WARNING accumMaybeM "ghc < 8.2.1 doesn't seem to be able to specialize functions in this class, which can lead to poor performance" #-}
#endif
class Reflex t => Accumulator t f | f -> t where
  accum :: (MonadHold t m, MonadFix m) => (a -> b -> a) -> a -> Event t b -> m (f a)
  accum f = accumMaybe $ \v o -> Just $ f v o
  accumM :: (MonadHold t m, MonadFix m) => (a -> b -> PushM t a) -> a -> Event t b -> m (f a)
  accumM f = accumMaybeM $ \v o -> Just <$> f v o
  accumMaybe :: (MonadHold t m, MonadFix m) => (a -> b -> Maybe a) -> a -> Event t b -> m (f a)
  accumMaybe f = accumMaybeM $ \v o -> return $ f v o
  accumMaybeM :: (MonadHold t m, MonadFix m) => (a -> b -> PushM t (Maybe a)) -> a -> Event t b -> m (f a)
  mapAccum :: (MonadHold t m, MonadFix m) => (a -> b -> (a, c)) -> a -> Event t b -> m (f a, Event t c)
  mapAccum f = mapAccumMaybe $ \v o -> bimap Just Just $ f v o
  mapAccumM :: (MonadHold t m, MonadFix m) => (a -> b -> PushM t (a, c)) -> a -> Event t b -> m (f a, Event t c)
  mapAccumM f = mapAccumMaybeM $ \v o -> bimap Just Just <$> f v o
  mapAccumMaybe :: (MonadHold t m, MonadFix m) => (a -> b -> (Maybe a, Maybe c)) -> a -> Event t b -> m (f a, Event t c)
  mapAccumMaybe f = mapAccumMaybeM $ \v o -> return $ f v o
  mapAccumMaybeM :: (MonadHold t m, MonadFix m) => (a -> b -> PushM t (Maybe a, Maybe c)) -> a -> Event t b -> m (f a, Event t c)

accumDyn :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> a) -> a -> Event t b -> m (Dynamic t a)
accumDyn f = accumMaybeDyn $ \v o -> Just $ f v o
accumMDyn :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> PushM t a) -> a -> Event t b -> m (Dynamic t a)
accumMDyn f = accumMaybeMDyn $ \v o -> Just <$> f v o
accumMaybeDyn :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> Maybe a) -> a -> Event t b -> m (Dynamic t a)
accumMaybeDyn f = accumMaybeMDyn $ \v o -> return $ f v o
accumMaybeMDyn :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> PushM t (Maybe a)) -> a -> Event t b -> m (Dynamic t a)
accumMaybeMDyn f z e = do
  rec let e' = flip push e $ \o -> do
            v <- sample $ current d'
            f v o
      d' <- holdDyn z e'
  return d'
mapAccumDyn :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> (a, c)) -> a -> Event t b -> m (Dynamic t a, Event t c)
mapAccumDyn f = mapAccumMaybeDyn $ \v o -> bimap Just Just $ f v o
mapAccumMDyn :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> PushM t (a, c)) -> a -> Event t b -> m (Dynamic t a, Event t c)
mapAccumMDyn f = mapAccumMaybeMDyn $ \v o -> bimap Just Just <$> f v o
mapAccumMaybeDyn :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> (Maybe a, Maybe c)) -> a -> Event t b -> m (Dynamic t a, Event t c)
mapAccumMaybeDyn f = mapAccumMaybeMDyn $ \v o -> return $ f v o
mapAccumMaybeMDyn :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> PushM t (Maybe a, Maybe c)) -> a -> Event t b -> m (Dynamic t a, Event t c)
mapAccumMaybeMDyn f z e = do
  rec let e' = flip push e $ \o -> do
            v <- sample $ current d'
            result <- f v o
            return $ case result of
              (Nothing, Nothing) -> Nothing
              _ -> Just result
      d' <- holdDyn z $ fmapMaybe fst e'
  return (d', fmapMaybe snd e')

{-# INLINE accumB #-}
accumB :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> a) -> a -> Event t b -> m (Behavior t a)
accumB f = accumMaybeB $ \v o -> Just $ f v o
{-# INLINE accumMB #-}
accumMB :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> PushM t a) -> a -> Event t b -> m (Behavior t a)
accumMB f = accumMaybeMB $ \v o -> Just <$> f v o
{-# INLINE accumMaybeB #-}
accumMaybeB :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> Maybe a) -> a -> Event t b -> m (Behavior t a)
accumMaybeB f = accumMaybeMB $ \v o -> return $ f v o
{-# INLINE accumMaybeMB #-}
accumMaybeMB :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> PushM t (Maybe a)) -> a -> Event t b -> m (Behavior t a)
accumMaybeMB f z e = do
  rec let e' = flip push e $ \o -> do
            v <- sample d'
            f v o
      d' <- hold z e'
  return d'
{-# INLINE mapAccumB #-}
mapAccumB :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> (a, c)) -> a -> Event t b -> m (Behavior t a, Event t c)
mapAccumB f = mapAccumMaybeB $ \v o -> bimap Just Just $ f v o
{-# INLINE mapAccumMB #-}
mapAccumMB :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> PushM t (a, c)) -> a -> Event t b -> m (Behavior t a, Event t c)
mapAccumMB f = mapAccumMaybeMB $ \v o -> bimap Just Just <$> f v o
{-# INLINE mapAccumMaybeB #-}
mapAccumMaybeB :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> (Maybe a, Maybe c)) -> a -> Event t b -> m (Behavior t a, Event t c)
mapAccumMaybeB f = mapAccumMaybeMB $ \v o -> return $ f v o

{-# INLINE mapAccumMaybeMB #-}
mapAccumMaybeMB :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> PushM t (Maybe a, Maybe c)) -> a -> Event t b -> m (Behavior t a, Event t c)
mapAccumMaybeMB f z e = do
  rec let e' = flip push e $ \o -> do
            v <- sample d'
            result <- f v o
            return $ case result of
              (Nothing, Nothing) -> Nothing
              _ -> Just result
      d' <- hold z $ fmapMaybe fst e'
  return (d', fmapMaybe snd e')

-- | Accumulate occurrences of an 'Event', producing an output occurrence each
-- time.  Discard the underlying 'Accumulator'.
{-# INLINE mapAccum_ #-}
mapAccum_ :: forall t m a b c. (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> (a, c)) -> a -> Event t b -> m (Event t c)
mapAccum_ f z e = do
  (_, result) <- mapAccumB f z e
  return result

-- | Accumulate occurrences of an 'Event', possibly producing an output
-- occurrence each time.  Discard the underlying 'Accumulator'.
{-# INLINE mapAccumMaybe_ #-}
mapAccumMaybe_ :: forall t m a b c. (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> (Maybe a, Maybe c)) -> a -> Event t b -> m (Event t c)
mapAccumMaybe_ f z e = do
  (_, result) <- mapAccumMaybeB f z e
  return result

-- | Accumulate occurrences of an 'Event', using a 'PushM' action and producing
-- an output occurrence each time.  Discard the underlying 'Accumulator'.
{-# INLINE mapAccumM_ #-}
mapAccumM_ :: forall t m a b c. (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> PushM t (a, c)) -> a -> Event t b -> m (Event t c)
mapAccumM_ f z e = do
  (_, result) <- mapAccumMB f z e
  return result

-- | Accumulate occurrences of an 'Event', using a 'PushM' action and possibly
-- producing an output occurrence each time.  Discard the underlying
-- 'Accumulator'.
{-# INLINE mapAccumMaybeM_ #-}
mapAccumMaybeM_ :: forall t m a b c. (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> PushM t (Maybe a, Maybe c)) -> a -> Event t b -> m (Event t c)
mapAccumMaybeM_ f z e = do
  (_, result) <- mapAccumMaybeMB f z e
  return result

instance Reflex t => Accumulator t (Dynamic t) where
  accumMaybeM = accumMaybeMDyn
  mapAccumMaybeM = mapAccumMaybeMDyn

instance Reflex t => Accumulator t (Behavior t) where
  accumMaybeM = accumMaybeMB
  mapAccumMaybeM = mapAccumMaybeMB

instance Reflex t => Accumulator t (Event t) where
  accumMaybeM f z e = updated <$> accumMaybeM f z e
  mapAccumMaybeM f z e = first updated <$> mapAccumMaybeM f z e

-- | Create a new 'Event' by combining each occurence with the next value of the
-- list using the supplied function. If the list runs out of items, all
-- subsequent 'Event' occurrences will be ignored.
zipListWithEvent :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> c) -> [a] -> Event t b -> m (Event t c)
zipListWithEvent f l e = do
  let f' a b = case a of
        h:t -> (Just t, Just $ f h b)
        _ -> (Nothing, Nothing) --TODO: Unsubscribe the event?
  mapAccumMaybe_ f' l e

-- | Assign a number to each occurence of the given 'Event', starting from 0
{-# INLINE numberOccurrences #-}
numberOccurrences :: (Reflex t, MonadHold t m, MonadFix m, Num b) => Event t a -> m (Event t (b, a))
numberOccurrences = numberOccurrencesFrom 0

-- | Assign a number to each occurence of the given 'Event'
{-# INLINE numberOccurrencesFrom #-}
numberOccurrencesFrom :: (Reflex t, MonadHold t m, MonadFix m, Num b) => b -> Event t a -> m (Event t (b, a))
numberOccurrencesFrom = mapAccum_ (\n a -> let !next = n + 1 in (next, (n, a)))

-- | Assign a number to each occurence of the given 'Event'; discard the occurrences' values
{-# INLINE numberOccurrencesFrom_ #-}
numberOccurrencesFrom_ :: (Reflex t, MonadHold t m, MonadFix m, Num b) => b -> Event t a -> m (Event t b)
numberOccurrencesFrom_ = mapAccum_ (\n _ -> let !next = n + 1 in (next, n))

-- | This is used to sample the value of a 'Behavior' using an 'Event'.
--
-- The '<@>' operator is intended to be used in conjunction with
-- the 'Applicative' instance for 'Behavior'.
--
-- This is useful when we want to combine the values of one 'Event' and
-- the value of several 'Behavior's at the time the 'Event' is firing.
--
-- If we have:
--
-- > f  :: a -> b -> c -> d
-- > b1 :: Behavior t a
-- > b2 :: Behavior t b
-- > e  :: Event t c
--
-- then we can do:
--
-- > f <$> b1 <*> b2 <@> e :: Event t d
--
-- in order to apply the function 'f' to the relevant values.
--
-- The alternative would be something like:
--
-- > attachWith (\(x1, x2) y -> f x1 x2 y) ((,) <$> b1 <*> b2) e :: Event t d
--
-- or a variation involing a custom data type to hold the combination of
-- 'Behavior's even when that combination might only ever be used by 'f'.
--
-- A more suggestive example might be:
--
-- > handleMouse <$> bPlayerState <*> bMousePosition <@> eMouseClick :: Event t (GameState -> GameState)
--
(<@>) :: Reflex t => Behavior t (a -> b) -> Event t a -> Event t b
(<@>) b = push $ \x -> do
  f <- sample b
  return . Just . f $ x
infixl 4 <@>

-- | An version of '<@>' that does not use the value of the 'Event'.
--
-- Alternatively, it is 'tag' in operator form.
--
-- This is useful when we want to combine the values of several
-- 'Behavior's at particular points in time using an 'Applicative'
-- style syntax.
--
-- If we have:
--
-- > g  :: a -> b -> d
-- > b1 :: Behavior t a
-- > b2 :: Behavior t b
-- > e  :: Event t c
--
-- where 'e' is firing at the points in time of interest.
--
-- Then we can use '<@':
--
-- > g <$> b1 <*> b2 <@  e :: Event t d
--
-- to combine the values of 'b1' and 'b2' at each of those points of time,
-- with the function 'g' being used to combine the values.
--
-- This is the same as '<@>' except that the 'Event' is being used only
-- to act as a trigger.
(<@) :: (Reflex t) => Behavior t b -> Event t a -> Event t b
(<@) = tag
infixl 4 <@

------------------
-- Adjustable
------------------

-- | A 'Monad' that supports adjustment over time.  After an action has been
-- run, if the given events fire, it will adjust itself so that its net effect
-- is as though it had originally been run with the new value.  Note that there
-- is some issue here with persistent side-effects: obviously, IO (and some
-- other side-effects) cannot be undone, so it is up to the instance implementer
-- to determine what the best meaning for this class is in such cases.
class (Reflex t, Monad m) => Adjustable t m | m -> t where
  runWithReplace :: m a -> Event t (m b) -> m (a, Event t b)
  traverseIntMapWithKeyWithAdjust :: (IntMap.Key -> v -> m v') -> IntMap v -> Event t (PatchIntMap v) -> m (IntMap v', Event t (PatchIntMap v'))
  traverseDMapWithKeyWithAdjust :: GCompare k => (forall a. k a -> v a -> m (v' a)) -> DMap k v -> Event t (PatchDMap k v) -> m (DMap k v', Event t (PatchDMap k v'))
  traverseDMapWithKeyWithAdjustWithMove :: GCompare k => (forall a. k a -> v a -> m (v' a)) -> DMap k v -> Event t (PatchDMapWithMove k v) -> m (DMap k v', Event t (PatchDMapWithMove k v'))

instance Adjustable t m => Adjustable t (ReaderT r m) where
  runWithReplace a0 a' = do
    r <- ask
    lift $ runWithReplace (runReaderT a0 r) $ fmap (`runReaderT` r) a'
  traverseIntMapWithKeyWithAdjust f dm0 dm' = do
    r <- ask
    lift $ traverseIntMapWithKeyWithAdjust (\k v -> runReaderT (f k v) r) dm0 dm'
  traverseDMapWithKeyWithAdjust f dm0 dm' = do
    r <- ask
    lift $ traverseDMapWithKeyWithAdjust (\k v -> runReaderT (f k v) r) dm0 dm'
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = do
    r <- ask
    lift $ traverseDMapWithKeyWithAdjustWithMove (\k v -> runReaderT (f k v) r) dm0 dm'

sequenceDMapWithAdjust :: (GCompare k, Adjustable t m) => DMap k m -> Event t (PatchDMap k m) -> m (DMap k Identity, Event t (PatchDMap k Identity))
sequenceDMapWithAdjust = traverseDMapWithKeyWithAdjust $ \_ -> fmap Identity

sequenceDMapWithAdjustWithMove :: (GCompare k, Adjustable t m) => DMap k m -> Event t (PatchDMapWithMove k m) -> m (DMap k Identity, Event t (PatchDMapWithMove k Identity))
sequenceDMapWithAdjustWithMove = traverseDMapWithKeyWithAdjustWithMove $ \_ -> fmap Identity

mapMapWithAdjustWithMove :: forall t m k v v'. (Adjustable t m, Ord k) => (k -> v -> m v') -> Map k v -> Event t (PatchMapWithMove k v) -> m (Map k v', Event t (PatchMapWithMove k v'))
mapMapWithAdjustWithMove f m0 m' = do
  (out0 :: DMap (Const2 k v) (Constant v'), out') <- traverseDMapWithKeyWithAdjustWithMove (\(Const2 k) (Identity v) -> Constant <$> f k v) (mapToDMap m0) (const2PatchDMapWithMoveWith Identity <$> m')
  return (dmapToMapWith (\(Constant v') -> v') out0, patchDMapWithMoveToPatchMapWithMoveWith (\(Constant v') -> v') <$> out')

------------------
-- Cheap Functions
------------------

{-# INLINE pushAlwaysCheap #-}
pushAlwaysCheap :: Reflex t => (a -> PushM t b) -> Event t a -> Event t b
pushAlwaysCheap f = pushCheap (fmap Just . f)

{-# INLINE fmapMaybeCheap #-}
fmapMaybeCheap :: Reflex t => (a -> Maybe b) -> Event t a -> Event t b
fmapMaybeCheap f = pushCheap $ return . f

{-# INLINE fforMaybeCheap #-}
fforMaybeCheap :: Reflex t => Event t a -> (a -> Maybe b) -> Event t b
fforMaybeCheap = flip fmapMaybeCheap

{-# INLINE fforCheap #-}
fforCheap :: Reflex t => Event t a -> (a -> b) -> Event t b
fforCheap = flip fmapCheap

{-# INLINE fmapCheap #-}
fmapCheap :: Reflex t => (a -> b) -> Event t a -> Event t b
fmapCheap f = pushCheap $ return . Just . f

{-# INLINE tagCheap #-}
tagCheap :: Reflex t => Behavior t b -> Event t a -> Event t b
tagCheap b = pushAlwaysCheap $ \_ -> sample b

{-# INLINE mergeWithCheap #-}
mergeWithCheap :: Reflex t => (a -> a -> a) -> [Event t a] -> Event t a
mergeWithCheap = mergeWithCheap' id

{-# INLINE mergeWithCheap' #-}
mergeWithCheap' :: Reflex t => (a -> b) -> (b -> b -> b) -> [Event t a] -> Event t b
mergeWithCheap' f g = mergeWithFoldCheap' $ foldl1 g . fmap f

{-# INLINE mergeWithFoldCheap' #-}
mergeWithFoldCheap' :: Reflex t => (NonEmpty a -> b) -> [Event t a] -> Event t b
mergeWithFoldCheap' f es =
  fmapCheap (f . (\(h : t) -> h :| t) . IntMap.elems)
  . mergeInt
  . IntMap.fromDistinctAscList
  $ zip [0 :: Int ..] es

--------------------------------------------------------------------------------
-- Deprecated functions
--------------------------------------------------------------------------------

{-# DEPRECATED MonadAdjust "Use Adjustable instead" #-}
type MonadAdjust = Adjustable

-- | Create a new 'Event' that occurs if at least one of the supplied 'Event's
-- occurs. If both occur at the same time they are combined using 'mappend'.
{-# DEPRECATED appendEvents "If a 'Semigroup a' instance is available, use 'mappend'; otherwise, use 'alignWith (mergeThese mappend)' instead" #-}
appendEvents :: (Reflex t, Monoid a) => Event t a -> Event t a -> Event t a
appendEvents = alignWith $ mergeThese mappend

-- | Alias for 'headE'
{-# DEPRECATED onceE "Use 'headE' instead" #-}
onceE :: MonadHold t m => Event t a -> m (Event t a)
onceE = headE

-- | Run both sides of a 'These' monadically, combining the results.
{-# DEPRECATED sequenceThese "Use bisequenceA or bisequence from the bifunctors package instead" #-}
#ifdef USE_TEMPLATE_HASKELL
{-# ANN sequenceThese "HLint: ignore Use fmap" #-}
#endif
sequenceThese :: Monad m => These (m a) (m b) -> m (These a b)
sequenceThese t = case t of
  This ma -> fmap This ma
  These ma mb -> liftM2 These ma mb
  That mb -> fmap That mb

{-# DEPRECATED switchPromptly "Use 'switchHoldPromptly' instead. The 'switchHold*' naming convention was chosen because those functions are more closely related to each other than they are to 'switch'. " #-}
switchPromptly :: (Reflex t, MonadHold t m) => Event t a -> Event t (Event t a) -> m (Event t a)
switchPromptly = switchHoldPromptly
{-# DEPRECATED switchPromptOnly "Use 'switchHoldPromptOnly' instead. The 'switchHold*' naming convention was chosen because those functions are more closely related to each other than they are to 'switch'. " #-}
switchPromptOnly :: (Reflex t, MonadHold t m) => Event t a -> Event t (Event t a) -> m (Event t a)
switchPromptOnly = switchHoldPromptOnly
