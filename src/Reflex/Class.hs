{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Reflex.Class where

import Control.Applicative
import Control.Monad.Identity hiding (forM, forM_, mapM, mapM_, sequence, sequence_)
import Control.Monad.Reader hiding (forM, forM_, mapM, mapM_, sequence, sequence_)
import Control.Monad.State.Strict hiding (forM, forM_, mapM, mapM_, sequence, sequence_)
import Control.Monad.Trans.Cont (ContT ())
import Control.Monad.Trans.Except (ExceptT ())
import Control.Monad.Trans.RWS (RWST ())
import Control.Monad.Trans.Writer (WriterT ())
import Data.Align
import Data.Bifunctor
import Data.Dependent.Map (DMap, DSum (..), GCompare (..), GOrdering (..))
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (ShowTag (..))
import Data.Foldable
import Data.Functor.Compose
import Data.Functor.Constant
import Data.Functor.Misc
import Data.GADT.Compare ((:~:) (..), GEq (..))
import Data.GADT.Show (GShow (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import Data.Semigroup (Semigroup, sconcat, stimes, stimesIdempotentMonoid, (<>))
import Data.These
import Data.Traversable

-- Note: must come last to silence warnings due to AMP on GHC < 7.10
import Prelude hiding (foldl, mapM, mapM_, sequence, sequence_)

import Debug.Trace (trace)

class (MonadHold t (PushM t), MonadSample t (PullM t), MonadFix (PushM t), Functor (Event t), Functor (Behavior t), Functor (Dynamic t), Monad (Dynamic t)) => Reflex t where
  -- | A container for a value that can change over time.  Behaviors can be sampled at will, but it is not possible to be notified when they change
  data Behavior t :: * -> *
  -- | A stream of occurrences.  During any given frame, an Event is either occurring or not occurring; if it is occurring, it will contain a value of the given type (its "occurrence type")
  data Event t :: * -> *
  -- | A container for a value that can change over time and allows notifications on changes.
  -- Basically a combination of a 'Behavior' and an 'Event', with a rule that the Behavior will
  -- change if and only if the Event fires.
  data Dynamic t :: * -> *
  -- | A container for a value that can change over time and allows notifications on changes.
  -- Basically a combination of a 'Behavior' and an 'Event', with a rule that the Behavior will
  -- change if and only if the Event fires.
  data Incremental t :: (* -> *) -> * -> *
  -- | An Event with no occurrences
  never :: Event t a
  -- | Create a Behavior that always has the given value
  constant :: a -> Behavior t a --TODO: Refactor to use 'pure' from Applicative instead; however, we need to make sure that encouraging Applicative-style use of Behaviors doesn't have a negative performance impact
  -- | Create an Event from another Event; the provided function can sample Behaviors and hold Events, and use the results to produce a occurring (Just) or non-occurring (Nothing) result
  push :: (a -> PushM t (Maybe b)) -> Event t a -> Event t b
  -- | A monad for doing complex push-based calculations efficiently
  type PushM t :: * -> *
  -- | Create a Behavior by reading from other Behaviors; the result will be recomputed whenever any of the read Behaviors changes
  pull :: PullM t a -> Behavior t a
  -- | A monad for doing complex pull-based calculations efficiently
  type PullM t :: * -> *
  -- | Merge a collection of events; the resulting Event will only occur if at least one input event is occuring, and will contain all of the input keys that are occurring simultaneously
  merge :: GCompare k => DMap k (Event t) -> Event t (DMap k Identity) --TODO: Generalize to get rid of DMap use --TODO: Provide a type-level guarantee that the result is not empty
  -- | Efficiently fan-out an event to many destinations.  This function should be partially applied, and then the result applied repeatedly to create child events
  fan :: GCompare k => Event t (DMap k Identity) -> EventSelector t k --TODO: Can we help enforce the partial application discipline here?  The combinator is worthless without it
  -- | Create an Event that will occur whenever the currently-selected input Event occurs
  switch :: Behavior t (Event t a) -> Event t a
  -- | Create an Event that will occur whenever the input event is occurring and its occurrence value, another Event, is also occurring
  coincidence :: Event t (Event t a) -> Event t a
  -- | Extract the 'Behavior' of a 'Dynamic'.
  current :: Dynamic t a -> Behavior t a
  -- | Extract the 'Event' of the 'Dynamic'.
  updated :: Dynamic t a -> Event t a
  -- | Create a new 'Dynamic'.  The given PullM must always return the most recent firing of the given Event, if any.
  unsafeBuildDynamic :: PullM t a -> Event t a -> Dynamic t a
  -- | Create a new 'Incremental'.  The given PullM's value must always change in the same way that the accumulated application of patches would change that value.
  unsafeBuildIncremental :: Patch p => PullM t a -> Event t (p a) -> Incremental t p a
  -- | Create a merge whose parents can change over time
  mergeIncremental :: GCompare k => Incremental t PatchDMap (DMap k (Event t)) -> Event t (DMap k Identity)
  -- | Extract the 'Behavior' component of an 'Incremental'
  currentIncremental :: Patch p => Incremental t p a -> Behavior t a
  -- | Extract the 'Event' component of an 'Incremental'
  updatedIncremental :: Patch p => Incremental t p a -> Event t (p a)
  -- | Convert an 'Incremental' to a 'Dynamic'
  incrementalToDynamic :: Patch p => Incremental t p a -> Dynamic t a

class Patch p where
  apply :: p a -> a -> Maybe a -- Return Nothing if no change is necessary

instance Patch Identity where
  apply (Identity a) _ = Just a

data PatchDMap a where
  PatchDMap :: GCompare k => DMap k (Compose Maybe v) -> PatchDMap (DMap k v)

instance GCompare k => Semigroup (PatchDMap (DMap k v)) where
  PatchDMap a <> PatchDMap b = PatchDMap $ a `mappend` b --TODO: Add a semigroup instance for DMap
  -- PatchDMap is idempotent, so stimes n is id for every n
#if MIN_VERSION_semigroups(0,17,0)
  stimes = stimesIdempotentMonoid
#else
  times1p n x = case compare n 0 of
    LT -> error "stimesIdempotentMonoid: negative multiplier"
    EQ -> mempty
    GT -> x
#endif

instance GCompare k => Monoid (PatchDMap (DMap k v)) where
  mempty = PatchDMap mempty
  mappend = (<>)

instance Patch PatchDMap where
  apply (PatchDMap diff) old = Just $! insertions `DMap.union` (old `DMap.difference` deletions) --TODO: return Nothing sometimes --Note: the strict application here is critical to ensuring that incremental merges don't hold onto all their prerequisite events forever; can we make this more robust?
    where insertions = DMap.mapMaybeWithKey (const $ getCompose) diff
          deletions = DMap.mapMaybeWithKey (const $ nothingToJust . getCompose) diff
          nothingToJust = \case
            Nothing -> Just $ Constant ()
            Just _ -> Nothing

unsafeDynamic :: Reflex t => Behavior t a -> Event t a -> Dynamic t a
unsafeDynamic = unsafeBuildDynamic . sample

constDyn :: Reflex t => a -> Dynamic t a
constDyn = pure

class (Applicative m, Monad m) => MonadSample t m | m -> t where
  -- | Get the current value in the Behavior
  sample :: Behavior t a -> m a

class MonadSample t m => MonadHold t m where
  -- | Create a new Behavior whose value will initially be equal to the given value and will be updated whenever the given Event occurs.  The update takes effect immediately after the Event occurs; if the occurrence that sets the Behavior (or one that is simultaneous with it) is used to sample the Behavior, it will see the *old* value of the Behavior, not the new one.
  hold :: a -> Event t a -> m (Behavior t a)
  -- | Create a 'Dynamic' value using the given initial value that changes every
  -- time the 'Event' occurs.
  holdDyn :: a -> Event t a -> m (Dynamic t a)
  -- | Create an 'Incremental' value using the given initial value that changes every
  -- time the 'Event' occurs.
  holdIncremental :: Patch p => a -> Event t (p a) -> m (Incremental t p a)

newtype EventSelector t k = EventSelector { select :: forall a. k a -> Event t a }

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance MonadSample t m => MonadSample t (ReaderT r m) where
  sample = lift . sample

instance MonadHold t m => MonadHold t (ReaderT r m) where
  hold a0 = lift . hold a0
  holdDyn a0 = lift . holdDyn a0
  holdIncremental a0 = lift . holdIncremental a0

instance (MonadSample t m, Monoid r) => MonadSample t (WriterT r m) where
  sample = lift . sample

instance (MonadHold t m, Monoid r) => MonadHold t (WriterT r m) where
  hold a0 = lift . hold a0
  holdDyn a0 = lift . holdDyn a0
  holdIncremental a0 = lift . holdIncremental a0

instance MonadSample t m => MonadSample t (StateT s m) where
  sample = lift . sample

instance MonadHold t m => MonadHold t (StateT s m) where
  hold a0 = lift . hold a0
  holdDyn a0 = lift . holdDyn a0
  holdIncremental a0 = lift . holdIncremental a0

instance MonadSample t m => MonadSample t (ExceptT e m) where
  sample = lift . sample

instance MonadHold t m => MonadHold t (ExceptT e m) where
  hold a0 = lift . hold a0
  holdDyn a0 = lift . holdDyn a0
  holdIncremental a0 = lift . holdIncremental a0

instance (MonadSample t m, Monoid w) => MonadSample t (RWST r w s m) where
  sample = lift . sample

instance (MonadHold t m, Monoid w) => MonadHold t (RWST r w s m) where
  hold a0 = lift . hold a0
  holdDyn a0 = lift . holdDyn a0
  holdIncremental a0 = lift . holdIncremental a0

instance MonadSample t m => MonadSample t (ContT r m) where
  sample = lift . sample

instance MonadHold t m => MonadHold t (ContT r m) where
  hold a0 = lift . hold a0
  holdDyn a0 = lift . holdDyn a0
  holdIncremental a0 = lift . holdIncremental a0

--------------------------------------------------------------------------------
-- Convenience functions
--------------------------------------------------------------------------------

-- | Create an Event from another Event.
-- The provided function can sample 'Behavior's and hold 'Event's.
pushAlways :: Reflex t => (a -> PushM t b) -> Event t a -> Event t b
pushAlways f = push (fmap Just . f)

-- | Flipped version of 'fmap'.
ffor :: Functor f => f a -> (a -> b) -> f b
ffor = flip fmap

instance Reflex t => Functor (Behavior t) where
  fmap f = pull . fmap f . sample

instance Reflex t => Applicative (Behavior t) where
  pure = constant
  f <*> x = pull $ sample f `ap` sample x
  _ *> b = b
  a <* _ = a

instance Reflex t => Monad (Behavior t) where
  a >>= f = pull $ sample a >>= sample . f
  -- Note: it is tempting to write (_ >> b = b); however, this would result in (fail x >> return y) succeeding (returning y), which violates the law that (a >> b = a >>= \_ -> b), since the implementation of (>>=) above actually will fail.  Since we can't examine Behaviors other than by using sample, I don't think it's possible to write (>>) to be more efficient than the (>>=) above.
  return = constant
  fail = error "Monad (Behavior t) does not support fail"

instance (Reflex t, Semigroup a) => Semigroup (Behavior t a) where
  a <> b = pull $ liftM2 (<>) (sample a) (sample b)
  sconcat = pull . fmap sconcat . mapM sample
#if MIN_VERSION_semigroups(0,17,0)
  stimes n = fmap $ stimes n
#else
  times1p n = fmap $ times1p n
#endif

instance (Reflex t, Monoid a) => Monoid (Behavior t a) where
  mempty = constant mempty
  mappend a b = pull $ liftM2 mappend (sample a) (sample b)
  mconcat = pull . fmap mconcat . mapM sample

--TODO: See if there's a better class in the standard libraries already
-- | A class for values that combines filtering and mapping using 'Maybe'.
class FunctorMaybe f where
  -- | Combined mapping and filtering function.
  fmapMaybe :: (a -> Maybe b) -> f a -> f b

instance FunctorMaybe [] where
  fmapMaybe f = catMaybes . fmap f

-- | Flipped version of 'fmapMaybe'.
fforMaybe :: FunctorMaybe f => f a -> (a -> Maybe b) -> f b
fforMaybe = flip fmapMaybe

-- | Filter 'f a' using the provided predicate.
-- Relies on 'fforMaybe'.
ffilter :: FunctorMaybe f => (a -> Bool) -> f a -> f a
ffilter f = fmapMaybe $ \x -> if f x then Just x else Nothing

instance Reflex t => FunctorMaybe (Event t) where
  fmapMaybe f = push $ return . f

instance Reflex t => Functor (Event t) where
  fmap f = fmapMaybe $ Just . f

-- | Replace each occurrence value of the 'Event' with the value of the
-- 'Behavior' at the time of that occurrence.
tag :: Reflex t => Behavior t b -> Event t a -> Event t b
tag b = pushAlways $ \_ -> sample b

-- | Create a new 'Event' that combines occurences of supplied 'Event'
-- with the current value of the 'Behavior'.
attach :: Reflex t => Behavior t a -> Event t b -> Event t (a, b)
attach = attachWith (,)

-- | Create a new 'Event' that occurs when the supplied 'Event' occurs
-- by combining it with the current value of the 'Behavior'.
attachWith :: Reflex t => (a -> b -> c) -> Behavior t a -> Event t b -> Event t c
attachWith f = attachWithMaybe $ \a b -> Just $ f a b

-- | Create a new 'Event' by combining each occurence with the current
-- value of the 'Behavior'. The occurrence is discarded if the combining function
-- returns Nothing
attachWithMaybe :: Reflex t => (a -> b -> Maybe c) -> Behavior t a -> Event t b -> Event t c
attachWithMaybe f b e = flip push e $ \o -> (`f` o) <$> sample b

-- | Alias for 'headE'
onceE :: (Reflex t, MonadHold t m, MonadFix m) => Event t a -> m (Event t a)
onceE = headE

-- | Create a new 'Event' that only occurs on the first occurence of
-- the supplied 'Event'.
headE :: (Reflex t, MonadHold t m, MonadFix m) => Event t a -> m (Event t a)
headE e = do
  rec be <- hold e $ fmap (const never) e'
      let e' = switch be
      e' `seq` return ()
  return e'

-- | Create a new 'Event' that occurs on all but the first occurence
-- of the supplied 'Event'.
tailE :: (Reflex t, MonadHold t m, MonadFix m) => Event t a -> m (Event t a)
tailE e = snd <$> headTailE e

-- | Create a tuple of two 'Event's with the first one occuring only
-- the first time the supplied 'Event' occurs and the second occuring
-- on all but the first occurence.
headTailE :: (Reflex t, MonadHold t m, MonadFix m) => Event t a -> m (Event t a, Event t a)
headTailE e = do
  eHead <- headE e
  be <- hold never $ fmap (const e) eHead
  return (eHead, switch be)

-- | Split the supplied 'Event' into two individual 'Event's occuring
-- at the same time with the respective values from the tuple.
splitE :: Reflex t => Event t (a, b) -> (Event t a, Event t b)
splitE e = (fmap fst e, fmap snd e)

-- | Print the supplied 'String' and the value of the 'Event' on each
-- occurence. This should /only/ be used for debugging.
--
-- Note: As with Debug.Trace.trace, the message will only be printed if
-- the 'Event' is actually used.
traceEvent :: (Reflex t, Show a) => String -> Event t a -> Event t a
traceEvent s = traceEventWith $ \x -> s <> ": " <> show x

-- | Print the output of the supplied function on each occurence of
-- the 'Event'. This should /only/ be used for debugging.
--
-- Note: As with Debug.Trace.trace, the message will only be printed if
-- the 'Event' is actually used.
traceEventWith :: Reflex t => (a -> String) -> Event t a -> Event t a
traceEventWith f = push $ \x -> trace (f x) $ return $ Just x

-- | Tag type for 'Either' to use it as a 'DSum'.
data EitherTag l r a where
  LeftTag :: EitherTag l r l
  RightTag :: EitherTag l r r

instance GEq (EitherTag l r) where
  geq a b = case (a, b) of
    (LeftTag, LeftTag) -> Just Refl
    (RightTag, RightTag) -> Just Refl
    _ -> Nothing

instance GCompare (EitherTag l r) where
  gcompare a b = case (a, b) of
    (LeftTag, LeftTag) -> GEQ
    (LeftTag, RightTag) -> GLT
    (RightTag, LeftTag) -> GGT
    (RightTag, RightTag) -> GEQ

instance GShow (EitherTag l r) where
  gshowsPrec _ a = case a of
    LeftTag -> showString "LeftTag"
    RightTag -> showString "RightTag"

instance (Show l, Show r) => ShowTag (EitherTag l r) Identity where
  showTaggedPrec t n (Identity a) = case t of
    LeftTag -> showsPrec n a
    RightTag -> showsPrec n a

-- | Convert 'Either' to a 'DSum'. Inverse of 'dsumToEither'.
eitherToDSum :: Either a b -> DSum (EitherTag a b) Identity
eitherToDSum = \case
  Left a -> (LeftTag :=> Identity a)
  Right b -> (RightTag :=> Identity b)

-- | Convert 'DSum' to 'Either'. Inverse of 'eitherToDSum'.
dsumToEither :: DSum (EitherTag a b) Identity -> Either a b
dsumToEither = \case
  (LeftTag :=> Identity a) -> Left a
  (RightTag :=> Identity b) -> Right b

-- | Extract the values of a 'DMap' of 'EitherTag's.
dmapToThese :: DMap (EitherTag a b) Identity -> Maybe (These a b)
dmapToThese m = case (DMap.lookup LeftTag m, DMap.lookup RightTag m) of
  (Nothing, Nothing) -> Nothing
  (Just (Identity a), Nothing) -> Just $ This a
  (Nothing, Just (Identity b)) -> Just $ That b
  (Just (Identity a), Just (Identity b)) -> Just $ These a b

-- | Create a new 'Event' that occurs if at least one of the supplied
-- 'Event's occurs. If both occur at the same time they are combined
-- using 'mappend'.
appendEvents :: (Reflex t, Monoid a) => Event t a -> Event t a -> Event t a
appendEvents e1 e2 = mergeThese mappend <$> align e1 e2

{-# DEPRECATED sequenceThese "Use bisequenceA or bisequence from the bifunctors package instead" #-}
sequenceThese :: Monad m => These (m a) (m b) -> m (These a b)
sequenceThese t = case t of
  This ma -> fmap This ma
  These ma mb -> liftM2 These ma mb
  That mb -> fmap That mb

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

-- | Create a new 'Event' that occurs if at least one of the 'Event's
-- in the list occurs. If multiple occur at the same time they are
-- folded from the left with the given function.
mergeWith :: Reflex t => (a -> a -> a) -> [Event t a] -> Event t a
mergeWith f es = fmap (Prelude.foldl1 f . map (\(Const2 _ :=> Identity v) -> v) . DMap.toList)
               . merge
               . DMap.fromDistinctAscList
               . map (\(k, v) -> Const2 k :=> v)
               $ zip [0 :: Int ..] es

-- | Create a new 'Event' that occurs if at least one of the 'Event's
-- in the list occurs. If multiple occur at the same time the value is
-- the value of the leftmost event.
leftmost :: Reflex t => [Event t a] -> Event t a
leftmost = mergeWith const

-- | Create a new 'Event' that occurs if at least one of the 'Event's
-- in the list occurs and has a list of the values of all 'Event's
-- occuring at that time.
mergeList :: Reflex t => [Event t a] -> Event t (NonEmpty a)
mergeList [] = never
mergeList es = mergeWith (<>) $ map (fmap (:|[])) es

-- | Create a new 'Event' combining the map of 'Event's into an
-- 'Event' that occurs if at least one of them occurs and has a map of
-- values of all 'Event's occuring at that time.
mergeMap :: (Reflex t, Ord k) => Map k (Event t a) -> Event t (Map k a)
mergeMap = fmap dmapToMap . merge . mapWithFunctorToDMap

-- | Split the event into an 'EventSelector' that allows efficient
-- selection of the individual 'Event's.
fanMap :: (Reflex t, Ord k) => Event t (Map k a) -> EventSelector t (Const2 k a)
fanMap = fan . fmap mapToDMap

-- | Switches to the new event whenever it receives one; the new event is used immediately, on the same frame that it is switched to
switchPromptly :: forall t m a. (Reflex t, MonadHold t m) => Event t a -> Event t (Event t a) -> m (Event t a)
switchPromptly ea0 eea = do
  bea <- hold ea0 eea
  let eLag = switch bea
      eCoincidences = coincidence eea
  return $ leftmost [eCoincidences, eLag]

instance Reflex t => Align (Event t) where
  nil = never
  align ea eb = fmapMaybe dmapToThese $ merge $ DMap.fromList [LeftTag :=> ea, RightTag :=> eb]

-- | Create a new 'Event' that only occurs if the supplied 'Event'
-- occurs and the 'Behavior' is true at the time of occurence.
gate :: Reflex t => Behavior t Bool -> Event t a -> Event t a
gate = attachWithMaybe $ \allow a -> if allow then Just a else Nothing

-- | Create a new behavior given a starting behavior and switch to a the
--   behvior carried by the event when it fires.
switcher :: (Reflex t, MonadHold t m)
        => Behavior t a -> Event t (Behavior t a) -> m (Behavior t a)
switcher b eb = pull . (sample <=< sample) <$> hold b eb

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

instance (Reflex t, Monoid a) => Monoid (Dynamic t a) where
  mconcat = fmap (mconcat . map (\(Const2 _ :=> Identity v) -> v) . DMap.toList) . distributeDMapOverDynPure . DMap.fromList . map (\(k, v) -> Const2 k :=> v) . zip [0 :: Int ..]
  mempty = constDyn mempty
  mappend = zipDynWith mappend

distributeDMapOverDynPure :: forall t k. (Reflex t, GCompare k) => DMap k (Dynamic t) -> Dynamic t (DMap k Identity)
distributeDMapOverDynPure dm = case DMap.toList dm of
  [] -> constDyn DMap.empty
  [k :=> v] -> fmap (DMap.singleton k . Identity) v
  _ ->
    let getInitial = DMap.traverseWithKey (\_ -> fmap Identity . sample . current) dm
        edmPre = merge $ rewrapDMap updated dm
        result = unsafeBuildDynamic getInitial $ flip pushAlways edmPre $ \news -> do
          olds <- sample $ current result
          return $ DMap.unionWithKey (\_ _ new -> new) olds news
    in result

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

mapAccum_ :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> (a, c)) -> a -> Event t b -> m (Event t c)
mapAccum_ f z e = do
  (_ :: Behavior t a, result) <- mapAccum f z e
  return result

mapAccumMaybe_ :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> (Maybe a, Maybe c)) -> a -> Event t b -> m (Event t c)
mapAccumMaybe_ f z e = do
  (_ :: Behavior t a, result) <- mapAccumMaybe f z e
  return result

mapAccumM_ :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> PushM t (a, c)) -> a -> Event t b -> m (Event t c)
mapAccumM_ f z e = do
  (_ :: Behavior t a, result) <- mapAccumM f z e
  return result

mapAccumMaybeM_ :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> PushM t (Maybe a, Maybe c)) -> a -> Event t b -> m (Event t c)
mapAccumMaybeM_ f z e = do
  (_ :: Behavior t a, result) <- mapAccumMaybeM f z e
  return result

instance Reflex t => Accumulator t (Dynamic t) where
  accumMaybeM f z e = do
    rec let e' = flip push e $ \o -> do
              v <- sample $ current d'
              f v o
        d' <- holdDyn z e'
    return d'
  mapAccumMaybeM f z e = do
    rec let e' = flip push e $ \o -> do
              v <- sample $ current d'
              result <- f v o
              return $ case result of
                (Nothing, Nothing) -> Nothing
                _ -> Just result
        d' <- holdDyn z $ fmapMaybe fst e'
    return (d', fmapMaybe snd e')

instance Reflex t => Accumulator t (Behavior t) where
  accumMaybeM f z e = current <$> accumMaybeM f z e
  mapAccumMaybeM f z e = first current <$> mapAccumMaybeM f z e

instance Reflex t => Accumulator t (Event t) where
  accumMaybeM f z e = updated <$> accumMaybeM f z e
  mapAccumMaybeM f z e = first updated <$> mapAccumMaybeM f z e

-- | Create a new 'Event' by combining each occurence with the next value
-- of the list using the supplied function. If the list runs out of items,
-- all subsequent 'Event' occurrences will be ignored.
zipListWithEvent :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> c) -> [a] -> Event t b -> m (Event t c)
zipListWithEvent f l e = do
  let f' a b = case a of
        h:t -> (Just t, Just $ f h b)
        _ -> (Nothing, Nothing) --TODO: Unsubscribe the event?
  mapAccumMaybe_ f' l e
