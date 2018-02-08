{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, GADTs, ScopedTypeVariables, FunctionalDependencies, RecursiveDo, UndecidableInstances, GeneralizedNewtypeDeriving, StandaloneDeriving, EmptyDataDecls, NoMonomorphismRestriction, TypeOperators, DeriveDataTypeable, PackageImports, TemplateHaskell, LambdaCase, CPP #-}
module Reflex.Class where

import Control.Applicative
import Control.Monad.Identity hiding (mapM, mapM_, forM, forM_, sequence, sequence_)
import Control.Monad.State.Strict hiding (mapM, mapM_, forM, forM_, sequence, sequence_)
import Control.Monad.Reader hiding (mapM, mapM_, forM, forM_, sequence, sequence_)
import Control.Monad.Trans.Writer (WriterT())
import Control.Monad.Trans.Except (ExceptT())
import Control.Monad.Trans.Cont (ContT())
import Control.Monad.Trans.RWS (RWST())
import Data.List.NonEmpty (NonEmpty (..))
import Data.These
import Data.Align
import Data.GADT.Compare (GEq (..), (:~:) (..))
import Data.GADT.Show (GShow (..))
import Data.Dependent.Sum (ShowTag (..))
import Data.Map (Map)
import Data.Dependent.Map (DMap, DSum (..), GCompare (..), GOrdering (..))
import qualified Data.Dependent.Map as DMap
import Data.Functor.Misc
import Data.Semigroup
import Data.Traversable

-- Note: must come last to silence warnings due to AMP on GHC < 7.10
import Prelude hiding (mapM, mapM_, sequence, sequence_, foldl)

import Debug.Trace (trace)

class (MonadHold t (PushM t), MonadSample t (PullM t), MonadFix (PushM t), Functor (Event t), Functor (Behavior t)) => Reflex t where
  -- | A container for a value that can change over time.  Behaviors can be sampled at will, but it is not possible to be notified when they change
  data Behavior t :: * -> *
  -- | A stream of occurrences.  During any given frame, an Event is either occurring or not occurring; if it is occurring, it will contain a value of the given type (its "occurrence type")
  data Event t :: * -> *
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
  merge :: GCompare k => DMap (WrapArg (Event t) k) -> Event t (DMap k) --TODO: Generalize to get rid of DMap use --TODO: Provide a type-level guarantee that the result is not empty
  -- | Efficiently fan-out an event to many destinations.  This function should be partially applied, and then the result applied repeatedly to create child events
  fan :: GCompare k => Event t (DMap k) -> EventSelector t k --TODO: Can we help enforce the partial application discipline here?  The combinator is worthless without it
  -- | Create an Event that will occur whenever the currently-selected input Event occurs
  switch :: Behavior t (Event t a) -> Event t a
  -- | Create an Event that will occur whenever the input event is occurring and its occurrence value, another Event, is also occurring
  coincidence :: Event t (Event t a) -> Event t a

class (Applicative m, Monad m) => MonadSample t m | m -> t where
  -- | Get the current value in the Behavior
  sample :: Behavior t a -> m a

class MonadSample t m => MonadHold t m where
  -- | Create a new Behavior whose value will initially be equal to the given value and will be updated whenever the given Event occurs
  hold :: a -> Event t a -> m (Behavior t a)

newtype EventSelector t k = EventSelector { select :: forall a. k a -> Event t a }

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance MonadSample t m => MonadSample t (ReaderT r m) where
  sample = lift . sample

instance MonadHold t m => MonadHold t (ReaderT r m) where
  hold a0 = lift . hold a0

instance (MonadSample t m, Monoid r) => MonadSample t (WriterT r m) where
  sample = lift . sample

instance (MonadHold t m, Monoid r) => MonadHold t (WriterT r m) where
  hold a0 = lift . hold a0

instance MonadSample t m => MonadSample t (StateT s m) where
  sample = lift . sample

instance MonadHold t m => MonadHold t (StateT s m) where
  hold a0 = lift . hold a0

instance MonadSample t m => MonadSample t (ExceptT e m) where
  sample = lift . sample

instance MonadHold t m => MonadHold t (ExceptT e m) where
  hold a0 = lift . hold a0

instance (MonadSample t m, Monoid w) => MonadSample t (RWST r w s m) where
  sample = lift . sample

instance (MonadHold t m, Monoid w) => MonadHold t (RWST r w s m) where
  hold a0 = lift . hold a0

instance MonadSample t m => MonadSample t (ContT r m) where
  sample = lift . sample

instance MonadHold t m => MonadHold t (ContT r m) where
  hold a0 = lift . hold a0

--------------------------------------------------------------------------------
-- Convenience functions
--------------------------------------------------------------------------------

-- | Create an Event from another Event.
-- The provided function can sample 'Behavior's and hold 'Event's.
pushAlways :: Reflex t => (a -> PushM t b) -> Event t a -> Event t b
pushAlways f = push (liftM Just . f)

-- | Flipped version of 'fmap'.
ffor :: Functor f => f a -> (a -> b) -> f b
ffor = flip fmap

instance Reflex t => Functor (Behavior t) where
  fmap f = pull . liftM f . sample

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
  sconcat = pull . liftM sconcat . mapM sample
#if MIN_VERSION_semigroups(0,17,0)
  stimes n = fmap $ stimes n
#else
  times1p n = fmap $ times1p n
#endif

instance (Reflex t, Monoid a) => Monoid (Behavior t a) where
  mempty = constant mempty
  mappend a b = pull $ liftM2 mappend (sample a) (sample b)
  mconcat = pull . liftM mconcat . mapM sample

--TODO: See if there's a better class in the standard libraries already
-- | A class for values that combines filtering and mapping using 'Maybe'.
class FunctorMaybe f where
  -- | Combined mapping and filtering function.
  fmapMaybe :: (a -> Maybe b) -> f a -> f b

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

-- | Create a new 'Event' by combining each occurence with the next value
-- of the list using the supplied function. If the list runs out of items,
-- all subsequent 'Event' occurrences will be ignored.
zipListWithEvent :: (Reflex t, MonadHold t m, MonadFix m) => (a -> b -> c) -> [a] -> Event t b -> m (Event t c)
zipListWithEvent f l e = do
  rec lb <- hold l eTail
      let eBoth = flip push e $ \o -> do
            l' <- sample lb
            return $ case l' of
              (h : t) -> Just (f h o, t)
              [] -> Nothing
      let eTail = fmap snd eBoth
      lb `seq` eBoth `seq` eTail `seq` return ()
  return $ fmap fst eBoth

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
attachWithMaybe f b e = flip push e $ \o -> liftM (flip f o) $ sample b

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
tailE e = liftM snd $ headTailE e

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

instance (Show l, Show r) => ShowTag (EitherTag l r) where
  showTaggedPrec t n a = case t of
    LeftTag -> showsPrec n a
    RightTag -> showsPrec n a

-- | Convert 'Either' to a 'DSum'. Inverse of 'dsumToEither'.
eitherToDSum :: Either a b -> DSum (EitherTag a b)
eitherToDSum = \case
  Left a -> LeftTag :=> a
  Right b -> RightTag :=> b

-- | Convert 'DSum' to 'Either'. Inverse of 'eitherToDSum'.
dsumToEither :: DSum (EitherTag a b) -> Either a b
dsumToEither = \case
  LeftTag :=> a -> Left a
  RightTag :=> b -> Right b

-- | Extract the values of a 'DMap' of 'EitherTag's.
dmapToThese :: DMap (EitherTag a b) -> Maybe (These a b)
dmapToThese m = case (DMap.lookup LeftTag m, DMap.lookup RightTag m) of
  (Nothing, Nothing) -> Nothing
  (Just a, Nothing) -> Just $ This a
  (Nothing, Just b) -> Just $ That b
  (Just a, Just b) -> Just $ These a b

-- | Create a new 'Event' that occurs if at least one of the supplied
-- 'Event's occurs. If both occur at the same time they are combined
-- using 'mappend'.
appendEvents :: (Reflex t, Monoid a) => Event t a -> Event t a -> Event t a
appendEvents e1 e2 = mergeThese mappend <$> align e1 e2

{-# DEPRECATED sequenceThese "Use bisequenceA or bisequence from the bifunctors package instead" #-}
sequenceThese :: Monad m => These (m a) (m b) -> m (These a b)
sequenceThese t = case t of
  This ma -> liftM This ma
  These ma mb -> liftM2 These ma mb
  That mb -> liftM That mb

instance (Semigroup a, Reflex t) => Monoid (Event t a) where
  mempty = never
  mappend a b = mconcat [a, b]
  mconcat = fmap sconcat . mergeList

-- | Create a new 'Event' that occurs if at least one of the 'Event's
-- in the list occurs. If multiple occur at the same time they are
-- folded from the left with the given function.
mergeWith :: Reflex t => (a -> a -> a) -> [Event t a] -> Event t a
mergeWith f es = fmap (Prelude.foldl1 f . map (\(Const2 _ :=> v) -> v) . DMap.toList) $ merge $ DMap.fromList $ map (\(k, v) -> WrapArg (Const2 k) :=> v) $ zip [0 :: Int ..] es

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
  align ea eb = fmapMaybe dmapToThese $ merge $ DMap.fromList [WrapArg LeftTag :=> ea, WrapArg RightTag :=> eb]

-- | Create a new 'Event' that only occurs if the supplied 'Event'
-- occurs and the 'Behavior' is true at the time of occurence.
gate :: Reflex t => Behavior t Bool -> Event t a -> Event t a
gate = attachWithMaybe $ \allow a -> if allow then Just a else Nothing

-- | Create a new behavior given a starting behavior and switch to the
--   behavior carried by the event when it fires.
switcher :: (Reflex t, MonadHold t m)
        => Behavior t a -> Event t (Behavior t a) -> m (Behavior t a)
switcher b eb = pull . (sample <=< sample) <$> hold b eb
