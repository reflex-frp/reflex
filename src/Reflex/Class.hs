{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, RankNTypes, GADTs, ScopedTypeVariables, FunctionalDependencies, RecursiveDo, UndecidableInstances, GeneralizedNewtypeDeriving, StandaloneDeriving, EmptyDataDecls, NoMonomorphismRestriction, TypeOperators, DeriveDataTypeable, PackageImports, TemplateHaskell, LambdaCase #-}
module Reflex.Class where

import Prelude hiding (mapM, mapM_, sequence, sequence_, foldl)

import Control.Monad.Identity hiding (mapM, mapM_, forM, forM_, sequence, sequence_)
import Control.Monad.State.Strict hiding (mapM, mapM_, forM, forM_, sequence, sequence_)
import Control.Monad.Reader hiding (mapM, mapM_, forM, forM_, sequence, sequence_)
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

import Debug.Trace (trace)

class (MonadHold t (PushM t), MonadSample t (PullM t), Functor (Event t), Functor (Behavior t)) => Reflex t where
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

class Monad m => MonadSample t m | m -> t where
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

--------------------------------------------------------------------------------
-- Convenience functions
--------------------------------------------------------------------------------

pushAlways :: Reflex t => (a -> PushM t b) -> Event t a -> Event t b
pushAlways f e = push (liftM Just . f) e

ffor :: Functor f => f a -> (a -> b) -> f b
ffor = flip fmap

instance Reflex t => Functor (Behavior t) where
  fmap f = pull . liftM f . sample

--TODO: See if there's a better class in the standard libraries already
class FunctorMaybe f where
  fmapMaybe :: (a -> Maybe b) -> f a -> f b

fforMaybe :: FunctorMaybe f => f a -> (a -> Maybe b) -> f b
fforMaybe = flip fmapMaybe

ffilter :: FunctorMaybe f => (a -> Bool) -> f a -> f a
ffilter f = fmapMaybe $ \x -> if f x then Just x else Nothing

instance Reflex t => FunctorMaybe (Event t) where
  fmapMaybe f = push $ return . f

instance Reflex t => Functor (Event t) where
  fmap f = fmapMaybe $ Just . f

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

-- | Replace the occurrence value of the Event with the value of the Behavior at the time of the occurrence
tag :: Reflex t => Behavior t b -> Event t a -> Event t b
tag b = pushAlways $ \_ -> sample b

attachWithMaybe :: Reflex t => (a -> b -> Maybe c) -> Behavior t a -> Event t b -> Event t c
attachWithMaybe f b e = flip push e $ \o -> liftM (flip f o) $ sample b

attachWith :: Reflex t => (a -> b -> c) -> Behavior t a -> Event t b -> Event t c
attachWith f = attachWithMaybe $ \a b -> Just $ f a b

attach :: Reflex t => Behavior t a -> Event t b -> Event t (a, b)
attach = attachWith (,)

onceE :: (Reflex t, MonadHold t m, MonadFix m) => Event t a -> m (Event t a)
onceE = headE

headE :: (Reflex t, MonadHold t m, MonadFix m) => Event t a -> m (Event t a)
headE e = do
  rec be <- hold e $ fmap (const never) e'
      let e' = switch be
      e' `seq` return ()
  return e'

tailE :: (Reflex t, MonadHold t m, MonadFix m) => Event t a -> m (Event t a)
tailE e = liftM snd $ headTailE e

headTailE :: (Reflex t, MonadHold t m, MonadFix m) => Event t a -> m (Event t a, Event t a)
headTailE e = do
  eHead <- headE e
  be <- hold never $ fmap (const e) eHead
  return (eHead, switch be)

splitE :: Reflex t => Event t (a, b) -> (Event t a, Event t b)
splitE e = (fmap fst e, fmap snd e)

traceEvent :: (Reflex t, Show a) => String -> Event t a -> Event t a
traceEvent s = traceEventWith $ \x -> s <> ": " <> show x

traceEventWith :: Reflex t => (a -> String) -> Event t a -> Event t a
traceEventWith f = push $ \x -> trace (f x) $ return $ Just x

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

eitherToDSum :: Either a b -> DSum (EitherTag a b)
eitherToDSum = \case
  Left a -> LeftTag :=> a
  Right b -> RightTag :=> b

dsumToEither :: DSum (EitherTag a b) -> Either a b
dsumToEither = \case
  LeftTag :=> a -> Left a
  RightTag :=> b -> Right b

dmapToThese :: DMap (EitherTag a b) -> Maybe (These a b)
dmapToThese m = case (DMap.lookup LeftTag m, DMap.lookup RightTag m) of
  (Nothing, Nothing) -> Nothing
  (Just a, Nothing) -> Just $ This a
  (Nothing, Just b) -> Just $ That b
  (Just a, Just b) -> Just $ These a b

appendEvents :: (Reflex t, Monoid a) => Event t a -> Event t a -> Event t a
appendEvents e1 e2 = fmap (mergeThese mappend) $ align e1 e2

sequenceThese :: Monad m => These (m a) (m b) -> m (These a b)
sequenceThese t = case t of
  This ma -> liftM This ma
  These ma mb -> liftM2 These ma mb
  That mb -> liftM That mb

instance (Semigroup a, Reflex t) => Monoid (Event t a) where
  mempty = never
  mappend a b = mconcat [a, b]
  mconcat = fmap sconcat . mergeList

mergeWith :: Reflex t => (a -> a -> a) -> [Event t a] -> Event t a
mergeWith f es = fmap (Prelude.foldl1 f . map (\(Const2 _ :=> v) -> v) . DMap.toList) $ merge $ DMap.fromList $ map (\(k, v) -> WrapArg (Const2 k) :=> v) $ zip [0 :: Int ..] es

leftmost :: Reflex t => [Event t a] -> Event t a
leftmost = mergeWith const

mergeList :: Reflex t => [Event t a] -> Event t (NonEmpty a)
mergeList [] = never
mergeList es = mergeWith (<>) $ map (fmap (:|[])) es

mergeMap :: (Reflex t, Ord k) => Map k (Event t a) -> Event t (Map k a)
mergeMap = fmap dmapToMap . merge . mapWithFunctorToDMap

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

gate :: Reflex t => Behavior t Bool -> Event t a -> Event t a
gate = attachWithMaybe $ \allow a -> if allow then Just a else Nothing
