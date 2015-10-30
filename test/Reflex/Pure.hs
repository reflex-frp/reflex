{- | This module provides a pure implementation of Reflex, which is intended
to serve as a reference for the semantics of the Reflex class.  All implementations
of Reflex should produce the same results as this implementation, although performance
and laziness/strictness may differ.
-}

{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, EmptyDataDecls, InstanceSigs #-}
module Reflex.Pure where

import Reflex.Class
import Data.Functor.Misc

import Control.Monad
import Data.MemoTrie
import Data.Dependent.Map (DMap, GCompare)
import qualified Data.Dependent.Map as DMap

data Pure t

-- | The Enum instance of t must be dense: for all x :: t, there must not exist
--   any y :: t such that pred x < y < x. The HasTrie instance will be used exclusively
--   to memoize functions of t, not for any of its other capabilities.
instance (Enum t, HasTrie t, Ord t) => Reflex (Pure t) where

  newtype Behavior (Pure t) a = Behavior { unBehavior :: t -> a }
  newtype Event (Pure t) a    = Event { unEvent :: t -> Maybe a }

  type PushM (Pure t) = (->) t
  type PullM (Pure t) = (->) t

  never :: Event (Pure t) a
  never = Event $ \_ -> Nothing

  constant :: a -> Behavior (Pure t) a
  constant x = Behavior $ \_ -> x

  push :: (a -> PushM (Pure t) (Maybe b)) -> Event (Pure t) a -> Event (Pure t) b
  push f e = Event $ memo $ \t -> unEvent e t >>= \o -> f o t

  pull :: PullM (Pure t) a -> Behavior (Pure t) a
  pull = Behavior . memo

  merge :: GCompare k => DMap (WrapArg (Event (Pure t)) k) -> Event (Pure t) (DMap k)
  merge events = Event $ memo $ \t ->
    let currentOccurrences = unwrapDMapMaybe (($ t) . unEvent) events
    in if DMap.null currentOccurrences
       then Nothing
       else Just currentOccurrences

  fan :: GCompare k => Event (Pure t) (DMap k) -> EventSelector (Pure t) k
  fan e = EventSelector $ \k -> Event $ \t -> unEvent e t >>= DMap.lookup k

  switch :: Behavior (Pure t) (Event (Pure t) a) -> Event (Pure t) a
  switch b = Event $ memo $ \t -> unEvent (unBehavior b t) t

  coincidence :: Event (Pure t) (Event (Pure t) a) -> Event (Pure t) a
  coincidence e = Event $ memo $ \t -> unEvent e t >>= \o -> unEvent o t

instance Ord t => MonadSample (Pure t) ((->) t) where

  sample :: Behavior (Pure t) a -> (t -> a)
  sample = unBehavior

instance (Enum t, HasTrie t, Ord t) => MonadHold (Pure t) ((->) t) where

  hold :: a -> Event (Pure t) a -> (t -> (Behavior (Pure t) a))
  hold initialValue e initialTime = Behavior f
    where f = memo $ \sampleTime ->
            -- Really, the sampleTime should never be prior to the initialTime,
            -- because that would mean the Behavior is being sampled before being created.
            if sampleTime <= initialTime
            then initialValue
            else let lastTime = pred sampleTime
                 in case unEvent e lastTime of
                   Nothing -> f lastTime
                   Just x  -> x
