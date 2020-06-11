{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reflex.Plan.Pure where

import Reflex
import Reflex.Pure
import Reflex.TestPlan

import Control.Applicative
import Control.Monad.Fix
import Control.Monad.State
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Data.Bifunctor
import Data.Maybe
import Data.Monoid
import Prelude


mapToPureEvent :: IntMap a -> Event (Pure Int) a
mapToPureEvent m = Event $ flip IntMap.lookup m

type TimeM = (->) Int
newtype PurePlan a = PurePlan { unPlan :: StateT IntSet TimeM a } deriving (Functor, Applicative, Monad, MonadFix)

liftPlan :: TimeM a -> PurePlan a
liftPlan = PurePlan . lift

instance MonadHold (Pure Int) PurePlan where
  hold initial  = liftPlan . hold initial
  holdDyn initial = liftPlan . holdDyn initial
  holdIncremental initial = liftPlan . holdIncremental initial
  buildDynamic getInitial = liftPlan . buildDynamic getInitial
  headE = liftPlan . headE
  now = liftPlan now

instance MonadSample (Pure Int) PurePlan where
  sample = liftPlan . sample


instance TestPlan (Pure Int) PurePlan where
  plan occs = do
    PurePlan . modify $ IntSet.union (IntMap.keysSet m)
    return $ mapToPureEvent m
      where m = IntMap.fromList (first fromIntegral <$> occs)

runPure :: PurePlan a -> (a, IntSet)
runPure (PurePlan p) = runStateT p mempty $ 0

relevantTimes :: IntSet -> IntSet
relevantTimes occs = IntSet.fromList [0..l + 1]
  where l = fromMaybe 0 (fst <$> IntSet.maxView occs)

testBehavior :: (Behavior (Pure Int) a, IntSet) -> IntMap a
testBehavior (b, occs) = IntMap.fromSet (sample b) (relevantTimes occs)

testEvent :: (Event (Pure Int) a, IntSet) -> IntMap (Maybe a)
testEvent (Event readEvent, occs) = IntMap.fromSet readEvent (relevantTimes occs)






