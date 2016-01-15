{-# LANGUAGE FunctionalDependencies, BangPatterns, UndecidableInstances, ConstraintKinds, GADTs, ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, RankNTypes, RecursiveDo, FlexibleContexts, StandaloneDeriving #-}
module Reflex.TestPlan
  ( TestPlan(..)

  , TestCase (..)
  , testE, testB
  , TestE, TestB

  , planList

  ) where

import Reflex.Class
import Control.Monad.Fix

import qualified Data.IntMap as IntMap
import Data.IntMap

import Prelude

class (Reflex t, MonadHold t m, MonadFix m) => TestPlan t m where
  -- | Speicify a plan of an input Event firing
  -- Occurances must be in the future (i.e. Time > 0)
  -- Initial specification is

  plan :: [(Word, a)] -> m (Event t a)


planList :: TestPlan t m => [a] -> m (Event t a)
planList xs = plan $ zip [1..] xs

type TestE a = forall t m. TestPlan t m => m (Event t a)
type TestB a = forall t m. TestPlan t m => m (Behavior t a)

data TestCase  where
  TestE  :: (Show a, Eq a) => TestE a -> TestCase
  TestB  :: (Show a, Eq a) => TestB a -> TestCase

-- Helpers to declare test cases
testE :: (Eq a, Show a) => String -> TestE a -> (String, TestCase)
testE name test = (name, TestE test)

testB :: (Eq a, Show a) => String -> TestB a -> (String, TestCase)
testB name test = (name, TestB test)

