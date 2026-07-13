{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Reflex.TestPlan
  ( TestPlan(..)

  , TestCase (..)
  , TestMeta (..)
  , emptyMeta
  , testE, testB
  , testE', testB'
  , xfail
  , TestE, TestB

  , planList

  ) where

import Control.DeepSeq
import Control.Monad.Fix
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Word
import Reflex.Class


import Prelude

class (Reflex t, MonadHold t m, MonadFix m) => TestPlan t m where
  -- | Specify a plan of an input Event firing
  -- Occurrences must be in the future (i.e. Time > 0)
  -- Initial specification is

  plan :: [(Word, a)] -> m (Event t a)


planList :: TestPlan t m => [a] -> m (Event t a)
planList xs = plan $ zip [1..] xs

type TestE a = forall t m. TestPlan t m => m (Event t a)
type TestB a = forall t m. TestPlan t m => m (Behavior t a)

data TestMeta v = TestMeta
  { expectedOutput   :: Maybe (IntMap v)
    -- ^ When 'Nothing', the Pure implementation is used as the reference.
  , expectedFailures :: [String]
    -- ^ Names of implementations expected to diverge from the reference output.
  }

emptyMeta :: TestMeta v
emptyMeta = TestMeta Nothing []

data TestCase  where
  TestE  :: (Show a, Eq a, NFData a) => TestMeta (Maybe a) -> TestE a -> TestCase
  TestB  :: (Show a, Eq a, NFData a) => TestMeta a         -> TestB a -> TestCase

-- Helpers to declare test cases
testE :: (Eq a, Show a, NFData a) => String -> TestE a -> (String, TestCase)
testE name test = (name, TestE emptyMeta test)

testB :: (Eq a, Show a, NFData a) => String -> TestB a -> (String, TestCase)
testB name test = (name, TestB emptyMeta test)

-- | Declare an Event test case with a pinned expected output, given as the
-- frames at which the Event fires paired with the value fired at each.
testE' :: (Eq a, Show a, NFData a) => String -> [(Int, a)] -> TestE a -> (String, TestCase)
testE' name occurrences test =
  (name, TestE emptyMeta { expectedOutput = Just (Just <$> IntMap.fromList occurrences) } test)

-- | Declare a Behavior test case with a pinned expected output, given as the
-- value sampled at every frame from 0 to the last relevant frame.
testB' :: (Eq a, Show a, NFData a) => String -> [(Int, a)] -> TestB a -> (String, TestCase)
testB' name values test =
  (name, TestB emptyMeta { expectedOutput = Just (IntMap.fromList values) } test)

-- | Mark the named implementations as expected to diverge from the reference
-- output for this test case.
xfail :: [String] -> (String, TestCase) -> (String, TestCase)
xfail implementations (name, TestE meta test) =
  (name, TestE meta { expectedFailures = implementations } test)
xfail implementations (name, TestB meta test) =
  (name, TestB meta { expectedFailures = implementations } test)
