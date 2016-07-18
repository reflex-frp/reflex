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
  , testE, testB
  , TestE, TestB

  , planList

  ) where

import Control.DeepSeq
import Control.Monad.Fix
import Data.Word
import Reflex.Class


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
  TestE  :: (Show a, Eq a, NFData a) => TestE a -> TestCase
  TestB  :: (Show a, Eq a, NFData a) => TestB a -> TestCase

-- Helpers to declare test cases
testE :: (Eq a, Show a, NFData a) => String -> TestE a -> (String, TestCase)
testE name test = (name, TestE test)

testB :: (Eq a, Show a, NFData a) => String -> TestB a -> (String, TestCase)
testB name test = (name, TestB test)

